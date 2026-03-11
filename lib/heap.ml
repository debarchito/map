type gen = Young | Old

type gc_event =
  | MinorStart
  | MinorEnd   of { promoted : int }
  | MajorMark  of { steps : int }
  | MajorSweep of { steps : int; freed : int }
  | MajorEnd

type stats = {
  young_used  : int;
  young_total : int;
  old_used    : int;
  old_total   : int;
  n_chunks    : int;
  alloc_count : int;
}

type obj_info = {
  addr   : int;
  tag    : int;
  size   : int;
  gen    : gen;
  marked : bool;
  fwd    : int;
  fields : Value.t array;
}

let tag_free    = 0
let tag_forward = 1

module type HEAP_TRACER = sig
  type ctx
  val on_alloc      : ctx -> addr:int -> size:int -> tag:int -> unit
  val on_free       : ctx -> addr:int -> unit
  val on_promote    : ctx -> addr:int -> unit
  val on_gc         : ctx -> gc_event -> unit
  val on_write_slot : ctx -> chunk_idx:int -> slot:int -> Value.t -> unit
end

module NoHeapTracer = struct
  type ctx = unit
  let on_alloc      () ~addr:_ ~size:_ ~tag:_ = ()
  let on_free       () ~addr:_                = ()
  let on_promote    () ~addr:_               = ()
  let on_gc         () _                     = ()
  let on_write_slot () ~chunk_idx:_ ~slot:_ _ = ()
end

module FullHeapTracer = struct
  type ctx = {
    mutable allocs   : (int * int * int) list;
    mutable frees    : int list;
    mutable promotes : int list;
    mutable events   : gc_event list;
    metadata         : bytes;
    chunk_size       : int;
    sample_rate      : int;
    mutable tick     : int;
  }
  let make ~max_chunks ~chunk_size ~sample_rate =
    { allocs      = [];
      frees       = [];
      promotes    = [];
      events      = [];
      metadata    = Bytes.make (max_chunks * chunk_size) '\000';
      chunk_size;
      sample_rate;
      tick        = 0 }
  let on_alloc      ctx ~addr ~size ~tag = ctx.allocs   <- (addr, size, tag) :: ctx.allocs
  let on_free       ctx ~addr           = ctx.frees    <- addr :: ctx.frees
  let on_promote    ctx ~addr           = ctx.promotes <- addr :: ctx.promotes
  let on_gc         ctx event           = ctx.events   <- event :: ctx.events
  let on_write_slot ctx ~chunk_idx ~slot v =
    ctx.tick <- ctx.tick + 1;
    if ctx.tick mod ctx.sample_rate = 0 then
      Bytes.set ctx.metadata (chunk_idx * ctx.chunk_size + slot)
        (match v with
         | Value.MNull        -> '\x00'
         | Value.MInt _       -> '\x01'
         | Value.MFloat _     -> '\x02'
         | Value.MPtr _       -> '\x03'
         | Value.MNativePtr _ -> '\x04')
end

module type HEAP_INTF = sig
  type tracer_ctx

  type chunk = {
    data              : Value.t array;
    size              : int;
    mutable top       : int;
    gen               : gen;
    mutable free_list : int;
  }

  type t

  val create            : Config.t -> tracer_ctx -> t
  val alloc             : t -> size:int -> tag:int -> int
  val alloc_old         : t -> size:int -> tag:int -> int
  val free_old          : t -> int -> unit
  val read              : t -> int -> int -> Value.t
  val write             : t -> int -> int -> Value.t -> unit
  val get_tag           : t -> int -> int
  val get_size          : t -> int -> int
  val get_mark          : t -> int -> bool
  val get_fwd           : t -> int -> int
  val set_tag           : t -> int -> int -> unit
  val set_mark          : t -> int -> bool -> unit
  val set_fwd           : t -> int -> int -> unit
  val is_young          : t -> int -> bool
  val is_old            : t -> int -> bool
  val is_card_dirty     : t -> int -> bool
  val clear_card        : t -> int -> unit
  val needs_minor_gc    : t -> bool
  val reset_young       : t -> unit
  val iter_young_chunks     : t -> (int -> chunk -> unit) -> unit
  val iter_old_chunks       : t -> (int -> chunk -> unit) -> unit
  val iter_dirty_old_chunks : t -> (int -> chunk -> unit) -> unit
  val stats             : t -> stats
  val inspect           : t -> int -> obj_info
  val tracer            : t -> tracer_ctx
end

module Heap(H : HEAP_TRACER) : HEAP_INTF with type tracer_ctx = H.ctx = struct
  type tracer_ctx = H.ctx

  type chunk = {
    data              : Value.t array;
    size              : int;
    mutable top       : int;
    gen               : gen;
    mutable free_list : int;
  }

  type t = {
    mutable chunks      : chunk array;
    mutable n_chunks    : int;
    chunk_size          : int;
    chunk_shift         : int;
    chunk_mask          : int;
    mutable young       : int;
    mutable alloc_count : int;
    young_limit         : int;
    card_table          : bytes;
    tracer_ctx          : H.ctx;
  }

  let header_words = 4
  let hdr_tag_slot  = 0
  let hdr_size_slot = 1
  let hdr_mark_slot = 2
  let hdr_fwd_slot  = 3

  let chunk_of t addr = addr lsr t.chunk_shift
  let slot_of  t addr = addr land t.chunk_mask

  let is_young t addr = t.chunks.(chunk_of t addr).gen = Young
  let is_old   t addr = t.chunks.(chunk_of t addr).gen = Old

  let header_slot addr = addr - header_words

  let raw t addr slot =
    t.chunks.(chunk_of t addr).data.(slot_of t addr + slot)

  let raw_set t addr slot v =
    let ci = chunk_of t addr in
    let s  = slot_of  t addr + slot in
    t.chunks.(ci).data.(s) <- v;
    H.on_write_slot t.tracer_ctx ~chunk_idx:ci ~slot:s v

  let get_tag  t addr = Value.as_int (raw t (header_slot addr) hdr_tag_slot)
  let get_size t addr = Value.as_int (raw t (header_slot addr) hdr_size_slot)
  let get_mark t addr = Value.as_int (raw t (header_slot addr) hdr_mark_slot) <> 0
  let get_fwd  t addr = Value.as_int (raw t (header_slot addr) hdr_fwd_slot)

  let set_tag  t addr v = raw_set t (header_slot addr) hdr_tag_slot  (Value.MInt v)
  let set_mark t addr v = raw_set t (header_slot addr) hdr_mark_slot (Value.MInt (if v then 1 else 0))
  let set_fwd  t addr v = raw_set t (header_slot addr) hdr_fwd_slot  (Value.MInt v)

  let set_header t addr ~tag ~size =
    let hs = header_slot addr in
    raw_set t hs hdr_tag_slot  (Value.MInt tag);
    raw_set t hs hdr_size_slot (Value.MInt size);
    raw_set t hs hdr_mark_slot (Value.MInt 0);
    raw_set t hs hdr_fwd_slot  (Value.MInt (-1))

  let mark_card t addr =
    Bytes.set t.card_table (chunk_of t addr) '\001'

  let clear_card t chunk_idx =
    Bytes.set t.card_table chunk_idx '\000'

  let is_card_dirty t chunk_idx =
    Bytes.get t.card_table chunk_idx <> '\000'

  let write_barrier t addr value =
    match value with
    | Value.MPtr _ | Value.MNativePtr _ ->
      if is_old t addr then mark_card t addr
    | _ -> ()

  let check_bounds t addr field =
    let sz = get_size t addr in
    if field < 0 || field >= sz then
      raise (Value.EBoundsError
        (Printf.sprintf "heap field %d out of bounds (object size %d)" field sz))

  let read t addr field =
    check_bounds t addr field;
    raw t addr field

  let write t addr field value =
    check_bounds t addr field;
    write_barrier t addr value;
    raw_set t addr field value

  let make_chunk size gen = {
    data      = Array.make size Value.MNull;
    size;
    top       = 0;
    gen;
    free_list = -1;
  }

  let is_power_of_two n = n > 0 && n land (n - 1) = 0

  let log2 n =
    let rec go acc x = if x = 1 then acc else go (acc + 1) (x lsr 1) in
    go 0 n

  let create (cfg : Config.t) ctx =
    if not (is_power_of_two cfg.chunk_size) then
      raise (Invalid_argument
        (Printf.sprintf "chunk_size must be a power of two, got %d" cfg.chunk_size));
    let shift = log2 cfg.chunk_size in
    let mask  = cfg.chunk_size - 1 in
    { chunks      = Array.init cfg.max_chunks (fun _ -> make_chunk cfg.chunk_size Young);
      n_chunks    = 1;
      chunk_size  = cfg.chunk_size;
      chunk_shift = shift;
      chunk_mask  = mask;
      young       = 0;
      alloc_count = 0;
      young_limit = cfg.young_limit;
      card_table  = Bytes.make cfg.max_chunks '\000';
      tracer_ctx  = ctx }

  let add_chunk t gen =
    if t.n_chunks >= Array.length t.chunks then
      raise (Value.EAllocError "heap exhausted: max_chunks reached");
    let c   = make_chunk t.chunk_size gen in
    let idx = t.n_chunks in
    t.chunks.(idx) <- c;
    t.n_chunks     <- t.n_chunks + 1;
    idx

  let alloc t ~size ~tag =
    if size <= 0 then
      raise (Value.EAllocError
        (Printf.sprintf "alloc: size must be > 0, got %d" size));
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise (Value.EAllocError
        (Printf.sprintf "alloc: object size %d exceeds chunk_size %d" size t.chunk_size));
    let yc = t.chunks.(t.young) in
    if yc.top + needed > yc.size then begin
      let idx = add_chunk t Young in
      t.young <- idx
    end;
    let yc   = t.chunks.(t.young) in
    let addr = t.young * t.chunk_size + yc.top + header_words in
    set_header t addr ~tag ~size;
    yc.top        <- yc.top + needed;
    t.alloc_count <- t.alloc_count + 1;
    H.on_alloc t.tracer_ctx ~addr ~size ~tag;
    addr

  let coalesce_chunk t ci =
    let c = t.chunks.(ci) in
    if c.free_list = -1 then ()
    else begin
      let free_slots = ref [] in
      let cur = ref c.free_list in
      while !cur <> -1 do
        free_slots := !cur :: !free_slots;
        cur := Value.as_int c.data.(!cur)
      done;
      let arr = Array.of_list (List.sort compare !free_slots) in
      let n   = Array.length arr in
      let i = ref 0 in
      while !i < n do
        let start = arr.(!i) in
        let addr  = ci * t.chunk_size + start + header_words in
        let sz    = get_size t addr in
        let total = ref (header_words + sz) in
        let j     = ref (!i + 1) in
        while !j < n && arr.(!j) = start + !total do
          let next_addr = ci * t.chunk_size + arr.(!j) + header_words in
          let next_sz   = get_size t next_addr in
          total := !total + header_words + next_sz;
          incr j
        done;
        let merged_size = !total - header_words in
        set_header t addr ~tag:tag_free ~size:merged_size;
        arr.(!i) <- start;
        i := !j
      done;
      c.free_list <- -1;
      for k = n - 1 downto 0 do
        let slot = arr.(k) in
        c.data.(slot) <- Value.MInt c.free_list;
        c.free_list   <- slot
      done
    end

  let alloc_old t ~size ~tag =
    if size <= 0 then
      raise (Value.EAllocError
        (Printf.sprintf "alloc_old: size must be > 0, got %d" size));
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise (Value.EAllocError
        (Printf.sprintf "alloc_old: object size %d exceeds chunk_size %d" size t.chunk_size));
    let found = ref (-1) in
    let ci    = ref 0 in
    while !found = -1 && !ci < t.n_chunks do
      let c = t.chunks.(!ci) in
      if c.gen = Old then begin
        let prev = ref (-1) in
        let cur  = ref c.free_list in
        while !found = -1 && !cur <> -1 do
          let free_addr = !ci * t.chunk_size + !cur + header_words in
          let free_size = get_size t free_addr in
          if free_size >= size then begin
            let next = Value.as_int c.data.(!cur) in
            if !prev = -1 then c.free_list    <- next
            else               c.data.(!prev) <- Value.MInt next;
            set_header t free_addr ~tag ~size;
            for i = 0 to size - 1 do
              c.data.(!cur + header_words + i) <- Value.MNull
            done;
            found := free_addr
          end else begin
            prev := !cur;
            cur  := Value.as_int c.data.(!cur)
          end
        done;
        if !found = -1 && c.free_list <> -1 then begin
          coalesce_chunk t !ci;
          let cur2 = ref c.free_list in
          while !found = -1 && !cur2 <> -1 do
            let free_addr = !ci * t.chunk_size + !cur2 + header_words in
            let free_size = get_size t free_addr in
            if free_size >= size then begin
              let next    = Value.as_int c.data.(!cur2) in
              c.free_list <- next;
              set_header t free_addr ~tag ~size;
              for i = 0 to size - 1 do
                c.data.(!cur2 + header_words + i) <- Value.MNull
              done;
              found := free_addr
            end else
              cur2 := Value.as_int c.data.(!cur2)
          done
        end
      end;
      incr ci
    done;
    if !found = -1 then begin
      ci := 0;
      while !found = -1 && !ci < t.n_chunks do
        let c = t.chunks.(!ci) in
        if c.gen = Old && c.top + needed <= c.size then begin
          let addr = !ci * t.chunk_size + c.top + header_words in
          set_header t addr ~tag ~size;
          c.top  <- c.top + needed;
          found  := addr
        end;
        incr ci
      done
    end;
    if !found = -1 then begin
      let idx  = add_chunk t Old in
      let c    = t.chunks.(idx) in
      let addr = idx * t.chunk_size + c.top + header_words in
      set_header t addr ~tag ~size;
      c.top  <- c.top + needed;
      found  := addr
    end;
    H.on_alloc t.tracer_ctx ~addr:!found ~size ~tag;
    !found

  let free_old t addr =
    let c_idx = chunk_of t addr in
    let s     = slot_of  t addr in
    let c     = t.chunks.(c_idx) in
    let size  = get_size t addr in
    for i = 0 to size - 1 do
      c.data.(s + i) <- Value.MNull
    done;
    let hdr_s       = s - header_words in
    set_header t addr ~tag:tag_free ~size:(size);
    c.data.(hdr_s)  <- Value.MInt c.free_list;
    c.free_list     <- hdr_s;
    H.on_free t.tracer_ctx ~addr

  let needs_minor_gc t =
    t.alloc_count >= t.young_limit

  let reset_young t =
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then begin
        c.top <- 0;
        Array.fill c.data 0 (Array.length c.data) Value.MNull
      end
    done;
    t.alloc_count <- 0

  let iter_young_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Young then f i t.chunks.(i)
    done

  let iter_old_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Old then f i t.chunks.(i)
    done

  let iter_dirty_old_chunks t f =
    for i = 0 to t.n_chunks - 1 do
      if t.chunks.(i).gen = Old && is_card_dirty t i then f i t.chunks.(i)
    done

  let stats t =
    let young_used  = ref 0 in
    let young_total = ref 0 in
    let old_used    = ref 0 in
    let old_total   = ref 0 in
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      match c.gen with
      | Young ->
        young_used  := !young_used  + c.top;
        young_total := !young_total + c.size
      | Old ->
        old_used    := !old_used    + c.top;
        old_total   := !old_total   + c.size
    done;
    { young_used  = !young_used;
      young_total = !young_total;
      old_used    = !old_used;
      old_total   = !old_total;
      n_chunks    = t.n_chunks;
      alloc_count = t.alloc_count }

  let inspect t addr =
    let size   = get_size t addr in
    let fields = Array.init size (fun i -> raw t addr i) in
    { addr;
      tag    = get_tag  t addr;
      size;
      gen    = t.chunks.(chunk_of t addr).gen;
      marked = get_mark t addr;
      fwd    = get_fwd  t addr;
      fields }

  let tracer t = t.tracer_ctx
end

module FastHeap  = Heap(NoHeapTracer)
module DebugHeap = Heap(FullHeapTracer)
