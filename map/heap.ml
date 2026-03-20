open Map_core
include Sigs.Heap

module No_tracer = struct
  type ctx = unit

  let on_alloc () ~addr:_ ~size:_ ~tag:_ = ()
  let on_free () ~addr:_ = ()
  let on_promote () ~addr:_ = ()
  let on_gc () _ = ()
  let on_write_slot () ~chunk_idx:_ ~slot:_ _ = ()
end

module Full_tracer = struct
  type ctx = {
    mutable allocs : (int * int * int) list;
    mutable frees : int list;
    mutable promotes : int list;
    mutable events : event list;
    metadata : bytes;
    chunk_size : int;
    sample_rate : int;
    mutable tick : int;
  }

  let make ~max_chunks ~chunk_size ~sample_rate =
    {
      allocs = [];
      frees = [];
      promotes = [];
      events = [];
      metadata = Bytes.make (max_chunks * chunk_size) '\000';
      chunk_size;
      sample_rate;
      tick = 0;
    }

  let on_alloc ctx ~addr ~size ~tag =
    ctx.allocs <- (addr, size, tag) :: ctx.allocs

  let on_free ctx ~addr = ctx.frees <- addr :: ctx.frees
  let on_promote ctx ~addr = ctx.promotes <- addr :: ctx.promotes
  let on_gc ctx ev = ctx.events <- ev :: ctx.events

  let on_write_slot ctx ~chunk_idx ~slot v =
    ctx.tick <- ctx.tick + 1;
    if ctx.tick mod ctx.sample_rate = 0 then
      Bytes.set ctx.metadata
        ((chunk_idx * ctx.chunk_size) + slot)
        (match v with
        | Value.Nil -> '\x00'
        | Value.Int _ -> '\x01'
        | Value.Float _ -> '\x02'
        | Value.Ptr _ -> '\x03'
        | Value.NativePtr _ -> '\x04'
        | Value.NativeFun _ -> '\x05'
        | Value.NativeFib _ -> '\x06')
end

let log2 n =
  let rec go acc x = if x = 1 then acc else go (acc + 1) (x lsr 1) in
  go 0 n

module Make (H : TRACER) : S with type tracer_ctx = H.ctx = struct
  type tracer_ctx = H.ctx

  let num_classes = 10
  let header_words = 1
  let min_free_size = 3

  let size_class sz =
    match sz with
    | 2 | 3 -> 0
    | 4 -> 1
    | 5 | 6 -> 2
    | 7 | 8 -> 3
    | n when n <= 16 -> 4
    | n when n <= 32 -> 5
    | n when n <= 64 -> 6
    | n when n <= 128 -> 7
    | n when n <= 256 -> 8
    | _ -> 9

  type chunk = {
    data : Value.t array;
    size : int;
    mutable top : int;
    gen : gen;
    mutable free_list : int array;
  }

  type t = {
    mutable chunks : chunk array;
    mutable n_chunks : int;
    chunk_size : int;
    chunk_shift : int;
    chunk_mask : int;
    mutable young : int;
    mutable alloc_count : int;
    young_limit : int;
    card_table : bytes;
    tracer_ctx : H.ctx;
  }

  let chunk_of t addr = addr lsr t.chunk_shift
  let slot_of t addr = addr land t.chunk_mask
  let is_young t addr = t.chunks.(chunk_of t addr).gen = Young
  let is_old t addr = t.chunks.(chunk_of t addr).gen = Old

  let encode_header ~tag ~size ~mark =
    (size lsl 10) lor (tag lsl 2) lor if mark then 2 else 0

  let get_raw_header t addr =
    let c = t.chunks.(chunk_of t addr) in
    let s = slot_of t addr - header_words in
    match c.data.(s) with
    | Value.Int h -> h
    | _ -> raise (Exception.Alloc_error "heap: corrupted object header")

  let get_tag t addr = (get_raw_header t addr lsr 2) land 0xFF
  let get_size t addr = get_raw_header t addr lsr 10
  let get_mark t addr = get_raw_header t addr land 2 <> 0

  let get_fwd t addr =
    if get_tag t addr = Tag.forward then
      match t.chunks.(chunk_of t addr).data.(slot_of t addr) with
      | Value.Int f -> f
      | _ -> -1
    else -1

  let set_raw_header t addr h =
    let ci = chunk_of t addr in
    let s = slot_of t addr - header_words in
    t.chunks.(ci).data.(s) <- Value.Int h;
    H.on_write_slot t.tracer_ctx ~chunk_idx:ci ~slot:s (Value.Int h)

  let set_header t addr ~tag ~size ~mark =
    set_raw_header t addr (encode_header ~tag ~size ~mark)

  let set_tag t addr v =
    set_header t addr ~tag:v ~size:(get_size t addr) ~mark:(get_mark t addr)

  let set_mark t addr v =
    set_header t addr ~tag:(get_tag t addr) ~size:(get_size t addr) ~mark:v

  let set_fwd t addr v =
    set_header t addr ~tag:Tag.forward ~size:(get_size t addr)
      ~mark:(get_mark t addr);
    let ci = chunk_of t addr in
    let s = slot_of t addr in
    t.chunks.(ci).data.(s) <- Value.Int v;
    H.on_write_slot t.tracer_ctx ~chunk_idx:ci ~slot:s (Value.Int v)

  let mark_card t addr = Bytes.set t.card_table (chunk_of t addr) '\001'
  let clear_card t ci = Bytes.set t.card_table ci '\000'
  let is_card_dirty t ci = Bytes.get t.card_table ci <> '\000'

  let write_barrier t addr value =
    match value with
    | Value.Ptr _ | Value.NativePtr _ -> if is_old t addr then mark_card t addr
    | _ -> ()

  let check_bounds t addr field =
    let sz = get_size t addr in
    if field < 0 || field >= sz then
      raise
        (Exception.Bounds_error
           (Printf.sprintf "heap field %d out of bounds (size %d)" field sz))

  let read t addr field =
    check_bounds t addr field;
    t.chunks.(chunk_of t addr).data.(slot_of t addr + field)

  let write t addr field value =
    check_bounds t addr field;
    write_barrier t addr value;
    let ci = chunk_of t addr in
    let s = slot_of t addr + field in
    t.chunks.(ci).data.(s) <- value;
    H.on_write_slot t.tracer_ctx ~chunk_idx:ci ~slot:s value

  let make_chunk size gen =
    {
      data = Array.make size Value.Nil;
      size;
      top = 0;
      gen;
      free_list = Array.make num_classes (-1);
    }

  let create (cfg : Config.Heap.t) ctx =
    let shift = log2 cfg.chunk_size in
    let mask = cfg.chunk_size - 1 in
    {
      chunks =
        Array.init cfg.max_chunks (fun _ -> make_chunk cfg.chunk_size Young);
      n_chunks = 1;
      chunk_size = cfg.chunk_size;
      chunk_shift = shift;
      chunk_mask = mask;
      young = 0;
      alloc_count = 0;
      young_limit = cfg.young_limit;
      card_table = Bytes.make cfg.max_chunks '\000';
      tracer_ctx = ctx;
    }

  let add_chunk t gen =
    if t.n_chunks >= Array.length t.chunks then
      raise (Exception.Alloc_error "heap exhausted: max_chunks reached");
    let idx = t.n_chunks in
    t.chunks.(idx) <- make_chunk t.chunk_size gen;
    t.n_chunks <- t.n_chunks + 1;
    idx

  let fl_get_next (c : chunk) slot =
    match c.data.(slot + 1) with Value.Int n -> n | _ -> -1

  let fl_get_prev (c : chunk) slot =
    match c.data.(slot + 2) with Value.Int n -> n | _ -> -1

  let fl_set_next (c : chunk) slot v = c.data.(slot + 1) <- Value.Int v
  let fl_set_prev (c : chunk) slot v = c.data.(slot + 2) <- Value.Int v

  let fl_insert (c : chunk) cls slot size =
    c.data.(slot) <- Value.Int (encode_header ~tag:Tag.free ~size ~mark:false);
    c.data.(slot + size) <- Value.Int size;
    let old_head = c.free_list.(cls) in
    fl_set_next c slot old_head;
    fl_set_prev c slot (-1);
    if old_head <> -1 then fl_set_prev c old_head slot;
    c.free_list.(cls) <- slot

  let fl_remove (c : chunk) cls slot =
    let next = fl_get_next c slot in
    let prev = fl_get_prev c slot in
    if prev = -1 then c.free_list.(cls) <- next else fl_set_next c prev next;
    if next <> -1 then fl_set_prev c next prev

  let alloc t ~size ~tag =
    let size = max min_free_size size in
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise
        (Exception.Alloc_error
           (Printf.sprintf "alloc: size %d exceeds chunk_size" size));
    let yc = t.chunks.(t.young) in
    if yc.top + needed > yc.size then t.young <- add_chunk t Young;
    let yc = t.chunks.(t.young) in
    let addr = (t.young * t.chunk_size) + yc.top + header_words in
    set_header t addr ~tag ~size ~mark:false;
    yc.top <- yc.top + needed;
    t.alloc_count <- t.alloc_count + 1;
    H.on_alloc t.tracer_ctx ~addr ~size ~tag;
    addr

  let alloc_old t ~size ~tag =
    let size = max min_free_size size in
    let needed = header_words + size in
    if needed > t.chunk_size then
      raise
        (Exception.Alloc_error
           (Printf.sprintf "alloc_old: size %d exceeds chunk_size" size));
    let found = ref (-1) in
    let start_cls = size_class size in
    let ci = ref 0 in
    while !found = -1 && !ci < t.n_chunks do
      let c = t.chunks.(!ci) in
      if c.gen = Old then begin
        let cls = ref start_cls in
        while !found = -1 && !cls < num_classes do
          let cur = ref c.free_list.(!cls) in
          while !found = -1 && !cur <> -1 do
            let h = match c.data.(!cur) with Value.Int h -> h | _ -> 0 in
            let free_size = h lsr 10 in
            let next = fl_get_next c !cur in
            if free_size >= size then begin
              fl_remove c !cls !cur;
              let remainder = free_size - size - header_words in
              let actual_size =
                if remainder >= min_free_size then begin
                  let new_slot = !cur + header_words + size in
                  fl_insert c (size_class remainder) new_slot remainder;
                  size
                end
                else free_size
              in
              let addr = (!ci * t.chunk_size) + !cur + header_words in
              set_header t addr ~tag ~size:actual_size ~mark:false;
              Array.fill c.data (!cur + header_words) actual_size Value.Nil;
              found := addr
            end
            else cur := next
          done;
          incr cls
        done
      end;
      incr ci
    done;
    if !found = -1 then begin
      ci := 0;
      while !found = -1 && !ci < t.n_chunks do
        let c = t.chunks.(!ci) in
        if c.gen = Old && c.top + needed <= c.size then begin
          let addr = (!ci * t.chunk_size) + c.top + header_words in
          set_header t addr ~tag ~size ~mark:false;
          c.top <- c.top + needed;
          found := addr
        end;
        incr ci
      done
    end;
    if !found = -1 then begin
      let idx = add_chunk t Old in
      let c = t.chunks.(idx) in
      let addr = (idx * t.chunk_size) + c.top + header_words in
      set_header t addr ~tag ~size ~mark:false;
      c.top <- c.top + needed;
      found := addr
    end;
    H.on_alloc t.tracer_ctx ~addr:!found ~size ~tag;
    !found

  let free_old t addr =
    let ci = chunk_of t addr in
    let c = t.chunks.(ci) in
    let slot = slot_of t addr - header_words in
    let size = get_size t addr in
    for i = 0 to size - 1 do
      match c.data.(slot + header_words + i) with
      | Value.NativePtr np -> Value.call_finalizer np
      | _ -> ()
    done;
    Array.fill c.data (slot + header_words) size Value.Nil;
    let mut_slot = ref slot in
    let mut_size = ref size in
    let next_slot = slot + header_words + size in
    if next_slot < c.top then begin
      let next_h = match c.data.(next_slot) with Value.Int h -> h | _ -> 0 in
      let next_tag = (next_h lsr 2) land 0xFF in
      if next_tag = Tag.free then begin
        let next_size = next_h lsr 10 in
        fl_remove c (size_class next_size) next_slot;
        mut_size := !mut_size + header_words + next_size
      end
    end;
    if slot > 0 then begin
      let footer_val =
        match c.data.(slot - 1) with Value.Int n -> n | _ -> -1
      in
      if footer_val >= min_free_size then begin
        let prev_size = footer_val in
        let prev_slot = slot - header_words - prev_size in
        if prev_slot >= 0 then begin
          let prev_h =
            match c.data.(prev_slot) with Value.Int h -> h | _ -> 0
          in
          let prev_tag = (prev_h lsr 2) land 0xFF in
          if prev_tag = Tag.free && prev_h lsr 10 = prev_size then begin
            fl_remove c (size_class prev_size) prev_slot;
            mut_slot := prev_slot;
            mut_size := !mut_size + header_words + prev_size
          end
        end
      end
    end;
    fl_insert c (size_class !mut_size) !mut_slot !mut_size;
    H.on_free t.tracer_ctx ~addr

  let run_finalizers_young t =
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then begin
        let pos = ref 0 in
        while !pos < c.top do
          let h = match c.data.(!pos) with Value.Int h -> h | _ -> 0 in
          let tag = (h lsr 2) land 0xFF in
          let size = max min_free_size (h lsr 10) in
          if tag <> Tag.free && tag <> Tag.forward then
            for f = 0 to size - 1 do
              match c.data.(!pos + header_words + f) with
              | Value.NativePtr np -> Value.call_finalizer np
              | _ -> ()
            done;
          pos := !pos + header_words + size
        done
      end
    done

  let needs_minor_gc t = t.alloc_count >= t.young_limit

  let reset_young t =
    run_finalizers_young t;
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then begin
        c.top <- 0;
        Array.fill c.data 0 c.size Value.Nil
      end
    done;
    t.alloc_count <- 0

  let chunk_size t = t.chunk_size

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

  let iter_chunk_objects t ci f =
    let c = t.chunks.(ci) in
    let pos = ref 0 in
    while !pos < c.top do
      let h = match c.data.(!pos) with Value.Int h -> h | _ -> 0 in
      let tag = (h lsr 2) land 0xFF in
      let size = max min_free_size (h lsr 10) in
      if tag <> Tag.free && tag <> Tag.forward then
        f ((ci * t.chunk_size) + !pos + header_words);
      pos := !pos + header_words + size
    done

  let iter_objects t f =
    for ci = 0 to t.n_chunks - 1 do
      iter_chunk_objects t ci f
    done

  let stats t =
    let yu = ref 0 in
    let yt = ref 0 in
    let ou = ref 0 in
    let ot = ref 0 in
    for i = 0 to t.n_chunks - 1 do
      let c = t.chunks.(i) in
      if c.gen = Young then (
        yu := !yu + c.top;
        yt := !yt + c.size)
      else (
        ou := !ou + c.top;
        ot := !ot + c.size)
    done;
    {
      young_used = !yu;
      young_total = !yt;
      young_limit = t.young_limit;
      old_used = !ou;
      old_total = !ot;
      n_chunks = t.n_chunks;
      alloc_count = t.alloc_count;
    }

  let inspect t addr =
    let size = get_size t addr in
    {
      addr;
      tag = get_tag t addr;
      size;
      gen = t.chunks.(chunk_of t addr).gen;
      marked = get_mark t addr;
      fwd = get_fwd t addr;
      fields = Array.init size (read t addr);
    }

  let tracer t = t.tracer_ctx
  let on_gc t ev = H.on_gc t.tracer_ctx ev
  let on_promote t addr = H.on_promote t.tracer_ctx ~addr
end

module Fast = Make (No_tracer)
module Debug = Make (Full_tracer)
