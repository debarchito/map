open Map.Core
open Map.Namespace

module Make(H : Map.Heap.S) = struct

  let bytes_per_slot = 7
  let slots_for n    = (n + bytes_per_slot - 1) / bytes_per_slot

  let registry     = Exception.create ()
  let e_bad_arg    = Exception.register registry ~name:"buffer/bad_arg"
  let e_bounds     = Exception.register registry ~name:"buffer/bounds"
  let e_bad_object = Exception.register registry ~name:"buffer/bad_object"

  let alloc heap capacity =
    let slots = slots_for capacity in
    let addr  = H.alloc heap ~size:(2 + slots) ~tag:Fold_core.Tag.buffer in
    H.write heap addr 0 (Value.Int capacity);
    H.write heap addr 1 (Value.Int 0);
    addr

  let capacity heap addr =
    match H.read heap addr 0 with
    | Value.Int n -> n
    | _           -> Exception.throw e_bad_object "buffer/capacity: bad object"

  let length heap addr =
    match H.read heap addr 1 with
    | Value.Int n -> n
    | _           -> Exception.throw e_bad_object "buffer/length: bad object"

  let set_length heap addr n =
    let cap = capacity heap addr in
    if n < 0 || n > cap then
      Exception.throw e_bounds "buffer/set_length: out of capacity";
    H.write heap addr 1 (Value.Int n)

  let read_byte heap addr i =
    let cap = capacity heap addr in
    if i < 0 || i >= cap then
      Exception.throw e_bounds "buffer/read_byte: out of bounds";
    let slot = i / bytes_per_slot in
    let off  = i mod bytes_per_slot in
    match H.read heap addr (2 + slot) with
    | Value.Int v -> (v lsr (off * 8)) land 0xFF
    | _           -> Exception.throw e_bad_object "buffer/read_byte: bad slot"

  let write_byte heap addr i byte =
    let cap = capacity heap addr in
    if i < 0 || i >= cap then
      Exception.throw e_bounds "buffer/write_byte: out of bounds";
    let slot = i / bytes_per_slot in
    let off  = i mod bytes_per_slot in
    let old  = match H.read heap addr (2 + slot) with
               | Value.Int v -> v | _ -> 0 in
    let mask = lnot (0xFF lsl (off * 8)) in
    let nv   = (old land mask) lor ((byte land 0xFF) lsl (off * 8)) in
    H.write heap addr (2 + slot) (Value.Int nv)

  let blit_from_bytes heap addr offset (src : bytes) src_off len =
    for i = 0 to len - 1 do
      write_byte heap addr (offset + i)
        (Char.code (Bytes.get src (src_off + i)))
    done

  let blit_to_bytes heap addr offset len =
    let dst = Bytes.make len '\000' in
    for i = 0 to len - 1 do
      Bytes.set dst i (Char.chr (read_byte heap addr (offset + i)))
    done;
    dst

  let copy heap src_addr src_off dst_addr dst_off len =
    for i = 0 to len - 1 do
      write_byte heap dst_addr (dst_off + i)
        (read_byte heap src_addr (src_off + i))
    done

  let fill heap addr offset len byte =
    for i = 0 to len - 1 do
      write_byte heap addr (offset + i) (byte land 0xFF)
    done

  let compare heap a_addr a_off b_addr b_off len =
    let result = ref 0 in
    let i      = ref 0 in
    while !result = 0 && !i < len do
      let a = read_byte heap a_addr (a_off + !i) in
      let b = read_byte heap b_addr (b_off + !i) in
      result := Int.compare a b;
      incr i
    done;
    !result

  module NS = Map.Namespace.Make(H)

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/buffer" [
        native "e_bad_arg"    (Value.Int e_bad_arg);
        native "e_bounds"     (Value.Int e_bounds);
        native "e_bad_object" (Value.Int e_bad_object);

        nif "make" (fun args ->
          match args with
          | [| Value.Int cap |] ->
            if cap < 0 then
              Exception.throw e_bad_arg "buffer/make: capacity must be >= 0";
            Value.Ptr (alloc heap cap)
          | _ -> Exception.throw e_bad_arg "buffer/make expects int");

        nif "capacity" (fun args ->
          match args with
          | [| Value.Ptr addr |] -> Value.Int (capacity heap addr)
          | _ -> Exception.throw e_bad_arg "buffer/capacity expects ptr");

        nif "length" (fun args ->
          match args with
          | [| Value.Ptr addr |] -> Value.Int (length heap addr)
          | _ -> Exception.throw e_bad_arg "buffer/length expects ptr");

        nif "set_length" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int n |] ->
            set_length heap addr n;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "buffer/set_length expects ptr int");

        nif "read_byte" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int i |] ->
            Value.Int (read_byte heap addr i)
          | _ -> Exception.throw e_bad_arg "buffer/read_byte expects ptr int");

        nif "write_byte" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int i; Value.Int byte |] ->
            write_byte heap addr i byte;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "buffer/write_byte expects ptr int int");

        nif "copy" (fun args ->
          match args with
          | [| Value.Ptr src; Value.Int src_off;
               Value.Ptr dst; Value.Int dst_off;
               Value.Int len |] ->
            if len < 0 then
              Exception.throw e_bad_arg "buffer/copy: length must be >= 0";
            copy heap src src_off dst dst_off len;
            Value.Nil
          | _ -> Exception.throw e_bad_arg
              "buffer/copy expects ptr int ptr int int");

        nif "fill" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int offset;
               Value.Int len;  Value.Int byte |] ->
            if len < 0 then
              Exception.throw e_bad_arg "buffer/fill: length must be >= 0";
            fill heap addr offset len byte;
            Value.Nil
          | _ -> Exception.throw e_bad_arg
              "buffer/fill expects ptr int int int");

        nif "compare" (fun args ->
          match args with
          | [| Value.Ptr a; Value.Int a_off;
               Value.Ptr b; Value.Int b_off;
               Value.Int len |] ->
            if len < 0 then
              Exception.throw e_bad_arg "buffer/compare: length must be >= 0";
            Value.Int (compare heap a a_off b b_off len)
          | _ -> Exception.throw e_bad_arg
              "buffer/compare expects ptr int ptr int int");
      ])

end
