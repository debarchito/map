open Core

module Heap = Sigs.Heap

module Make(H : Heap.S) = struct

  let words_for_bytes n = (n + 7) / 8

  let alloc_string heap s =
    let len  = String.length s in
    let nw   = words_for_bytes len in
    let addr = H.alloc heap ~size:(1 + nw) ~tag:Tag.string in
    H.write heap addr 0 (Value.Int len);
    for w = 0 to nw - 1 do
      let word = ref 0 in
      for b = 0 to 7 do
        let i = w * 8 + b in
        if i < len then
          word := !word lor (Char.code s.[i] lsl (56 - b * 8))
      done;
      H.write heap addr (1 + w) (Value.Int !word)
    done;
    Value.Ptr addr

  let read_string heap addr =
    match H.read heap addr 0 with
    | Value.Int len ->
      let buf = Bytes.create len in
      let nw  = words_for_bytes len in
      for w = 0 to nw - 1 do
        (match H.read heap addr (1 + w) with
         | Value.Int word ->
           for b = 0 to 7 do
             let i = w * 8 + b in
             if i < len then
               Bytes.set buf i (Char.chr ((word lsr (56 - b * 8)) land 0xFF))
           done
         | _ -> ())
      done;
      Bytes.to_string buf
    | _ -> ""

  let expect_str heap v =
    match v with
    | Value.Ptr addr when H.get_tag heap addr = Tag.string ->
      read_string heap addr
    | _ -> raise (Exception.Type_error "expected string")

  let ns heap =
    let open Map.Namespace in
    let str  = alloc_string heap in
    let estr = expect_str heap in
    ns "string" [
      fn "len" (function
        | [| Value.Ptr addr |] ->
          (match H.read heap addr 0 with
           | Value.Int n -> Value.Int n
           | _ -> raise (Exception.Type_error "string/len: bad string"))
        | _ -> raise (Exception.Type_error "string/len expects string"));

      fn "concat" (function
        | [| a; b |] -> str (estr a ^ estr b)
        | _ -> raise (Exception.Type_error "string/concat expects 2 strings"));

      fn "get" (function
        | [| s; Value.Int i |] ->
          let buf = estr s in
          let b0  = Char.code buf.[i] in
          let cp  =
            if b0 land 0x80 = 0 then b0
            else if b0 land 0xE0 = 0xC0 then
              ((b0 land 0x1F) lsl 6)  lor (Char.code buf.[i+1] land 0x3F)
            else if b0 land 0xF0 = 0xE0 then
              ((b0 land 0x0F) lsl 12) lor ((Char.code buf.[i+1] land 0x3F) lsl 6)
                                      lor  (Char.code buf.[i+2] land 0x3F)
            else
              ((b0 land 0x07) lsl 18) lor ((Char.code buf.[i+1] land 0x3F) lsl 12)
                                      lor ((Char.code buf.[i+2] land 0x3F) lsl 6)
                                      lor  (Char.code buf.[i+3] land 0x3F)
          in
          Value.Int cp
        | _ -> raise (Exception.Type_error "string/get expects string int"));

      fn "byte_len" (function
        | [| Value.Ptr addr |] ->
          (match H.read heap addr 0 with
           | Value.Int n -> Value.Int n
           | _ -> raise (Exception.Type_error "string/byte_len: bad string"))
        | _ -> raise (Exception.Type_error "string/byte_len expects string"));

      fn "slice" (function
        | [| s; Value.Int lo; Value.Int hi |] ->
          let buf = estr s in
          let len = String.length buf in
          let lo  = max 0 lo and hi = min len hi in
          str (String.sub buf lo (hi - lo))
        | _ -> raise (Exception.Type_error "string/slice expects string int int"));

      fn "eq" (function
        | [| a; b |] -> Value.Int (if estr a = estr b then 1 else 0)
        | _ -> raise (Exception.Type_error "string/eq expects 2 strings"));

      fn "compare" (function
        | [| a; b |] -> Value.Int (String.compare (estr a) (estr b))
        | _ -> raise (Exception.Type_error "string/compare expects 2 strings"));

      fn "from_int" (function
        | [| Value.Int n |] -> str (string_of_int n)
        | _ -> raise (Exception.Type_error "string/from_int expects int"));

      fn "from_float" (function
        | [| Value.Float f |] -> str (string_of_float f)
        | _ -> raise (Exception.Type_error "string/from_float expects float"));

      fn "from_float_fmt" (function
        | [| Value.Float f |] -> str (Printf.sprintf "%g" f)
        | _ -> raise (Exception.Type_error "string/from_float_fmt expects float"));

      fn "to_int" (function
        | [| s |] ->
          (match int_of_string_opt (estr s) with
           | Some n -> Value.Int n
           | None   -> raise (Exception.Type_error "string/to_int: not an int"))
        | _ -> raise (Exception.Type_error "string/to_int expects string"));

      fn "to_float" (function
        | [| s |] ->
          (match float_of_string_opt (estr s) with
           | Some f -> Value.Float f
           | None   -> raise (Exception.Type_error "string/to_float: not a float"))
        | _ -> raise (Exception.Type_error "string/to_float expects string"));

      fn "contains" (function
        | [| s; sub |] ->
          let s    = estr s and sub = estr sub in
          let ls   = String.length s and lsub = String.length sub in
          let rec check i =
            if i + lsub > ls then 0
            else if String.sub s i lsub = sub then 1
            else check (i + 1)
          in
          Value.Int (if lsub = 0 then 1 else check 0)
        | _ -> raise (Exception.Type_error "string/contains expects 2 strings"));

      fn "split" (function
        | [| s; sep |] ->
          let parts = String.split_on_char (estr sep).[0] (estr s) in
          let n     = List.length parts in
          let addr  = H.alloc heap ~size:n ~tag:Tag.vector in
          List.iteri (fun i p -> H.write heap addr i (str p)) parts;
          Value.Ptr addr
        | _ -> raise (Exception.Type_error "string/split expects string sep"));

      fn "join" (function
        | [| Value.Ptr arr; sep |] ->
          let sep   = estr sep in
          let n     =
            match H.read heap arr 0 with
            | Value.Int n -> n
            | _           -> raise (Exception.Type_error "string/join: expected vector")
          in
          let parts = List.init n (fun i -> estr (H.read heap arr (1 + i))) in
          str (String.concat sep parts)
        | _ -> raise (Exception.Type_error "string/join expects vector sep"));

    fn "format" (fun args ->
      if Array.length args < 1 then
        raise (Exception.Type_error "string/format expects fmt + args");
      let fmt_s = estr args.(0) in
      let nargs  = Array.length args - 1 in
      let buf    = Buffer.create 64 in
      let len    = String.length fmt_s in
      let ai     = ref 0 in
      let i      = ref 0 in
      while !i < len do
        if fmt_s.[!i] = '%' && !i + 1 < len then begin
          let spec = fmt_s.[!i + 1] in
          let arg  = if !ai < nargs then args.(!ai + 1) else Value.Nil in
          (match spec with
           | 's' -> Buffer.add_string buf (estr arg); incr ai
           | 'd' ->
             (match arg with
              | Value.Int n   -> Buffer.add_string buf (string_of_int n)
              | Value.Float f -> Buffer.add_string buf (string_of_int (int_of_float f))
              | _             -> Buffer.add_string buf "?");
             incr ai
           | 'f' ->
             (match arg with
              | Value.Float f -> Buffer.add_string buf (string_of_float f)
              | Value.Int n   -> Buffer.add_string buf (string_of_float (float_of_int n))
              | _             -> Buffer.add_string buf "?");
             incr ai
           | 'g' ->
             (match arg with
              | Value.Float f -> Buffer.add_string buf (Printf.sprintf "%g" f)
              | Value.Int n   -> Buffer.add_string buf (string_of_int n)
              | _             -> Buffer.add_string buf "?");
             incr ai
           | '%' -> Buffer.add_char buf '%'
           | c   -> Buffer.add_char buf '%'; Buffer.add_char buf c);
          i := !i + 2
        end else begin
          Buffer.add_char buf fmt_s.[!i];
          incr i
        end
      done;
      str (Buffer.contents buf));
    ]

end
