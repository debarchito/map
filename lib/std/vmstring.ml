open Map_core

type vm_string = {
  content         : string;
  mutable cp_len  : int;  (* -1 = not yet computed *)
}

let string_id : vm_string Type.Id.t = Type.Id.make ()

let make s =
  Value.make_native ~tag:string_id ~finalizer:None { content = s; cp_len = -1 }

let get v =
  match Value.get_native string_id v with
  | Some vs -> vs
  | None    -> raise (Value.ETypeError "expected string")

let get_content v = (get v).content

let codepoint_length vs =
  if vs.cp_len >= 0 then vs.cp_len
  else begin
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String vs.content) in
    let rec count n =
      match Uutf.decode decoder with
      | `Uchar _     -> count (n + 1)
      | `End | `Await -> n
      | `Malformed _ -> raise (Value.ETypeError "malformed UTF-8")
    in
    let len = count 0 in
    vs.cp_len <- len;
    len
  end

(* Boyer-Moore-Horspool for contains — no sub allocations *)
let bytes_contains haystack needle =
  let nh = String.length haystack in
  let nn = String.length needle in
  if nn = 0 then true
  else if nn > nh then false
  else begin
    let skip = Array.make 256 nn in
    for i = 0 to nn - 2 do
      skip.(Char.code needle.[i]) <- nn - 1 - i
    done;
    let rec search i =
      if i > nh - nn then false
      else begin
        let rec match_at j =
          if j < 0 then true
          else if haystack.[i + j] <> needle.[j] then false
          else match_at (j - 1)
        in
        if match_at (nn - 1) then true
        else search (i + skip.(Char.code haystack.[i + nn - 1]))
      end
    in
    search 0
  end

let native_length = Value.MNativeFn (fun args ->
  match args with
  | [| s |] -> Value.MInt (codepoint_length (get s))
  | _       -> raise (Value.ETypeError "length expects string"))

let native_byte_length = Value.MNativeFn (fun args ->
  match args with
  | [| s |] -> Value.MInt (String.length (get_content s))
  | _       -> raise (Value.ETypeError "byte_length expects string"))

let native_concat = Value.MNativeFn (fun args ->
  match args with
  | [| a; b |] -> make (get_content a ^ get_content b)
  | _          -> raise (Value.ETypeError "concat expects two strings"))

let native_substring = Value.MNativeFn (fun args ->
  match args with
  | [| s; Value.MInt start; Value.MInt len |] ->
    let vs      = get s in
    let str     = vs.content in
    let buf     = Buffer.create (min len 64) in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let rec skip n =
      match Uutf.decode decoder with
      | `Uchar _ when n > 0  -> skip (n - 1)
      | `Uchar u             -> Uutf.Buffer.add_utf_8 buf u; collect (len - 1)
      | `End | `Await        -> ()
      | `Malformed _         -> raise (Value.ETypeError "malformed UTF-8")
    and collect n =
      if n <= 0 then ()
      else match Uutf.decode decoder with
      | `Uchar u      -> Uutf.Buffer.add_utf_8 buf u; collect (n - 1)
      | `End | `Await -> ()
      | `Malformed _  -> raise (Value.ETypeError "malformed UTF-8")
    in
    skip start;
    make (Buffer.contents buf)
  | _ -> raise (Value.ETypeError "substring expects string int int"))

let native_get_byte = Value.MNativeFn (fun args ->
  match args with
  | [| s; Value.MInt i |] ->
    let str = get_content s in
    if i < 0 || i >= String.length str then
      raise (Value.EBoundsError (Printf.sprintf "byte index %d out of bounds" i))
    else
      Value.MInt (Char.code str.[i])
  | _ -> raise (Value.ETypeError "get_byte expects string int"))

let native_get_codepoint = Value.MNativeFn (fun args ->
  match args with
  | [| s; Value.MInt target |] ->
    let str     = get_content s in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let rec seek n =
      match Uutf.decode decoder with
      | `Uchar u when n = target -> Uchar.to_int u
      | `Uchar _                 -> seek (n + 1)
      | `End | `Await            ->
        raise (Value.EBoundsError (Printf.sprintf "codepoint index %d out of bounds" target))
      | `Malformed _             ->
        raise (Value.ETypeError "malformed UTF-8")
    in
    Value.MInt (seek 0)
  | _ -> raise (Value.ETypeError "get_codepoint expects string int"))

let native_eq = Value.MNativeFn (fun args ->
  match args with
  | [| a; b |] -> Value.MInt (if get_content a = get_content b then 1 else 0)
  | _          -> raise (Value.ETypeError "eq expects two strings"))

let native_lt = Value.MNativeFn (fun args ->
  match args with
  | [| a; b |] -> Value.MInt (if get_content a < get_content b then 1 else 0)
  | _          -> raise (Value.ETypeError "lt expects two strings"))

let native_contains = Value.MNativeFn (fun args ->
  match args with
  | [| s; sub |] ->
    Value.MInt (if bytes_contains (get_content s) (get_content sub) then 1 else 0)
  | _ -> raise (Value.ETypeError "contains expects two strings"))

let native_starts_with = Value.MNativeFn (fun args ->
  match args with
  | [| s; prefix |] ->
    let str = get_content s and pre = get_content prefix in
    let ls  = String.length str and lp = String.length pre in
    let rec check i =
      if i >= lp then true
      else if str.[i] <> pre.[i] then false
      else check (i + 1)
    in
    Value.MInt (if lp <= ls && check 0 then 1 else 0)
  | _ -> raise (Value.ETypeError "starts_with expects two strings"))

let native_ends_with = Value.MNativeFn (fun args ->
  match args with
  | [| s; suffix |] ->
    let str  = get_content s and suf = get_content suffix in
    let ls   = String.length str and lsuf = String.length suf in
    let off  = ls - lsuf in
    let rec check i =
      if i >= lsuf then true
      else if str.[off + i] <> suf.[i] then false
      else check (i + 1)
    in
    Value.MInt (if lsuf <= ls && check 0 then 1 else 0)
  | _ -> raise (Value.ETypeError "ends_with expects two strings"))

let native_trim = Value.MNativeFn (fun args ->
  match args with
  | [| s |] -> make (String.trim (get_content s))
  | _       -> raise (Value.ETypeError "trim expects string"))

let native_uppercase = Value.MNativeFn (fun args ->
  match args with
  | [| s |] ->
    let buf     = Buffer.create 16 in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String (get_content s)) in
    let rec loop () =
      match Uutf.decode decoder with
      | `Uchar u ->
        let c  = Uchar.to_int u in
        let c' = if c >= 0x61 && c <= 0x7A then c - 32 else c in
        Uutf.Buffer.add_utf_8 buf (Uchar.of_int c');
        loop ()
      | `End | `Await -> ()
      | `Malformed _  -> raise (Value.ETypeError "malformed UTF-8")
    in
    loop ();
    make (Buffer.contents buf)
  | _ -> raise (Value.ETypeError "uppercase expects string"))

let native_lowercase = Value.MNativeFn (fun args ->
  match args with
  | [| s |] ->
    let buf     = Buffer.create 16 in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String (get_content s)) in
    let rec loop () =
      match Uutf.decode decoder with
      | `Uchar u ->
        let c  = Uchar.to_int u in
        let c' = if c >= 0x41 && c <= 0x5A then c + 32 else c in
        Uutf.Buffer.add_utf_8 buf (Uchar.of_int c');
        loop ()
      | `End | `Await -> ()
      | `Malformed _  -> raise (Value.ETypeError "malformed UTF-8")
    in
    loop ();
    make (Buffer.contents buf)
  | _ -> raise (Value.ETypeError "lowercase expects string"))

let native_to_int = Value.MNativeFn (fun args ->
  match args with
  | [| s |] ->
    (match int_of_string_opt (String.trim (get_content s)) with
     | Some n -> Value.MInt n
     | None   -> raise (Value.ETypeError "string->int: invalid integer"))
  | _ -> raise (Value.ETypeError "to_int expects string"))

let native_to_float = Value.MNativeFn (fun args ->
  match args with
  | [| s |] ->
    (match float_of_string_opt (String.trim (get_content s)) with
     | Some f -> Value.MFloat f
     | None   -> raise (Value.ETypeError "string->float: invalid float"))
  | _ -> raise (Value.ETypeError "to_float expects string"))

let native_from_int = Value.MNativeFn (fun args ->
  match args with
  | [| Value.MInt n |] -> make (string_of_int n)
  | _                  -> raise (Value.ETypeError "from_int expects int"))

let native_from_float = Value.MNativeFn (fun args ->
  match args with
  | [| Value.MFloat f |] -> make (string_of_float f)
  | _                    -> raise (Value.ETypeError "from_float expects float"))

let native_print = Value.MNativeFn (fun args ->
  match args with
  | [| s |] -> print_string (get_content s); Value.MNil
  | _       -> raise (Value.ETypeError "print expects string"))

let native_println = Value.MNativeFn (fun args ->
  match args with
  | [| s |] -> print_endline (get_content s); Value.MNil
  | _       -> raise (Value.ETypeError "println expects string"))

let table = [|
  native_length;
  native_byte_length;
  native_concat;
  native_substring;
  native_get_byte;
  native_get_codepoint;
  native_eq;
  native_lt;
  native_contains;
  native_starts_with;
  native_ends_with;
  native_trim;
  native_uppercase;
  native_lowercase;
  native_to_int;
  native_to_float;
  native_from_int;
  native_from_float;
  native_print;
  native_println;
|]

module Index = struct
  let length        = 0
  let byte_length   = 1
  let concat        = 2
  let substring     = 3
  let get_byte      = 4
  let get_codepoint = 5
  let eq            = 6
  let lt            = 7
  let contains      = 8
  let starts_with   = 9
  let ends_with     = 10
  let trim          = 11
  let uppercase     = 12
  let lowercase     = 13
  let to_int        = 14
  let to_float      = 15
  let from_int      = 16
  let from_float    = 17
  let print         = 18
  let println       = 19
end
