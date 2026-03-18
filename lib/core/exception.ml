exception Type_error     of string
exception Bounds_error   of string
exception Div_by_zero    of string
exception Stack_overflow of string
exception Alloc_error    of string
exception Native_error   of exn
exception Registered     of int * string

type t =
  | Type_error
  | Bounds_error
  | Div_by_zero
  | Stack_overflow
  | Alloc_error
  | Native_error

let to_int = function
  | Type_error     -> 0
  | Bounds_error   -> 1
  | Div_by_zero    -> 2
  | Stack_overflow -> 3
  | Alloc_error    -> 4
  | Native_error   -> 5

let of_int = function
  | 0 -> Some Type_error
  | 1 -> Some Bounds_error
  | 2 -> Some Div_by_zero
  | 3 -> Some Stack_overflow
  | 4 -> Some Alloc_error
  | 5 -> Some Native_error
  | _ -> None

let to_string = function
  | Type_error     -> "TypeError"
  | Bounds_error   -> "BoundsError"
  | Div_by_zero    -> "DivByZero"
  | Stack_overflow -> "StackOverflow"
  | Alloc_error    -> "AllocError"
  | Native_error   -> "NativeError"

let reserved_codes = 6

type registry = {
  mutable entries   : (int * string) array;
  mutable next_code : int;
}

let create () = {
  entries   = Array.make 64 (0, "");
  next_code = reserved_codes;
}

let register registry ~name =
  let code = registry.next_code in
  registry.entries.(code - reserved_codes) <- (code, name);
  registry.next_code <- code + 1;
  code

let name_of registry code =
  if code < reserved_codes then
    of_int code |> Option.map to_string
  else
    let idx = code - reserved_codes in
    if idx < Array.length registry.entries then
      let (_, name) = registry.entries.(idx) in
      if name = "" then None else Some name
    else
      None

let throw code msg =
  raise (Registered (code, msg))
