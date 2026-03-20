type 'a native_resource = {
  value     : 'a;
  tag       : 'a Type.Id.t;
  finalizer : ('a -> unit) option;
}

type native_ptr = Resource : 'a native_resource -> native_ptr

type t =
  | Int       of int
  | Float     of float
  | Ptr       of int
  | NativePtr of native_ptr
  | NativeFun of (t array -> t)
  | NativeFib of (t array -> fib_result)
  | Nil

and fib_result =
  | Done    of t
  | Suspend of (unit -> fib_result)
  | Yield

let make_native : type a. tag:a Type.Id.t -> finalizer:(a -> unit) option
                -> a -> t =
  fun ~tag ~finalizer value ->
    NativePtr (Resource { value; tag; finalizer })

let get_native : type a. a Type.Id.t -> t -> a option =
  fun tag v ->
    match v with
    | NativePtr (Resource r) ->
      begin match Type.Id.provably_equal tag r.tag with
      | Some Type.Equal -> Some r.value
      | None            -> None
      end
    | _ -> None

let call_finalizer (Resource r) =
  match r.finalizer with
  | Some f -> f r.value
  | None   -> ()

let is_int    = function Int _       -> true | _ -> false
let is_float  = function Float _     -> true | _ -> false
let is_ptr    = function Ptr _       -> true | _ -> false
let is_native = function NativePtr _ -> true | _ -> false
let is_fun    = function NativeFun _ -> true | _ -> false
let is_fib    = function NativeFib _ -> true | _ -> false
let is_nil    = function Nil         -> true | _ -> false

let is_gc_root = function
  | Ptr _ | NativePtr _ | NativeFun _ | NativeFib _ -> true
  | _                                                -> false

let to_int = function
  | Int n -> n
  | _     -> raise (Exception.Type_error "expected Int")

let to_float = function
  | Float f -> f
  | _       -> raise (Exception.Type_error "expected Float")

let to_ptr = function
  | Ptr p -> p
  | _     -> raise (Exception.Type_error "expected Ptr")

let to_int_opt = function
  | Int n -> Some n
  | _     -> None

let to_float_opt = function
  | Float f -> Some f
  | _       -> None

let to_ptr_opt = function
  | Ptr p -> Some p
  | _     -> None

let equal a b =
  match a, b with
  | Int a,   Int b   -> a = b
  | Float a, Float b -> a = b
  | Ptr a,   Ptr b   -> a = b
  | Nil,     Nil     -> true
  | _,       _       -> false

let pp fmt = function
  | Int n       -> Format.fprintf fmt "Int(%d)" n
  | Float f     -> Format.fprintf fmt "Float(%g)" f
  | Ptr p       -> Format.fprintf fmt "Ptr(0x%x)" p
  | NativePtr _ -> Format.fprintf fmt "NativePtr(<opaque>)"
  | NativeFun _ -> Format.fprintf fmt "NativeFun(<fn>)"
  | NativeFib _ -> Format.fprintf fmt "NativeFib(<fib>)"
  | Nil         -> Format.fprintf fmt "Nil"

let to_string v =
  pp Format.str_formatter v;
  Format.flush_str_formatter ()
