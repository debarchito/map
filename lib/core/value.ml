exception ENativeError   of exn
exception ETypeError     of string
exception EDivByZero     of string
exception EStackOverflow of string
exception EAllocError    of string
exception EBoundsError   of string

type 'a native_resource = {
  value     : 'a;
  tag       : 'a Type.Id.t;
  finalizer : ('a -> unit) option;
}

type native_ptr = NativePtr : 'a native_resource -> native_ptr

type t =
  | MInt       of int
  | MFloat     of float
  | MPtr       of int
  | MNativePtr of native_ptr
  | MNativeFn  of (t array -> t)
  | MNil

type error =
  | ENativeError
  | ETypeError
  | EDivByZero
  | EStackOverflow
  | EAllocError
  | EBoundsError

let int_of_error = function
  | ENativeError   -> 0
  | ETypeError     -> 1
  | EDivByZero     -> 2
  | EStackOverflow -> 3
  | EAllocError    -> 4
  | EBoundsError   -> 5

let error_of_int = function
  | 0 -> Some ENativeError
  | 1 -> Some ETypeError
  | 2 -> Some EDivByZero
  | 3 -> Some EStackOverflow
  | 4 -> Some EAllocError
  | 5 -> Some EBoundsError
  | _ -> None

let make_native : type a. tag:a Type.Id.t -> finalizer:(a -> unit) option -> a -> t =
  fun ~tag ~finalizer value ->
    MNativePtr (NativePtr { value; tag; finalizer })

let get_native : type a. a Type.Id.t -> t -> a option =
  fun tag v ->
    match v with
    | MNativePtr (NativePtr r) ->
      begin match Type.Id.provably_equal tag r.tag with
      | Some Type.Equal -> Some r.value
      | None            -> None
      end
    | _ -> None

let call_finalizer np =
  let (NativePtr r) = np in
  match r.finalizer with
  | Some f -> f r.value
  | None   -> ()

let is_int    = function MInt _       -> true | _ -> false
let is_float  = function MFloat _     -> true | _ -> false
let is_ptr    = function MPtr _       -> true | _ -> false
let is_native = function MNativePtr _ -> true | _ -> false
let is_fn     = function MNativeFn _  -> true | _ -> false
let is_null   = function MNil        -> true | _ -> false

let is_gc_root = function
  | MPtr _ | MNativePtr _ -> true
  | _                     -> false

let as_int = function
  | MInt n -> n
  | _      -> raise (ETypeError "expected Int")

let as_float = function
  | MFloat f -> f
  | _        -> raise (ETypeError "expected Float")

let as_ptr = function
  | MPtr p -> p
  | _      -> raise (ETypeError "expected Ptr")

let pp fmt = function
  | MInt n       -> Format.fprintf fmt "Int(%d)" n
  | MFloat f     -> Format.fprintf fmt "Float(%g)" f
  | MPtr p       -> Format.fprintf fmt "Ptr(0x%x)" p
  | MNativePtr _ -> Format.fprintf fmt "NativePtr(<opaque>)"
  | MNativeFn _  -> Format.fprintf fmt "NativeFn(<fn>)"
  | MNil        -> Format.fprintf fmt "Null"

let to_string v =
  pp Format.str_formatter v;
  Format.flush_str_formatter ()
