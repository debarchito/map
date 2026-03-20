open Map.Core
open Map.Namespace

module Make (H : Map.Heap.S) = struct
  module NS = Map.Namespace.Make (H)

  let registry = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"record/bad_arg"
  let e_bounds = Exception.register registry ~name:"record/bounds"
  let e_bad_object = Exception.register registry ~name:"record/bad_object"

  let make heap schema_id nfields =
    let addr = H.alloc heap ~size:(2 + nfields) ~tag:Fold_core.Tag.record in
    H.write heap addr 0 (Value.Int schema_id);
    H.write heap addr 1 (Value.Int nfields);
    for i = 0 to nfields - 1 do
      H.write heap addr (2 + i) Value.Nil
    done;
    Value.Ptr addr

  let nfields heap addr =
    match H.read heap addr 1 with
    | Value.Int n -> n
    | _ -> Exception.throw e_bad_object "record/nfields: bad object"

  let get heap addr i =
    let n = nfields heap addr in
    if i < 0 || i >= n then
      Exception.throw e_bounds
        (Printf.sprintf "record/get: field %d out of bounds (n %d)" i n);
    H.read heap addr (2 + i)

  let set heap addr i v =
    let n = nfields heap addr in
    if i < 0 || i >= n then
      Exception.throw e_bounds
        (Printf.sprintf "record/set: field %d out of bounds (n %d)" i n);
    H.write heap addr (2 + i) v

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/record"
         [
           native "e_bad_arg" (Value.Int e_bad_arg);
           native "e_bounds" (Value.Int e_bounds);
           native "e_bad_object" (Value.Int e_bad_object);
           nif "make" (fun args ->
               match args with
               | [| Value.Int schema_id; Value.Int nfields |] ->
                   make heap schema_id nfields
               | _ -> Exception.throw e_bad_arg "record/make expects int int");
           nif "schema" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> (
                   match H.read heap addr 0 with
                   | Value.Int s -> Value.Int s
                   | _ ->
                       Exception.throw e_bad_object "record/schema: bad object")
               | _ -> Exception.throw e_bad_arg "record/schema expects ptr");
           nif "nfields" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> Value.Int (nfields heap addr)
               | _ -> Exception.throw e_bad_arg "record/nfields expects ptr");
           nif "get" (fun args ->
               match args with
               | [| Value.Ptr addr; Value.Int i |] -> get heap addr i
               | _ -> Exception.throw e_bad_arg "record/get expects ptr int");
           nif "set" (fun args ->
               match args with
               | [| Value.Ptr addr; Value.Int i; v |] ->
                   set heap addr i v;
                   Value.Nil
               | _ ->
                   Exception.throw e_bad_arg "record/set expects ptr int value");
           nif "is_record" (fun args ->
               match args with
               | [| Value.Ptr addr |] ->
                   Value.Int
                     (if H.get_tag heap addr = Fold_core.Tag.record then 1
                      else 0)
               | [| _ |] -> Value.Int 0
               | _ -> Exception.throw e_bad_arg "record/is_record expects 1 arg");
           nif "is_schema" (fun args ->
               match args with
               | [| Value.Ptr addr; Value.Int s |] -> (
                   match H.read heap addr 0 with
                   | Value.Int schema -> Value.Int (if schema = s then 1 else 0)
                   | _ ->
                       Exception.throw e_bad_object
                         "record/is_schema: bad object")
               | _ ->
                   Exception.throw e_bad_arg "record/is_schema expects ptr int");
         ])
end
