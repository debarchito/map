open Map.Core
open Map.Namespace

module Make(H : Map.Heap.S) = struct

  module NS = Map.Namespace.Make(H)

  let registry     = Exception.create ()
  let e_bad_arg    = Exception.register registry ~name:"vector/bad_arg"
  let e_bounds     = Exception.register registry ~name:"vector/bounds"
  let e_bad_object = Exception.register registry ~name:"vector/bad_object"

  let make heap n init =
    let addr = H.alloc heap ~size:(1 + n) ~tag:Fold_core.Tag.vector in
    H.write heap addr 0 (Value.Int n);
    for i = 0 to n - 1 do
      H.write heap addr (1 + i) init
    done;
    Value.Ptr addr

  let length heap addr =
    match H.read heap addr 0 with
    | Value.Int n -> n
    | _ -> Exception.throw e_bad_object "vector/length: bad object"

  let get heap addr i =
    let n = length heap addr in
    if i < 0 || i >= n then
      Exception.throw e_bounds (Printf.sprintf "vector/get: index %d out of bounds (len %d)" i n);
    H.read heap addr (1 + i)

  let set heap addr i v =
    let n = length heap addr in
    if i < 0 || i >= n then
      Exception.throw e_bounds (Printf.sprintf "vector/set: index %d out of bounds (len %d)" i n);
    H.write heap addr (1 + i) v

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/vector" [
        native "e_bad_arg"    (Value.Int e_bad_arg);
        native "e_bounds"     (Value.Int e_bounds);
        native "e_bad_object" (Value.Int e_bad_object);

        nif "make" (fun args ->
          match args with
          | [| Value.Int n |]       -> make heap n Value.Nil
          | [| Value.Int n; init |] -> make heap n init
          | _ -> Exception.throw e_bad_arg "vector/make expects int [init]");

        nif "length" (fun args ->
          match args with
          | [| Value.Ptr addr |] -> Value.Int (length heap addr)
          | _ -> Exception.throw e_bad_arg "vector/length expects ptr");

        nif "get" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int i |] -> get heap addr i
          | _ -> Exception.throw e_bad_arg "vector/get expects ptr int");

        nif "set" (fun args ->
          match args with
          | [| Value.Ptr addr; Value.Int i; v |] ->
            set heap addr i v; Value.Nil
          | _ -> Exception.throw e_bad_arg "vector/set expects ptr int value");

        nif "is_vector" (fun args ->
          match args with
          | [| Value.Ptr addr |] ->
            Value.Int (if H.get_tag heap addr = Fold_core.Tag.vector then 1 else 0)
          | [| _ |] -> Value.Int 0
          | _ -> Exception.throw e_bad_arg "vector/is_vector expects 1 arg");
      ])

end
