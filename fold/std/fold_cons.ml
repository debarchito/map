open Map.Core
open Map.Namespace

module Make (H : Map.Heap.S) = struct
  module NS = Map.Namespace.Make (H)

  let registry = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"cons/bad_arg"
  let e_bad_object = Exception.register registry ~name:"cons/bad_object"

  let make heap car cdr =
    let addr = H.alloc heap ~size:2 ~tag:Fold_core.Tag.cons in
    H.write heap addr 0 car;
    H.write heap addr 1 cdr;
    Value.Ptr addr

  let car heap addr = match H.read heap addr 0 with v -> v
  let cdr heap addr = match H.read heap addr 1 with v -> v
  let set_car heap addr v = H.write heap addr 0 v
  let set_cdr heap addr v = H.write heap addr 1 v
  let is_nil = function Value.Nil -> true | _ -> false

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/cons"
         [
           native "e_bad_arg" (Value.Int e_bad_arg);
           native "e_bad_object" (Value.Int e_bad_object);
           nif "make" (fun args ->
               match args with
               | [| car; cdr |] -> make heap car cdr
               | _ -> Exception.throw e_bad_arg "cons/make expects 2 args");
           nif "car" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> car heap addr
               | _ -> Exception.throw e_bad_arg "cons/car expects ptr");
           nif "cdr" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> cdr heap addr
               | _ -> Exception.throw e_bad_arg "cons/cdr expects ptr");
           nif "set_car" (fun args ->
               match args with
               | [| Value.Ptr addr; v |] ->
                   set_car heap addr v;
                   Value.Nil
               | _ -> Exception.throw e_bad_arg "cons/set_car expects ptr value");
           nif "set_cdr" (fun args ->
               match args with
               | [| Value.Ptr addr; v |] ->
                   set_cdr heap addr v;
                   Value.Nil
               | _ -> Exception.throw e_bad_arg "cons/set_cdr expects ptr value");
           nif "is_cons" (fun args ->
               match args with
               | [| Value.Ptr addr |] ->
                   Value.Int
                     (if H.get_tag heap addr = Fold_core.Tag.cons then 1 else 0)
               | [| _ |] -> Value.Int 0
               | _ -> Exception.throw e_bad_arg "cons/is_cons expects 1 arg");
           nif "is_nil" (fun args ->
               match args with
               | [| v |] -> Value.Int (if is_nil v then 1 else 0)
               | _ -> Exception.throw e_bad_arg "cons/is_nil expects 1 arg");
         ])
end
