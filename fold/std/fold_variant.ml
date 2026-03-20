open Map.Core
open Map.Namespace

module Make (H : Map.Heap.S) = struct
  module NS = Map.Namespace.Make (H)

  let registry = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"variant/bad_arg"
  let e_bad_object = Exception.register registry ~name:"variant/bad_object"

  (* variant layout: field 0 = Int tag_id, field 1 = payload value *)

  let make heap tag_id payload =
    let addr = H.alloc heap ~size:2 ~tag:Fold_core.Tag.variant in
    H.write heap addr 0 (Value.Int tag_id);
    H.write heap addr 1 payload;
    Value.Ptr addr

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/variant"
         [
           native "e_bad_arg" (Value.Int e_bad_arg);
           native "e_bad_object" (Value.Int e_bad_object);
           nif "make" (fun args ->
               match args with
               | [| Value.Int tag_id; payload |] -> make heap tag_id payload
               | [| Value.Int tag_id |] -> make heap tag_id Value.Nil
               | _ ->
                   Exception.throw e_bad_arg "variant/make expects int [value]");
           nif "tag" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> (
                   match H.read heap addr 0 with
                   | Value.Int t -> Value.Int t
                   | _ -> Exception.throw e_bad_object "variant/tag: bad object"
                   )
               | _ -> Exception.throw e_bad_arg "variant/tag expects ptr");
           nif "payload" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> H.read heap addr 1
               | _ -> Exception.throw e_bad_arg "variant/payload expects ptr");
           nif "is_variant" (fun args ->
               match args with
               | [| Value.Ptr addr |] ->
                   Value.Int
                     (if H.get_tag heap addr = Fold_core.Tag.variant then 1
                      else 0)
               | [| _ |] -> Value.Int 0
               | _ ->
                   Exception.throw e_bad_arg "variant/is_variant expects 1 arg");
           nif "is_tag" (fun args ->
               match args with
               | [| Value.Ptr addr; Value.Int t |] -> (
                   match H.read heap addr 0 with
                   | Value.Int tag -> Value.Int (if tag = t then 1 else 0)
                   | _ ->
                       Exception.throw e_bad_object "variant/is_tag: bad object"
                   )
               | _ -> Exception.throw e_bad_arg "variant/is_tag expects ptr int");
         ])
end
