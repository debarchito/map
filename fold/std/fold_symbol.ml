open Map.Core
open Map.Namespace

module Make (H : Map.Heap.S) = struct
  module NS = Map.Namespace.Make (H)
  module Str = Fold_string.Make (H)
  module Hsh = Fold_hash.Make (H)

  let registry = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"symbol/bad_arg"
  let e_bad_object = Exception.register registry ~name:"symbol/bad_object"
  let table_ref : Value.t ref = ref Value.Nil
  let names_ref : Value.t ref = ref Value.Nil
  let next_id : int ref = ref 0

  let init heap =
    table_ref := Hsh.make heap;
    names_ref := Str.of_ocaml heap ""

  let get_table () =
    match !table_ref with
    | Value.Ptr addr -> addr
    | _ -> failwith "symbol table not initialized"

  let intern heap s =
    let str_val = Str.of_ocaml heap s in
    let tbl = get_table () in
    match Hsh.hash_get_or heap tbl str_val Value.Nil with
    | Value.Int id ->
        let addr = H.alloc heap ~size:2 ~tag:Fold_core.Tag.symbol in
        H.write heap addr 0 (Value.Int id);
        H.write heap addr 1 str_val;
        Value.Ptr addr
    | _ ->
        let id = !next_id in
        incr next_id;
        ignore (Hsh.hash_set heap tbl str_val (Value.Int id));
        let addr = H.alloc heap ~size:2 ~tag:Fold_core.Tag.symbol in
        H.write heap addr 0 (Value.Int id);
        H.write heap addr 1 str_val;
        Value.Ptr addr

  let id_of heap addr =
    match H.read heap addr 0 with
    | Value.Int id -> id
    | _ -> Exception.throw e_bad_object "symbol/id_of: bad object"

  let name_of heap addr =
    match H.read heap addr 1 with
    | Value.Ptr _ as v ->
        Str.to_ocaml heap (match v with Value.Ptr a -> a | _ -> assert false)
    | _ -> Exception.throw e_bad_object "symbol/name_of: bad object"

  let name_val_of heap addr = H.read heap addr 1

  let register heap (reg : Map.Symbol.registry) =
    init heap;
    NS.register heap reg
      (ns "std/symbol"
         [
           native "e_bad_arg" (Value.Int e_bad_arg);
           native "e_bad_object" (Value.Int e_bad_object);
           nif "intern" (fun args ->
               match args with
               | [| s |] ->
                   let str =
                     match s with
                     | Value.Ptr addr
                       when H.get_tag heap addr = Fold_core.Tag.string ->
                         Str.to_ocaml heap addr
                     | _ ->
                         Exception.throw e_bad_arg
                           "symbol/intern expects string"
                   in
                   intern heap str
               | _ -> Exception.throw e_bad_arg "symbol/intern expects string");
           nif "name" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> name_val_of heap addr
               | _ -> Exception.throw e_bad_arg "symbol/name expects symbol ptr");
           nif "eq" (fun args ->
               match args with
               | [| Value.Ptr a; Value.Ptr b |] ->
                   Value.Int (if id_of heap a = id_of heap b then 1 else 0)
               | _ ->
                   Exception.throw e_bad_arg "symbol/eq expects 2 symbol ptrs");
           nif "id" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> Value.Int (id_of heap addr)
               | _ -> Exception.throw e_bad_arg "symbol/id expects symbol ptr");
         ])
end
