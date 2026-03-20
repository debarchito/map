open Map.Core
open Map.Namespace

module Make (H : Map.Heap.S) = struct
  module NS = Map.Namespace.Make (H)

  let registry = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"hash/bad_arg"
  let e_bad_object = Exception.register registry ~name:"hash/bad_object"
  let e_not_found = Exception.register registry ~name:"hash/not_found"
  let initial_cap = 8
  let tombstone = Value.Int (-1)
  let is_empty = function Value.Nil -> true | _ -> false
  let is_tomb = function Value.Int -1 -> true | _ -> false
  let is_occupied v = (not (is_empty v)) && not (is_tomb v)

  let value_equal a b =
    match (a, b) with
    | Value.Int x, Value.Int y -> x = y
    | Value.Float x, Value.Float y -> x = y
    | Value.Ptr x, Value.Ptr y -> x = y
    | Value.Nil, Value.Nil -> true
    | _, _ -> false

  let value_hash = function
    | Value.Int n -> n land max_int
    | Value.Float f -> Hashtbl.hash f
    | Value.Ptr p -> p
    | _ -> 0

  let cap heap addr =
    match H.read heap addr 0 with
    | Value.Int n -> n
    | _ -> Exception.throw e_bad_object "hash/cap: bad object"

  let count heap addr =
    match H.read heap addr 1 with
    | Value.Int n -> n
    | _ -> Exception.throw e_bad_object "hash/count: bad object"

  let set_count heap addr n = H.write heap addr 1 (Value.Int n)
  let key_slot addr i c = 2 + (i mod c)
  let val_slot addr i c = 2 + c + (i mod c)

  let alloc_table heap c =
    let addr = H.alloc heap ~size:(2 + c + c) ~tag:Fold_core.Tag.hash in
    H.write heap addr 0 (Value.Int c);
    H.write heap addr 1 (Value.Int 0);
    addr

  let find_slot heap addr k =
    let c = cap heap addr in
    let start = value_hash k mod c in
    let i = ref start in
    let found = ref (-1) in
    let first_tomb = ref (-1) in
    let stop = ref false in
    while not !stop do
      let ks = 2 + (!i mod c) in
      let kv = H.read heap addr ks in
      if is_empty kv then begin
        if !first_tomb >= 0 then found := !first_tomb else found := !i mod c;
        stop := true
      end
      else if is_tomb kv then begin
        if !first_tomb < 0 then first_tomb := !i mod c;
        i := (!i + 1) mod c;
        if !i = start then begin
          found := if !first_tomb >= 0 then !first_tomb else !i;
          stop := true
        end
      end
      else if value_equal kv k then begin
        found := !i mod c;
        stop := true
      end
      else begin
        i := (!i + 1) mod c;
        if !i = start then begin
          found := if !first_tomb >= 0 then !first_tomb else -1;
          stop := true
        end
      end
    done;
    !found

  let rehash heap addr =
    let old_c = cap heap addr in
    let new_c = old_c * 2 in
    let new_addr = alloc_table heap new_c in
    for i = 0 to old_c - 1 do
      let kv = H.read heap addr (2 + i) in
      if is_occupied kv then begin
        let vv = H.read heap addr (2 + old_c + i) in
        let slot = find_slot heap new_addr kv in
        H.write heap new_addr (2 + slot) kv;
        H.write heap new_addr (2 + new_c + slot) vv
      end
    done;
    let n = count heap addr in
    set_count heap new_addr n;
    new_addr

  let make heap = Value.Ptr (alloc_table heap initial_cap)

  let hash_get heap addr k =
    let slot = find_slot heap addr k in
    if slot < 0 then Exception.throw e_not_found "hash/get: key not found";
    let kv = H.read heap addr (2 + slot) in
    if not (is_occupied kv) then
      Exception.throw e_not_found "hash/get: key not found";
    H.read heap addr (2 + cap heap addr + slot)

  let hash_get_or heap addr k default =
    let slot = find_slot heap addr k in
    if slot < 0 then default
    else
      let kv = H.read heap addr (2 + slot) in
      if not (is_occupied kv) then default
      else H.read heap addr (2 + cap heap addr + slot)

  let hash_has heap addr k =
    let slot = find_slot heap addr k in
    if slot < 0 then false else is_occupied (H.read heap addr (2 + slot))

  let hash_set heap addr k v =
    let c = cap heap addr in
    let n = count heap addr in
    let addr = if n * 10 >= c * 7 then rehash heap addr else addr in
    let c = cap heap addr in
    let slot = find_slot heap addr k in
    let kv = H.read heap addr (2 + slot) in
    if not (is_occupied kv) then begin
      H.write heap addr (2 + slot) k;
      H.write heap addr (2 + c + slot) v;
      set_count heap addr (n + 1)
    end
    else begin
      H.write heap addr (2 + c + slot) v
    end;
    Value.Ptr addr

  let hash_delete heap addr k =
    let slot = find_slot heap addr k in
    if slot >= 0 && is_occupied (H.read heap addr (2 + slot)) then begin
      let c = cap heap addr in
      H.write heap addr (2 + slot) tombstone;
      H.write heap addr (2 + c + slot) Value.Nil;
      set_count heap addr (count heap addr - 1)
    end

  let register heap (reg : Map.Symbol.registry) =
    NS.register heap reg
      (ns "std/hash"
         [
           native "e_bad_arg" (Value.Int e_bad_arg);
           native "e_bad_object" (Value.Int e_bad_object);
           native "e_not_found" (Value.Int e_not_found);
           nif "make" (fun args ->
               match args with
               | [||] | [| Value.Nil |] -> make heap
               | _ -> Exception.throw e_bad_arg "hash/make expects no args");
           nif "set" (fun args ->
               match args with
               | [| Value.Ptr addr; k; v |] -> hash_set heap addr k v
               | _ -> Exception.throw e_bad_arg "hash/set expects ptr key value");
           nif "get" (fun args ->
               match args with
               | [| Value.Ptr addr; k |] -> hash_get heap addr k
               | _ -> Exception.throw e_bad_arg "hash/get expects ptr key");
           nif "get_or" (fun args ->
               match args with
               | [| Value.Ptr addr; k; default |] ->
                   hash_get_or heap addr k default
               | _ ->
                   Exception.throw e_bad_arg
                     "hash/get_or expects ptr key default");
           nif "has" (fun args ->
               match args with
               | [| Value.Ptr addr; k |] ->
                   Value.Int (if hash_has heap addr k then 1 else 0)
               | _ -> Exception.throw e_bad_arg "hash/has expects ptr key");
           nif "delete" (fun args ->
               match args with
               | [| Value.Ptr addr; k |] ->
                   hash_delete heap addr k;
                   Value.Nil
               | _ -> Exception.throw e_bad_arg "hash/delete expects ptr key");
           nif "length" (fun args ->
               match args with
               | [| Value.Ptr addr |] -> Value.Int (count heap addr)
               | _ -> Exception.throw e_bad_arg "hash/length expects ptr");
           nif "is_hash" (fun args ->
               match args with
               | [| Value.Ptr addr |] ->
                   Value.Int
                     (if H.get_tag heap addr = Fold_core.Tag.hash then 1 else 0)
               | [| _ |] -> Value.Int 0
               | _ -> Exception.throw e_bad_arg "hash/is_hash expects 1 arg");
         ])
end
