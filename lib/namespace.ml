open Core

module Heap = Sigs.Heap

type fn =
  | Fun      of Value.t
  | Bytecode of Instr.t array * Value.t array

type t =
  | Leaf of string * fn
  | Node of string * t list

type indexed = {
  tree  : t;
  i     : string -> int;
  count : int;
}

let name_of = function
  | Leaf (n, _) -> n
  | Node (n, _) -> n

let ns   name children          = Node (name, children)
let native  name v              = Leaf (name, Fun v)
let fn   name f                 = Leaf (name, Fun (Value.NativeFun f))
let bytecode name code consts   = Leaf (name, Bytecode (code, consts))

let prepare root =
  let tbl = Hashtbl.create 64 in
  let rec walk path = function
    | Leaf _ -> ()
    | Node (_, children) ->
      List.iteri (fun i child ->
        let p = if path = "" then name_of child else path ^ "/" ^ name_of child in
        Hashtbl.add tbl p i;
        walk p child
      ) children
  in
  walk "" root;
  { tree  = root;
    count = Hashtbl.length tbl;
    i = fun path ->
      match Hashtbl.find_opt tbl path with
      | Some i -> i
      | None   -> failwith (Printf.sprintf "Map.Namespace.prepare: '%s' not found" path) }

module Make(H : Heap.S) = struct

  let rec load heap = function
    | Leaf (_, Fun v) ->
      v
    | Leaf (_, Bytecode (code, _)) ->
      let code_ptr = Value.make_native
        ~tag:Vm.bytecode_id
        ~finalizer:None
        code in
      let addr = H.alloc heap ~size:2 ~tag:Tag.closure in
      H.write heap addr 0 code_ptr;
      H.write heap addr 1 Value.Nil;
      Value.Ptr addr
    | Node (_, children) ->
      let n    = List.length children in
      let addr = H.alloc heap ~size:n ~tag:Tag.namespace in
      List.iteri (fun i child ->
        H.write heap addr i (load heap child)
      ) children;
      Value.Ptr addr

  let find_child children name =
    List.find_opt (fun n -> name_of n = name) children

  let resolve ns path =
    let parts = String.split_on_char '/' path in
    let rec walk node = function
      | []        -> Some node
      | p :: rest ->
        match node with
        | Node (_, children) ->
          (match find_child children p with
           | Some child -> walk child rest
           | None       -> None)
        | Leaf _ -> None
    in
    walk ns parts

  let resolve_index ns path =
    let parts = String.split_on_char '/' path in
    match List.rev parts with
    | []             -> None
    | target :: rest ->
      let parent_parts = List.rev rest in
      let parent_node  =
        if parent_parts = [] then Some ns
        else resolve ns (String.concat "/" parent_parts)
      in
      (match parent_node with
       | Some (Node (_, children)) ->
         let idx = ref None in
         List.iteri (fun i child ->
           if name_of child = target then idx := Some i
         ) children;
         !idx
       | _ -> None)

  let iter ns f =
    let rec walk path node =
      let p = if path = "" then name_of node else path ^ "/" ^ name_of node in
      match node with
      | Leaf (_, fn)       -> f p fn
      | Node (_, children) -> List.iter (walk p) children
    in
    walk "" ns

  let all_paths ns =
    let acc = ref [] in
    iter ns (fun path _ -> acc := path :: !acc);
    List.rev !acc

  let pp ns =
    let rec walk indent = function
      | Leaf (name, Fun (Value.NativeFun _)) ->
        Printf.printf "%s%s  [NativeFun]\n" indent name
      | Leaf (name, Fun v) ->
        Printf.printf "%s%s  [%s]\n" indent name (Value.to_string v)
      | Leaf (name, Bytecode _) ->
        Printf.printf "%s%s  [Bytecode]\n" indent name
      | Node (name, children) ->
        Printf.printf "%s%s/\n" indent name;
        List.iter (walk (indent ^ "  ")) children
    in
    walk "" ns

end
