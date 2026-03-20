type (_, _) equal = Equal : ('a, 'a) equal

module Id = struct
  type 'a t = { eq : 'b. 'b t -> ('a, 'b) equal option }

  let make (type a) () : a t =
    let rec id : a t =
      {
        eq =
          (fun other ->
            if Obj.repr id == Obj.repr other then Some (Obj.magic Equal)
            else None);
      }
    in
    id

  let provably_equal : type a b. a t -> b t -> (a, b) equal option =
   fun a b -> a.eq b
end
