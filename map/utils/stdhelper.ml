open Map_core

let expect_args name count types f = function
  | args when Array.length args <> count ->
      raise (Exception.Arity_error (Printf.sprintf "%s expects %d arguments, got %d" name count (Array.length args)))
  | args ->
      try f args with
      | Match_failure _ -> 
          raise (Exception.Type_error (Printf.sprintf "%s expects (%s)" name types))
