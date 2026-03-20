open Map_core
open Map.Namespace

let registry  = Exception.create ()
let e_bad_arg = Exception.register registry ~name:"math/bad_arg"
let e_arity   = Exception.register registry ~name:"math/arity"

let register (reg : Map.Symbol.registry) =
  Map.Namespace.register_pure reg
    (ns "std/math" [

      ns "const" [
        native "pi"   (Value.Float Float.pi);
        native "tau"  (Value.Float (2.0 *. Float.pi));
        native "e"    (Value.Float (Float.exp 1.0));
        native "grav" (Value.Float 9.80665);
      ];

      nif "abs" (fun args ->
        match args with
        | [| Value.Float x |] -> Value.Float (abs_float x)
        | [| _ |]             -> Exception.throw e_bad_arg "math/abs expects float"
        | _                   ->
          Exception.throw e_arity
            (Printf.sprintf "math/abs expects 1 argument, got %d" (Array.length args)));

      nif "sqrt" (fun args ->
        match args with
        | [| Value.Float x |] -> Value.Float (sqrt x)
        | [| _ |]             -> Exception.throw e_bad_arg "math/sqrt expects float"
        | _                   ->
          Exception.throw e_arity
            (Printf.sprintf "math/sqrt expects 1 argument, got %d" (Array.length args)));

      nif "sin" (fun args ->
        match args with
        | [| Value.Float x |] -> Value.Float (sin x)
        | [| _ |]             -> Exception.throw e_bad_arg "math/sin expects float"
        | _                   ->
          Exception.throw e_arity
            (Printf.sprintf "math/sin expects 1 argument, got %d" (Array.length args)));

      nif "cos" (fun args ->
        match args with
        | [| Value.Float x |] -> Value.Float (cos x)
        | [| _ |]             -> Exception.throw e_bad_arg "math/cos expects float"
        | _                   ->
          Exception.throw e_arity
            (Printf.sprintf "math/cos expects 1 argument, got %d" (Array.length args)));

      nif "atan2" (fun args ->
        match args with
        | [| Value.Float y; Value.Float x |] -> Value.Float (atan2 y x)
        | [| _; _ |]                         ->
          Exception.throw e_bad_arg "math/atan2 expects float float"
        | _                                  ->
          Exception.throw e_arity
            (Printf.sprintf "math/atan2 expects 2 arguments, got %d" (Array.length args)));

      nif "lerp" (fun args ->
        match args with
        | [| Value.Float a; Value.Float b; Value.Float t |] ->
          Value.Float (a +. t *. (b -. a))
        | [| _; _; _ |] ->
          Exception.throw e_bad_arg "math/lerp expects float float float"
        | _ ->
          Exception.throw e_arity
            (Printf.sprintf "math/lerp expects 3 arguments, got %d" (Array.length args)));

      nif "clamp" (fun args ->
        match args with
        | [| Value.Float x; Value.Float lo; Value.Float hi |] ->
          Value.Float (Float.max lo (Float.min hi x))
        | [| _; _; _ |] ->
          Exception.throw e_bad_arg "math/clamp expects float float float"
        | _ ->
          Exception.throw e_arity
            (Printf.sprintf "math/clamp expects 3 arguments, got %d" (Array.length args)));

      nif "map" (fun args ->
        match args with
        | [| Value.Float v; Value.Float s1; Value.Float e1;
             Value.Float s2; Value.Float e2 |] ->
          Value.Float (s2 +. (v -. s1) *. (e2 -. s2) /. (e1 -. s1))
        | [| _; _; _; _; _ |] ->
          Exception.throw e_bad_arg "math/map expects 5 floats"
        | _ ->
          Exception.throw e_arity
            (Printf.sprintf "math/map expects 5 arguments, got %d" (Array.length args)));

      ns "vec2" [
        nif "mag" (fun args ->
          match args with
          | [| Value.Float x; Value.Float y |] ->
            Value.Float (sqrt (x *. x +. y *. y))
          | [| _; _ |] ->
            Exception.throw e_bad_arg "vec2/mag expects float float"
          | _ ->
            Exception.throw e_arity
              (Printf.sprintf "vec2/mag expects 2 arguments, got %d" (Array.length args)));

        nif "dist" (fun args ->
          match args with
          | [| Value.Float x1; Value.Float y1; Value.Float x2; Value.Float y2 |] ->
            let dx = x2 -. x1 and dy = y2 -. y1 in
            Value.Float (sqrt (dx *. dx +. dy *. dy))
          | [| _; _; _; _ |] ->
            Exception.throw e_bad_arg "vec2/dist expects 4 floats"
          | _ ->
            Exception.throw e_arity
              (Printf.sprintf "vec2/dist expects 4 arguments, got %d" (Array.length args)));

        nif "dot" (fun args ->
          match args with
          | [| Value.Float x1; Value.Float y1; Value.Float x2; Value.Float y2 |] ->
            Value.Float (x1 *. x2 +. y1 *. y2)
          | [| _; _; _; _ |] ->
            Exception.throw e_bad_arg "vec2/dot expects 4 floats"
          | _ ->
            Exception.throw e_arity
              (Printf.sprintf "vec2/dot expects 4 arguments, got %d" (Array.length args)));
      ];

      ns "rng" [
        nif "float" (fun args ->
          match args with
          | [| Value.Float lo; Value.Float hi |] ->
            Value.Float (lo +. Random.float (hi -. lo))
          | _ -> Value.Float (Random.float 1.0));

        nif "int" (fun args ->
          match args with
          | [| Value.Int lo; Value.Int hi |] ->
            Value.Int (lo + Random.int (hi - lo))
          | _ -> Value.Int (Random.int 100));
      ];
    ])
