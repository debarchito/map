open Core

let ns =
  let open Map.Namespace in
  ns "math" [
    
    ns "const" [
      native "pi"   (Value.Float Float.pi);
      native "tau"  (Value.Float (2.0 *. Float.pi));
      native "e"    (Value.Float (Float.exp 1.0));
      native "grav" (Value.Float 9.80665);
    ];

    fn "abs"   (function [| Value.Float x |] -> Value.Float (abs_float x) | _ -> raise (Exception.Type_error "math/abs expects float"));
    fn "sqrt"  (function [| Value.Float x |] -> Value.Float (sqrt x)      | _ -> raise (Exception.Type_error "math/sqrt expects float"));
    fn "sin"   (function [| Value.Float x |] -> Value.Float (sin x)       | _ -> raise (Exception.Type_error "math/sin expects float"));
    fn "cos"   (function [| Value.Float x |] -> Value.Float (cos x)       | _ -> raise (Exception.Type_error "math/cos expects float"));
    fn "atan2" (function [| Value.Float y; Value.Float x |] -> Value.Float (atan2 y x) | _ -> raise (Exception.Type_error "math/atan2 expects y x"));

    fn "lerp" (function 
      | [| Value.Float a; Value.Float b; Value.Float t |] -> Value.Float (a +. t *. (b -. a))
      | _ -> raise (Exception.Type_error "math/lerp expects a, b, t"));
    
    fn "clamp" (function 
      | [| Value.Float x; Value.Float min; Value.Float max |] -> Value.Float (Float.max min (Float.min max x))
      | _ -> raise (Exception.Type_error "math/clamp expects x, min, max"));

    fn "map" (function 
      | [| Value.Float v; Value.Float s1; Value.Float e1; Value.Float s2; Value.Float e2 |] ->
          let res = s2 +. (v -. s1) *. (e2 -. s2) /. (e1 -. s1) in
          Value.Float res
      | _ -> raise (Exception.Type_error "math/map expects 5 floats"));

    ns "vec2" [
      fn "mag" (function 
        | [| Value.Float x; Value.Float y |] -> Value.Float (sqrt (x*.x +. y*.y))
        | _ -> raise (Exception.Type_error "v2/mag expects x y"));
      
      fn "dist" (function 
        | [| Value.Float x1; Value.Float y1; Value.Float x2; Value.Float y2 |] ->
            let dx, dy = x2 -. x1, y2 -. y1 in
            Value.Float (sqrt (dx*.dx +. dy*.dy))
        | _ -> raise (Exception.Type_error "v2/dist expects x1 y1 x2 y2"));

      fn "dot" (function 
        | [| Value.Float x1; Value.Float y1; Value.Float x2; Value.Float y2 |] ->
            Value.Float (x1 *. x2 +. y1 *. y2)
        | _ -> raise (Exception.Type_error "v2/dot expects x1 y1 x2 y2"));
    ];

    ns "rng" [
      fn "float" (function 
        | [| Value.Float min; Value.Float max |] -> Value.Float (min +. Random.float (max -. min))
        | _ -> Value.Float (Random.float 1.0));
      fn "int" (function 
        | [| Value.Int min; Value.Int max |] -> Value.Int (min + Random.int (max - min))
        | _ -> Value.Int (Random.int 100));
    ];
  ]
