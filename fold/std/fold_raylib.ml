open Map.Core
open Map.Namespace

module Make(H : Map.Heap.S) = struct

  module Fs = Fold_string.Make(H)
  module NS = Map.Namespace.Make(H)

  let registry  = Exception.create ()
  let e_bad_arg = Exception.register registry ~name:"raylib/bad_arg"

  let register heap (reg : Map.Symbol.registry) =
    let estr = Fs.expect_str heap in
    NS.register heap reg
      (ns "std/raylib" [

        nif "init_window" (fun args ->
          match args with
          | [| Value.Int w; Value.Int h; s |] ->
            Raylib.init_window w h (estr s);
            Raylib.set_target_fps 60;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "raylib/init_window expects int int string");

        nif "window_should_close" (fun _ ->
          Value.Int (if Raylib.window_should_close () then 1 else 0));

        nif "begin_drawing" (fun _ ->
          Raylib.begin_drawing ();
          Value.Nil);

        nif "end_drawing" (fun _ ->
          Raylib.end_drawing ();
          Value.Nil);

        nif "clear_background" (fun args ->
          match args with
          | [| Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
            Raylib.clear_background (Raylib.Color.create r g b a);
            Value.Nil
          | _ ->
            Raylib.clear_background Raylib.Color.raywhite;
            Value.Nil);

        nif "close_window" (fun _ ->
          Raylib.close_window ();
          Value.Nil);

        nif "draw_text" (fun args ->
          match args with
          | [| s; Value.Int x; Value.Int y; Value.Int size;
               Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
            Raylib.draw_text (estr s) x y size (Raylib.Color.create r g b a);
            Value.Nil
          | [| s; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (estr s) x y size Raylib.Color.darkgray;
            Value.Nil
          | _ -> Exception.throw e_bad_arg
              "raylib/draw_text expects string int int int [int int int int]");

        nif "draw_int" (fun args ->
          match args with
          | [| Value.Int n; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (string_of_int n) x y size Raylib.Color.darkgray;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "raylib/draw_int expects int int int int");

        nif "draw_float" (fun args ->
          match args with
          | [| Value.Float f; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (string_of_float f) x y size Raylib.Color.darkgray;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "raylib/draw_float expects float int int int");

        nif "set_target_fps" (fun args ->
          match args with
          | [| Value.Int fps |] ->
            Raylib.set_target_fps fps;
            Value.Nil
          | _ -> Exception.throw e_bad_arg "raylib/set_target_fps expects int");

        nif "get_frame_time" (fun _ ->
          Value.Float (Raylib.get_frame_time ()));

        nif "get_fps" (fun _ ->
          Value.Int (Raylib.get_fps ()));

        ns "shapes" [
          nif "draw_circle" (fun args ->
            match args with
            | [| Value.Int x; Value.Int y; Value.Float r;
                 Value.Int cr; Value.Int cg; Value.Int cb; Value.Int ca |] ->
              Raylib.draw_circle x y r (Raylib.Color.create cr cg cb ca);
              Value.Nil
            | [| Value.Int x; Value.Int y; Value.Float r |] ->
              Raylib.draw_circle x y r Raylib.Color.maroon;
              Value.Nil
            | _ -> Exception.throw e_bad_arg
                "raylib/shapes/draw_circle expects int int float [int int int int]");

          nif "draw_rect" (fun args ->
            match args with
            | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h;
                 Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
              Raylib.draw_rectangle x y w h (Raylib.Color.create r g b a);
              Value.Nil
            | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h |] ->
              Raylib.draw_rectangle x y w h Raylib.Color.darkblue;
              Value.Nil
            | _ -> Exception.throw e_bad_arg
                "raylib/shapes/draw_rect expects int int int int [int int int int]");

          nif "draw_line" (fun args ->
            match args with
            | [| Value.Int x1; Value.Int y1; Value.Int x2; Value.Int y2;
                 Value.Int r;  Value.Int g;  Value.Int b;  Value.Int a |] ->
              Raylib.draw_line x1 y1 x2 y2 (Raylib.Color.create r g b a);
              Value.Nil
            | _ -> Exception.throw e_bad_arg
                "raylib/shapes/draw_line expects int int int int int int int int");
        ];

        ns "input" [
          nif "is_key_down" (fun args ->
            match args with
            | [| Value.Int k |] ->
              Value.Int (if Raylib.is_key_down (Raylib.Key.of_int k) then 1 else 0)
            | _ -> Exception.throw e_bad_arg "raylib/input/is_key_down expects int");

          nif "is_key_pressed" (fun args ->
            match args with
            | [| Value.Int k |] ->
              Value.Int (if Raylib.is_key_pressed (Raylib.Key.of_int k) then 1 else 0)
            | _ -> Exception.throw e_bad_arg "raylib/input/is_key_pressed expects int");

          nif "mouse_x" (fun _ -> Value.Int (Raylib.get_mouse_x ()));
          nif "mouse_y" (fun _ -> Value.Int (Raylib.get_mouse_y ()));

          nif "is_mouse_down" (fun args ->
            match args with
            | [| Value.Int b |] ->
              Value.Int (if Raylib.is_mouse_button_down
                (Raylib.MouseButton.of_int b) then 1 else 0)
            | _ -> Exception.throw e_bad_arg "raylib/input/is_mouse_down expects int");
        ];
      ])

end
