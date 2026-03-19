open Core

module Heap = Sigs.Heap

module Make(H : Heap.S) = struct
  module Ms = Mstring.Make(H)

  let ns heap =
    let estr = Ms.expect_str heap in
    let open Map.Namespace in
    ns "gui" [
      fn "init_window" (function
        | [| Value.Int w; Value.Int h; s |] ->
            Raylib.init_window w h (estr s);
            Raylib.set_target_fps 60;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/init_window expects int int string"));

      fn "window_should_close" (fun _ ->
        Value.Int (if Raylib.window_should_close () then 1 else 0));

      fn "begin_drawing" (fun _ ->
        Raylib.begin_drawing ();
        Value.Nil);

      fn "end_drawing" (fun _ ->
        Raylib.end_drawing ();
        Value.Nil);

      fn "clear_background" (function
        | [| Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
            Raylib.clear_background (Raylib.Color.create r g b a);
            Value.Nil
        | _ ->
            Raylib.clear_background Raylib.Color.raywhite;
            Value.Nil);

      fn "close_window" (fun _ ->
        Raylib.close_window ();
        Value.Nil);

      fn "draw_text" (function
        | [| s; Value.Int x; Value.Int y; Value.Int size;
             Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
            Raylib.draw_text (estr s) x y size (Raylib.Color.create r g b a);
            Value.Nil
        | [| s; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (estr s) x y size Raylib.Color.darkgray;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/draw_text expects string int int int [int int int int]"));

      fn "draw_int" (function
        | [| Value.Int n; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (string_of_int n) x y size Raylib.Color.darkgray;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/draw_int expects int int int int"));

      fn "draw_float" (function
        | [| Value.Float f; Value.Int x; Value.Int y; Value.Int size |] ->
            Raylib.draw_text (string_of_float f) x y size Raylib.Color.darkgray;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/draw_float expects float int int int"));

      fn "set_target_fps" (function
        | [| Value.Int fps |] ->
            Raylib.set_target_fps fps;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/set_target_fps expects int"));

      fn "get_frame_time" (fun _ ->
        Value.Float (Raylib.get_frame_time ()));

      fn "get_fps" (fun _ ->
        Value.Int (Raylib.get_fps ()));

      ns "shapes" [
        fn "draw_circle" (function
          | [| Value.Int x; Value.Int y; Value.Float r;
               Value.Int cr; Value.Int cg; Value.Int cb; Value.Int ca |] ->
              Raylib.draw_circle x y r (Raylib.Color.create cr cg cb ca);
              Value.Nil
          | [| Value.Int x; Value.Int y; Value.Float r |] ->
              Raylib.draw_circle x y r Raylib.Color.maroon;
              Value.Nil
          | _ -> raise (Exception.Type_error "gui/shapes/draw_circle expects int int float [int int int int]"));

        fn "draw_rect" (function
          | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h;
               Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
              Raylib.draw_rectangle x y w h (Raylib.Color.create r g b a);
              Value.Nil
          | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h |] ->
              Raylib.draw_rectangle x y w h Raylib.Color.darkblue;
              Value.Nil
          | _ -> raise (Exception.Type_error "gui/shapes/draw_rect expects int int int int [int int int int]"));

        fn "draw_line" (function
          | [| Value.Int x1; Value.Int y1; Value.Int x2; Value.Int y2;
               Value.Int r; Value.Int g; Value.Int b; Value.Int a |] ->
              Raylib.draw_line x1 y1 x2 y2 (Raylib.Color.create r g b a);
              Value.Nil
          | _ -> raise (Exception.Type_error "gui/shapes/draw_line expects int int int int int int int int"));
      ];

      ns "input" [
        fn "is_key_down" (function
          | [| Value.Int k |] ->
              Value.Int (if Raylib.is_key_down (Raylib.Key.of_int k) then 1 else 0)
          | _ -> raise (Exception.Type_error "gui/input/is_key_down expects int"));

        fn "is_key_pressed" (function
          | [| Value.Int k |] ->
              Value.Int (if Raylib.is_key_pressed (Raylib.Key.of_int k) then 1 else 0)
          | _ -> raise (Exception.Type_error "gui/input/is_key_pressed expects int"));

        fn "mouse_x" (fun _ -> Value.Int (Raylib.get_mouse_x ()));
        fn "mouse_y" (fun _ -> Value.Int (Raylib.get_mouse_y ()));

        fn "is_mouse_down" (function
          | [| Value.Int b |] ->
              Value.Int (if Raylib.is_mouse_button_down (Raylib.MouseButton.of_int b) then 1 else 0)
          | _ -> raise (Exception.Type_error "gui/input/is_mouse_down expects int"));
      ];
    ]
end
