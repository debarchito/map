open Map_core

let ns =
  let open Map.Namespace in
  ns "gui" [
    fn "init_window" (function
      | [| Value.Int w; Value.Int h |] ->
          Raylib.init_window w h "TEST";
          Raylib.set_target_fps 60;
          Value.Nil
      | _ -> raise (Exception.Type_error "gui/init_window expects int int"));

    fn "window_should_close" (fun _ ->
      Value.Int (if Raylib.window_should_close () then 1 else 0));

    fn "begin_drawing" (fun _ -> 
      Raylib.begin_drawing (); 
      Value.Nil);

    fn "end_drawing" (fun _ -> 
      Raylib.end_drawing (); 
      Value.Nil);

    fn "clear_background" (fun _ ->
      Raylib.clear_background Raylib.Color.raywhite; 
      Value.Nil);

    fn "close_window" (fun _ -> 
      Raylib.close_window (); 
      Value.Nil);

    fn "draw_text" (function
      | [| Value.Int x; Value.Int y; Value.Int size |] ->
          Raylib.draw_text "Hello" x y size Raylib.Color.darkgray;
          Value.Nil
      | _ -> raise (Exception.Type_error "gui/draw_text expects int int int"));

    fn "draw_float" (function
      | [| Value.Float n; Value.Float r; Value.Int x; Value.Int y; Value.Int size |] ->
          Raylib.draw_text
            (Printf.sprintf "abs(%d) = %d" (int_of_float n) (int_of_float r))
            x y size Raylib.Color.darkgray;
          Value.Nil
      | _ -> raise (Exception.Type_error "gui/draw_float expects float float int int int"));

    ns "shapes" [
      fn "draw_circle" (function
        | [| Value.Int x; Value.Int y; Value.Float r |] ->
            Raylib.draw_circle x y r Raylib.Color.maroon;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/shapes/draw_circle expects int int float"));

      fn "draw_rect" (function
        | [| Value.Int x; Value.Int y; Value.Int w; Value.Int h |] ->
            Raylib.draw_rectangle x y w h Raylib.Color.darkblue;
            Value.Nil
        | _ -> raise (Exception.Type_error "gui/shapes/draw_rect expects int int int int"));
    ];
  ]
