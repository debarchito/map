open Map_core
open Map.Instr
open Map.Namespace

let cfg = Config.default

let root = index (ns "root" [
  ns "raylib" [
    fn "init_window" (Value.NativeFun (fun args ->
      match args with
      | [| Value.Int w; Value.Int h |] ->
        Raylib.init_window w h "MAP VM — Fixed";
        Raylib.set_target_fps 60;
        Value.Nil
      | _ -> raise (Exception.Type_error "init_window expects int int")));

    fn "window_should_close" (Value.NativeFun (fun _ ->
      Value.Int (if Raylib.window_should_close () then 1 else 0)));

    fn "begin_drawing" (Value.NativeFun (fun _ -> Raylib.begin_drawing (); Value.Nil));

    fn "end_drawing" (Value.NativeFun (fun _ -> Raylib.end_drawing (); Value.Nil));

    fn "clear_background" (Value.NativeFun (fun _ ->
      Raylib.clear_background Raylib.Color.raywhite; Value.Nil));

    fn "close_window" (Value.NativeFun (fun _ -> Raylib.close_window (); Value.Nil));

    fn "draw_float" (Value.NativeFun (fun args ->
      match args with
      | [| Value.Float n; Value.Float r; Value.Int x; Value.Int y; Value.Int size |] ->
        Raylib.draw_text
          (Printf.sprintf "fact(%d) = %d" (int_of_float n) (int_of_float r))
          x y size Raylib.Color.darkgray;
        Value.Nil
      | _ -> raise (Exception.Type_error "draw_float expects float float int int int")));
  ];

  ns "math" [
    bytecode "factorial" [|
      (*  1 *) LoadF (1, 1.0);
      (*  2 *) LteF  (2, 0, 1);
      (*  3 *) Jnz   (2, 9);
      (*  4 *) Mov   (5, 0);
      (*  5 *) LoadF (6, 1.0);
      (*  6 *) SubF  (6, 0, 6);
      (*  7 *) Call  (0, 6, 6, 7);
      (*  8 *) MulF  (1, 5, 7);
      (*  9 *) Ret   1;
      (* 10 *) LoadF (1, 1.0);
      (* 11 *) Ret   1;
   |] [||];
  ];
])

module Ns = Map.Namespace.Make(Map.Heap.Fast)

let program = [|
  (*  0 *) GetField  (1, 0, root.i "raylib");
  (*  1 *) Load      (7, 800);
  (*  2 *) Load      (8, 450);
  (*  3 *) GetField  (2, 1, root.i "raylib/init_window");
  (*  4 *) DCall     (2, 7, 8, 9);
  (*  5 *) GetField  (3, 0, root.i "math");
  (*  6 *) GetField  (2, 3, root.i "math/factorial");
  (*  7 *) LoadF     (10, 17.0);                                   (* input! *)
  (*  8 *) DCall     (2, 10, 10, 11);
  (*  9 *) GetField  (2, 1, root.i "raylib/window_should_close");
  (* 10 *) DCall     (2, 7, 7, 9);
  (* 11 *) Jnz       (9, 25);
  (* 12 *) GetField  (2, 1, root.i "raylib/begin_drawing");
  (* 13 *) DCall     (2, 7, 7, 9);
  (* 14 *) GetField  (2, 1, root.i "raylib/clear_background");
  (* 15 *) DCall     (2, 7, 7, 9);
  (* 16 *) Load      (12, 150);
  (* 17 *) Load      (13, 180);
  (* 18 *) Load      (14, 28);
  (* 19 *) GetField  (2, 1, root.i "raylib/draw_float");
  (* 20 *) DCall     (2, 10, 14, 9); 
  (* 21 *) GetField  (2, 1, root.i "raylib/end_drawing");
  (* 22 *) DCall     (2, 7, 7, 9);
  (* 23 *) Jmp       (9);
  (* 25 *) GetField  (2, 1, root.i "raylib/close_window");
  (* 26 *) DCall     (2, 7, 7, 9);
  (* 27 *) Halt;
|]

let () =
  let vm  = Map.Vm.Fast.create cfg program () () in
  let ptr = Ns.load (Map.Vm.Fast.heap vm) root.tree in
  Map.Vm.Fast.set_reg vm 0 ptr;
  Map.Vm.Fast.run vm;
  Printf.printf "status: %s\n" (Map.Vm.Fast.get_status vm)
