open Map_core
open Map.Instr

let cfg = Config.default

module BufferDebug  = Fold.Std.Buffer.Make(Map.Heap.Debug)
module StringDebug  = Fold.Std.String.Make(Map.Heap.Debug)
module SymbolDebug  = Fold.Std.Symbol.Make(Map.Heap.Debug)
module ConsDebug    = Fold.Std.Cons.Make(Map.Heap.Debug)
module VectorDebug  = Fold.Std.Vector.Make(Map.Heap.Debug)
module HashDebug    = Fold.Std.Hash.Make(Map.Heap.Debug)
module VariantDebug = Fold.Std.Variant.Make(Map.Heap.Debug)
module RecordDebug  = Fold.Std.Record.Make(Map.Heap.Debug)
module RaylibDebug  = Fold.Std.Raylib.Make(Map.Heap.Debug)

let program = Array.make 46 Nop

let () =
  let tc   = Map.Vm.Full_tracer.make () in
  let hc   = Map.Heap.Full_tracer.make
               ~max_chunks:cfg.Config.heap.max_chunks
               ~chunk_size:cfg.Config.heap.chunk_size
               ~sample_rate:1 in
  let vm   = Map.Vm.Debug.create cfg program hc tc in
  let heap = Map.Vm.Debug.heap vm in
  let reg  = Map.Vm.Debug.symbols vm in
  
  BufferDebug.register  heap reg;
  StringDebug.register  heap reg;
  SymbolDebug.register  heap reg;
  ConsDebug.register    heap reg;
  VectorDebug.register  heap reg;
  HashDebug.register    heap reg;
  VariantDebug.register heap reg;
  RecordDebug.register  heap reg;

  Fold.Std.Math.register reg;
  RaylibDebug.register  heap reg;

  let sym = Map.Symbol.sym reg in

  Map.Vm.Debug.set_const vm 0 (StringDebug.of_ocaml heap "Map Window");
  Map.Vm.Debug.set_const vm 1 (StringDebug.of_ocaml heap "(assert (cos %g) %g) ; #t");

  let p = program in
  (*  0 *) p.(0)  <- LoadS  (1,  sym "std/math/cos");
  (*  1 *) p.(1)  <- LoadF  (10, 9.0);
  (*  2 *) p.(2)  <- DCall  (1,  10, 10, 11);
  (*  3 *) p.(3)  <- LoadS  (4,  sym "std/raylib/init_window");
  (*  4 *) p.(4)  <- Load   (5,  800);
  (*  5 *) p.(5)  <- Load   (6,  450);
  (*  6 *) p.(6)  <- LoadK  (7,  0);
  (*  7 *) p.(7)  <- DCall  (4,  5,  7,  8);
  (*  8 *) p.(8)  <- LoadS  (20, sym "std/raylib/window_should_close");
  (*  9 *) p.(9)  <- DCall  (20, 5,  5,  21);
  (* 10 *) p.(10) <- Jnz    (21, 40);
  (* 11 *) p.(11) <- LoadS  (20, sym "std/raylib/begin_drawing");
  (* 12 *) p.(12) <- DCall  (20, 5,  5,  8);
  (* 13 *) p.(13) <- LoadS  (20, sym "std/raylib/clear_background");
  (* 14 *) p.(14) <- Load   (22, 245);
  (* 15 *) p.(15) <- Load   (23, 245);
  (* 16 *) p.(16) <- Load   (24, 245);
  (* 17 *) p.(17) <- Load   (25, 255);
  (* 18 *) p.(18) <- DCall  (20, 22, 25, 8);
  (* 19 *) p.(19) <- LoadS  (32, sym "std/string/format");
  (* 20 *) p.(20) <- LoadK  (33, 1);
  (* 21 *) p.(21) <- Mov    (34, 10);
  (* 22 *) p.(22) <- Mov    (35, 11);
  (* 23 *) p.(23) <- DCall  (32, 33, 35, 36);
  (* 24 *) p.(24) <- LoadS  (20, sym "std/raylib/draw_text");
  (* 25 *) p.(25) <- Load   (37, 150);
  (* 26 *) p.(26) <- Load   (38, 180);
  (* 27 *) p.(27) <- Load   (39, 28);
  (* 28 *) p.(28) <- DCall  (20, 36, 39, 8);
  (* 29 *) p.(29) <- LoadS  (20, sym "std/raylib/end_drawing");
  (* 30 *) p.(30) <- DCall  (20, 5,  5,  8);
  (* 31 *) p.(31) <- Jmp    8;
  (* 40 *) p.(40) <- LoadS  (20, sym "std/raylib/close_window");
  (* 41 *) p.(41) <- DCall  (20, 5,  5,  8);
  (* 42 *) p.(42) <- Halt;

  Map.Vm.Debug.run vm;

  Printf.printf "[STATUS] %s\n" (Map.Vm.Debug.get_status vm);

  Printf.printf "\n[FINAL REGISTERS]\n\n";
  for i = 0 to cfg.Config.vm.num_registers - 1 do
    let v = Map.Vm.Debug.get_reg vm i in
    match v with
    | Value.Nil -> ()
    | _ -> Printf.printf "r%-3d  %s\n" i (Value.to_string v)
  done
