open Map_core

let cfg = Config.default

module BufferDebug = Fold.Std.Buffer.Make (Map.Heap.Debug)
module StringDebug = Fold.Std.String.Make (Map.Heap.Debug)
module SymbolDebug = Fold.Std.Symbol.Make (Map.Heap.Debug)
module ConsDebug = Fold.Std.Cons.Make (Map.Heap.Debug)
module VectorDebug = Fold.Std.Vector.Make (Map.Heap.Debug)
module HashDebug = Fold.Std.Hash.Make (Map.Heap.Debug)
module VariantDebug = Fold.Std.Variant.Make (Map.Heap.Debug)
module RecordDebug = Fold.Std.Record.Make (Map.Heap.Debug)
module RaylibDebug = Fold.Std.Raylib.Make (Map.Heap.Debug)

let register_stdlib heap reg =
  BufferDebug.register heap reg;
  StringDebug.register heap reg;
  SymbolDebug.register heap reg;
  ConsDebug.register heap reg;
  VectorDebug.register heap reg;
  HashDebug.register heap reg;
  VariantDebug.register heap reg;
  RecordDebug.register heap reg;
  Fold.Std.Math.register reg;
  RaylibDebug.register heap reg

let () =
  let tc = Map.Vm.Full_tracer.make () in
  let hc =
    Map.Heap.Full_tracer.make ~max_chunks:cfg.Config.heap.max_chunks
      ~chunk_size:cfg.Config.heap.chunk_size ~sample_rate:1
  in

  let sym_vm = Map.Vm.Debug.create cfg (Array.make 1 Map.Instr.Nop) hc tc in
  let sym_heap = Map.Vm.Debug.heap sym_vm in
  let sym_reg = Map.Vm.Debug.symbols sym_vm in
  register_stdlib sym_heap sym_reg;
  let sym = Map.Symbol.sym sym_reg in

  let asm = Map.Asm.create () in
  let open Map.Instr in
  ignore (Map.Asm.push_scope asm);

  let k_title = Map.Asm.const_value asm Value.Nil in
  let k_fmt = Map.Asm.const_value asm Value.Nil in
  ignore (k_title, k_fmt);

  Map.Asm.emit asm (LoadS (1, sym "std/math/cos")) |> ignore;
  Map.Asm.emit asm (LoadF (10, 9.0)) |> ignore;
  Map.Asm.emit asm (DCall (1, 10, 10, 11)) |> ignore;

  Map.Asm.emit asm (LoadS (4, sym "std/raylib/init_window")) |> ignore;
  Map.Asm.emit asm (Load (5, 800)) |> ignore;
  Map.Asm.emit asm (Load (6, 450)) |> ignore;
  Map.Asm.emit asm (LoadK (7, 0)) |> ignore;
  Map.Asm.emit asm (DCall (4, 5, 7, 8)) |> ignore;

  Map.Asm.label asm "loop";
  Map.Asm.emit asm (LoadS (20, sym "std/raylib/window_should_close")) |> ignore;
  Map.Asm.emit asm (DCall (20, 20, 20, 21)) |> ignore;
  Map.Asm.jnz asm 21 "close";

  Map.Asm.emit asm (LoadS (20, sym "std/raylib/begin_drawing")) |> ignore;
  Map.Asm.emit asm (DCall (20, 20, 20, 8)) |> ignore;

  Map.Asm.emit asm (LoadS (20, sym "std/raylib/clear_background")) |> ignore;
  Map.Asm.emit asm (Load (1, 245)) |> ignore;
  Map.Asm.emit asm (Load (2, 245)) |> ignore;
  Map.Asm.emit asm (Load (3, 245)) |> ignore;
  Map.Asm.emit asm (Load (4, 255)) |> ignore;
  Map.Asm.emit asm (DCall (20, 1, 4, 8)) |> ignore;

  Map.Asm.emit asm (LoadS (32, sym "std/string/format")) |> ignore;
  Map.Asm.emit asm (LoadK (33, 1)) |> ignore;
  Map.Asm.emit asm (Mov (34, 10)) |> ignore;
  Map.Asm.emit asm (Mov (35, 11)) |> ignore;
  Map.Asm.emit asm (DCall (32, 33, 35, 36)) |> ignore;

  Map.Asm.emit asm (LoadS (20, sym "std/raylib/draw_text")) |> ignore;
  Map.Asm.emit asm (Mov (1, 36)) |> ignore;
  Map.Asm.emit asm (Load (2, 150)) |> ignore;
  Map.Asm.emit asm (Load (3, 180)) |> ignore;
  Map.Asm.emit asm (Load (4, 28)) |> ignore;
  Map.Asm.emit asm (DCall (20, 1, 4, 8)) |> ignore;

  Map.Asm.emit asm (LoadS (20, sym "std/raylib/end_drawing")) |> ignore;
  Map.Asm.emit asm (DCall (20, 20, 20, 8)) |> ignore;

  Map.Asm.jmp asm "loop";

  Map.Asm.label asm "close";
  Map.Asm.emit asm (LoadS (20, sym "std/raylib/close_window")) |> ignore;
  Map.Asm.emit asm (DCall (20, 20, 20, 8)) |> ignore;
  Map.Asm.emit asm Halt |> ignore;

  Map.Asm.pop_scope asm;

  let linked = Map.Asm.link asm in

  let vm = Map.Vm.Debug.create cfg linked.Map.Asm.program hc tc in
  let heap = Map.Vm.Debug.heap vm in
  let reg = Map.Vm.Debug.symbols vm in
  register_stdlib heap reg;

  Map.Vm.Debug.set_const vm 0 (StringDebug.of_ocaml heap "Map Window");
  Map.Vm.Debug.set_const vm 1
    (StringDebug.of_ocaml heap "(assert (cos %g) %g) ; #t");

  Map.Vm.Debug.run vm;

  Printf.printf "[STATUS] %s\n" (Map.Vm.Debug.get_status vm);
  Printf.printf "\n[FINAL REGISTERS]\n\n";
  for i = 0 to cfg.Config.vm.num_registers - 1 do
    let v = Map.Vm.Debug.get_reg vm i in
    match v with
    | Value.Nil -> ()
    | _ -> Printf.printf "r%-3d  %s\n" i (Value.to_string v)
  done
