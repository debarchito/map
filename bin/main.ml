open Core
open Map.Instr

let cfg = Config.default

module NsDebug      = Map.Namespace.Make(Map.Heap.Debug)
module GuiDebug     = Std.Gui.Make(Map.Heap.Debug)
module MstringDebug = Std.Mstring.Make(Map.Heap.Debug)

let program = Array.make 46 Nop

let () =
  let tc   = Map.Vm.Full_tracer.make () in
  let hc   = Map.Heap.Full_tracer.make
               ~max_chunks:cfg.Config.heap.max_chunks
               ~chunk_size:cfg.Config.heap.chunk_size
               ~sample_rate:1 in
  let vm   = Map.Vm.Debug.create cfg program hc tc in
  let heap = Map.Vm.Debug.heap vm in

  let prelude = Map.Namespace.(prepare (ns "root" [
    ns "std" [
      Std.Math.ns;
      GuiDebug.ns heap;
      MstringDebug.ns heap;
    ];
  ])) in

  Map.Vm.Debug.set_const vm 0 (MstringDebug.alloc_string heap "Map Window");
  Map.Vm.Debug.set_const vm 1 (MstringDebug.alloc_string heap "(assert (abs %g) %g) ; #t");

  let p = program in
  (*  0 *) p.(0)  <- GetField (1,  0,  prelude.i "std");
  (*  1 *) p.(1)  <- GetField (2,  1,  prelude.i "std/gui");
  (*  2 *) p.(2)  <- GetField (3,  1,  prelude.i "std/math");
  (*  3 *) p.(3)  <- GetField (4,  2,  prelude.i "std/gui/init_window");
  (*  4 *) p.(4)  <- Load     (5,  800);
  (*  5 *) p.(5)  <- Load     (6,  450);
  (*  6 *) p.(6)  <- LoadK    (7,  0);
  (*  7 *) p.(7)  <- DCall    (4,  5,  7,  8);
  (*  8 *) p.(8)  <- GetField (9,  3,  prelude.i "std/math/abs");
  (*  9 *) p.(9)  <- LoadF    (10, -17.0);
  (* 10 *) p.(10) <- DCall    (9,  10, 10, 11);
  (* 11 *) p.(11) <- GetField (20, 2,  prelude.i "std/gui/window_should_close");
  (* 12 *) p.(12) <- DCall    (20, 5,  5,  21);
  (* 13 *) p.(13) <- Jnz      (21, 42);
  (* 14 *) p.(14) <- GetField (20, 2,  prelude.i "std/gui/begin_drawing");
  (* 15 *) p.(15) <- DCall    (20, 5,  5,  8);
  (* 16 *) p.(16) <- GetField (20, 2,  prelude.i "std/gui/clear_background");
  (* 17 *) p.(17) <- Load     (22, 245);
  (* 18 *) p.(18) <- Load     (23, 245);
  (* 19 *) p.(19) <- Load     (24, 245);
  (* 20 *) p.(20) <- Load     (25, 255);
  (* 21 *) p.(21) <- DCall    (20, 22, 25, 8);
  (* 22 *) p.(22) <- GetField (40, 1,  prelude.i "std/string");
  (* 23 *) p.(23) <- GetField (32, 40, prelude.i "std/string/format");
  (* 24 *) p.(24) <- LoadK    (33, 1);   (* r33 = fmt *)
  (* 25 *) p.(25) <- Mov      (34, 10);  (* r34 = input -17.0 *)
  (* 26 *) p.(26) <- Mov      (35, 11);  (* r35 = result 17.0 *)
  (* 27 *) p.(27) <- DCall    (32, 33, 35, 34); (* format(r33=fmt, r34=input, r35=result) -> r34 *)
  (* 28 *) p.(28) <- GetField (20, 2,  prelude.i "std/gui/draw_text");
  (* 29 *) p.(29) <- Load     (35, 150);
  (* 30 *) p.(30) <- Load     (36, 180);
  (* 31 *) p.(31) <- Load     (37, 28);
  (* 32 *) p.(32) <- DCall    (20, 34, 37, 8);
  (* 33 *) p.(33) <- Load     (37, 28);
  (* 34 *) p.(34) <- DCall    (20, 34, 37, 8);
  (* 35 *) p.(35) <- GetField (20, 2,  prelude.i "std/gui/end_drawing");
  (* 36 *) p.(36) <- DCall    (20, 5,  5,  8);
  (* 37 *) p.(37) <- Jmp      11;
  (* 42 *) p.(42) <- GetField (20, 2,  prelude.i "std/gui/close_window");
  (* 43 *) p.(43) <- DCall    (20, 5,  5,  8);
  (* 44 *) p.(44) <- Halt;

  let ptr = NsDebug.load heap prelude.tree in
  Map.Vm.Debug.set_reg vm 0 ptr;
  Map.Vm.Debug.run vm;

  Printf.printf "\n=== STATUS ===\n";
  Printf.printf "  %s\n" (Map.Vm.Debug.get_status vm);

  Printf.printf "\n=== INSTRUCTION TRACE ===\n";
  List.iteri (fun step (pc, instr) ->
    Printf.printf "  [%4d] pc=%3d  %s\n" step pc (Map.Instr.to_string instr)
  ) (List.rev tc.Map.Vm.Full_tracer.instrs);

  Printf.printf "\n=== CALL TRACE ===\n";
  (match tc.Map.Vm.Full_tracer.calls with
   | [] -> Printf.printf "  (none)\n"
   | cs -> List.iter (fun (pc, target) ->
       Printf.printf "  call  from pc=%3d  to pc=%3d\n" pc target
     ) (List.rev cs));

  Printf.printf "\n=== RETURN TRACE ===\n";
  (match tc.Map.Vm.Full_tracer.rets with
   | [] -> Printf.printf "  (none)\n"
   | rs -> List.iter (fun pc ->
       Printf.printf "  ret   at   pc=%3d\n" pc
     ) (List.rev rs));

  Printf.printf "\n=== THROW TRACE ===\n";
  (match tc.Map.Vm.Full_tracer.throws with
   | [] -> Printf.printf "  (none)\n"
   | ts -> List.iter (fun pc ->
       Printf.printf "  throw at   pc=%3d\n" pc
     ) (List.rev ts));

  Printf.printf "\n=== GC EVENTS ===\n";
  (match hc.Map.Heap.Full_tracer.events with
   | [] -> Printf.printf "  (none)\n"
   | es -> List.iter (fun ev ->
       let s = match ev with
         | Map.Heap.Minor_start                  -> "MinorStart"
         | Map.Heap.Minor_end { promoted }       -> Printf.sprintf "MinorEnd promoted=%d" promoted
         | Map.Heap.Major_mark { steps }         -> Printf.sprintf "MajorMark steps=%d" steps
         | Map.Heap.Major_sweep { steps; freed } -> Printf.sprintf "MajorSweep steps=%d freed=%d" steps freed
         | Map.Heap.Major_end                    -> "MajorEnd"
       in
       Printf.printf "  %s\n" s
     ) (List.rev es));

  Printf.printf "\n=== HEAP ALLOCATIONS ===\n";
  (match hc.Map.Heap.Full_tracer.allocs with
   | [] -> Printf.printf "  (none)\n"
   | as_ -> List.iter (fun (addr, size, tag) ->
       Printf.printf "  alloc  addr=0x%04x  size=%3d  tag=%d\n" addr size tag
     ) (List.rev as_));

  Printf.printf "\n=== HEAP FREES ===\n";
  (match hc.Map.Heap.Full_tracer.frees with
   | [] -> Printf.printf "  (none)\n"
   | fs -> List.iter (fun addr ->
       Printf.printf "  free   addr=0x%04x\n" addr
     ) (List.rev fs));

  Printf.printf "\n=== HEAP PROMOTES ===\n";
  (match hc.Map.Heap.Full_tracer.promotes with
   | [] -> Printf.printf "  (none)\n"
   | ps -> List.iter (fun addr ->
       Printf.printf "  promote addr=0x%04x\n" addr
     ) (List.rev ps));

  Printf.printf "\n=== HEAP STATS ===\n";
  let s = Map.Vm.Debug.heap vm |> Map.Heap.Debug.stats in
  Printf.printf "  young_used  = %d\n" s.Map.Heap.young_used;
  Printf.printf "  young_total = %d\n" s.Map.Heap.young_total;
  Printf.printf "  old_used    = %d\n" s.Map.Heap.old_used;
  Printf.printf "  old_total   = %d\n" s.Map.Heap.old_total;
  Printf.printf "  n_chunks    = %d\n" s.Map.Heap.n_chunks;
  Printf.printf "  alloc_count = %d\n" s.Map.Heap.alloc_count;

  Printf.printf "\n=== NAMESPACE ===\n";
  NsDebug.pp prelude.tree;
  Printf.printf "  total entries indexed: %d\n" prelude.count;

  Printf.printf "\n=== FINAL REGISTERS (non-nil) ===\n";
  for i = 0 to cfg.Config.vm.num_registers - 1 do
    let v = Map.Vm.Debug.get_reg vm i in
    match v with
    | Value.Nil -> ()
    | _ -> Printf.printf "  r%-3d  %s\n" i (Value.to_string v)
  done
