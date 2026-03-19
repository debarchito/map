open Map_core
open Map.Instr

let cfg = Config.default

let prelude = Map.Namespace.(prepare (ns "root" [
  ns "std" [
    Map_std.Math.ns;
    Map_std.Gui.ns;
  ];
]))

let program = [|
  (*  0 *) GetField (1, 0, prelude.i "std");
  (*  1 *) GetField (1, 1, prelude.i "std/gui");
  (*  2 *) Load     (7, 800);
  (*  3 *) Load     (8, 450);
  (*  4 *) GetField (2, 1, prelude.i "std/gui/init_window");
  (*  5 *) DCall    (2, 7, 8, 9);
  (*  6 *) GetField (3, 0, prelude.i "std");
  (*  7 *) GetField (3, 3, prelude.i "std/math");
  (*  8 *) GetField (2, 3, prelude.i "std/math/abs");
  (*  9 *) LoadF    (10, -17.0);
  (* 10 *) DCall    (2, 10, 10, 11);
  (* 11 *) GetField (2, 1, prelude.i "std/gui/window_should_close");
  (* 12 *) DCall    (2, 7, 7, 9);
  (* 13 *) Jnz      (9, 26);
  (* 14 *) GetField (2, 1, prelude.i "std/gui/begin_drawing");
  (* 15 *) DCall    (2, 7, 7, 9);
  (* 16 *) GetField (2, 1, prelude.i "std/gui/clear_background");
  (* 17 *) DCall    (2, 7, 7, 9);
  (* 18 *) Load     (12, 150);
  (* 19 *) Load     (13, 180);
  (* 20 *) Load     (14, 28);
  (* 21 *) GetField (2, 1, prelude.i "std/gui/draw_float");
  (* 22 *) DCall    (2, 10, 14, 9);
  (* 23 *) GetField (2, 1, prelude.i "std/gui/end_drawing");
  (* 24 *) DCall    (2, 7, 7, 9);
  (* 25 *) Jmp      11;
  (* 26 *) GetField (2, 1, prelude.i "std/gui/close_window");
  (* 27 *) DCall    (2, 7, 7, 9);
  (* 28 *) Halt;
|]

module Ns      = Map.Namespace.Make(Map.Heap.Fast)
module NsDebug = Map.Namespace.Make(Map.Heap.Debug)

let () =
  let tc  = Map.Vm.Full_tracer.make () in
  let hc  = Map.Heap.Full_tracer.make
              ~max_chunks:cfg.Config.heap.max_chunks
              ~chunk_size:cfg.Config.heap.chunk_size
              ~sample_rate:1 in
  let vm  = Map.Vm.Debug.create cfg program hc tc in
  let ptr = NsDebug.load (Map.Vm.Debug.heap vm) prelude.tree in
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
         | Map.Heap.Minor_start                     -> "MinorStart"
         | Map.Heap.Minor_end { promoted }          -> Printf.sprintf "MinorEnd promoted=%d" promoted
         | Map.Heap.Major_mark { steps }            -> Printf.sprintf "MajorMark steps=%d" steps
         | Map.Heap.Major_sweep { steps; freed }    -> Printf.sprintf "MajorSweep steps=%d freed=%d" steps freed
         | Map.Heap.Major_end                       -> "MajorEnd"
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
