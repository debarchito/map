open Map_core
open Map.Instr

module Std = Map.Std.Make(Map.Heap.DebugHeap)

let cfg = Config.default

let factorial args =
  match args with
  | [| Value.MFloat n |] ->
    let rec fact x = if x <= 1.0 then 1.0 else x *. fact (x -. 1.0) in
    Value.MFloat (fact n)
  | _ -> raise (Value.ETypeError "factorial expects one float")

let program = [|
  (* r0 = stdlib ptr, pre-loaded *)
  (* r3 = factorial fn, pre-loaded *)

  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 2, 2, 6);                                 (* println(r2) — r2 = greeting string, pre-loaded *)

  SetF (7, 6.0);
  NCall (3, 7, 7, 8);                                 (* r8 = factorial(6.0) = 720.0 *)
  GetField (1, 0, Map.Std.String_idx.from_float);
  NCall (1, 8, 8, 9);                                 (* r9 = "720." *)
  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 9, 9, 10);                                (* println("720.") *)

  GetField (1, 0, Map.Std.String_idx.concat);
  NCall (1, 4, 5, 11);                                (* r11 = concat(r4, r5) — pre-loaded strings *)
  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 11, 11, 12);                              (* println(r11) *)

  GetField (1, 0, Map.Std.String_idx.length);
  NCall (1, 11, 11, 13);                              (* r13 = length(r11) *)
  GetField (1, 0, Map.Std.String_idx.from_int);
  NCall (1, 13, 13, 14);                              (* r14 = from_int(length) *)
  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 14, 14, 15);                              (* println(length as string) *)

  GetField (1, 0, Map.Std.String_idx.uppercase);
  NCall (1, 4, 4, 16);                                (* r16 = uppercase(r4) *)
  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 16, 16, 17);                              (* println(uppercase) *)

  SetF (7, 10.0);
  NCall (3, 7, 7, 8);                                 (* factorial, once again *)
  GetField (1, 0, Map.Std.String_idx.from_float);
  NCall (1, 8, 8, 9);
  GetField (1, 0, Map.Std.String_idx.println);
  NCall (1, 9, 9, 10);

  Halt;
|]

let () =
  let tc = Map.Vm.FullVMTracer.make () in
  let hc = Map.Heap.FullHeapTracer.make
             ~max_chunks:cfg.Config.max_chunks
             ~chunk_size:cfg.Config.chunk_size
             ~sample_rate:1 in
  let vm = Map.Vm.DebugVM.create cfg program hc tc in

  let stdlib_ptr = Std.load (Map.Vm.DebugVM.heap vm) in
  Map.Vm.DebugVM.set_reg vm 0 stdlib_ptr;
  Map.Vm.DebugVM.set_reg vm 2 (Map_std.Vmstring.make "hello from the VM!");
  Map.Vm.DebugVM.set_reg vm 3 (Value.MNativeFn factorial);
  Map.Vm.DebugVM.set_reg vm 4 (Map_std.Vmstring.make "hello, ");
  Map.Vm.DebugVM.set_reg vm 5 (Map_std.Vmstring.make "world!");

  Map.Vm.DebugVM.run vm;

  Printf.printf "--- Instruction trace ---\n";
  List.iteri (fun step (pc, instr) ->
    Printf.printf "  [%3d] pc=%2d  %s\n" step pc (Map.Instr.string_of_instr instr)
  ) (List.rev tc.Map.Vm.FullVMTracer.instrs);

  Printf.printf "\n--- Heap allocations ---\n";
  (match hc.Map.Heap.FullHeapTracer.allocs with
   | [] -> Printf.printf "  (none)\n"
   | as_ -> List.iter (fun (addr, size, tag) ->
       Printf.printf "  alloc  addr=0x%04x  size=%d  tag=%d\n" addr size tag
     ) (List.rev as_));

  Printf.printf "\n--- Final registers (non-nil) ---\n";
  for i = 0 to 15 do
    let v = Map.Vm.DebugVM.get_reg vm i in
    match v with
    | Value.MNil -> ()
    | _ -> Printf.printf "  r%-3d  %s\n" i (Value.to_string v)
  done;

  Printf.printf "\n--- Status ---\n";
  Printf.printf "  %s\n\n" (Map.Vm.DebugVM.get_status vm)
