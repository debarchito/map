open Map.Vm

let prog_arithmetic = [|
  Movif (0, 3.0);
  Movif (1, 4.0);
  Addf (2, 0, 1);
  Subf (3, 1, 0);
  Mulf (0, 2, 3);
  Divf (1, 0, 2);
  Halt;
|]

let prog_branching = [|
  Movi (0, 5);
  Movi (1, 0);
  Jzrel (0, 4);
  Add (1, 1, 0);
  Sub (0, 0, 1);
  Jnzrel (0, -4);
  Halt;
|]

let prog_exception = [|
  Movi (0, 10);
  Movi (1, 0);
  Try 7;
  Div (2, 0, 1);
  Endtry;
  Movi (3, 1);
  Jmp 8;
  Movif (0, 67.);
  Halt;
|]

let prog_stack = [|
  Movi (0, 7);
  Push 0;
  Movi (0, 42);
  Push 0;
  Pop 1;
  Pop 2;
  Halt;
|]

let prog_memory = [|
  Movi (0, 10);
  Store (0, 0);
  Load (1, 0);
  Movi (2, 20);
  Store (1, 2);
  Load (3, 1);
  Halt;
|]

let prog_float_memory = [|
  Movif (0, 3.14);
  Storef (0, 0);
  Loadf (1, 0);
  Movif (2, 2.0);
  Mulf (3, 1, 2);
  Halt;
|]

let prog_syscall = [|
  Movi (0, 24352);
  Syscall 0;
  Movif (0, 3.1415);
  Syscall 1;
  Halt;
|]

let prog_factorial = [|
  Movif (0, 6.0);
  Call (3, 0, 0, 0);   
  Halt;                
  Movif (1, 1.0);      
  Ltef (0, 0, 1);      
  Jnzrel (0, 7);       
  Pushf 0;             
  Movif (1, 1.0);      
  Subf (0, 0, 1);      
  Call (3, 0, 0, 0);   
  Popf 1;              
  Mulf (0, 0, 1);      
  Ret;
  Movif (0, 1.0);
  Ret;
|]

let run_program program =
  let vm = create_vm
    ~num_int_regs:8
    ~num_float_regs:4
    ~memory_size:64
    ~stack_size:16
    ~heap_size:32
    ~max_call_frames:8
    ~max_exception_handlers:8
    program
  in
  let start_time = Unix.gettimeofday () in
  run vm;
  let end_time = Unix.gettimeofday () in
  Printf.printf "Int r0:              %d\n" vm.int_regs.(0);
  Printf.printf "Int r1:              %d\n" vm.int_regs.(1);
  Printf.printf "Int r2:              %d\n" vm.int_regs.(2);
  Printf.printf "Int r3:              %d\n" vm.int_regs.(3);
  Printf.printf "Float r0:            %f\n" vm.float_regs.(0);
  Printf.printf "Float r1:            %f\n" vm.float_regs.(1);
  Printf.printf "Float r2:            %f\n" vm.float_regs.(2);
  Printf.printf "Float r3:            %f\n" vm.float_regs.(3);
  Printf.printf "No. of instructions: %d\n" vm.total_instructions;
  Printf.printf "Execution time:      %.6fs\n\n" (end_time -. start_time)

let () =
  (* run_program prog_arithmetic; *)
  (* run_program prog_branching; *)
  run_program prog_exception;
  (* run_program prog_stack; *)
  (* run_program prog_memory; *)
  (* run_program prog_float_memory; *)
  (* run_program prog_syscall; *)
  (* run_program prog_factorial; *)
