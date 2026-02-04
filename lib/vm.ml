type int_reg = int
type float_reg = int
type address = int
type immediate = int
type float_immediate = float

type value =
  | Int of int
  | Float of float
  | Null

type status =
  | Running
  | Halted
  | Uncaught of exception_info

and exception_info = {
  error_code: int;
  pc: int;
  stack_trace: call_frame list;
  message: string;
}

and call_frame = {
  return_pc : int;
  frame_base : int;
}

type opcode =
  | Mov    of int_reg * int_reg
  | Movi   of int_reg * immediate
  | Add    of int_reg * int_reg * int_reg
  | Sub    of int_reg * int_reg * int_reg
  | Mul    of int_reg * int_reg * int_reg
  | Div    of int_reg * int_reg * int_reg
  | Mod    of int_reg * int_reg * int_reg
  | Divu   of int_reg * int_reg * int_reg
  | Modu   of int_reg * int_reg * int_reg
  | And    of int_reg * int_reg * int_reg
  | Or     of int_reg * int_reg * int_reg
  | Xor    of int_reg * int_reg * int_reg
  | Shl    of int_reg * int_reg * int_reg
  | Shr    of int_reg * int_reg * int_reg
  | Shru   of int_reg * int_reg * int_reg
  | Eq     of int_reg * int_reg * int_reg
  | Lt     of int_reg * int_reg * int_reg
  | Lte    of int_reg * int_reg * int_reg
  | Ltu    of int_reg * int_reg * int_reg
  | Lteu   of int_reg * int_reg * int_reg
  | Movf   of float_reg * float_reg
  | Movif  of float_reg * float_immediate
  | Addf   of float_reg * float_reg * float_reg
  | Subf   of float_reg * float_reg * float_reg
  | Mulf   of float_reg * float_reg * float_reg
  | Divf   of float_reg * float_reg * float_reg
  | Eqf    of int_reg * float_reg * float_reg
  | Ltf    of int_reg * float_reg * float_reg
  | Ltef   of int_reg * float_reg * float_reg
  | Itof   of float_reg * int_reg
  | Ftoi   of int_reg * float_reg
  | Load   of int_reg * int
  | Store  of int * int_reg
  | Loadf  of float_reg * int
  | Storef of int * float_reg
  | Loadr  of int_reg * int_reg * int
  | Storer of int_reg * int * int_reg
  | Loadfr of float_reg * int_reg * int
  | Storefr of int_reg * int * float_reg
  | Loadu  of int_reg * int
  | Storeu of int * value
  | Loadur of int_reg * int_reg * int
  | Storeur of int_reg * int * value
  | Push   of int_reg
  | Pop    of int_reg
  | Pushf  of float_reg
  | Popf   of float_reg
  | Jmp    of address
  | Jrel   of int
  | Jz     of int_reg * address
  | Jnz    of int_reg * address
  | Jzrel  of int_reg * int
  | Jnzrel of int_reg * int
  | Call   of address * int * int * int
  | Ret
  | Try    of address
  | Endtry
  | Throw  of int_reg
  | Syscall of int
  | Breakpoint
  | Nop
  | Halt

type exception_handler = {
  handler_pc : int;
  handler_frame_sp : int;
}

type heap_block = {
  mutable size: int;
  mutable free: bool;
  mutable next: int;
}

type vm = {
  mutable pc : int;
  int_regs : int array;
  float_regs : float array;
  memory : value array;
  mutable stack_pointer : int;
  call_frames : call_frame array;
  mutable call_frame_sp : int;
  exception_handlers : exception_handler array;
  mutable exception_handler_sp : int;
  program : opcode array;
  mutable status : status;
  bounds_check : bool;
  mutable trace : bool;
  mutable trace_buffer : string list;
  mutable breakpoints : int list;
  mutable step_mode : bool;
  mutable watch_regs : int list;
  mutable instruction_counts : int array;
  mutable total_instructions : int;
  mutable heap_start : int;
  mutable heap_end : int;
  mutable free_list : int;
}

let create_vm
    ?(bounds_check=true)
    ?(trace=false)
    ?(step_mode=false)
    ~num_int_regs
    ~num_float_regs
    ~memory_size
    ~stack_size
    ~heap_size
    ~max_call_frames
    ~max_exception_handlers
    program =
  let heap_start = stack_size in
  let heap_end = stack_size + heap_size in
  let vm = {
    pc = 0;
    int_regs = Array.make num_int_regs 0;
    float_regs = Array.make num_float_regs 0.;
    memory = Array.make memory_size Null;
    stack_pointer = 0;
    call_frames = Array.make max_call_frames { return_pc = 0; frame_base = 0 };
    call_frame_sp = 0;
    exception_handlers = Array.make max_exception_handlers { handler_pc = 0; handler_frame_sp = 0 };
    exception_handler_sp = 0;
    program;
    status = Running;
    bounds_check;
    trace;
    trace_buffer = [];
    breakpoints = [];
    step_mode;
    watch_regs = [];
    instruction_counts = Array.make 256 0;
    total_instructions = 0;
    heap_start;
    heap_end;
    free_list = heap_start;
  } in
  vm.memory.(heap_start) <- Int heap_size;
  vm.memory.(heap_start + 1) <- Int 1;
  vm.memory.(heap_start + 2) <- Int (-1);
  vm

let[@inline] get_current_frame_base vm =
  if vm.call_frame_sp = 0 then 0
  else vm.call_frames.(vm.call_frame_sp - 1).frame_base

let get_stack_trace vm =
  let rec collect acc idx =
    if idx < 0 then acc
    else collect (vm.call_frames.(idx) :: acc) (idx - 1)
  in
  collect [] (vm.call_frame_sp - 1)

let throw_exception vm error_code message =
  let stack_trace = get_stack_trace vm in
  let rec unwind_stack () =
    if vm.exception_handler_sp = 0 then
      vm.status <- Uncaught { error_code; pc = vm.pc; stack_trace; message }
    else begin
      let handler = vm.exception_handlers.(vm.exception_handler_sp - 1) in
      if vm.call_frame_sp < handler.handler_frame_sp then begin
        vm.exception_handler_sp <- vm.exception_handler_sp - 1;
        unwind_stack ()
      end else begin
        let frame_base =
          if handler.handler_frame_sp = 0 then 0
          else vm.call_frames.(handler.handler_frame_sp - 1).frame_base
        in
        vm.int_regs.(0) <- error_code;
        vm.pc <- handler.handler_pc;
        vm.call_frame_sp <- handler.handler_frame_sp;
        vm.exception_handler_sp <- vm.exception_handler_sp - 1;
        vm.stack_pointer <- frame_base;
        vm.status <- Running
      end
    end
  in
  unwind_stack ()

let[@inline] check_int_reg vm reg name =
  if vm.bounds_check && (reg < 0 || reg >= Array.length vm.int_regs) then
    throw_exception vm 100 (Printf.sprintf "Integer register %d out of bounds in %s" reg name)

let[@inline] check_float_reg vm reg name =
  if vm.bounds_check && (reg < 0 || reg >= Array.length vm.float_regs) then
    throw_exception vm 101 (Printf.sprintf "Float register %d out of bounds in %s" reg name)

let[@inline] check_memory vm addr name =
  if vm.bounds_check && (addr < 0 || addr >= Array.length vm.memory) then
    throw_exception vm 102 (Printf.sprintf "Memory address %d out of bounds in %s" addr name)

let[@inline] check_pc vm name =
  if vm.bounds_check && (vm.pc < 0 || vm.pc >= Array.length vm.program) then
    throw_exception vm 104 (Printf.sprintf "Program counter %d out of bounds in %s" vm.pc name)

let[@inline] add_trace vm instr =
  if vm.trace then
    vm.trace_buffer <- (Printf.sprintf "PC=%d: %s" vm.pc instr) :: vm.trace_buffer

let get_int vm addr =
  match vm.memory.(addr) with
  | Int n -> n
  | _ -> throw_exception vm 110 (Printf.sprintf "Expected Int at address %d" addr); 0

let get_float vm addr =
  match vm.memory.(addr) with
  | Float f -> f
  | _ -> throw_exception vm 111 (Printf.sprintf "Expected Float at address %d" addr); 0.

let malloc vm size =
  let rec find_free ptr =
    if ptr = -1 then -1
    else
      let block_size = get_int vm ptr in
      let block_free = get_int vm (ptr + 1) in
      let block_next = get_int vm (ptr + 2) in
      if block_free = 1 && block_size >= size + 3 then
        ptr
      else
        find_free block_next
  in
  let ptr = find_free vm.free_list in
  if ptr = -1 then begin
    throw_exception vm 120 "Out of memory";
    -1
  end else begin
    let block_size = get_int vm ptr in
    let block_next = get_int vm (ptr + 2) in
    vm.memory.(ptr + 1) <- Int 0;
    if block_size > size + 6 then begin
      let new_block = ptr + size + 3 in
      let new_size = block_size - size - 3 in
      vm.memory.(new_block) <- Int new_size;
      vm.memory.(new_block + 1) <- Int 1;
      vm.memory.(new_block + 2) <- Int block_next;
      vm.memory.(ptr) <- Int size;
      vm.memory.(ptr + 2) <- Int new_block;
    end;
    ptr + 3
  end

let free vm ptr =
  if ptr < vm.heap_start + 3 || ptr >= vm.heap_end then
    throw_exception vm 121 (Printf.sprintf "Invalid free at address %d" ptr)
  else begin
    let block_ptr = ptr - 3 in
    vm.memory.(block_ptr + 1) <- Int 1;
  end

let memcpy vm dest src n =
  for i = 0 to n - 1 do
    check_memory vm (src + i) "memcpy src";
    check_memory vm (dest + i) "memcpy dest";
    vm.memory.(dest + i) <- vm.memory.(src + i)
  done

let memset vm dest value n =
  for i = 0 to n - 1 do
    check_memory vm (dest + i) "memset";
    vm.memory.(dest + i) <- value
  done

let strlen vm ptr =
  let rec measure len =
    match vm.memory.(ptr + len) with
    | Int 0 -> len
    | Int _ -> measure (len + 1)
    | _ -> throw_exception vm 122 "Invalid string"; 0
  in
  measure 0

let strcpy vm dest src =
  let len = strlen vm src in
  memcpy vm dest src (len + 1)

let strcmp vm s1 s2 =
  let rec compare idx =
    let c1 = get_int vm (s1 + idx) in
    let c2 = get_int vm (s2 + idx) in
    if c1 = 0 && c2 = 0 then 0
    else if c1 < c2 then -1
    else if c1 > c2 then 1
    else compare (idx + 1)
  in
  compare 0

let syscall vm code =
  match code with
  | 0 ->
      Printf.printf "%d\n" vm.int_regs.(0);
      flush stdout
  | 1 ->
      Printf.printf "%f\n" vm.float_regs.(0);
      flush stdout
  | 2 ->
      let c = vm.int_regs.(0) in
      if c >= 0 && c <= 255 then begin
        Printf.printf "%c" (Char.chr c);
        flush stdout
      end
  | 3 ->
      vm.int_regs.(0) <- read_int ()
  | 4 ->
      vm.float_regs.(0) <- read_float ()
  | 5 ->
      vm.int_regs.(0) <- int_of_float (Sys.time () *. 1000.0)
  | 10 ->
      vm.int_regs.(0) <- malloc vm vm.int_regs.(0)
  | 11 ->
      free vm vm.int_regs.(0)
  | 20 ->
      vm.int_regs.(0) <- strlen vm vm.int_regs.(0)
  | 21 ->
      strcpy vm vm.int_regs.(0) vm.int_regs.(1)
  | 22 ->
      vm.int_regs.(0) <- strcmp vm vm.int_regs.(0) vm.int_regs.(1)
  | 30 ->
      memcpy vm vm.int_regs.(0) vm.int_regs.(1) vm.int_regs.(2)
  | 31 ->
      memset vm vm.int_regs.(0) (Int vm.int_regs.(1)) vm.int_regs.(2)
  | 40 ->
      vm.float_regs.(0) <- sqrt vm.float_regs.(0)
  | 41 ->
      vm.float_regs.(0) <- sin vm.float_regs.(0)
  | 42 ->
      vm.float_regs.(0) <- cos vm.float_regs.(0)
  | 43 ->
      vm.float_regs.(0) <- vm.float_regs.(0) ** vm.float_regs.(1)
  | 44 ->
      vm.float_regs.(0) <- log vm.float_regs.(0)
  | 45 ->
      vm.float_regs.(0) <- exp vm.float_regs.(0)
  | 46 ->
      vm.float_regs.(0) <- floor vm.float_regs.(0)
  | 47 ->
      vm.float_regs.(0) <- ceil vm.float_regs.(0)
  | 48 ->
      vm.float_regs.(0) <- abs_float vm.float_regs.(0)
  | _ ->
      throw_exception vm 200 (Printf.sprintf "Unknown syscall code %d" code)

let[@inline] step vm =
  if vm.status <> Running then ()
  else begin
    check_pc vm "step";
    if List.mem vm.pc vm.breakpoints then begin
      vm.step_mode <- true;
      Printf.printf "Breakpoint hit at PC %d\n" vm.pc;
      flush stdout
    end;
    let next_pc = vm.pc + 1 in
    vm.total_instructions <- vm.total_instructions + 1;
    match vm.program.(vm.pc) with
    
    | Mov (dest, src) ->
        add_trace vm (Printf.sprintf "Mov r%d <- r%d" dest src);
        check_int_reg vm dest "Mov dest";
        check_int_reg vm src "Mov src";
        vm.int_regs.(dest) <- vm.int_regs.(src);
        vm.pc <- next_pc
    
    | Movi (dest, imm) ->
        add_trace vm (Printf.sprintf "Movi r%d <- %d" dest imm);
        check_int_reg vm dest "Movi";
        vm.int_regs.(dest) <- imm;
        vm.pc <- next_pc
    
    | Add (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Add r%d <- r%d + r%d" dest reg_a reg_b);
        check_int_reg vm dest "Add dest";
        check_int_reg vm reg_a "Add a";
        check_int_reg vm reg_b "Add b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) + vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Sub (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Sub r%d <- r%d - r%d" dest reg_a reg_b);
        check_int_reg vm dest "Sub dest";
        check_int_reg vm reg_a "Sub a";
        check_int_reg vm reg_b "Sub b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) - vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Mul (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Mul r%d <- r%d * r%d" dest reg_a reg_b);
        check_int_reg vm dest "Mul dest";
        check_int_reg vm reg_a "Mul a";
        check_int_reg vm reg_b "Mul b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) * vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Div (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Div r%d <- r%d / r%d" dest reg_a reg_b);
        check_int_reg vm dest "Div dest";
        check_int_reg vm reg_a "Div a";
        check_int_reg vm reg_b "Div b";
        let divisor = vm.int_regs.(reg_b) in
        if divisor = 0 then
          throw_exception vm 1 "Division by zero"
        else begin
          vm.int_regs.(dest) <- vm.int_regs.(reg_a) / divisor;
          vm.pc <- next_pc
        end
    
    | Mod (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Mod r%d <- r%d %% r%d" dest reg_a reg_b);
        check_int_reg vm dest "Mod dest";
        check_int_reg vm reg_a "Mod a";
        check_int_reg vm reg_b "Mod b";
        let divisor = vm.int_regs.(reg_b) in
        if divisor = 0 then
          throw_exception vm 2 "Modulo by zero"
        else begin
          vm.int_regs.(dest) <- vm.int_regs.(reg_a) mod divisor;
          vm.pc <- next_pc
        end
    
    | Divu (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Divu r%d <- r%d / r%d" dest reg_a reg_b);
        check_int_reg vm dest "Divu dest";
        check_int_reg vm reg_a "Divu a";
        check_int_reg vm reg_b "Divu b";
        let a = vm.int_regs.(reg_a) land max_int in
        let b = vm.int_regs.(reg_b) land max_int in
        if b = 0 then
          throw_exception vm 1 "Division by zero"
        else begin
          vm.int_regs.(dest) <- a / b;
          vm.pc <- next_pc
        end
    
    | Modu (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Modu r%d <- r%d %% r%d" dest reg_a reg_b);
        check_int_reg vm dest "Modu dest";
        check_int_reg vm reg_a "Modu a";
        check_int_reg vm reg_b "Modu b";
        let a = vm.int_regs.(reg_a) land max_int in
        let b = vm.int_regs.(reg_b) land max_int in
        if b = 0 then
          throw_exception vm 2 "Modulo by zero"
        else begin
          vm.int_regs.(dest) <- a mod b;
          vm.pc <- next_pc
        end
    
    | And (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "And r%d <- r%d & r%d" dest reg_a reg_b);
        check_int_reg vm dest "And dest";
        check_int_reg vm reg_a "And a";
        check_int_reg vm reg_b "And b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) land vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Or (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Or r%d <- r%d | r%d" dest reg_a reg_b);
        check_int_reg vm dest "Or dest";
        check_int_reg vm reg_a "Or a";
        check_int_reg vm reg_b "Or b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) lor vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Xor (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Xor r%d <- r%d ^ r%d" dest reg_a reg_b);
        check_int_reg vm dest "Xor dest";
        check_int_reg vm reg_a "Xor a";
        check_int_reg vm reg_b "Xor b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) lxor vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Shl (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Shl r%d <- r%d << r%d" dest reg_a reg_b);
        check_int_reg vm dest "Shl dest";
        check_int_reg vm reg_a "Shl a";
        check_int_reg vm reg_b "Shl b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) lsl vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Shr (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Shr r%d <- r%d >> r%d" dest reg_a reg_b);
        check_int_reg vm dest "Shr dest";
        check_int_reg vm reg_a "Shr a";
        check_int_reg vm reg_b "Shr b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) asr vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Shru (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Shru r%d <- r%d >>> r%d" dest reg_a reg_b);
        check_int_reg vm dest "Shru dest";
        check_int_reg vm reg_a "Shru a";
        check_int_reg vm reg_b "Shru b";
        vm.int_regs.(dest) <- vm.int_regs.(reg_a) lsr vm.int_regs.(reg_b);
        vm.pc <- next_pc
    
    | Eq (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Eq r%d <- r%d == r%d" dest reg_a reg_b);
        check_int_reg vm dest "Eq dest";
        check_int_reg vm reg_a "Eq a";
        check_int_reg vm reg_b "Eq b";
        vm.int_regs.(dest) <- if vm.int_regs.(reg_a) = vm.int_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Lt (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Lt r%d <- r%d < r%d" dest reg_a reg_b);
        check_int_reg vm dest "Lt dest";
        check_int_reg vm reg_a "Lt a";
        check_int_reg vm reg_b "Lt b";
        vm.int_regs.(dest) <- if vm.int_regs.(reg_a) < vm.int_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Lte (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Lte r%d <- r%d <= r%d" dest reg_a reg_b);
        check_int_reg vm dest "Lte dest";
        check_int_reg vm reg_a "Lte a";
        check_int_reg vm reg_b "Lte b";
        vm.int_regs.(dest) <- if vm.int_regs.(reg_a) <= vm.int_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Ltu (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Ltu r%d <- r%d < r%d (unsigned)" dest reg_a reg_b);
        check_int_reg vm dest "Ltu dest";
        check_int_reg vm reg_a "Ltu a";
        check_int_reg vm reg_b "Ltu b";
        let a = vm.int_regs.(reg_a) land max_int in
        let b = vm.int_regs.(reg_b) land max_int in
        vm.int_regs.(dest) <- if a < b then 1 else 0;
        vm.pc <- next_pc
    
    | Lteu (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Lteu r%d <- r%d <= r%d (unsigned)" dest reg_a reg_b);
        check_int_reg vm dest "Lteu dest";
        check_int_reg vm reg_a "Lteu a";
        check_int_reg vm reg_b "Lteu b";
        let a = vm.int_regs.(reg_a) land max_int in
        let b = vm.int_regs.(reg_b) land max_int in
        vm.int_regs.(dest) <- if a <= b then 1 else 0;
        vm.pc <- next_pc
    
    | Movf (dest, src) ->
        add_trace vm (Printf.sprintf "Movf f%d <- f%d" dest src);
        check_float_reg vm dest "Movf dest";
        check_float_reg vm src "Movf src";
        vm.float_regs.(dest) <- vm.float_regs.(src);
        vm.pc <- next_pc
    
    | Movif (dest, fimm) ->
        add_trace vm (Printf.sprintf "Movif f%d <- %f" dest fimm);
        check_float_reg vm dest "Movif";
        vm.float_regs.(dest) <- fimm;
        vm.pc <- next_pc
    
    | Addf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Addf f%d <- f%d + f%d" dest reg_a reg_b);
        check_float_reg vm dest "Addf dest";
        check_float_reg vm reg_a "Addf a";
        check_float_reg vm reg_b "Addf b";
        vm.float_regs.(dest) <- vm.float_regs.(reg_a) +. vm.float_regs.(reg_b);
        vm.pc <- next_pc
    
    | Subf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Subf f%d <- f%d - f%d" dest reg_a reg_b);
        check_float_reg vm dest "Subf dest";
        check_float_reg vm reg_a "Subf a";
        check_float_reg vm reg_b "Subf b";
        vm.float_regs.(dest) <- vm.float_regs.(reg_a) -. vm.float_regs.(reg_b);
        vm.pc <- next_pc
    
    | Mulf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Mulf f%d <- f%d * f%d" dest reg_a reg_b);
        check_float_reg vm dest "Mulf dest";
        check_float_reg vm reg_a "Mulf a";
        check_float_reg vm reg_b "Mulf b";
        vm.float_regs.(dest) <- vm.float_regs.(reg_a) *. vm.float_regs.(reg_b);
        vm.pc <- next_pc
    
    | Divf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Divf f%d <- f%d / f%d" dest reg_a reg_b);
        check_float_reg vm dest "Divf dest";
        check_float_reg vm reg_a "Divf a";
        check_float_reg vm reg_b "Divf b";
        vm.float_regs.(dest) <- vm.float_regs.(reg_a) /. vm.float_regs.(reg_b);
        vm.pc <- next_pc
    
    | Eqf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Eqf r%d <- f%d == f%d" dest reg_a reg_b);
        check_int_reg vm dest "Eqf dest";
        check_float_reg vm reg_a "Eqf a";
        check_float_reg vm reg_b "Eqf b";
        vm.int_regs.(dest) <- if vm.float_regs.(reg_a) = vm.float_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Ltf (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Ltf r%d <- f%d < f%d" dest reg_a reg_b);
        check_int_reg vm dest "Ltf dest";
        check_float_reg vm reg_a "Ltf a";
        check_float_reg vm reg_b "Ltf b";
        vm.int_regs.(dest) <- if vm.float_regs.(reg_a) < vm.float_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Ltef (dest, reg_a, reg_b) ->
        add_trace vm (Printf.sprintf "Ltef r%d <- f%d <= f%d" dest reg_a reg_b);
        check_int_reg vm dest "Ltef dest";
        check_float_reg vm reg_a "Ltef a";
        check_float_reg vm reg_b "Ltef b";
        vm.int_regs.(dest) <- if vm.float_regs.(reg_a) <= vm.float_regs.(reg_b) then 1 else 0;
        vm.pc <- next_pc
    
    | Itof (dest, src) ->
        add_trace vm (Printf.sprintf "Itof f%d <- (float)r%d" dest src);
        check_float_reg vm dest "Itof dest";
        check_int_reg vm src "Itof src";
        vm.float_regs.(dest) <- float_of_int vm.int_regs.(src);
        vm.pc <- next_pc
    
    | Ftoi (dest, src) ->
        add_trace vm (Printf.sprintf "Ftoi r%d <- (int)f%d" dest src);
        check_int_reg vm dest "Ftoi dest";
        check_float_reg vm src "Ftoi src";
        vm.int_regs.(dest) <- int_of_float vm.float_regs.(src);
        vm.pc <- next_pc
    
    | Load (dest_reg, offset) ->
        add_trace vm (Printf.sprintf "Load r%d <- [%d]" dest_reg offset);
        check_int_reg vm dest_reg "Load dest";
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Load";
        vm.int_regs.(dest_reg) <- get_int vm addr;
        vm.pc <- next_pc
    
    | Store (offset, src_reg) ->
        add_trace vm (Printf.sprintf "Store [%d] <- r%d" offset src_reg);
        check_int_reg vm src_reg "Store src";
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Store";
        vm.memory.(addr) <- Int vm.int_regs.(src_reg);
        vm.pc <- next_pc
    
    | Loadf (dest_reg, offset) ->
        add_trace vm (Printf.sprintf "Loadf f%d <- [%d]" dest_reg offset);
        check_float_reg vm dest_reg "Loadf dest";
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Loadf";
        vm.float_regs.(dest_reg) <- get_float vm addr;
        vm.pc <- next_pc
    
    | Storef (offset, src_reg) ->
        add_trace vm (Printf.sprintf "Storef [%d] <- f%d" offset src_reg);
        check_float_reg vm src_reg "Storef src";
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Storef";
        vm.memory.(addr) <- Float vm.float_regs.(src_reg);
        vm.pc <- next_pc
    
    | Loadr (dest_reg, base_reg, offset) ->
        add_trace vm (Printf.sprintf "Loadr r%d <- [r%d + %d]" dest_reg base_reg offset);
        check_int_reg vm dest_reg "Loadr dest";
        check_int_reg vm base_reg "Loadr base";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Loadr";
        vm.int_regs.(dest_reg) <- get_int vm addr;
        vm.pc <- next_pc
    
    | Storer (base_reg, offset, src_reg) ->
        add_trace vm (Printf.sprintf "Storer [r%d + %d] <- r%d" base_reg offset src_reg);
        check_int_reg vm base_reg "Storer base";
        check_int_reg vm src_reg "Storer src";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Storer";
        vm.memory.(addr) <- Int vm.int_regs.(src_reg);
        vm.pc <- next_pc
    
    | Loadfr (dest_reg, base_reg, offset) ->
        add_trace vm (Printf.sprintf "Loadfr f%d <- [r%d + %d]" dest_reg base_reg offset);
        check_float_reg vm dest_reg "Loadfr dest";
        check_int_reg vm base_reg "Loadfr base";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Loadfr";
        vm.float_regs.(dest_reg) <- get_float vm addr;
        vm.pc <- next_pc
    
    | Storefr (base_reg, offset, src_reg) ->
        add_trace vm (Printf.sprintf "Storefr [r%d + %d] <- f%d" base_reg offset src_reg);
        check_int_reg vm base_reg "Storefr base";
        check_float_reg vm src_reg "Storefr src";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Storefr";
        vm.memory.(addr) <- Float vm.float_regs.(src_reg);
        vm.pc <- next_pc
    
    | Loadu (dest_reg, offset) ->
        add_trace vm (Printf.sprintf "Loadu r%d <- [%d]" dest_reg offset);
        check_int_reg vm dest_reg "Loadu dest";
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Loadu";
        vm.int_regs.(dest_reg) <- (match vm.memory.(addr) with
          | Int _ -> 0
          | Float _ -> 1
          | Null -> 2);
        vm.pc <- next_pc
    
    | Storeu (offset, value) ->
        add_trace vm (Printf.sprintf "Storeu [%d] <- value" offset);
        let addr = get_current_frame_base vm + offset in
        check_memory vm addr "Storeu";
        vm.memory.(addr) <- value;
        vm.pc <- next_pc
    
    | Loadur (dest_reg, base_reg, offset) ->
        add_trace vm (Printf.sprintf "Loadur r%d <- [r%d + %d]" dest_reg base_reg offset);
        check_int_reg vm dest_reg "Loadur dest";
        check_int_reg vm base_reg "Loadur base";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Loadur";
        vm.int_regs.(dest_reg) <- (match vm.memory.(addr) with
          | Int _ -> 0
          | Float _ -> 1
          | Null -> 2);
        vm.pc <- next_pc
    
    | Storeur (base_reg, offset, value) ->
        add_trace vm (Printf.sprintf "Storeur [r%d + %d] <- value" base_reg offset);
        check_int_reg vm base_reg "Storeur base";
        let addr = vm.int_regs.(base_reg) + offset in
        check_memory vm addr "Storeur";
        vm.memory.(addr) <- value;
        vm.pc <- next_pc
    
    | Push reg ->
        add_trace vm (Printf.sprintf "Push r%d" reg);
        check_int_reg vm reg "Push";
        check_memory vm vm.stack_pointer "Push";
        vm.memory.(vm.stack_pointer) <- Int vm.int_regs.(reg);
        vm.stack_pointer <- vm.stack_pointer + 1;
        vm.pc <- next_pc
    
    | Pop reg ->
        add_trace vm (Printf.sprintf "Pop r%d" reg);
        check_int_reg vm reg "Pop";
        if vm.stack_pointer = 0 then
          throw_exception vm 130 "Stack underflow"
        else begin
          vm.stack_pointer <- vm.stack_pointer - 1;
          check_memory vm vm.stack_pointer "Pop";
          vm.int_regs.(reg) <- get_int vm vm.stack_pointer;
          vm.pc <- next_pc
        end
    
    | Pushf reg ->
        add_trace vm (Printf.sprintf "Pushf f%d" reg);
        check_float_reg vm reg "Pushf";
        check_memory vm vm.stack_pointer "Pushf";
        vm.memory.(vm.stack_pointer) <- Float vm.float_regs.(reg);
        vm.stack_pointer <- vm.stack_pointer + 1;
        vm.pc <- next_pc
    
    | Popf reg ->
        add_trace vm (Printf.sprintf "Popf f%d" reg);
        check_float_reg vm reg "Popf";
        if vm.stack_pointer = 0 then
          throw_exception vm 130 "Stack underflow"
        else begin
          vm.stack_pointer <- vm.stack_pointer - 1;
          check_memory vm vm.stack_pointer "Popf";
          vm.float_regs.(reg) <- get_float vm vm.stack_pointer;
          vm.pc <- next_pc
        end
    
    | Jmp target_addr ->
        add_trace vm (Printf.sprintf "Jmp %d" target_addr);
        vm.pc <- target_addr
    
    | Jrel offset ->
        add_trace vm (Printf.sprintf "Jrel %d" offset);
        vm.pc <- vm.pc + offset
    
    | Jz (reg, target_addr) ->
        add_trace vm (Printf.sprintf "Jz r%d, %d" reg target_addr);
        check_int_reg vm reg "Jz";
        vm.pc <- if vm.int_regs.(reg) = 0 then target_addr else next_pc
    
    | Jnz (reg, target_addr) ->
        add_trace vm (Printf.sprintf "Jnz r%d, %d" reg target_addr);
        check_int_reg vm reg "Jnz";
        vm.pc <- if vm.int_regs.(reg) <> 0 then target_addr else next_pc
    
    | Jzrel (reg, offset) ->
        add_trace vm (Printf.sprintf "Jzrel r%d, %d" reg offset);
        check_int_reg vm reg "Jzrel";
        vm.pc <- if vm.int_regs.(reg) = 0 then vm.pc + offset else next_pc
    
    | Jnzrel (reg, offset) ->
        add_trace vm (Printf.sprintf "Jnzrel r%d, %d" reg offset);
        check_int_reg vm reg "Jnzrel";
        vm.pc <- if vm.int_regs.(reg) <> 0 then vm.pc + offset else next_pc
    
    | Call (target_addr, num_int_args, num_float_args, num_locals) ->
        add_trace vm (Printf.sprintf "Call %d (iargs=%d, fargs=%d, locals=%d)" 
          target_addr num_int_args num_float_args num_locals);
        if vm.call_frame_sp >= Array.length vm.call_frames then
          throw_exception vm 105 "Call stack overflow"
        else begin
          let frame_base = vm.stack_pointer in
          for i = 0 to num_int_args - 1 do
            check_int_reg vm i "Call int arg";
            let addr = frame_base + i in
            check_memory vm addr "Call int arg store";
            vm.memory.(addr) <- Int vm.int_regs.(i)
          done;
          for i = 0 to num_float_args - 1 do
            check_float_reg vm i "Call float arg";
            let addr = frame_base + num_int_args + i in
            check_memory vm addr "Call float arg store";
            vm.memory.(addr) <- Float vm.float_regs.(i)
          done;
          vm.call_frames.(vm.call_frame_sp) <- { return_pc = next_pc; frame_base };
          vm.call_frame_sp <- vm.call_frame_sp + 1;
          vm.stack_pointer <- frame_base + num_locals;
          vm.pc <- target_addr
        end
    
    | Ret ->
        add_trace vm "Ret";
        if vm.call_frame_sp = 0 then
          throw_exception vm 106 "Return from empty call stack"
        else begin
          let frame = vm.call_frames.(vm.call_frame_sp - 1) in
          vm.call_frame_sp <- vm.call_frame_sp - 1;
          vm.stack_pointer <- frame.frame_base;
          vm.pc <- frame.return_pc
        end
    
    | Try handler_addr ->
        add_trace vm (Printf.sprintf "Try %d" handler_addr);
        if vm.exception_handler_sp >= Array.length vm.exception_handlers then
          throw_exception vm 107 "Exception handler stack overflow"
        else begin
          vm.exception_handlers.(vm.exception_handler_sp) <- 
            { handler_pc = handler_addr; handler_frame_sp = vm.call_frame_sp };
          vm.exception_handler_sp <- vm.exception_handler_sp + 1;
          vm.pc <- next_pc
        end
    
    | Endtry ->
        add_trace vm "Endtry";
        if vm.exception_handler_sp = 0 then
          throw_exception vm 108 "Endtry without matching try"
        else begin
          vm.exception_handler_sp <- vm.exception_handler_sp - 1;
          vm.pc <- next_pc
        end
    
    | Throw reg ->
        add_trace vm (Printf.sprintf "Throw r%d" reg);
        check_int_reg vm reg "Throw";
        throw_exception vm vm.int_regs.(reg) "User exception"
    
    | Syscall code ->
        add_trace vm (Printf.sprintf "Syscall %d" code);
        syscall vm code;
        vm.pc <- next_pc
    
    | Breakpoint ->
        add_trace vm "Breakpoint";
        vm.step_mode <- true;
        Printf.printf "Breakpoint instruction at PC %d\n" vm.pc;
        vm.pc <- next_pc
    
    | Nop ->
        add_trace vm "Nop";
        vm.pc <- next_pc
    
    | Halt ->
        add_trace vm "Halt";
        vm.status <- Halted
  end

let rec run vm =
  match vm.status with
  | Running ->
      step vm;
      if vm.step_mode then
        ()
      else
        run vm
  | _ -> ()

let run_continue vm =
  vm.step_mode <- false;
  run vm

let get_trace vm =
  List.rev vm.trace_buffer

let print_trace vm =
  List.iter print_endline (get_trace vm)

let print_exception_info info =
  Printf.printf "Uncaught exception %d at PC %d: %s\n" 
    info.error_code info.pc info.message;
  Printf.printf "Stack trace:\n";
  List.iteri (fun i frame ->
    Printf.printf "  #%d: return_pc=%d frame_base=%d\n" 
      i frame.return_pc frame.frame_base
  ) info.stack_trace

let set_breakpoint vm pc =
  if not (List.mem pc vm.breakpoints) then
    vm.breakpoints <- pc :: vm.breakpoints

let clear_breakpoint vm pc =
  vm.breakpoints <- List.filter (fun x -> x <> pc) vm.breakpoints

let print_registers vm =
  Printf.printf "=== Integer Registers ===\n";
  Array.iteri (fun i v ->
    if i mod 4 = 0 then Printf.printf "\n";
    Printf.printf "r%-2d: %-10d  " i v
  ) vm.int_regs;
  Printf.printf "\n\n=== Float Registers ===\n";
  Array.iteri (fun i v ->
    if i mod 4 = 0 then Printf.printf "\n";
    Printf.printf "f%-2d: %-10f  " i v
  ) vm.float_regs;
  Printf.printf "\n"

let print_memory vm start len =
  Printf.printf "Memory [%d..%d]\n" start (start + len - 1);
  for i = start to min (start + len - 1) (Array.length vm.memory - 1) do
    match vm.memory.(i) with
    | Int n -> Printf.printf "[%d]: Int %d\n" i n
    | Float f -> Printf.printf "[%d]: Float %f\n" i f
    | Null -> Printf.printf "[%d]: Null\n" i
  done

let print_stats vm =
  Printf.printf "Total instructions executed: %d\n" vm.total_instructions;
  Printf.printf "PC: %d\n" vm.pc;
  Printf.printf "Stack pointer: %d\n" vm.stack_pointer;
  Printf.printf "Call frame depth: %d\n" vm.call_frame_sp;
  Printf.printf "Exception handler depth: %d\n" vm.exception_handler_sp;
  Printf.printf "Status: %s\n" (match vm.status with
    | Running -> "Running"
    | Halted -> "Halted"
    | Uncaught _ -> "Uncaught Exception")
