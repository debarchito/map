open Map_core

let bytecode_id : Instr.t array Type.Id.t = Type.Id.make ()

type frame = {
  return_pc : int;
  ret_dst : int;
  reg_base : int;
  program : Instr.t array;
  handler_base : int;
}

type handler = { handler_pc : int; handler_frame : int; handler_reg : int }

module type TRACER = sig
  type ctx

  val on_instr : ctx -> pc:int -> Instr.t -> unit
  val on_call : ctx -> pc:int -> target:int -> unit
  val on_ret : ctx -> pc:int -> unit
  val on_throw : ctx -> pc:int -> unit
end

module No_tracer = struct
  type ctx = unit

  let on_instr () ~pc:_ _ = ()
  let on_call () ~pc:_ ~target:_ = ()
  let on_ret () ~pc:_ = ()
  let on_throw () ~pc:_ = ()
end

module Full_tracer = struct
  type ctx = {
    mutable instrs : (int * Instr.t) list;
    mutable calls : (int * int) list;
    mutable rets : int list;
    mutable throws : int list;
  }

  let make () = { instrs = []; calls = []; rets = []; throws = [] }
  let on_instr ctx ~pc instr = ctx.instrs <- (pc, instr) :: ctx.instrs
  let on_call ctx ~pc ~target = ctx.calls <- (pc, target) :: ctx.calls
  let on_ret ctx ~pc = ctx.rets <- pc :: ctx.rets
  let on_throw ctx ~pc = ctx.throws <- pc :: ctx.throws
end

module type S = sig
  type tracer_ctx
  type heap_tracer_ctx
  type heap_t
  type t

  val create : Config.t -> Instr.t array -> heap_tracer_ctx -> tracer_ctx -> t
  val run : t -> unit
  val step : t -> unit
  val reset : t -> unit
  val is_done : t -> bool
  val get_reg : t -> int -> Value.t
  val set_reg : t -> int -> Value.t -> unit
  val get_status : t -> string
  val heap : t -> heap_t
  val set_const : t -> int -> Value.t -> unit
  val symbols : t -> Symbol.registry
end

module Make (H : Heap.S) (T : TRACER) :
  S
    with type tracer_ctx = T.ctx
     and type heap_tracer_ctx = H.tracer_ctx
     and type heap_t = H.t = struct
  type tracer_ctx = T.ctx
  type heap_tracer_ctx = H.tracer_ctx
  type heap_t = H.t

  module G = Gc.Make (H)

  type status = Running | Halted | Fault of string

  type t = {
    heap : H.t;
    gc : Config.Gc.t;
    constants : Value.t array;
    program : Instr.t array;
    regs : Value.t array;
    mutable max_reg : int;
    frames : frame array;
    mutable frame_sp : int;
    handlers : handler array;
    mutable handler_sp : int;
    mutable pc : int;
    mutable status : status;
    tracer_ctx : T.ctx;
    symbols : Symbol.registry;
  }

  let create (cfg : Config.t) program heap_tracer_ctx tracer_ctx =
    let heap = H.create cfg.heap heap_tracer_ctx in
    let regs = Array.make cfg.vm.num_registers Value.Nil in
    let constants = Array.make cfg.bytecode.max_constants Value.Nil in
    {
      heap;
      gc = cfg.gc;
      constants;
      program;
      regs;
      max_reg = 0;
      frames =
        Array.make cfg.vm.max_call_depth
          {
            return_pc = 0;
            ret_dst = 0;
            reg_base = 0;
            program;
            handler_base = 0;
          };
      frame_sp = 0;
      handlers =
        Array.make cfg.vm.max_exception_depth
          { handler_pc = 0; handler_frame = 0; handler_reg = 0 };
      handler_sp = 0;
      pc = 0;
      status = Running;
      tracer_ctx;
      symbols = Symbol.create ();
    }

  let roots vm = [| vm.regs; vm.constants |]
  let fault vm msg = vm.status <- Fault msg

  let current_program vm =
    if vm.frame_sp = 0 then vm.program else vm.frames.(vm.frame_sp - 1).program

  let current_base vm =
    if vm.frame_sp = 0 then 0 else vm.frames.(vm.frame_sp - 1).reg_base

  let current_handler_base vm =
    if vm.frame_sp = 0 then 0 else vm.frames.(vm.frame_sp - 1).handler_base

  let reg_get vm r =
    let i = current_base vm + r in
    if i < 0 || i >= Array.length vm.regs then (
      fault vm (Printf.sprintf "register %d out of range" r);
      Value.Nil)
    else begin
      if i > vm.max_reg then vm.max_reg <- i;
      vm.regs.(i)
    end

  let reg_set vm r v =
    let i = current_base vm + r in
    if i < 0 || i >= Array.length vm.regs then
      fault vm (Printf.sprintf "register %d out of range" r)
    else begin
      if i > vm.max_reg then vm.max_reg <- i;
      vm.regs.(i) <- v
    end

  let reg_get_abs vm i =
    if i < 0 || i >= Array.length vm.regs then (
      fault vm (Printf.sprintf "register %d out of range" i);
      Value.Nil)
    else begin
      if i > vm.max_reg then vm.max_reg <- i;
      vm.regs.(i)
    end

  let reg_set_abs vm i v =
    if i < 0 || i >= Array.length vm.regs then
      fault vm (Printf.sprintf "register %d out of range" i)
    else begin
      if i > vm.max_reg then vm.max_reg <- i;
      vm.regs.(i) <- v
    end

  let to_int vm v =
    match v with
    | Value.Int n -> n
    | _ ->
        fault vm "expected Int";
        0

  let rec throw_code vm code =
    T.on_throw vm.tracer_ctx ~pc:vm.pc;
    if vm.handler_sp = 0 then
      fault vm (Printf.sprintf "uncaught exception code %d" code)
    else begin
      let h = vm.handlers.(vm.handler_sp - 1) in
      vm.handler_sp <- vm.handler_sp - 1;
      while vm.frame_sp > h.handler_frame do
        T.on_ret vm.tracer_ctx ~pc:vm.pc;
        vm.frame_sp <- vm.frame_sp - 1
      done;
      reg_set_abs vm h.handler_reg (Value.Int code);
      vm.pc <- h.handler_pc;
      vm.status <- Running
    end

  and guard_with vm default f =
    match f () with
    | v -> v
    | exception Exception.Bounds_error m ->
        fault vm m;
        default
    | exception Exception.Alloc_error m ->
        fault vm m;
        default
    | exception Exception.Type_error m ->
        fault vm m;
        default
    | exception Exception.Arity_error m ->
        fault vm m;
        default
    | exception Exception.Div_by_zero m ->
        fault vm m;
        default
    | exception Exception.Stack_overflow m ->
        fault vm m;
        default
    | exception Exception.Native_error e ->
        fault vm (Printexc.to_string e);
        default
    | exception Exception.Registered (code, _) ->
        throw_code vm code;
        default

  let guard vm f = guard_with vm Value.Nil f
  let guard_ vm f = guard_with vm () f
  let guard_int vm f = guard_with vm (-1) f
  let guard_fib vm f = guard_with vm (Value.Done Value.Nil) f
  let throw vm reg = throw_code vm (to_int vm (reg_get vm reg))

  let maybe_gc vm =
    if H.needs_minor_gc vm.heap then begin
      let r = G.run vm.heap vm.gc ~roots:(roots vm) Minor in
      match r with
      | Some Gc.Major -> G.run vm.heap vm.gc ~roots:(roots vm) Major |> ignore
      | _ -> ()
    end

  let do_call vm ~pc ~target ~arg_start ~arg_end ~ret_dst ~program =
    if vm.frame_sp >= Array.length vm.frames then
      throw_code vm (Exception.to_int Exception.Stack_overflow)
    else begin
      T.on_call vm.tracer_ctx ~pc ~target;
      let base = current_base vm in
      vm.frames.(vm.frame_sp) <-
        {
          return_pc = vm.pc;
          ret_dst = base + ret_dst;
          reg_base = base + arg_start;
          program;
          handler_base = vm.handler_sp;
        };
      vm.frame_sp <- vm.frame_sp + 1;
      vm.pc <- target;
      ignore arg_end
    end

  let do_tcall vm ~pc ~target ~arg_start ~arg_end ~program =
    if vm.frame_sp = 0 then fault vm "tail call from top level"
    else begin
      T.on_call vm.tracer_ctx ~pc ~target;
      let base = current_base vm in
      let frame = vm.frames.(vm.frame_sp - 1) in
      vm.handler_sp <- frame.handler_base;
      vm.frames.(vm.frame_sp - 1) <-
        { frame with reg_base = base + arg_start; program };
      vm.pc <- target;
      ignore arg_end
    end

  let do_dcall vm ~pc ~fn_reg ~arg_start ~arg_end ~ret_dst =
    let base = current_base vm in
    match vm.regs.(base + fn_reg) with
    | Value.NativeFun fn ->
        let args =
          Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1)
        in
        let result = guard vm (fun () -> fn args) in
        vm.regs.(base + ret_dst) <- result
    | Value.NativeFib fib ->
        let args =
          Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1)
        in
        let rec run = function
          | Value.Done v -> vm.regs.(base + ret_dst) <- v
          | Value.Suspend k -> run (guard_fib vm k)
          | Value.Yield -> fault vm "DCall: unhandled Yield outside scheduler"
        in
        run (guard_fib vm (fun () -> fib args))
    | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure -> (
        let code_val = guard vm (fun () -> H.read vm.heap addr 0) in
        match Value.get_native bytecode_id code_val with
        | Some code ->
            do_call vm ~pc ~target:0 ~arg_start ~arg_end ~ret_dst ~program:code
        | None -> fault vm "DCall: closure has invalid code pointer")
    | _ -> fault vm "DCall: expected NativeFun, NativeFib, or closure Ptr"

  let do_tdcall vm ~pc ~fn_reg ~arg_start ~arg_end =
    let base = current_base vm in
    match vm.regs.(base + fn_reg) with
    | Value.NativeFun fn ->
        let args =
          Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1)
        in
        ignore (guard vm (fun () -> fn args))
    | Value.NativeFib fib ->
        let args =
          Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1)
        in
        let rec run = function
          | Value.Done _ -> ()
          | Value.Suspend k -> run (guard_fib vm k)
          | Value.Yield -> fault vm "TDCall: unhandled Yield outside scheduler"
        in
        run (guard_fib vm (fun () -> fib args))
    | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure -> (
        let code_val = guard vm (fun () -> H.read vm.heap addr 0) in
        match Value.get_native bytecode_id code_val with
        | Some code ->
            do_tcall vm ~pc ~target:0 ~arg_start ~arg_end ~program:code
        | None -> fault vm "TDCall: closure has invalid code pointer")
    | _ -> fault vm "TDCall: expected NativeFun, NativeFib, or closure Ptr"

  let step vm =
    match vm.status with
    | Halted | Fault _ -> ()
    | Running ->
        let pc = vm.pc in
        let program = current_program vm in
        let base = current_base vm in
        if pc < 0 || pc >= Array.length program then
          fault vm (Printf.sprintf "pc %d out of bounds" pc)
        else begin
          let instr = program.(pc) in
          T.on_instr vm.tracer_ctx ~pc instr;
          vm.pc <- pc + 1;
          match instr with
          | Instr.Nop -> ()
          | Instr.Mov (dst, src) -> vm.regs.(base + dst) <- vm.regs.(base + src)
          | Instr.Load (dst, n) -> vm.regs.(base + dst) <- Value.Int n
          | Instr.LoadF (dst, f) -> vm.regs.(base + dst) <- Value.Float f
          | Instr.LoadNil dst -> vm.regs.(base + dst) <- Value.Nil
          | Instr.LoadK (dst, idx) ->
              if idx < 0 || idx >= Array.length vm.constants then
                fault vm (Printf.sprintf "constant index %d out of range" idx)
              else vm.regs.(base + dst) <- vm.constants.(idx)
          | Instr.LoadS (dst, id) -> (
              match Symbol.resolve vm.symbols id with
              | Some v -> vm.regs.(base + dst) <- v
              | None ->
                  fault vm (Printf.sprintf "LoadS: unresolved symbol 0x%Lx" id))
          | Instr.Add (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x + y)
              | _ -> fault vm "Add: expected Int")
          | Instr.Sub (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x - y)
              | _ -> fault vm "Sub: expected Int")
          | Instr.Mul (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x * y)
              | _ -> fault vm "Mul: expected Int")
          | Instr.Div (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  if y = 0 then
                    throw_code vm (Exception.to_int Exception.Div_by_zero)
                  else vm.regs.(base + dst) <- Value.Int (x / y)
              | _ -> fault vm "Div: expected Int")
          | Instr.Mod (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  if y = 0 then
                    throw_code vm (Exception.to_int Exception.Div_by_zero)
                  else vm.regs.(base + dst) <- Value.Int (x mod y)
              | _ -> fault vm "Mod: expected Int")
          | Instr.AddF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Float (x +. y)
              | _ -> fault vm "AddF: expected Float")
          | Instr.SubF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Float (x -. y)
              | _ -> fault vm "SubF: expected Float")
          | Instr.MulF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Float (x *. y)
              | _ -> fault vm "MulF: expected Float")
          | Instr.DivF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Float (x /. y)
              | _ -> fault vm "DivF: expected Float")
          | Instr.And (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x land y)
              | _ -> fault vm "And: expected Int")
          | Instr.Or (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x lor y)
              | _ -> fault vm "Or: expected Int")
          | Instr.Xor (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x lxor y)
              | _ -> fault vm "Xor: expected Int")
          | Instr.Shl (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x lsl y)
              | _ -> fault vm "Shl: expected Int")
          | Instr.Shr (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x asr y)
              | _ -> fault vm "Shr: expected Int")
          | Instr.ShrU (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (x lsr y)
              | _ -> fault vm "ShrU: expected Int")
          | Instr.Eq (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (if x = y then 1 else 0)
              | _ -> fault vm "Eq: expected Int")
          | Instr.EqF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Int (if x = y then 1 else 0)
              | _ -> fault vm "EqF: expected Float")
          | Instr.Ne (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (if x <> y then 1 else 0)
              | _ -> fault vm "Ne: expected Int")
          | Instr.NeF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Int (if x <> y then 1 else 0)
              | _ -> fault vm "NeF: expected Float")
          | Instr.Lt (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (if x < y then 1 else 0)
              | _ -> fault vm "Lt: expected Int")
          | Instr.LtF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Int (if x < y then 1 else 0)
              | _ -> fault vm "LtF: expected Float")
          | Instr.LtU (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <-
                    Value.Int (if x land max_int < y land max_int then 1 else 0)
              | _ -> fault vm "LtU: expected Int")
          | Instr.Lte (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <- Value.Int (if x <= y then 1 else 0)
              | _ -> fault vm "Lte: expected Int")
          | Instr.LteU (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Int x, Value.Int y ->
                  vm.regs.(base + dst) <-
                    Value.Int
                      (if x land max_int <= y land max_int then 1 else 0)
              | _ -> fault vm "LteU: expected Int")
          | Instr.LteF (dst, a, b) -> (
              match (vm.regs.(base + a), vm.regs.(base + b)) with
              | Value.Float x, Value.Float y ->
                  vm.regs.(base + dst) <- Value.Int (if x <= y then 1 else 0)
              | _ -> fault vm "LteF: expected Float")
          | Instr.I2F (dst, src) -> (
              match vm.regs.(base + src) with
              | Value.Int n ->
                  vm.regs.(base + dst) <- Value.Float (float_of_int n)
              | _ -> fault vm "I2F: expected Int")
          | Instr.F2I (dst, src) -> (
              match vm.regs.(base + src) with
              | Value.Float f ->
                  vm.regs.(base + dst) <- Value.Int (int_of_float f)
              | _ -> fault vm "F2I: expected Float")
          | Instr.Alloc (dst, tag, size) ->
              maybe_gc vm;
              let addr = guard_int vm (fun () -> H.alloc vm.heap ~size ~tag) in
              if addr >= 0 then vm.regs.(base + dst) <- Value.Ptr addr
          | Instr.GetField (dst, obj, field) -> (
              match vm.regs.(base + obj) with
              | Value.Ptr addr ->
                  let v = guard vm (fun () -> H.read vm.heap addr field) in
                  vm.regs.(base + dst) <- v
              | _ -> fault vm "GetField: expected Ptr")
          | Instr.SetField (obj, field, src) -> (
              match vm.regs.(base + obj) with
              | Value.Ptr addr ->
                  let v = vm.regs.(base + src) in
                  guard_ vm (fun () -> H.write vm.heap addr field v)
              | _ -> fault vm "SetField: expected Ptr")
          | Instr.Jmp target -> vm.pc <- target
          | Instr.Jz (reg, target) -> (
              match vm.regs.(base + reg) with
              | Value.Int 0 -> vm.pc <- target
              | Value.Int _ -> ()
              | _ -> fault vm "Jz: expected Int")
          | Instr.Jnz (reg, target) -> (
              match vm.regs.(base + reg) with
              | Value.Int 0 -> ()
              | Value.Int _ -> vm.pc <- target
              | _ -> fault vm "Jnz: expected Int")
          | Instr.Call (target, arg_start, arg_end, ret_dst) ->
              do_call vm ~pc ~target ~arg_start ~arg_end ~ret_dst
                ~program:(current_program vm)
          | Instr.TCall (target, arg_start, arg_end) ->
              do_tcall vm ~pc ~target ~arg_start ~arg_end
                ~program:(current_program vm)
          | Instr.DCall (fn_reg, arg_start, arg_end, ret_dst) ->
              do_dcall vm ~pc ~fn_reg ~arg_start ~arg_end ~ret_dst;
              maybe_gc vm
          | Instr.TDCall (fn_reg, arg_start, arg_end) ->
              do_tdcall vm ~pc ~fn_reg ~arg_start ~arg_end;
              maybe_gc vm
          | Instr.Ret src ->
              if vm.frame_sp = 0 then fault vm "return from empty call stack"
              else begin
                T.on_ret vm.tracer_ctx ~pc;
                let result = vm.regs.(base + src) in
                let frame = vm.frames.(vm.frame_sp - 1) in
                vm.handler_sp <- frame.handler_base;
                vm.frame_sp <- vm.frame_sp - 1;
                vm.pc <- frame.return_pc;
                reg_set_abs vm frame.ret_dst result
              end
          | Instr.Try (handler_pc, catch_reg) ->
              if vm.handler_sp >= Array.length vm.handlers then
                fault vm "handler stack overflow"
              else begin
                vm.handlers.(vm.handler_sp) <-
                  {
                    handler_pc;
                    handler_frame = vm.frame_sp;
                    handler_reg = base + catch_reg;
                  };
                vm.handler_sp <- vm.handler_sp + 1
              end
          | Instr.EndTry ->
              if vm.handler_sp = 0 then fault vm "EndTry without Try"
              else if vm.handler_sp <= current_handler_base vm then
                fault vm "EndTry would pop handler from outer frame"
              else vm.handler_sp <- vm.handler_sp - 1
          | Instr.Throw reg -> throw vm reg
          | Instr.Halt -> vm.status <- Halted
        end

  let run vm =
    while vm.status = Running do
      step vm
    done

  let reset vm =
    vm.pc <- 0;
    vm.frame_sp <- 0;
    vm.handler_sp <- 0;
    vm.max_reg <- 0;
    vm.status <- Running;
    Array.fill vm.regs 0 (Array.length vm.regs) Value.Nil

  let is_done vm = match vm.status with Running -> false | _ -> true
  let get_reg vm i = vm.regs.(i)
  let set_reg vm i v = vm.regs.(i) <- v
  let set_const vm i v = vm.constants.(i) <- v
  let heap vm = vm.heap
  let symbols vm = vm.symbols

  let get_status vm =
    match vm.status with
    | Running -> "running"
    | Halted -> "halted"
    | Fault m -> Printf.sprintf "fault: %s" m
end

module Fast = Make (Heap.Fast) (No_tracer)
module Debug = Make (Heap.Debug) (Full_tracer)
