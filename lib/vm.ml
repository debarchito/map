open Map_core

let bytecode_id : Instr.t array Type.Id.t = Type.Id.make ()

type frame = {
  return_pc : int;
  ret_dst   : int;
  reg_base  : int;
  program   : Instr.t array;
}

type handler = {
  handler_pc    : int;
  handler_frame : int;
  handler_reg   : int;
}

module type TRACER = sig
  type ctx
  val on_instr : ctx -> pc:int -> Instr.t -> unit
  val on_call  : ctx -> pc:int -> target:int -> unit
  val on_ret   : ctx -> pc:int -> unit
  val on_throw : ctx -> pc:int -> unit
end

module No_tracer = struct
  type ctx = unit
  let on_instr  () ~pc:_ _         = ()
  let on_call   () ~pc:_ ~target:_ = ()
  let on_ret    () ~pc:_           = ()
  let on_throw  () ~pc:_           = ()
end

module Full_tracer = struct
  type ctx = {
    mutable instrs : (int * Instr.t) list;
    mutable calls  : (int * int) list;
    mutable rets   : int list;
    mutable throws : int list;
  }
  let make () = { instrs = []; calls = []; rets = []; throws = [] }
  let on_instr  ctx ~pc instr   = ctx.instrs <- (pc, instr) :: ctx.instrs
  let on_call   ctx ~pc ~target = ctx.calls  <- (pc, target) :: ctx.calls
  let on_ret    ctx ~pc         = ctx.rets   <- pc :: ctx.rets
  let on_throw  ctx ~pc         = ctx.throws <- pc :: ctx.throws
end

module type S = sig
  type tracer_ctx
  type heap_tracer_ctx
  type heap_t
  type t

  val create     : Config.t -> Instr.t array -> heap_tracer_ctx -> tracer_ctx -> t
  val run        : t -> unit
  val step       : t -> unit
  val reset      : t -> unit
  val is_done    : t -> bool
  val get_reg    : t -> int -> Value.t
  val set_reg    : t -> int -> Value.t -> unit
  val get_status : t -> string
  val heap       : t -> heap_t
end

module Make(H : Heap.S)(T : TRACER) : S
  with type tracer_ctx      = T.ctx
  and  type heap_tracer_ctx = H.tracer_ctx
  and  type heap_t          = H.t = struct

  type tracer_ctx      = T.ctx
  type heap_tracer_ctx = H.tracer_ctx
  type heap_t          = H.t

  module G = Gc.Make(H)

  type status =
    | Running
    | Halted
    | Fault of string

  type t = {
    heap               : H.t;
    gc                 : Config.Gc.t;
    constants          : Value.t array;
    program            : Instr.t array;
    regs               : Value.t array;
    frames             : frame array;
    mutable frame_sp   : int;
    handlers           : handler array;
    mutable handler_sp : int;
    mutable pc         : int;
    mutable status     : status;
    tracer_ctx         : T.ctx;
  }

  let create (cfg : Config.t) program heap_tracer_ctx tracer_ctx =
    let heap = H.create cfg.heap heap_tracer_ctx in
    { heap;
      gc          = cfg.gc;
      constants   = Array.make cfg.bytecode.max_constants Value.Nil;
      program;
      regs        = Array.make cfg.vm.num_registers Value.Nil;
      frames      = Array.make cfg.vm.max_call_depth
                      { return_pc = 0; ret_dst = 0; reg_base = 0; program };
      frame_sp    = 0;
      handlers    = Array.make cfg.vm.max_exception_depth
                      { handler_pc = 0; handler_frame = 0; handler_reg = 0 };
      handler_sp  = 0;
      pc          = 0;
      status      = Running;
      tracer_ctx }

  let fault vm msg =
    vm.status <- Fault msg

  let current_program vm =
    if vm.frame_sp = 0 then vm.program
    else vm.frames.(vm.frame_sp - 1).program

  let current_base vm =
    if vm.frame_sp = 0 then 0
    else vm.frames.(vm.frame_sp - 1).reg_base

  let reg_get vm r =
    let i = current_base vm + r in
    if i < 0 || i >= Array.length vm.regs then
      (fault vm (Printf.sprintf "register %d out of range" r); Value.Nil)
    else
      vm.regs.(i)

  let reg_set vm r v =
    let i = current_base vm + r in
    if i < 0 || i >= Array.length vm.regs then
      fault vm (Printf.sprintf "register %d out of range" r)
    else
      vm.regs.(i) <- v

  let reg_get_abs vm i =
    if i < 0 || i >= Array.length vm.regs then
      (fault vm (Printf.sprintf "register %d out of range" i); Value.Nil)
    else
      vm.regs.(i)

  let reg_set_abs vm i v =
    if i < 0 || i >= Array.length vm.regs then
      fault vm (Printf.sprintf "register %d out of range" i)
    else
      vm.regs.(i) <- v

  let to_int vm v =
    match v with
    | Value.Int n -> n
    | _ -> fault vm "expected Int"; 0

  let to_float vm v =
    match v with
    | Value.Float f -> f
    | _ -> fault vm "expected Float"; 0.0

  let to_ptr vm v =
    match v with
    | Value.Ptr p -> p
    | _ -> fault vm "expected Ptr"; 0

  let rec guard vm f =
    match f () with
    | v                                    -> v
    | exception Exception.Bounds_error   m -> fault vm m; Value.Nil
    | exception Exception.Alloc_error    m -> fault vm m; Value.Nil
    | exception Exception.Type_error     m -> fault vm m; Value.Nil
    | exception Exception.Div_by_zero    m -> fault vm m; Value.Nil
    | exception Exception.Stack_overflow m -> fault vm m; Value.Nil
    | exception Exception.Native_error   e ->
        fault vm (Printexc.to_string e); Value.Nil
    | exception Exception.Registered (code, _) ->
        throw_code vm code; Value.Nil

  and guard_ vm f =
    match f () with
    | ()                                   -> ()
    | exception Exception.Bounds_error   m -> fault vm m
    | exception Exception.Alloc_error    m -> fault vm m
    | exception Exception.Type_error     m -> fault vm m
    | exception Exception.Div_by_zero    m -> fault vm m
    | exception Exception.Stack_overflow m -> fault vm m
    | exception Exception.Native_error   e ->
        fault vm (Printexc.to_string e)
    | exception Exception.Registered (code, _) ->
        throw_code vm code

  and guard_int vm f =
    match f () with
    | v                                    -> v
    | exception Exception.Bounds_error   m -> fault vm m; -1
    | exception Exception.Alloc_error    m -> fault vm m; -1
    | exception Exception.Type_error     m -> fault vm m; -1
    | exception Exception.Div_by_zero    m -> fault vm m; -1
    | exception Exception.Stack_overflow m -> fault vm m; -1
    | exception Exception.Native_error   e ->
        fault vm (Printexc.to_string e); -1
    | exception Exception.Registered (code, _) ->
        throw_code vm code; -1

  and throw_code vm code =
    T.on_throw vm.tracer_ctx ~pc:vm.pc;
    if vm.handler_sp = 0 then
      fault vm (Printf.sprintf "uncaught exception code %d" code)
    else begin
      let h          = vm.handlers.(vm.handler_sp - 1) in
      vm.handler_sp <- vm.handler_sp - 1;
      vm.frame_sp   <- h.handler_frame;
      reg_set_abs vm h.handler_reg (Value.Int code);
      vm.pc         <- h.handler_pc;
      vm.status     <- Running
    end

  let throw vm reg =
    throw_code vm (to_int vm (reg_get vm reg))

  let maybe_gc vm =
    if H.needs_minor_gc vm.heap then begin
      let result = G.run vm.heap vm.gc ~roots:vm.regs Minor in
      match result with
      | Some Gc.Major -> G.run vm.heap vm.gc ~roots:vm.regs Major |> ignore
      | _             -> ()
    end

  let do_call vm ~pc ~target ~arg_start ~arg_end ~ret_dst ~program =
    if vm.frame_sp >= Array.length vm.frames then
      throw_code vm (Exception.to_int Exception.Stack_overflow)
    else begin
      T.on_call vm.tracer_ctx ~pc ~target;
      let base = current_base vm in
      vm.frames.(vm.frame_sp) <- {
        return_pc = vm.pc;
        ret_dst   = base + ret_dst;
        reg_base  = base + arg_start;
        program;
      };
      vm.frame_sp <- vm.frame_sp + 1;
      vm.pc       <- target;
      ignore arg_end
    end

  let do_tcall vm ~pc ~target ~arg_start ~arg_end ~program =
    if vm.frame_sp = 0 then
      fault vm "tail call from top level"
    else begin
      T.on_call vm.tracer_ctx ~pc ~target;
      let base  = current_base vm in
      let frame = vm.frames.(vm.frame_sp - 1) in
      vm.frames.(vm.frame_sp - 1) <- {
        frame with
        reg_base = base + arg_start;
        program;
      };
      vm.pc <- target;
      ignore arg_end
    end

  let do_dcall vm ~pc ~fn_reg ~arg_start ~arg_end ~ret_dst =
    let base = current_base vm in
    let args = Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1) in
    match reg_get vm fn_reg with
    | Value.NativeFun fn ->
      let result = guard vm (fun () -> fn args) in
      reg_set vm ret_dst result
    | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure ->
      let code_val = guard vm (fun () -> H.read vm.heap addr 0) in
      (match Value.get_native bytecode_id code_val with
       | Some code ->
         do_call vm ~pc ~target:0 ~arg_start ~arg_end ~ret_dst ~program:code
       | None -> fault vm "DCall: closure has invalid code pointer")
    | _ -> fault vm "DCall: expected NativeFun or closure Ptr"

  let do_tdcall vm ~pc ~fn_reg ~arg_start ~arg_end =
    let base = current_base vm in
    let args = Array.sub vm.regs (base + arg_start) (arg_end - arg_start + 1) in
    match reg_get vm fn_reg with
    | Value.NativeFun fn ->
      let result = guard vm (fun () -> fn args) in
      ignore result
    | Value.Ptr addr when H.get_tag vm.heap addr = Tag.closure ->
      let code_val = guard vm (fun () -> H.read vm.heap addr 0) in
      (match Value.get_native bytecode_id code_val with
       | Some code ->
         do_tcall vm ~pc ~target:0 ~arg_start ~arg_end ~program:code
       | None -> fault vm "TDCall: closure has invalid code pointer")
    | _ -> fault vm "TDCall: expected NativeFun or closure Ptr"

  let step vm =
    match vm.status with
    | Halted | Fault _ -> ()
    | Running ->
      let pc      = vm.pc in
      let program = current_program vm in
      if pc < 0 || pc >= Array.length program then
        fault vm (Printf.sprintf "pc %d out of bounds" pc)
      else begin
        let instr = program.(pc) in
        T.on_instr vm.tracer_ctx ~pc instr;
        vm.pc <- pc + 1;
        match instr with

        | Instr.Nop -> ()

        | Instr.Mov (dst, src) ->
          reg_set vm dst (reg_get vm src)

        | Instr.Load (dst, n) ->
          reg_set vm dst (Value.Int n)

        | Instr.LoadF (dst, f) ->
          reg_set vm dst (Value.Float f)

        | Instr.LoadNil dst ->
          reg_set vm dst Value.Nil

        | Instr.LoadK (dst, idx) ->
          if idx < 0 || idx >= Array.length vm.constants then
            fault vm (Printf.sprintf "constant index %d out of range" idx)
          else
            reg_set vm dst vm.constants.(idx)

        | Instr.Add (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) + to_int vm (reg_get vm b)))

        | Instr.Sub (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) - to_int vm (reg_get vm b)))

        | Instr.Mul (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) * to_int vm (reg_get vm b)))

        | Instr.Div (dst, a, b) ->
          let divisor = to_int vm (reg_get vm b) in
          if divisor = 0 then
            throw_code vm (Exception.to_int Exception.Div_by_zero)
          else
            reg_set vm dst (Value.Int (to_int vm (reg_get vm a) / divisor))

        | Instr.Mod (dst, a, b) ->
          let divisor = to_int vm (reg_get vm b) in
          if divisor = 0 then
            throw_code vm (Exception.to_int Exception.Div_by_zero)
          else
            reg_set vm dst (Value.Int (to_int vm (reg_get vm a) mod divisor))

        | Instr.AddF (dst, a, b) ->
          reg_set vm dst (Value.Float
            (to_float vm (reg_get vm a) +. to_float vm (reg_get vm b)))

        | Instr.SubF (dst, a, b) ->
          reg_set vm dst (Value.Float
            (to_float vm (reg_get vm a) -. to_float vm (reg_get vm b)))

        | Instr.MulF (dst, a, b) ->
          reg_set vm dst (Value.Float
            (to_float vm (reg_get vm a) *. to_float vm (reg_get vm b)))

        | Instr.DivF (dst, a, b) ->
          reg_set vm dst (Value.Float
            (to_float vm (reg_get vm a) /. to_float vm (reg_get vm b)))

        | Instr.And (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) land to_int vm (reg_get vm b)))

        | Instr.Or (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) lor to_int vm (reg_get vm b)))

        | Instr.Xor (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) lxor to_int vm (reg_get vm b)))

        | Instr.Shl (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) lsl to_int vm (reg_get vm b)))

        | Instr.Shr (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) asr to_int vm (reg_get vm b)))

        | Instr.ShrU (dst, a, b) ->
          reg_set vm dst (Value.Int
            (to_int vm (reg_get vm a) lsr to_int vm (reg_get vm b)))

        | Instr.Eq (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_int vm (reg_get vm a) =  to_int vm (reg_get vm b) then 1 else 0))

        | Instr.EqF (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_float vm (reg_get vm a) =  to_float vm (reg_get vm b) then 1 else 0))

        | Instr.Ne (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_int vm (reg_get vm a) <> to_int vm (reg_get vm b) then 1 else 0))

        | Instr.NeF (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_float vm (reg_get vm a) <> to_float vm (reg_get vm b) then 1 else 0))

        | Instr.Lt (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_int vm (reg_get vm a) <  to_int vm (reg_get vm b) then 1 else 0))

        | Instr.LtF (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_float vm (reg_get vm a) <  to_float vm (reg_get vm b) then 1 else 0))

        | Instr.LtU (dst, a, b) ->
          let a = to_int vm (reg_get vm a) land max_int in
          let b = to_int vm (reg_get vm b) land max_int in
          reg_set vm dst (Value.Int (if a < b then 1 else 0))

        | Instr.Lte (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_int vm (reg_get vm a) <= to_int vm (reg_get vm b) then 1 else 0))

        | Instr.LteU (dst, a, b) ->
          let a = to_int vm (reg_get vm a) land max_int in
          let b = to_int vm (reg_get vm b) land max_int in
          reg_set vm dst (Value.Int (if a <= b then 1 else 0))

        | Instr.LteF (dst, a, b) ->
          reg_set vm dst (Value.Int
            (if to_float vm (reg_get vm a) <= to_float vm (reg_get vm b) then 1 else 0))

        | Instr.I2F (dst, src) ->
          reg_set vm dst
            (Value.Float (float_of_int (to_int vm (reg_get vm src))))

        | Instr.F2I (dst, src) ->
          reg_set vm dst
            (Value.Int (int_of_float (to_float vm (reg_get vm src))))

        | Instr.Alloc (dst, tag, size) ->
          maybe_gc vm;
          let addr = guard_int vm (fun () -> H.alloc vm.heap ~size ~tag) in
          if addr >= 0 then reg_set vm dst (Value.Ptr addr)

        | Instr.GetField (dst, obj, field) ->
          let addr = to_ptr vm (reg_get vm obj) in
          let v    = guard vm (fun () -> H.read vm.heap addr field) in
          reg_set vm dst v

        | Instr.SetField (obj, field, src) ->
          let addr = to_ptr vm (reg_get vm obj) in
          let v    = reg_get vm src in
          guard_ vm (fun () -> H.write vm.heap addr field v)

        | Instr.Jmp target ->
          vm.pc <- target

        | Instr.Jz (reg, target) ->
          if to_int vm (reg_get vm reg) = 0 then vm.pc <- target

        | Instr.Jnz (reg, target) ->
          if to_int vm (reg_get vm reg) <> 0 then vm.pc <- target

        | Instr.Call (target, arg_start, arg_end, ret_dst) ->
          do_call vm ~pc ~target ~arg_start ~arg_end ~ret_dst
            ~program:(current_program vm)

        | Instr.TCall (target, arg_start, arg_end) ->
          do_tcall vm ~pc ~target ~arg_start ~arg_end
            ~program:(current_program vm)

        | Instr.DCall (fn_reg, arg_start, arg_end, ret_dst) ->
          do_dcall vm ~pc ~fn_reg ~arg_start ~arg_end ~ret_dst

        | Instr.TDCall (fn_reg, arg_start, arg_end) ->
          do_tdcall vm ~pc ~fn_reg ~arg_start ~arg_end

        | Instr.Ret src ->
          if vm.frame_sp = 0 then
            fault vm "return from empty call stack"
          else begin
            T.on_ret vm.tracer_ctx ~pc;
            let result  = reg_get vm src in
            let frame   = vm.frames.(vm.frame_sp - 1) in
            vm.frame_sp <- vm.frame_sp - 1;
            vm.pc       <- frame.return_pc;
            reg_set_abs vm frame.ret_dst result
          end

        | Instr.Try (handler_pc, catch_reg) ->
          if vm.handler_sp >= Array.length vm.handlers then
            fault vm "handler stack overflow"
          else begin
            let base = current_base vm in
            vm.handlers.(vm.handler_sp) <- {
              handler_pc;
              handler_frame = vm.frame_sp;
              handler_reg   = base + catch_reg;
            };
            vm.handler_sp <- vm.handler_sp + 1
          end

        | Instr.EndTry ->
          if vm.handler_sp = 0 then fault vm "EndTry without Try"
          else vm.handler_sp <- vm.handler_sp - 1

        | Instr.Throw reg ->
          throw vm reg

        | Instr.Halt ->
          vm.status <- Halted
      end

  let run vm =
    while vm.status = Running do
      step vm
    done

  let reset vm =
    vm.pc         <- 0;
    vm.frame_sp   <- 0;
    vm.handler_sp <- 0;
    vm.status     <- Running;
    Array.fill vm.regs 0 (Array.length vm.regs) Value.Nil

  let is_done vm =
    match vm.status with
    | Running -> false
    | _       -> true

  let get_reg    vm i   = vm.regs.(i)
  let set_reg    vm i v = vm.regs.(i) <- v
  let heap       vm     = vm.heap

  let get_status vm =
    match vm.status with
    | Running  -> "running"
    | Halted   -> "halted"
    | Fault  m -> Printf.sprintf "fault: %s" m

end

module Fast  = Make(Heap.Fast)(No_tracer)
module Debug = Make(Heap.Debug)(Full_tracer)
