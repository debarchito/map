open Map_core

type frame = {
  return_pc : int;
  ret_dst   : int;
  reg_base  : int;
}

type handler = {
  handler_pc    : int;
  handler_frame : int;
  handler_reg   : int;
}

module type VM_TRACER = sig
  type ctx
  val on_instr : ctx -> pc:int -> Instr.instr -> unit
  val on_call  : ctx -> pc:int -> target:int -> unit
  val on_ret   : ctx -> pc:int -> unit
  val on_throw : ctx -> pc:int -> unit
end

module NoVMTracer = struct
  type ctx = unit
  let on_instr  () ~pc:_ _         = ()
  let on_call   () ~pc:_ ~target:_ = ()
  let on_ret    () ~pc:_           = ()
  let on_throw  () ~pc:_           = ()
end

module FullVMTracer = struct
  type ctx = {
    mutable instrs : (int * Instr.instr) list;
    mutable calls  : (int * int) list;
    mutable rets   : int list;
    mutable throws : int list;
  }
  let make () = { instrs = []; calls = []; rets = []; throws = [] }
  let on_instr  ctx ~pc instr    = ctx.instrs <- (pc, instr) :: ctx.instrs
  let on_call   ctx ~pc ~target  = ctx.calls  <- (pc, target) :: ctx.calls
  let on_ret    ctx ~pc          = ctx.rets   <- pc :: ctx.rets
  let on_throw  ctx ~pc          = ctx.throws <- pc :: ctx.throws
end

module type VM_INTF = sig
  type tracer_ctx
  type heap_tracer_ctx
  type heap_t
  type t
  val create     : Config.t -> Instr.instr array -> heap_tracer_ctx -> tracer_ctx -> t
  val run        : t -> unit
  val step       : t -> unit
  val reset      : t -> unit
  val is_done    : t -> bool
  val get_reg    : t -> int -> Value.t
  val get_status : t -> string
  val set_reg    : t -> int -> Value.t -> unit
  val heap       : t -> heap_t
end

module VM(H : Heap.HEAP_INTF)(T : VM_TRACER) : VM_INTF
  with type tracer_ctx      = T.ctx
  and  type heap_tracer_ctx = H.tracer_ctx
  and  type heap_t          = H.t = struct

  type tracer_ctx      = T.ctx
  type heap_tracer_ctx = H.tracer_ctx
  type heap_t          = H.t

  module G = Gc.GC(H)

  type status =
    | Running
    | Halted
    | Fault of string

  type t = {
    heap               : H.t;
    gc                 : Gc.gc_state;
    program            : Instr.instr array;
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
    let heap = H.create cfg heap_tracer_ctx in
    let gc   = G.make_state
                 ~major_threshold:cfg.major_threshold
                 ~major_growth_factor:cfg.major_growth_factor in
    { heap;
      gc;
      program;
      regs        = Array.make cfg.num_registers Value.MNil;
      frames      = Array.make cfg.max_call_depth
                      { return_pc = 0; ret_dst = 0; reg_base = 0 };
      frame_sp    = 0;
      handlers    = Array.make cfg.max_exception_depth
                      { handler_pc = 0; handler_frame = 0; handler_reg = 0 };
      handler_sp  = 0;
      pc          = 0;
      status      = Running;
      tracer_ctx }

  let fault vm msg =
    vm.status <- Fault msg

  let current_base vm =
    if vm.frame_sp = 0 then 0
    else vm.frames.(vm.frame_sp - 1).reg_base

  let reg_get vm r =
    let i = current_base vm + r in
    if i < 0 || i >= Array.length vm.regs then
      (fault vm (Printf.sprintf "register %d out of range" r); Value.MNil)
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
      (fault vm (Printf.sprintf "register %d out of range" i); Value.MNil)
    else
      vm.regs.(i)

  let reg_set_abs vm i v =
    if i < 0 || i >= Array.length vm.regs then
      fault vm (Printf.sprintf "register %d out of range" i)
    else
      vm.regs.(i) <- v

  let as_int vm v =
    match v with
    | Value.MInt n -> n
    | _ -> fault vm "expected int"; 0

  let as_float vm v =
    match v with
    | Value.MFloat f -> f
    | _ -> fault vm "expected float"; 0.0

  let as_ptr vm v =
    match v with
    | Value.MPtr p -> p
    | _ -> fault vm "expected ptr"; 0

  let guard vm f =
    match f () with
    | v                                -> v
    | exception Value.EBoundsError   m -> fault vm m; Value.MNil
    | exception Value.EAllocError    m -> fault vm m; Value.MNil
    | exception Value.ETypeError     m -> fault vm m; Value.MNil
    | exception Value.EDivByZero     m -> fault vm m; Value.MNil
    | exception Value.EStackOverflow m -> fault vm m; Value.MNil
    | exception Value.ENativeError   e ->
        fault vm (Printexc.to_string e); Value.MNil

  let guard_ vm f =
    match f () with
    | ()                               -> ()
    | exception Value.EBoundsError   m -> fault vm m
    | exception Value.EAllocError    m -> fault vm m
    | exception Value.ETypeError     m -> fault vm m
    | exception Value.EDivByZero     m -> fault vm m
    | exception Value.EStackOverflow m -> fault vm m
    | exception Value.ENativeError   e ->
        fault vm (Printexc.to_string e)

  let guard_int vm f =
    match f () with
    | v                                -> v
    | exception Value.EBoundsError   m -> fault vm m; -1
    | exception Value.EAllocError    m -> fault vm m; -1
    | exception Value.ETypeError     m -> fault vm m; -1
    | exception Value.EDivByZero     m -> fault vm m; -1
    | exception Value.EStackOverflow m -> fault vm m; -1
    | exception Value.ENativeError   e ->
        fault vm (Printexc.to_string e); -1

  let throw_code vm code =
    T.on_throw vm.tracer_ctx ~pc:vm.pc;
    if vm.handler_sp = 0 then
      fault vm (Printf.sprintf "uncaught exception code %d" code)
    else begin
      let h          = vm.handlers.(vm.handler_sp - 1) in
      vm.handler_sp <- vm.handler_sp - 1;
      vm.frame_sp   <- h.handler_frame;
      reg_set_abs vm (current_base vm + h.handler_reg) (Value.MInt code);
      vm.pc         <- h.handler_pc;
      vm.status     <- Running
    end

  let throw vm reg =
    throw_code vm (as_int vm (reg_get vm reg))

  let maybe_gc vm =
    if H.needs_minor_gc vm.heap then begin
      let result = G.run vm.heap vm.gc ~roots:vm.regs Minor in
      match result with
      | Some Gc.Major -> G.run vm.heap vm.gc ~roots:vm.regs Major |> ignore
      | _             -> ()
    end

  let step vm =
    match vm.status with
    | Halted | Fault _ -> ()
    | Running ->
      let pc = vm.pc in
      if pc < 0 || pc >= Array.length vm.program then
        fault vm (Printf.sprintf "pc %d out of bounds" pc)
      else begin
        let instr = vm.program.(pc) in
        T.on_instr vm.tracer_ctx ~pc instr;
        vm.pc <- pc + 1;
        match instr with

        | Instr.Nop -> ()

        | Instr.Mov (dst, src) ->
          reg_set vm dst (reg_get vm src)

        | Instr.Set (dst, n) ->
          reg_set vm dst (Value.MInt n)

        | Instr.SetF (dst, f) ->
          reg_set vm dst (Value.MFloat f)

        | Instr.SetNil dst ->
          reg_set vm dst Value.MNil

        | Instr.Add (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) + as_int vm (reg_get vm b)))

        | Instr.Sub (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) - as_int vm (reg_get vm b)))

        | Instr.Mul (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) * as_int vm (reg_get vm b)))

        | Instr.Div (dst, a, b) ->
          let divisor = as_int vm (reg_get vm b) in
          if divisor = 0 then
            throw_code vm (Value.int_of_error Value.EDivByZero)
          else
            reg_set vm dst (Value.MInt (as_int vm (reg_get vm a) / divisor))

        | Instr.Mod (dst, a, b) ->
          let divisor = as_int vm (reg_get vm b) in
          if divisor = 0 then
            throw_code vm (Value.int_of_error Value.EDivByZero)
          else
            reg_set vm dst (Value.MInt (as_int vm (reg_get vm a) mod divisor))

        | Instr.AddF (dst, a, b) ->
          reg_set vm dst (Value.MFloat
            (as_float vm (reg_get vm a) +. as_float vm (reg_get vm b)))

        | Instr.SubF (dst, a, b) ->
          reg_set vm dst (Value.MFloat
            (as_float vm (reg_get vm a) -. as_float vm (reg_get vm b)))

        | Instr.MulF (dst, a, b) ->
          reg_set vm dst (Value.MFloat
            (as_float vm (reg_get vm a) *. as_float vm (reg_get vm b)))

        | Instr.DivF (dst, a, b) ->
          reg_set vm dst (Value.MFloat
            (as_float vm (reg_get vm a) /. as_float vm (reg_get vm b)))

        | Instr.And (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) land as_int vm (reg_get vm b)))

        | Instr.Or (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) lor as_int vm (reg_get vm b)))

        | Instr.Xor (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) lxor as_int vm (reg_get vm b)))

        | Instr.Shl (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) lsl as_int vm (reg_get vm b)))

        | Instr.Shr (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) asr as_int vm (reg_get vm b)))

        | Instr.ShrU (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (as_int vm (reg_get vm a) lsr as_int vm (reg_get vm b)))

        | Instr.Eq (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_int vm (reg_get vm a) =  as_int vm (reg_get vm b) then 1 else 0))

        | Instr.EqF (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_float vm (reg_get vm a) =  as_float vm (reg_get vm b) then 1 else 0))

        | Instr.Ne (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_int vm (reg_get vm a) <> as_int vm (reg_get vm b) then 1 else 0))

        | Instr.Lt (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_int vm (reg_get vm a) <  as_int vm (reg_get vm b) then 1 else 0))

        | Instr.LtF (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_float vm (reg_get vm a) <  as_float vm (reg_get vm b) then 1 else 0))

        | Instr.LtU (dst, a, b) ->
          let a = as_int vm (reg_get vm a) land max_int in
          let b = as_int vm (reg_get vm b) land max_int in
          reg_set vm dst (Value.MInt (if a < b then 1 else 0))

        | Instr.Lte (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_int vm (reg_get vm a) <= as_int vm (reg_get vm b) then 1 else 0))

        | Instr.LteU (dst, a, b) ->
          let a = as_int vm (reg_get vm a) land max_int in
          let b = as_int vm (reg_get vm b) land max_int in
          reg_set vm dst (Value.MInt (if a <= b then 1 else 0))

        | Instr.LteF (dst, a, b) ->
          reg_set vm dst (Value.MInt
            (if as_float vm (reg_get vm a) <= as_float vm (reg_get vm b) then 1 else 0))

        | Instr.I2F (dst, src) ->
          reg_set vm dst
            (Value.MFloat (float_of_int (as_int vm (reg_get vm src))))

        | Instr.F2I (dst, src) ->
          reg_set vm dst
            (Value.MInt (int_of_float (as_float vm (reg_get vm src))))

        | Instr.Alloc (dst, tag, size) ->
          maybe_gc vm;
          let addr = guard_int vm (fun () -> H.alloc vm.heap ~size ~tag) in
          if addr >= 0 then reg_set vm dst (Value.MPtr addr)

        | Instr.GetField (dst, obj, field) ->
          let addr = as_ptr vm (reg_get vm obj) in
          let v    = guard vm (fun () -> H.read vm.heap addr field) in
          reg_set vm dst v

        | Instr.SetField (obj, field, src) ->
          let addr = as_ptr vm (reg_get vm obj) in
          let v    = reg_get vm src in
          guard_ vm (fun () -> H.write vm.heap addr field v)

        | Instr.Jmp target ->
          vm.pc <- target

        | Instr.Jz (reg, target) ->
          if as_int vm (reg_get vm reg) = 0 then vm.pc <- target

        | Instr.Jnz (reg, target) ->
          if as_int vm (reg_get vm reg) <> 0 then vm.pc <- target

        | Instr.Call (target, arg_start, arg_end, ret_dst) ->
          if vm.frame_sp >= Array.length vm.frames then
            throw_code vm (Value.int_of_error Value.EStackOverflow)
          else begin
            T.on_call vm.tracer_ctx ~pc ~target;
            let base = current_base vm in
            vm.frames.(vm.frame_sp) <- {
              return_pc = vm.pc;
              ret_dst   = base + ret_dst;
              reg_base  = base + arg_start;
            };
            vm.frame_sp <- vm.frame_sp + 1;
            vm.pc       <- target;
            ignore arg_end
          end

        | Instr.TCall (target, arg_start, arg_end) ->
          if vm.frame_sp = 0 then
            fault vm "tail call from top level"
          else begin
            T.on_call vm.tracer_ctx ~pc ~target;
            let base  = current_base vm in
            let frame = vm.frames.(vm.frame_sp - 1) in
            vm.frames.(vm.frame_sp - 1) <- {
              frame with
              reg_base = base + arg_start;
            };
            vm.pc <- target;
            ignore arg_end
          end

        | Instr.NCall (fn_reg, arg_start, arg_end, ret_dst) ->
          (match reg_get vm fn_reg with
           | Value.MNativeFn fn ->
             let base   = current_base vm in
             let args   = Array.sub vm.regs (base + arg_start)
                            (arg_end - arg_start + 1) in
             let result = guard vm (fun () -> fn args) in
             reg_set vm ret_dst result
           | _ -> fault vm "NCall: not a native function")

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
    Array.fill vm.regs 0 (Array.length vm.regs) Value.MNil

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

module FastVM  = VM(Heap.FastHeap)(NoVMTracer)
module DebugVM = VM(Heap.DebugHeap)(FullVMTracer)
