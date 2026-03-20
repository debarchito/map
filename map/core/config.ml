module Heap = struct
  type t = { chunk_size : int; young_limit : int; max_chunks : int }

  let default = { chunk_size = 4096; young_limit = 1024; max_chunks = 1024 }

  let validate t =
    if t.chunk_size land (t.chunk_size - 1) <> 0 then
      failwith "Config.Heap: chunk_size must be a power of two";
    if t.young_limit <= 0 then failwith "Config.Heap: young_limit must be > 0";
    if t.max_chunks <= 0 then failwith "Config.Heap: max_chunks must be > 0"
end

module Gc = struct
  type t = { mutable major_threshold : int; major_growth_factor : float }

  let default = { major_threshold = 4096; major_growth_factor = 2.0 }

  let validate t =
    if t.major_threshold <= 0 then
      failwith "Config.Gc: major_threshold must be > 0";
    if t.major_growth_factor <= 1.0 then
      failwith "Config.Gc: major_growth_factor must be > 1.0"
end

module Vm = struct
  type t = {
    num_registers : int;
    max_call_depth : int;
    max_exception_depth : int;
  }

  let default =
    { num_registers = 256; max_call_depth = 512; max_exception_depth = 64 }

  let validate t =
    if t.num_registers <= 0 then failwith "Config.Vm: num_registers must be > 0";
    if t.max_call_depth <= 0 then
      failwith "Config.Vm: max_call_depth must be > 0";
    if t.max_exception_depth <= 0 then
      failwith "Config.Vm: max_exception_depth must be > 0"
end

module Scheduler = struct
  type t = { fiber_quantum : int; max_fibers : int; max_call_depth : int }

  let default =
    { fiber_quantum = 1000; max_fibers = 1024; max_call_depth = 512 }

  let validate t =
    if t.fiber_quantum <= 0 then
      failwith "Config.Scheduler: fiber_quantum must be > 0";
    if t.max_fibers <= 0 then
      failwith "Config.Scheduler: max_fibers must be > 0"
end

module Bytecode = struct
  type t = { max_constants : int; max_functions : int }

  let default = { max_constants = 4096; max_functions = 1024 }

  let validate t =
    if t.max_constants <= 0 then
      failwith "Config.Bytecode: max_constants must be > 0";
    if t.max_functions <= 0 then
      failwith "Config.Bytecode: max_functions must be > 0"
end

type t = {
  heap : Heap.t;
  gc : Gc.t;
  vm : Vm.t;
  scheduler : Scheduler.t;
  bytecode : Bytecode.t;
}

let default =
  {
    heap = Heap.default;
    gc = Gc.default;
    vm = Vm.default;
    scheduler = Scheduler.default;
    bytecode = Bytecode.default;
  }

let validate t =
  Heap.validate t.heap;
  Gc.validate t.gc;
  Vm.validate t.vm;
  Scheduler.validate t.scheduler;
  Bytecode.validate t.bytecode
