type t = {
  num_registers       : int;
  max_call_depth      : int;
  max_exception_depth : int;
  chunk_size          : int;
  young_limit         : int;
  max_chunks          : int;
}

let default = {
  num_registers       = 256;
  max_call_depth      = 512;
  max_exception_depth = 64;
  chunk_size          = 4096;
  young_limit         = 1024;
  max_chunks          = 1024;
}

let validate t =
  if t.num_registers       <= 0 then failwith "Config: num_registers must be > 0";
  if t.max_call_depth      <= 0 then failwith "Config: max_call_depth must be > 0";
  if t.max_exception_depth <= 0 then failwith "Config: max_exception_depth must be > 0";
  if t.chunk_size          <= 0 then failwith "Config: chunk_size must be > 0";
  if t.young_limit         <= 0 then failwith "Config: young_limit must be > 0";
  if t.max_chunks          <= 0 then failwith "Config: max_chunks must be > 0"
