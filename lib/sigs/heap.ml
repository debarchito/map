open Core

type gen = Young | Old

type event =
  | Minor_start
  | Minor_end   of { promoted : int }
  | Major_mark  of { steps : int }
  | Major_sweep of { steps : int; freed : int }
  | Major_end

type stats = {
  young_used  : int;
  young_total : int;
  old_used    : int;
  old_total   : int;
  n_chunks    : int;
  alloc_count : int;
}

type obj_info = {
  addr   : int;
  tag    : int;
  size   : int;
  gen    : gen;
  marked : bool;
  fwd    : int;
  fields : Value.t array;
}

module type TRACER = sig
  type ctx
  val on_alloc      : ctx -> addr:int -> size:int -> tag:int -> unit
  val on_free       : ctx -> addr:int -> unit
  val on_promote    : ctx -> addr:int -> unit
  val on_gc         : ctx -> event -> unit
  val on_write_slot : ctx -> chunk_idx:int -> slot:int -> Value.t -> unit
end

module type S = sig
  type tracer_ctx
  type chunk = {
    data              : Value.t array;
    size              : int;
    mutable top       : int;
    gen               : gen;
    mutable free_list : int array;
  }
  type t
  val create                : Config.Heap.t -> tracer_ctx -> t
  val alloc                 : t -> size:int -> tag:int -> int
  val alloc_old             : t -> size:int -> tag:int -> int
  val free_old              : t -> int -> unit
  val read                  : t -> int -> int -> Value.t
  val write                 : t -> int -> int -> Value.t -> unit
  val get_tag               : t -> int -> int
  val get_size              : t -> int -> int
  val get_mark              : t -> int -> bool
  val get_fwd               : t -> int -> int
  val set_tag               : t -> int -> int -> unit
  val set_mark              : t -> int -> bool -> unit
  val set_fwd               : t -> int -> int -> unit
  val is_young              : t -> int -> bool
  val is_old                : t -> int -> bool
  val is_card_dirty         : t -> int -> bool
  val clear_card            : t -> int -> unit
  val needs_minor_gc        : t -> bool
  val reset_young           : t -> unit
  val chunk_size            : t -> int
  val on_gc                 : t -> event -> unit
  val iter_young_chunks     : t -> (int -> chunk -> unit) -> unit
  val iter_old_chunks       : t -> (int -> chunk -> unit) -> unit
  val iter_dirty_old_chunks : t -> (int -> chunk -> unit) -> unit
  val iter_chunk_objects    : t -> int -> (int -> unit) -> unit
  val iter_objects          : t -> (int -> unit) -> unit
  val stats                 : t -> stats
  val inspect               : t -> int -> obj_info
  val tracer                : t -> tracer_ctx
end
