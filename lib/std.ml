open Map_core

module Make(H : Heap.HEAP_INTF) = struct

  let load heap =
    let all_fns = Array.concat [
      Map_std.Vmstring.table;
    ] in
    let n    = Array.length all_fns in
    let addr = H.alloc heap ~size:n ~tag:0 in
    Array.iteri (fun i fn ->
      H.write heap addr i fn
    ) all_fns;
    Value.MPtr addr

end

module String_idx = struct
  let base          = 0
  let length        = base + Map_std.Vmstring.Index.length
  let byte_length   = base + Map_std.Vmstring.Index.byte_length
  let concat        = base + Map_std.Vmstring.Index.concat
  let substring     = base + Map_std.Vmstring.Index.substring
  let get_byte      = base + Map_std.Vmstring.Index.get_byte
  let get_codepoint = base + Map_std.Vmstring.Index.get_codepoint
  let eq            = base + Map_std.Vmstring.Index.eq
  let lt            = base + Map_std.Vmstring.Index.lt
  let contains      = base + Map_std.Vmstring.Index.contains
  let starts_with   = base + Map_std.Vmstring.Index.starts_with
  let ends_with     = base + Map_std.Vmstring.Index.ends_with
  let trim          = base + Map_std.Vmstring.Index.trim
  let uppercase     = base + Map_std.Vmstring.Index.uppercase
  let lowercase     = base + Map_std.Vmstring.Index.lowercase
  let to_int        = base + Map_std.Vmstring.Index.to_int
  let to_float      = base + Map_std.Vmstring.Index.to_float
  let from_int      = base + Map_std.Vmstring.Index.from_int
  let from_float    = base + Map_std.Vmstring.Index.from_float
  let print         = base + Map_std.Vmstring.Index.print
  let println       = base + Map_std.Vmstring.Index.println
end
