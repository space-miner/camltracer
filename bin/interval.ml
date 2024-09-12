open Base

module Interval = struct
  open Float

  type t =
    { min : Float.t
    ; max : Float.t
    }
  [@@deriving sexp]

  let empty = { min = infinity; max = neg infinity }
  let universe = { min = neg infinity; max = infinity }
  let size { min; max } = max - min
  let contains { min; max } x = min <= x && x <= max
  let surrounds { min; max } x = min < x && x < max
end
