open Base

module Dielectric = struct
  type t = { mutable refraction_index : Float.t } [@@deriving sexp]
end
