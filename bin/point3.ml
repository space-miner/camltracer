open Base
open Vec3

module Point3 = struct
  type t = Vec3.t [@@deriving sexp]
end
