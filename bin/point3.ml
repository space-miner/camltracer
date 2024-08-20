open Vec3

module Point3 = struct
  include Vec3

  type t = Vec3.t [@@deriving sexp]
end
