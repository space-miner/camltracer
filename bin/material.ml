open Lambertian
open Metal
open Dielectric

module Material = struct
  type t =
    | Init
    | Lambertian of Lambertian.t
    | Metal of Metal.t
    | Dielectric of Dielectric.t
  [@@deriving sexp]
end
