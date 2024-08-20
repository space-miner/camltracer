open Base
open Point3
open Vec3
open Color

module Ray = struct
  type t =
    { origin : Point3.t
    ; direction : Vec3.t
    }
  [@@deriving sexp]

  let position ray time = Point3.(ray.origin + scale ray.direction time)

  let ray_color ray =
    let unit_direction = Point3.unit_vector ray.direction in
    let a = 0.5 *. (unit_direction.g +. 1.) in
    Color.(
      scale { r = 1.; g = 1.; b = 1. } (1. -. a) + scale { r = 0.5; g = 0.7; b = 1. } a)
  ;;
end
