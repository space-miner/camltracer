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

  let position ray time : Point3.t = Point3.(ray.origin + scale ray.direction time)

  let hit_sphere (center : Point3.t) (radius : Float.t) (ray : t) : Bool.t =
    let oc = Point3.(center - ray.origin) in
    let a = Vec3.dot ray.direction ray.direction in
    let b = 2. *. Vec3.dot oc ray.direction in
    let c = Point3.dot oc oc -. (radius *. radius) in
    let discriminant = (b *. b) -. (4. *. a *. c) in
    Float.(discriminant >= 0.)
  ;;

  let ray_color ray : Color.t =
    let center = Vec3.{ r = 0.; g = 0.; b = -1. } in
    let radius = 0.5 in
    if hit_sphere center radius ray
    then Vec3.{ r = 1.; g = 0.; b = 0. }
    else (
      let unit_direction = Point3.unit_vector ray.direction in
      let a = 0.5 *. (unit_direction.g +. 1.) in
      Vec3.(
        scale { r = 1.; g = 1.; b = 1. } (1. -. a) + scale { r = 0.5; g = 0.7; b = 1. } a))
  ;;
end
