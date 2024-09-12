open Base
open Stdio
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

  let hit_sphere (center : Point3.t) (radius : Float.t) (ray : t) : Float.t =
    let oc = Point3.(center - ray.origin) in
    let a = Vec3.dot ray.direction ray.direction in
    (* let b = 2. *. Vec3.dot oc ray.direction in *)
    let h = Vec3.dot ray.direction oc in
    let c = Point3.dot oc oc -. Float.square radius in
    (* let discriminant = Float.(square b - (4. * a * c)) in *)
    let discriminant = Float.(square h - (a * c)) in
    if Float.(discriminant < 0.)
    then Float.neg 1. (* else Float.(neg b - (sqrt discriminant / (2.0 * a))) *)
    else Float.((h - sqrt discriminant) / a)
  ;;

  let ray_color ray : Color.t =
    let center = Vec3.{ r = 0.; g = 0.; b = -1. } in
    let radius = 0.5 in
    let t = hit_sphere center radius ray in
    if Float.(t > 0.)
    then (
      let n = Vec3.( - ) (position ray t) center in
      let unit_n = Vec3.unit_vector n in
      Color.scale { r = unit_n.r +. 1.; g = unit_n.g +. 1.; b = unit_n.b +. 1. } 0.5)
    else (
      let unit_direction = Point3.unit_vector ray.direction in
      let a = 0.5 *. (unit_direction.g +. 1.) in
      Color.(
        scale { r = 1.; g = 1.; b = 1. } (1. -. a) + scale { r = 0.5; g = 0.7; b = 1. } a))
  ;;
end
