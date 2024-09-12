open Base
open Hitrecord
open Sphere

module Hittable = struct
  type t = Sphere of Sphere.t [@@deriving sexp]

  let hit hittable ray ray_tmin ray_tmax hit_record =
    match hittable with
    | Sphere s -> Sphere.hit s ray ray_tmin ray_tmax hit_record
  ;;
end
