open Base
open Hitrecord
open Sphere

module Hittable = struct
  type t = Sphere of Sphere.t [@@deriving sexp]

  let hit hittable ray time_interval hit_record =
    match hittable with
    | Sphere s -> Sphere.hit s ray time_interval hit_record
  ;;
end
