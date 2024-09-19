open Base
open Ray
open Vec3
open Point3
open Material

module HitRecord = struct
  type a

  type t =
    { mutable point : Point3.t
    ; mutable normal : Vec3.t
    ; mutable time : Float.t
    ; mutable front_face : Bool.t
    ; mutable material : Material.t
    }
  [@@deriving sexp]

  let set_face_normal (hit_record : t) (ray : Ray.t) (outward_normal : Vec3.t) =
    hit_record.front_face <- Float.(Vec3.dot ray.direction outward_normal < 0.);
    hit_record.normal
    <- (if Bool.equal hit_record.front_face true
        then outward_normal
        else Vec3.neg outward_normal)
  ;;
end
