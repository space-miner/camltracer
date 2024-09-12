open Base
open Hitrecord
open Interval
open Point3
open Ray
open Vec3

module Sphere = struct
  type t =
    { center : Point3.t
    ; radius : Float.t
    }
  [@@deriving sexp]

  let hit sphere (ray : Ray.t) (time_interval : Interval.t) (hit_record : HitRecord.t) =
    let oc = Point3.(sphere.center - ray.origin) in
    (* let a = ray.direction |> Vec3.length |> Float.square in *)
    let a = Vec3.dot ray.direction ray.direction in
    let h = Vec3.dot ray.direction oc in
    (* let c = Float.(square (Point3.length oc) - square sphere.radius) in *)
    let c = Point3.dot oc oc -. Float.square sphere.radius in
    let discriminant = Float.(square h - (a * c)) in
    if Float.(discriminant < 0.)
    then false
    else (
      let sqrt_discriminant = Float.sqrt discriminant in
      let root1 = Float.((h - sqrt_discriminant) / a) in
      let root2 = Float.((h + sqrt_discriminant) / a) in
      if (not (Interval.surrounds time_interval root1))
         && not (Interval.surrounds time_interval root2)
      then false
      else (
        let root = if Interval.surrounds time_interval root1 then root1 else root2 in
        hit_record.time <- root;
        hit_record.point <- Ray.position ray hit_record.time;
        hit_record.normal
        <- Vec3.scale Point3.(hit_record.point - sphere.center) (1. /. sphere.radius);
        (* Hitrecord.set_face_normal hit_record ray hit_record.normal; *)
        hit_record.front_face <- Float.(Vec3.dot ray.direction hit_record.normal < 0.);
        hit_record.normal
        <- (if Bool.equal hit_record.front_face true
            then hit_record.normal
            else Vec3.neg hit_record.normal);
        true))
  ;;
  (*   else ( *)
  (*     let sqrt_discriminant = Float.sqrt discriminant in *)
  (*     let root = Float.((h - sqrt_discriminant) / a) in *)
  (*     if Float.(root <= ray_tmin) || Float.(ray_tmax <= root) *)
  (*     then ( *)
  (*       let root = Float.((h + sqrt_discriminant) / a) in *)
  (*       if Float.(root <= ray_tmin) || Float.(ray_tmax <= root) *)
  (*       then false *)
  (*       else ( *)
  (*         hit_record.time <- root; *)
  (*         hit_record.point <- Ray.position ray root; *)
  (*         hit_record.normal *)
  (*         <- Vec3.scale Point3.(hit_record.point - sphere.center) (1. /. sphere.radius); *)
  (*         hit_record.front_face <- Float.(Vec3.dot ray.direction hit_record.normal < 0.); *)
  (*         hit_record.normal *)
  (*         <- (if Bool.equal hit_record.front_face true *)
  (*             then hit_record.normal *)
  (*             else Vec3.neg hit_record.normal); *)
  (*         true)) *)
  (*     else ( *)
  (*       hit_record.time <- root; *)
  (*       hit_record.point <- Ray.position ray root; *)
  (*       hit_record.normal *)
  (*       <- Vec3.scale Point3.(hit_record.point - sphere.center) (1. /. sphere.radius); *)
  (*       hit_record.front_face <- Float.(Vec3.dot ray.direction hit_record.normal < 0.); *)
  (*       hit_record.normal *)
  (*       <- (if Bool.equal hit_record.front_face true *)
  (*           then hit_record.normal *)
  (*           else Vec3.neg hit_record.normal); *)
  (*       true)) *)
  (* ;; *)
end
