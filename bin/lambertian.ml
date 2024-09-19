open Base
open Color
open Vec3
open Ray

module Lambertian = struct
  type t = { albedo : Color.t } [@@deriving sexp]

  (* let scatter t ray_in hit_record attenuation ray_scattered = *)
  (*   let scatter_direction = Vec3.(hit_record.normal + random_unit_vector ()) in *)
  (*   (if Vec3.near_zero scatter_direction *)
  (*    then ray_scattered := hit_record.normal *)
  (*    else *)
  (*      ray_scattered := Ray.{ origin = hit_record.point; direction = scatter_direction }); *)
  (*   attenuation := t.albedo; *)
  (*   true *)
  (* ;; *)
end
