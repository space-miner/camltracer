open Base
open Color
open Ray

module Metal = struct
  type t =
    { albedo : Color.t
    ; mutable fuzz : Float.t
    }
  [@@deriving sexp]

  (* let scatter t ray_in (hit_record : HitRecord.t) attenuation ray_scattered = *)
  (*   let ray_reflected = Ray.reflect ray_in hit_record.normal in *)
  (*   (ray_scattered := Ray.{ origin = hit_record.point; direction = ray_reflected }); *)
  (*   attenuation := t.albedo; *)
  (*   true *)
  (* ;; *)
end
