open Color
open Ray
open Color
open Ray
open Lambertian
open Metal

module Material = struct
  type t =
    | Init
    | Lambertian of Lambertian.t
    | Metal of Metal.t
  [@@deriving sexp]

  (* let scatter *)
  (*   material *)
  (*   (ray_in : Ray.t) *)
  (*   (hit_record : HitRecord.t) *)
  (*   (attenuation : Color.t ref) *)
  (*   (ray_scattered : Ray.t ref) *)
  (*   = *)
  (*   match material with *)
  (*   | Init -> failwith "todo" *)
  (*   | Lambertian l -> Lambertian.scatter l ray_in hit_record attenuation ray_scattered *)
  (*   | Metal m -> Metal.scatter m ray_in hit_record attenuation ray_scattered *)
  (* ;; *)
end
