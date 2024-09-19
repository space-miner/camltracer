open Base
open Stdio
open Vec3
open Color
open Point3
open Ray
open Hitrecord
open Hittablelist
open Interval
open Material
open Metal
open Lambertian
open Dielectric

let metal_scatter
  (t : Metal.t)
  (ray_in : Ray.t)
  (hit_record : HitRecord.t)
  (attenuation : Color.t ref)
  (scattered : Ray.t ref)
  =
  (* normalize fuzz *)
  t.fuzz <- Float.min t.fuzz 1.;
  let reflected = Vec3.reflect ray_in.direction hit_record.normal in
  let reflected_and_fuzzed =
    Vec3.(unit_vector reflected + scale (random_unit_vector ()) t.fuzz)
  in
  (scattered := Ray.{ origin = hit_record.point; direction = reflected_and_fuzzed });
  attenuation := t.albedo;
  Float.(Vec3.dot !scattered.direction hit_record.normal > 0.)
;;

let lambertian_scatter
  (t : Lambertian.t)
  (ray_in : Ray.t)
  (hit_record : HitRecord.t)
  (attenuation : Color.t ref)
  (scattered : Ray.t ref)
  =
  let scatter_direction = Vec3.(hit_record.normal + random_unit_vector ()) in
  let _ =
    if Vec3.near_zero scatter_direction
    then scattered := Ray.{ origin = hit_record.point; direction = hit_record.normal }
    else scattered := Ray.{ origin = hit_record.point; direction = scatter_direction }
  in
  attenuation := t.albedo;
  true
;;

let dielectric_scatter
  (t : Dielectric.t)
  (ray_in : Ray.t)
  (hit_record : HitRecord.t)
  (attenuation : Color.t ref)
  (scattered : Ray.t ref)
  : Bool.t
  =
  let reflectance cosine refraction_index =
    let r0 = (1. -. refraction_index) /. (1. +. refraction_index) |> Float.square in
    Float.(r0 + ((1. - r0) * int_pow (1. - cosine) 5))
  in
  attenuation := { r = 1.; g = 1.; b = 1. };
  let refraction_index =
    if hit_record.front_face then 1. /. t.refraction_index else t.refraction_index
  in
  let unit_direction = Vec3.unit_vector ray_in.direction in
  let cos_theta = Float.min Vec3.(dot (neg unit_direction) hit_record.normal) 1. in
  let sin_theta = Float.(sqrt (1. -. square cos_theta)) in
  let cannot_refract = Float.(refraction_index *. sin_theta > 1.) in
  if cannot_refract || Float.(reflectance cos_theta refraction_index > Random.float 1.)
  then (
    let reflected = Vec3.reflect unit_direction hit_record.normal in
    scattered := Ray.{ origin = hit_record.point; direction = reflected })
  else (
    let refracted = Vec3.refract unit_direction hit_record.normal refraction_index in
    scattered := Ray.{ origin = hit_record.point; direction = refracted });
  true
;;

let scatter
  (material : Material.t)
  (ray_in : Ray.t)
  (hit_record : HitRecord.t)
  (attenuation : Color.t ref)
  (scattered : Ray.t ref)
  =
  match material with
  | Lambertian l -> lambertian_scatter l ray_in hit_record attenuation scattered
  | Metal m -> metal_scatter m ray_in hit_record attenuation scattered
  | Dielectric d -> dielectric_scatter d ray_in hit_record attenuation scattered
  | _ -> failwith "todo"
;;

module Camera = struct
  type t =
    { aspect_ratio : Float.t
    ; image_width : Float.t
    ; image_height : Float.t
    ; samples_per_pixel : Int.t
    ; pixel_samples_scale : Float.t
    ; center : Point3.t
    ; pixel00_loc : Point3.t
    ; pixel_delta_u : Vec3.t
    ; pixel_delta_v : Vec3.t
    ; depth : Int.t
    ; defocus_angle : Float.t
    ; focus_distance : Float.t
    ; defocus_disk_u : Vec3.t
    ; defocus_disk_v : Vec3.t
    }
  [@@deriving sexp]

  let make
    ~aspect_ratio
    ~image_width
    ~samples_per_pixel
    ~depth
    ~vertical_field_of_vision
    ~look_from
    ~look_at
    ~up_vector
    ~defocus_angle
    ~focus_distance
    =
    let image_height =
      Int.max 1 (Int.of_float (image_width /. aspect_ratio)) |> Float.of_int
    in
    let pixel_samples_scale = 1. /. Float.of_int samples_per_pixel in
    (* camera *)
    let camera_center = look_from in
    let theta = vertical_field_of_vision *. Float.pi /. 180. in
    let h = Float.tan (theta /. 2.) in
    let viewport_height = 2. *. h *. focus_distance in
    let viewport_width = viewport_height *. (image_width /. image_height) in
    (* calculate unit basis vectors *)
    let basis_w = Vec3.(unit_vector (look_from - look_at)) in
    let basis_u = Vec3.(unit_vector (cross up_vector basis_w)) in
    let basis_v = Vec3.cross basis_w basis_u in
    (* vectors across viewport edges *)
    let viewport_u = Vec3.scale basis_u viewport_width in
    let viewport_v = Vec3.(scale (neg basis_v) viewport_height) in
    (* horizontal and vertical delta vectors *)
    let pixel_delta_u = Point3.scale viewport_u (1. /. image_width) in
    let pixel_delta_v = Point3.scale viewport_v (1. /. image_height) in
    (* upper left pixel *)
    let viewport_upper_left =
      Vec3.(
        camera_center
        - scale basis_w focus_distance
        - scale viewport_u 0.5
        - scale viewport_v 0.5)
    in
    let pixel00_loc =
      Point3.(viewport_upper_left + scale (pixel_delta_u + pixel_delta_v) 0.5)
    in
    (* camera defocus disk basis vectors *)
    let degrees_to_radians deg = Float.(deg * pi / 180.) in
    let defocus_radius =
      Float.(focus_distance * tan (degrees_to_radians (defocus_angle / 2.)))
    in
    let defocus_disk_u = Vec3.scale basis_u defocus_radius in
    let defocus_disk_v = Vec3.scale basis_v defocus_radius in
    { aspect_ratio
    ; image_width
    ; image_height
    ; samples_per_pixel
    ; pixel_samples_scale
    ; center = camera_center
    ; pixel00_loc
    ; pixel_delta_u
    ; pixel_delta_v
    ; depth
    ; defocus_angle
    ; focus_distance
    ; defocus_disk_u
    ; defocus_disk_v
    }
  ;;

  let rec ray_color (ray : Ray.t) (depth : Int.t) (world : HittableList.t) : Color.t =
    if depth <= 0
    then Color.{ r = 0.; g = 0.; b = 0. }
    else (
      let hit_record =
        HitRecord.
          { point = Point3.{ r = 0.; g = 0.; b = 0. }
          ; normal = Vec3.{ r = 0.; g = 0.; b = 0. }
          ; time = 0.
          ; front_face = false
          ; material = Material.Init
          }
      in
      let time_interval = Interval.{ min = 0.001; max = Float.infinity } in
      if HittableList.hit world ray time_interval hit_record
      then (
        let ray_scattered =
          ref
            Ray.
              { origin = { r = 0.; g = 0.; b = 0. }
              ; direction = { r = 0.; g = 0.; b = 0. }
              }
        in
        let attenuation = ref Vec3.{ r = 0.; g = 0.; b = 0. } in
        let hit_material = hit_record.material in
        if scatter hit_material ray hit_record attenuation ray_scattered
        then Color.( * ) !attenuation (ray_color !ray_scattered (depth - 1) world)
        else Color.{ r = 0.; g = 0.; b = 0. })
      else (
        let unit_direction = Point3.unit_vector ray.direction in
        let a = 0.5 *. (unit_direction.g +. 1.) in
        Color.(
          scale { r = 1.; g = 1.; b = 1. } (1. -. a)
          + scale { r = 0.5; g = 0.7; b = 1. } a)))
  ;;

  let defocus_disk_sample camera =
    let point = Vec3.random_in_unit_disk () in
    Point3.(
      camera.center
      + scale camera.defocus_disk_u point.r
      + scale camera.defocus_disk_v point.g)
  ;;

  let get_ray camera i j =
    let r_offset = Random.float_range (-0.5) 0.5 in
    let g_offset = Random.float_range (-0.5) 0.5 in
    let pixel_sample =
      Point3.(
        camera.pixel00_loc
        + scale camera.pixel_delta_u (Float.of_int i +. r_offset)
        + scale camera.pixel_delta_v (Float.of_int j +. g_offset))
    in
    let origin =
      if Float.(camera.defocus_angle <= 0.)
      then camera.center
      else defocus_disk_sample camera
    in
    let direction = Point3.(pixel_sample - origin) in
    Ray.{ origin; direction }
  ;;

  let render camera world =
    let _image_header =
      Out_channel.printf
        "P3\n%i %i\n255\n"
        (Int.of_float camera.image_width)
        (Int.of_float camera.image_height)
    in
    for j = 0 to Int.of_float camera.image_height - 1 do
      for i = 0 to Int.of_float camera.image_width - 1 do
        let acc_pixel_color =
          List.fold
            (List.init camera.samples_per_pixel ~f:Fn.id)
            ~init:Vec3.{ r = 0.; g = 0.; b = 0. }
            ~f:(fun acc _ ->
              let ray = get_ray camera i j in
              Color.(acc + ray_color ray camera.depth world))
        in
        print_endline Color.(to_string (scale acc_pixel_color camera.pixel_samples_scale))
      done
    done
  ;;
end
