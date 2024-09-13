open Base
open Stdio
open Vec3
open Color
open Point3
open Ray
open Hitrecord
open Hittablelist
open Interval

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
    }
  [@@deriving sexp]

  let make aspect_ratio image_width samples_per_pixel depth =
    let image_height =
      Int.max 1 (Int.of_float (image_width /. aspect_ratio)) |> Float.of_int
    in
    let pixel_samples_scale = 1. /. Float.of_int samples_per_pixel in
    (* camera *)
    let focal_length = 1. in
    let viewport_height = 2. in
    let viewport_width = viewport_height *. (image_width /. image_height) in
    let camera_center = Vec3.{ r = 0.; g = 0.; b = 0. } in
    (* vectors across viewport edges *)
    let viewport_u = Vec3.{ r = viewport_width; g = 0.; b = 0. } in
    let viewport_v = Vec3.{ r = 0.; g = Float.neg viewport_height; b = 0. } in
    (* horizontal and vertical delta vectors *)
    let pixel_delta_u = Point3.scale viewport_u (1. /. image_width) in
    let pixel_delta_v = Point3.scale viewport_v (1. /. image_height) in
    (* upper left pixel *)
    let viewport_upper_left =
      Vec3.(
        camera_center
        - { r = 0.; g = 0.; b = focal_length }
        - scale viewport_u 0.5
        - scale viewport_v 0.5)
    in
    let pixel00_loc =
      Point3.(viewport_upper_left + scale (pixel_delta_u + pixel_delta_v) 0.5)
    in
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
    }
  ;;

  let rec ray_color (ray : Ray.t) depth (world : HittableList.t) : Color.t =
    if depth <= 0
    then Color.{ r = 0.; g = 0.; b = 0. }
    else (
      let hit_record =
        HitRecord.
          { point = Point3.{ r = 0.; g = 0.; b = 0. }
          ; normal = Vec3.{ r = 0.; g = 0.; b = 0. }
          ; time = 0.
          ; front_face = false
          }
      in
      let time_interval = Interval.{ min = 0.001; max = Float.infinity } in
      if HittableList.hit world ray time_interval hit_record
      then (
        let direction = Vec3.random_on_hemisphere hit_record.normal in
        let ray = Ray.{ origin = hit_record.point; direction } in
        Color.scale (ray_color ray (depth - 1) world) 0.5
      else (
        let unit_direction = Point3.unit_vector ray.direction in
        let a = 0.5 *. (unit_direction.g +. 1.) in
        Color.(
          scale { r = 1.; g = 1.; b = 1. } (1. -. a)
          + scale { r = 0.5; g = 0.7; b = 1. } a)))
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
    let origin = camera.center in
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
