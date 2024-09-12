open Base
open Stdio
open Color
open Point3
open Vec3
open Ray
open Sphere
open Hitrecord
open Hittable
open Hittablelist
open Interval

let ray_color (ray : Ray.t) (world : HittableList.t) : Color.t =
  let hit_record =
    HitRecord.
      { point = Point3.{ r = 0.; g = 0.; b = 0. }
      ; normal = Vec3.{ r = 0.; g = 0.; b = 0. }
      ; time = 0.
      ; front_face = false
      }
  in
  let time_interval = Interval.{ min = 0.; max = Float.infinity } in
  if HittableList.hit world ray time_interval hit_record
  then (
    let HitRecord.{ normal = { r = nr; g = ng; b = nb }; _ } = hit_record in
    Color.scale Color.{ r = 1. +. nr; g = 1. +. ng; b = 1. +. nb } 0.5)
  else (
    let unit_direction = Point3.unit_vector ray.direction in
    let a = 0.5 *. (unit_direction.g +. 1.) in
    Color.(
      scale { r = 1.; g = 1.; b = 1. } (1. -. a) + scale { r = 0.5; g = 0.7; b = 1. } a))
;;

let () =
  (* image *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400. in
  let image_height =
    Int.max 1 (Int.of_float (image_width /. aspect_ratio)) |> Float.of_int
  in
  (* world *)
  let world = ref [] in
  HittableList.add
    world
    (Hittable.Sphere Sphere.{ center = Point3.{ r = 0.; g = 0.; b = -1. }; radius = 0.5 });
  HittableList.add
    world
    (Hittable.Sphere { center = Point3.{ r = 0.; g = -100.5; b = -1. }; radius = 100. });
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
  (* render *)
  let _image_header =
    Out_channel.printf
      "P3\n%i %i\n255\n"
      (Int.of_float image_width)
      (Int.of_float image_height)
  in
  for j = 0 to Int.of_float image_height - 1 do
    for i = 0 to Int.of_float image_width - 1 do
      let pixel_center =
        Point3.(
          pixel00_loc
          + scale pixel_delta_u (Float.of_int i)
          + scale pixel_delta_v (Float.of_int j))
      in
      let ray_direction = Point3.(pixel_center - camera_center) in
      let ray = Ray.{ origin = camera_center; direction = ray_direction } in
      (* let pixel_color = ray_color ray world in *)
      let pixel_color = ray_color ray world in
      print_endline (Color.to_string pixel_color)
    done
  done
;;
