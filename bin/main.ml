open Base
open Stdio
open Color
open Point3
open Vec3
open Ray

let () =
  (* image *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400. in
  let image_height =
    Int.max 1 (Int.of_float (image_width /. aspect_ratio)) |> Float.of_int
  in
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
      let pixel_color = Ray.ray_color ray in
      print_endline (Color.to_string pixel_color)
    done
  done
;;
