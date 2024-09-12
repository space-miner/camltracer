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
open Camera

let () =
  (* image *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400. in
  (* camera *)
  let camera = Camera.make aspect_ratio image_width in
  (* world *)
  let world = ref [] in
  HittableList.add
    world
    (Hittable.Sphere Sphere.{ center = Point3.{ r = 0.; g = 0.; b = -1. }; radius = 0.5 });
  HittableList.add
    world
    (Hittable.Sphere { center = Point3.{ r = 0.; g = -100.5; b = -1. }; radius = 100. });
  (* render *)
  Camera.render camera world
;;
