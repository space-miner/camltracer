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
open Material

let () =
  (* image *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 400. in
  let samples_per_pixel = 10 in
  let max_depth = 50 in
  let vertical_field_of_vision = 90. in
  (* camera *)
  let camera =
    Camera.make
      aspect_ratio
      image_width
      samples_per_pixel
      max_depth
      vertical_field_of_vision
  in
  (* material *)
  (* let ground = Material.Lambertian { albedo = { r = 0.8; g = 0.8; b = 0. } } in *)
  (* let center = Material.Lambertian { albedo = { r = 0.1; g = 0.2; b = 0.5 } } in *)
  (* let left = Material.Metal { albedo = { r = 0.8; g = 0.8; b = 0.8 }; fuzz = 0.3 } in *)
  (* let left = Material.Dielectric { refraction_index = 1.5 } in *)
  (* let bubble = Material.Dielectric { refraction_index = 1. /. 1.5 } in *)
  (* let right = Material.Metal { albedo = { r = 0.8; g = 0.6; b = 0.2 }; fuzz = 1. } in *)
  let k = Float.(cos (pi / 4.)) in
  let left = Material.Lambertian { albedo = { r = 0.; g = 0.; b = 1. } } in
  let right = Material.Lambertian { albedo = { r = 1.; g = 0.; b = 0. } } in
  (* world *)
  let world = ref [] in
  HittableList.add
    world
    (Hittable.Sphere
       { center = Point3.{ r = Float.neg k; g = 0.; b = -1. }
       ; radius = k
       ; material = left
       });
  HittableList.add
    world
    (Hittable.Sphere
       { center = Point3.{ r = k; g = 0.; b = -1. }; radius = k; material = right });
  (* HittableList.add *)
  (*   world *)
  (*   (Hittable.Sphere *)
  (*      { center = Point3.{ r = 0.; g = -100.5; b = -1. } *)
  (*      ; radius = 100. *)
  (*      ; material = ground *)
  (*      }); *)
  (* HittableList.add *)
  (*   world *)
  (*   (Hittable.Sphere *)
  (*      Sphere. *)
  (*        { center = Point3.{ r = 0.; g = 0.; b = -1.2 }; radius = 0.5; material = center }); *)
  (* HittableList.add *)
  (*   world *)
  (*   (Hittable.Sphere *)
  (*      Sphere. *)
  (*        { center = Point3.{ r = -1.; g = 0.; b = -1. }; radius = 0.5; material = left }); *)
  (* HittableList.add *)
  (*   world *)
  (*   (Hittable.Sphere *)
  (*      Sphere. *)
  (*        { center = Point3.{ r = -1.; g = 0.; b = -1. }; radius = 0.4; material = bubble }); *)
  (* HittableList.add *)
  (*   world *)
  (*   (Hittable.Sphere *)
  (*      Sphere. *)
  (*        { center = Point3.{ r = 1.; g = 0.; b = -1. }; radius = 0.5; material = right }); *)
  (* render *)
  Camera.render camera world
;;
