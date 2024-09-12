open Base
open Stdio
open Hittable
open Hitrecord
open Interval
open Ray

module HittableList = struct
  type t = Hittable.t List.t ref

  let sexp_of_t (lst_ref : t) =
    let lst = !lst_ref in
    List.sexp_of_t Hittable.sexp_of_t lst
  ;;

  let t_of_sexp sexp =
    let lst = List.t_of_sexp Hittable.t_of_sexp sexp in
    ref lst
  ;;

  let add hittable_list obj = hittable_list := List.append !hittable_list [ obj ]

  let hit
    (hittable_list : t)
    (ray : Ray.t)
    (time_interval : Interval.t)
    (hit_record : HitRecord.t)
    =
    let temp_record =
      HitRecord.
        { point = Point3.{ r = 0.; g = 0.; b = 0. }
        ; normal = Vec3.{ r = 0.; g = 0.; b = 0. }
        ; time = 0.
        ; front_face = false
        }
    in
    let hit_anything = ref false in
    let closest_so_far = ref time_interval.max in
    List.iter
      ~f:(fun obj ->
        let time_interval = Interval.{ min = time_interval.min; max = !closest_so_far } in
        if Hittable.hit obj ray time_interval temp_record
        then (
          hit_anything := true;
          closest_so_far := Float.min !closest_so_far temp_record.time;
          hit_record.point <- temp_record.point;
          hit_record.normal <- temp_record.normal;
          hit_record.time <- temp_record.time;
          hit_record.front_face <- temp_record.front_face))
        (* hit_record := temp_record)) *)
      !hittable_list;
    (* if !hit_anything then print_s [%sexp (temp_record : HitRecord.t)]; *)
    !hit_anything
  ;;
end
