open Base
open Vec3
open Interval

module Color = struct
  include Vec3

  type t = Vec3.t [@@deriving sexp]

  let to_string (color : t) =
    let interval = Interval.{ min = 0.000; max = 0.999 } in
    [ color.r; color.g; color.b ]
    |> List.map ~f:(fun x ->
      256. *. Interval.clamp interval x |> Int.of_float |> Int.to_string)
    |> String.concat ~sep:" "
  ;;
end
