open Base
open Vec3

module Color = struct
  include Vec3

  type t = Vec3.t [@@deriving sexp]

  let to_string (color : t) =
    [ color.r; color.g; color.b ]
    |> List.map ~f:(fun x -> x *. 255.999 |> Int.of_float |> Int.to_string)
    |> String.concat ~sep:" "
  ;;
end
