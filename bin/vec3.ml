open Base

module Vec3 = struct
  type t =
    { r : float
    ; g : float
    ; b : float
    }
  [@@deriving sexp]

  let neg t = Float.{ r = neg t.r; g = neg t.g; b = neg t.b }
  let ( - ) t1 t2 = { r = t1.r -. t2.r; g = t1.g -. t2.g; b = t1.b -. t2.b }
  let ( + ) t1 t2 = { r = t1.r +. t2.r; g = t1.g +. t2.g; b = t1.b +. t2.b }
  let ( * ) t1 t2 = { r = t1.r *. t2.r; g = t1.g *. t2.g; b = t1.b *. t2.b }
  let ( / ) t1 t2 = { r = t1.r /. t2.r; g = t1.g /. t2.g; b = t1.b /. t2.b }
  let scale t factor = { r = t.r *. factor; g = t.g *. factor; b = t.b *. factor }

  let length t =
    [ t.r; t.g; t.b ]
    |> List.map ~f:Float.square
    |> List.fold ~init:0. ~f:( +. )
    |> Float.sqrt
  ;;

  let sum_rgb t = List.fold [ t.r; t.g; t.b ] ~init:0. ~f:( +. )
  let dot t1 t2 = sum_rgb (t1 * t2)

  let cross t1 t2 =
    { r = (t1.g *. t2.b) -. (t1.b *. t2.g)
    ; g = (t1.b *. t2.r) -. (t1.r *. t2.b)
    ; b = (t1.r *. t2.g) -. (t1.g *. t2.r)
    }
  ;;

  let unit_vector t = scale t (1. /. length t)
end
