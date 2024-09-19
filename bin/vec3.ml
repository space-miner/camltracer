open Base

module Vec3 = struct
  type t =
    { r : Float.t
    ; g : Float.t
    ; b : Float.t
    }
  [@@deriving sexp]

  let neg { r; g; b } = Float.{ r = neg r; g = neg g; b = neg b }
  let ( - ) t1 t2 = { r = t1.r -. t2.r; g = t1.g -. t2.g; b = t1.b -. t2.b }
  let ( + ) t1 t2 = { r = t1.r +. t2.r; g = t1.g +. t2.g; b = t1.b +. t2.b }
  let ( * ) t1 t2 = { r = t1.r *. t2.r; g = t1.g *. t2.g; b = t1.b *. t2.b }
  let ( / ) t1 t2 = { r = t1.r /. t2.r; g = t1.g /. t2.g; b = t1.b /. t2.b }
  let scale { r; g; b } factor = { r = r *. factor; g = g *. factor; b = b *. factor }
  let length { r; g; b } = Float.(sqrt (square r + square g + square b))
  let sum_rgb { r; g; b } = Float.(r + g + b)
  let dot t1 t2 = sum_rgb (t1 * t2)

  let cross t1 t2 =
    { r = (t1.g *. t2.b) -. (t1.b *. t2.g)
    ; g = (t1.b *. t2.r) -. (t1.r *. t2.b)
    ; b = (t1.r *. t2.g) -. (t1.g *. t2.r)
    }
  ;;

  let unit_vector t = scale t (1. /. length t)

  let rec random_unit_vector () =
    let epsilon = 1e-160 in
    let rand_vec =
      { r = Random.float_range (-1.) 1.
      ; g = Random.float_range (-1.) 1.
      ; b = Random.float_range (-1.) 1.
      }
    in
    if Float.(dot rand_vec rand_vec < epsilon)
    then random_unit_vector ()
    else unit_vector rand_vec
  ;;

  let random_on_hemisphere normal =
    let vec = random_unit_vector () in
    if Float.(dot vec normal > 0.) then vec else neg vec
  ;;

  let near_zero { r; g; b } =
    let epsilon = 1e-8 in
    Float.(abs r < epsilon && abs g < epsilon && abs b < epsilon)
  ;;

  let reflect t1 t2 =
    (* projection t1 onto t2 *)
    let projection = dot t1 t2 in
    t1 - scale t2 (2. *. projection)
  ;;
end
