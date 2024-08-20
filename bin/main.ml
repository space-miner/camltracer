open Base
open Stdio
open Color

let () =
  let image_width = 256 in
  let image_height = 256 in
  let _image_header = Out_channel.printf "P3\n%i %i\n255\n" image_width image_height in
  for j = 0 to image_height - 1 do
    for i = 0 to image_width - 1 do
      let r = Float.of_int i /. Float.of_int (image_width - 1) in
      let g = Float.of_int j /. Float.of_int (image_height - 1) in
      let b = Float.of_int 0 in
      print_endline (Color.to_string { r; g; b })
    done
  done
;;
