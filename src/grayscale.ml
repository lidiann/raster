open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let average = (r + g + b) / 3 in
    average, average, average)
;;

let%expect_test "grayscale" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let grey_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let my_grey_image = transform image in
  (* CR leli: Could you extract this into a separate helper function *)
  let compare = Image.compare my_grey_image grey_image in
  (* CR leli: Print some sample pixels that are incorrect *)
  (* Also want to print how many pixels are incorrect and/or which *)
  print_endline (Int.to_string compare);
  [%expect {|0|}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
