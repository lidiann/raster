open Core

let match_threshold_against color ~rgb_threshold ~max =
  let float_color = Int.to_float color in
  match Float.( >= ) float_color rgb_threshold with
  | true -> Float.to_int (Float.round (max -. float_color))
  | false -> Float.to_int (Float.round float_color)
;;

let transform image ~(threshold : float) : _ =
  let max = Int.to_float (Image.max_val image) in
  let rgb_threshold = max *. threshold in
  Image.map image ~f:(fun (r, g, b) ->
    let new_red = match_threshold_against r ~rgb_threshold ~max in
    let new_green = match_threshold_against g ~rgb_threshold ~max in
    let new_blue = match_threshold_against b ~rgb_threshold ~max in
    new_red, new_green, new_blue)
;;

let%expect_test "solarize" =
  let my_image =
    transform
      (Image.load_ppm ~filename:"../images/meadow.ppm")
      ~threshold:0.40
  in
  let solarize_image =
    Image.load_ppm ~filename:"../images/reference-meadow_solarize.ppm"
  in
  let compare = Image.compare my_image solarize_image in
  print_endline compare;
  [%expect {|Number pixels incorrect: 0 |}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"N the threshold to use when inverting"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
