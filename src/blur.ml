open Core

let get_radius_around_pixel image ~x ~y ~radius =
  Image.slice
    image
    ~x_start:(max 0 (x - radius))
    ~x_end:(min (Image.width image - 1) (x + radius))
    ~y_start:(max 0 (y - radius))
    ~y_end:(min (Image.height image - 1) (y + radius))
;;

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius : _ =
  let copy = Image.copy image in
  Image.mapi copy ~f:(fun ~x ~y _pixel ->
    Image.mean_pixel (get_radius_around_pixel image ~x ~y ~radius))
;;

let%expect_test "blur" =
  let my_image =
    transform
      (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
      ~radius:3
  in
  let bluescreen_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let compare = Image.compare my_image bluescreen_image in
  print_endline (Int.to_string compare);
  [%expect {|0|}]
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
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
