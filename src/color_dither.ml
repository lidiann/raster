open Core

let adjust_pixel ~red_error ~green_error ~blue_error image ~x ~y ~adjust =
  let width = Image.width image - 1 in
  let height = Image.height image - 1 in
  if x <= width && x >= 0 && y <= height
  then (
    let red_adjustment = adjust *. red_error /. 16. in
    let green_adjustment = adjust *. green_error /. 16. in
    let blue_adjustment = adjust *. blue_error /. 16. in
    Image.set
      image
      ~x
      ~y
      (Pixel.( + )
         (Image.get image ~x ~y)
         ( Float.to_int (Float.round red_adjustment)
         , Float.to_int (Float.round green_adjustment)
         , Float.to_int (Float.round blue_adjustment) )))
;;

let distribute_to_adj ~x ~y image ~red_error ~green_error ~blue_error =
  (* Add 7/16 error to pixel to the right *)
  adjust_pixel
    ~red_error
    ~green_error
    ~blue_error
    image
    ~x:(x + 1)
    ~y
    ~adjust:7.;
  (* Add 3/16 error to bottom left *)
  adjust_pixel
    ~red_error
    ~green_error
    ~blue_error
    image
    ~x:(x + 1)
    ~y
    ~adjust:3.;
  (* Add 5/16 error to below *)
  adjust_pixel
    ~red_error
    ~green_error
    ~blue_error
    image
    ~x:(x + 1)
    ~y
    ~adjust:5.;
  (* Add 1/16 error to bottom right *)
  adjust_pixel
    ~red_error
    ~green_error
    ~blue_error
    image
    ~x:(x + 1)
    ~y
    ~adjust:1.
;;

let dither image ~x ~y pixel ~(channel : int) : Pixel.t =
  let max = Image.max_val image in
  let red = Pixel.red pixel in
  let green = Pixel.green pixel in
  let blue = Pixel.blue pixel in
  let new_red = Int.round red ~to_multiple_of:max / (channel - 1) in
  let new_green = Int.round green ~to_multiple_of:max / (channel - 1) in
  let new_blue = Int.round blue ~to_multiple_of:max / (channel - 1) in
  let red_error = Int.to_float (red - new_red) in
  let green_error = Int.to_float (green - new_green) in
  let blue_error = Int.to_float (blue - new_blue) in
  distribute_to_adj ~x ~y image ~red_error ~blue_error ~green_error;
  new_red, new_green, new_blue
;;

(* This should look familiar by now! *)
let transform image ~channel =
  Grayscale.transform image
  |> Image.mapi ~f:(fun ~x ~y pixel -> dither image ~x ~y pixel ~channel)
;;

let%expect_test "dither" =
  let my_image =
    transform
      (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
      ~channel:2
  in
  let dither_image =
    Image.load_ppm
      ~filename:"../images/reference-beach_portrait_dither_color.ppm"
  in
  let compare = Image.compare my_image dither_image in
  print_endline compare;
  [%expect {|Number pixels incorrect: 0 |}]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and channel =
        flag
          "channel"
          (required Command.Param.int)
          ~doc:"N the number of channels for dithering"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~channel in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_dither_color.ppm")]
;;
