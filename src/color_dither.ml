open Core

module Adjustment = struct
  type t =
    { error_adjustment : float
    ; x_coord : int
    ; y_coord : int
    }
end

module PixelError = struct
  type t =
    { red : int
    ; green : int
    ; blue : int
    }
end

let adjust_pixel ~red_error ~green_error ~blue_error image ~x ~y ~adjust =
  let width = Image.width image - 1 in
  let height = Image.height image - 1 in
  (* Make sure we are only adding to pixels that exist *)
  if x <= width && x >= 0 && y <= height
  then (
    (* CR leli: Keep the numerator/denominator *)
    let red_adjustment = adjust *. red_error in
    let green_adjustment = adjust *. green_error in
    let blue_adjustment = adjust *. blue_error in
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

let dither image ~x ~y pixel ~(channel : int) : Pixel.t =
  let max = Image.max_val image in
  (* Get original RGB values of the pixel *)
  let red = Pixel.red pixel in
  let green = Pixel.green pixel in
  let blue = Pixel.blue pixel in
  let colors = [ red; green; blue ] in
  (* Get new RGB values of the pixel by rounding to nearest channel *)
  let new_colors =
    List.map colors ~f:(fun color ->
      Int.round color ~to_multiple_of:max / (channel - 1))
  in
  (* Get errors for each color given the new and old rgb values *)
  let errors =
    List.map (List.zip_exn colors new_colors) ~f:(fun (color, new_color) ->
      Int.to_float (color - new_color))
  in
  (* Distributes the errors to adjacent pixels *)
  let pixels_to_update =
    [ x + 1, y, 7. /. 16.
    ; x - 1, y + 1, 3. /. 16.
    ; x, y + 1, 5. /. 16.
    ; x + 1, y + 1, 1. /. 16.
    ]
  in
  List.iter pixels_to_update ~f:(fun (x, y, adjust) ->
    adjust_pixel
      ~red_error:(List.nth_exn errors 0)
      ~green_error:(List.nth_exn errors 1)
      ~blue_error:(List.nth_exn errors 2)
      image
      ~x
      ~y
      ~adjust);
  (* Return new RGB values *)
  ( List.nth_exn new_colors 0
  , List.nth_exn new_colors 1
  , List.nth_exn new_colors 2 )
;;

(* This should look familiar by now! *)
let transform image ~channel =
  Image.mapi image ~f:(fun ~x ~y pixel -> dither image ~x ~y pixel ~channel)
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
