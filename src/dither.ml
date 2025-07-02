open Core

let adjust_pixel ~error image ~x ~y ~adjust =
  let width = Image.width image - 1 in
  let height = Image.height image - 1 in
  if x <= width && x >= 0 && y <= height
  then (
    let adjustment = adjust *. error /. 16. in
    Image.set
      image
      ~x
      ~y
      (Pixel.( + )
         (Image.get image ~x ~y)
         (Pixel.of_int (Float.to_int (Float.round adjustment)))))
;;

let distribute_to_adj ~x ~y image ~error =
  (* Add 7/16 error to pixel to the right *)
  adjust_pixel ~error image ~x:(x + 1) ~y ~adjust:7.;
  (* Add 3/16 error to bottom left *)
  adjust_pixel ~error image ~x:(x - 1) ~y:(y + 1) ~adjust:3.;
  (* Add 5/16 error to below *)
  adjust_pixel ~error image ~x ~y:(y + 1) ~adjust:5.;
  (* Add 1/16 error to bottom right *)
  adjust_pixel ~error image ~x:(x + 1) ~y:(y + 1) ~adjust:1.
;;

(* match pixel_val > max / 2 with
   | true ->
   let error = Int.to_float (pixel_val - max) in
   distribute_to_adj ~x ~y image ~error ~width ~height;
   Pixel.of_int max
   | false ->
   let error = Int.to_float pixel_val in
   distribute_to_adj ~x ~y image ~error ~width ~height;
   Pixel.zero *)

let dither image ~x ~y pixel : Pixel.t =
  let max = Image.max_val image in
  let pixel_val = Pixel.red pixel in
  let new_pixel_val = Int.round pixel_val ~to_multiple_of:max in
  let error = Int.to_float (pixel_val - new_pixel_val) in
  distribute_to_adj ~x ~y image ~error;
  Pixel.of_int new_pixel_val
;;

(* This should look familiar by now! *)
let transform image =
  let grey_image = Grayscale.transform image in
  Image.mapi grey_image ~f:(fun ~x ~y pixel -> dither grey_image ~x ~y pixel)
;;

let%expect_test "dither" =
  let my_image =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let dither_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
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
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
