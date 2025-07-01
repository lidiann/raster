open Core

let distribute_to_adj ~x ~y image ~error ~width ~height =
  (* Add 7/16 error to pixel to the right *)
  if x + 1 <= width
  then (
    let adjustment = 7. *. error /. 16. in
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y)
         (Pixel.of_int (Float.to_int (Float.round adjustment)))));
  (* Add 3/16 error to bottom left *)
  if x - 1 >= 0 && y + 1 <= height
  then (
    let adjustment = 3. *. error /. 16. in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x - 1) ~y:(y + 1))
         (Pixel.of_int (Float.to_int (Float.round adjustment)))));
  (* Add 5/16 error to below *)
  if y + 1 <= height
  then (
    let adjustment = 5. *. error /. 16. in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x ~y:(y + 1))
         (Pixel.of_int (Float.to_int (Float.round adjustment)))));
  (* Add 1/16 error to bottom right *)
  if x + 1 <= width && y + 1 <= height
  then (
    let adjustment = error /. 16. in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y:(y + 1))
         (Pixel.of_int (Float.to_int (Float.round adjustment)))))
;;

let dither image ~x ~y pixel : Pixel.t =
  let max = Image.max_val image in
  let pixel_val = Pixel.red pixel in
  let width = Image.width image - 1 in
  let height = Image.height image - 1 in
  match pixel_val > max / 2 with
  | true ->
    let error = Int.to_float (pixel_val - max) in
    distribute_to_adj ~x ~y image ~error ~width ~height;
    Pixel.of_int max
  | false ->
    let error = Int.to_float pixel_val in
    distribute_to_adj ~x ~y image ~error ~width ~height;
    Pixel.zero
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
