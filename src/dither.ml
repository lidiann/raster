open Core

let dither image ~x ~y pixel =
  let max = Image.max_val image in
  let error = (max / 2) - Pixel.red pixel in
  let height = Image.height image - 1 in
  let width = Image.width image - 1 in
  (* Distrubute to adjacent pixels *)
  if (not (Int.equal x 0)) || not (Int.equal y 0)
  then (
    match Pixel.red pixel + (3 / 16 * error) < 0 with
    | true -> Image.set image ~x:(x - 1) ~y:(y - 1) Pixel.zero
    | false ->
      Image.set
        image
        ~x:(x - 1)
        ~y:(y - 1)
        (Pixel.( + ) pixel (Pixel.of_int (3 / 16 * error))));
  if not (Int.equal x width)
  then (
    match Pixel.red pixel + (7 / 16 * error) < 0 with
    | true -> Image.set image ~x:(x + 1) ~y Pixel.zero
    | false ->
      Image.set
        image
        ~x:(x + 1)
        ~y
        (Pixel.( + ) pixel (Pixel.of_int (7 / 16 * error))));
  if not (Int.equal y height)
  then (
    match Pixel.red pixel + (5 / 16 * error) < 0 with
    | true -> Image.set image ~x ~y:(y - 1) Pixel.zero
    | false ->
      Image.set
        image
        ~x
        ~y:(y - 1)
        (Pixel.( + ) pixel (Pixel.of_int (5 / 16 * error))));
  if (not (Int.equal x 0)) || not (Int.equal y height)
  then (
    match Pixel.red pixel + (1 / 16 * error) < 0 with
    | true -> Image.set image ~x:(x + 1) ~y:(y - 1) Pixel.zero
    | false ->
      Image.set
        image
        ~x:(x + 1)
        ~y:(y - 1)
        (Pixel.( + ) pixel (Pixel.of_int (1 / 16 * error))));
  match error > 0 with true -> Pixel.of_int max | false -> Pixel.zero
;;

(* This should look familiar by now! *)
let transform image =
  let image = Grayscale.transform image in
  Image.mapi image ~f:(fun ~x ~y pixel -> dither image ~x ~y pixel)
;;

let%expect_test "dither" =
  let my_image =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let dither_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let compare = Image.compare my_image dither_image in
  print_endline (Int.to_string compare);
  [%expect {|0|}]
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
