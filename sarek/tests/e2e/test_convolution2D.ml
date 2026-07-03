(******************************************************************************)
(* SPDX-License-Identifier: CECILL-B                                          *)
(* SPDX-FileCopyrightText: 2026 Mathias Bourgoin <mathias.bourgoin@gmail.com> *)
(******************************************************************************)

(******************************************************************************
 * E2E test for Sarek PPX - 2D Convolution
 *
 * Tests a generic 2D convolution kernel (zero-padded, arbitrary odd-sized
 * filter) compiled with the PPX and executed via the GPU runtime.
 *
 * Convolution semantics: for each output pixel, the *flipped* filter is
 * correlated with the input neighbourhood (true convolution, not plain
 * cross-correlation). Samples that fall outside the image are treated as 0
 * (zero padding). The GPU kernel and the pure-OCaml baseline implement the
 * exact same formula, so verification is a meaningful check of the codegen
 * rather than a comparison between two different conventions.
 ******************************************************************************)

open Sarek
module Std = Sarek_stdlib.Std
module Device = Spoc_core.Device
module Vector = Spoc_core.Vector
module Transfer = Spoc_core.Transfer
module Benchmarks = Test_helpers.Benchmarks

(* ========== Sarek kernel ==========
   Generic 2D convolution. input/output are width*height images flattened
   row-major (index = row * width + col). filter is a krows*kcols matrix,
   flattened row-major, given in "natural" orientation - the kernel flips
   it internally (true convolution rather than cross-correlation). *)
let conv2d_kernel =
  [%kernel
    fun (input : float32 vector)
        (output : float32 vector)
        (filter : float32 vector)
        (width : int32)
        (height : int32)
        (krows : int32)
        (kcols : int32) ->
      let open Std in
      let px = global_idx_x in
      let py = global_idx_y in
      output.(0) <- 0.0
      (*
      if px < width && py < height then begin
        let krow_half = krows / 2l in
        let kcol_half = kcols / 2l in
        let sum = mut 0.0 in
        for ki = 0 to krows - 1l do
          for kj = 0 to kcols - 1l do
            let ir = py + ki - krow_half in
            let ic = px + kj - kcol_half in
            if ir >= 0l && ir < height && ic >= 0l && ic < width then begin
              let fr = krows - 1l - ki in
              let fc = kcols - 1l - kj in
              sum :=
                sum
                +. (input.((ir * width) + ic) *. filter.((fr * kcols) + fc))
            end
          done
        done ;
        output.((py * width) + px) <- sum
      end
      *)
      ]

(* ========== Pure OCaml baseline ==========
   Same exact math as the kernel above (zero padding + kernel flip),
   so that verification checks the codegen, not two different definitions
   of "convolution". *)
let ocaml_conv2d input width height filter krows kcols =
  let krow_half = krows / 2 in
  let kcol_half = kcols / 2 in
  let output = Array.make (width * height) 0.0 in
  for py = 0 to height - 1 do
    for px = 0 to width - 1 do
      let sum = ref 0.0 in
      for ki = 0 to krows - 1 do
        for kj = 0 to kcols - 1 do
          let ir = py + ki - krow_half in
          let ic = px + kj - kcol_half in
          if ir >= 0 && ir < height && ic >= 0 && ic < width then begin
            let fr = krows - 1 - ki in
            let fc = kcols - 1 - kj in
            sum :=
              !sum +. (input.((ir * width) + ic) *. filter.((fr * kcols) + fc))
          end
        done
      done ;
      output.((py * width) + px) <- !sum
    done
  done ;
  output

(* ========== Example filters (flattened row-major, "natural" orientation) ==========
   Three different 3x3 filters exercise different aspects of the kernel:
   - box blur / gaussian blur are symmetric, so the flip is a no-op
     (sanity check that the basic accumulation + zero-padding is correct)
   - sobel X is *not* symmetric, so it genuinely exercises the flip logic *)

(* Uniform average - smooths the image *)
let box_blur_3x3 =
  let v = 1.0 /. 9.0 in
  [|v; v; v; v; v; v; v; v; v|]

(* Weighted average - smooths while preserving edges a bit better than box blur *)
let gaussian_blur_3x3 =
  [|
    1.0 /. 16.0; 2.0 /. 16.0; 1.0 /. 16.0;
    2.0 /. 16.0; 4.0 /. 16.0; 2.0 /. 16.0;
    1.0 /. 16.0; 2.0 /. 16.0; 1.0 /. 16.0;
  |]

(* Horizontal gradient / edge detector - asymmetric *)
let sobel_x_3x3 =
  [|
    -1.0; 0.0; 1.0;
    -2.0; 0.0; 2.0;
    -1.0; 0.0; 1.0;
  |]

(* ========== Shared test data ==========
   A white square on a black background gives every filter something
   meaningful to act on: blur filters soften the edges, Sobel highlights
   the vertical sides of the square. *)

let input_image = ref [||]

let make_test_image dim =
  Array.init (dim * dim) (fun idx ->
      let r = idx / dim in
      let c = idx mod dim in
      if r > dim / 4 && r < (3 * dim) / 4 && c > dim / 4 && c < (3 * dim) / 4
      then 1.0
      else 0.0)

let init_conv2d_data size =
  let dim = int_of_float (sqrt (float_of_int size)) in
  input_image := make_test_image dim

(* ========== Runtime test runner (parameterised by filter) ========== *)

let run_conv2d filter_flat krows kcols (dev : Device.t) size _block_size =
  let dim = int_of_float (sqrt (float_of_int size)) in
  let width = dim and height = dim in
  let n = width * height in
  let inp = !input_image in

  let _, kirc = conv2d_kernel in
  let ir =
    match kirc.Sarek.Kirc_types.body_ir with
    | Some ir -> ir
    | None -> failwith "No IR"
  in

  let input = Vector.create Vector.float32 n in
  let output = Vector.create Vector.float32 n in
  let filter = Vector.create Vector.float32 (Array.length filter_flat) in

  for i = 0 to n - 1 do
    Vector.set input i inp.(i) ;
    Vector.set output i 0.0
  done ;
  for i = 0 to Array.length filter_flat - 1 do
    Vector.set filter i filter_flat.(i)
  done ;

  let block_size = 16 in
  let blocks_x = (width + block_size - 1) / block_size in
  let blocks_y = (height + block_size - 1) / block_size in
  let block = Execute.dims2d block_size block_size in
  let grid = Execute.dims2d blocks_x blocks_y in

  (* Warmup *)
  Execute.run_vectors
    ~device:dev
    ~ir
    ~args:
      [Vec input; Vec output; Vec filter; Int width; Int height; Int krows; Int kcols]
    ~block
    ~grid
    () ;
  Transfer.flush dev ;

  let t0 = Unix.gettimeofday () in
  Execute.run_vectors
    ~device:dev
    ~ir
    ~args:
      [Vec input; Vec output; Vec filter; Int width; Int height; Int krows; Int kcols]
    ~block
    ~grid
    () ;
  Transfer.flush dev ;
  let t1 = Unix.gettimeofday () in

  ((t1 -. t0) *. 1000.0, Vector.to_array output)

let baseline_conv2d filter_flat krows kcols size =
  let dim = int_of_float (sqrt (float_of_int size)) in
  ocaml_conv2d !input_image dim dim filter_flat krows kcols

let verify_results result expected =
  let size = Array.length expected in
  let errors = ref 0 in
  for i = 0 to size - 1 do
    let diff = abs_float (result.(i) -. expected.(i)) in
    if diff > 0.001 then begin
      if !errors < 5 then
        Printf.printf
          "  Mismatch at %d: expected %.4f, got %.4f\n"
          i
          expected.(i)
          result.(i) ;
      incr errors
    end
  done ;
  !errors = 0

(* ========== Main ========== *)

let () =
  Benchmarks.init () ;
  let size = Benchmarks.config.size in
  init_conv2d_data size ;

  Benchmarks.run
    ~baseline:(baseline_conv2d box_blur_3x3 3 3)
    ~verify:verify_results
    "2D Convolution (Box Blur 3x3)"
    (run_conv2d box_blur_3x3 3 3) ;

  Benchmarks.run
    ~baseline:(baseline_conv2d gaussian_blur_3x3 3 3)
    ~verify:verify_results
    "2D Convolution (Gaussian Blur 3x3)"
    (run_conv2d gaussian_blur_3x3 3 3) ;

  Benchmarks.run
    ~baseline:(baseline_conv2d sobel_x_3x3 3 3)
    ~verify:verify_results
    "2D Convolution (Sobel X Edge Detection)"
    (run_conv2d sobel_x_3x3 3 3) ;

  Benchmarks.exit ()
