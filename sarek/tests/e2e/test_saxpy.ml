(******************************************************************************)
(* SPDX-License-Identifier: CECILL-B                                          *)
(* SPDX-FileCopyrightText: 2026 Mathias Bourgoin <mathias.bourgoin@gmail.com> *)
(******************************************************************************)

(******************************************************************************
 * E2E test for Sarek PPX - SAXPY 
 *
 * This test verifies that kernels compiled with the PPX can generate valid
 * GPU code and execute correctly via the GPU runtime.
 ******************************************************************************)

open Sarek
module Std = Sarek_stdlib.Std
module Device = Spoc_core.Device
module Vector = Spoc_core.Vector
module Transfer = Spoc_core.Transfer
module Benchmarks = Test_helpers.Benchmarks

(* Define kernel - vector addition c = a + b *)
let saxpy =
  [%kernel
    fun (a : float32 vector)
        (x : float32 vector)
        (b : float32 vector)
        (y : float32 vector)
        (n : int32) ->
      let open Std in
      let tid = global_thread_id in
      if tid < n then
        y.(tid) <- a.(0) *. x.(tid) +. b.(tid)
  ]

let compute_expected size =
  let a = 3.0 in
  let x = Array.init size (fun i -> float_of_int (size-i)) in
  let b = Array.init size (fun i -> float_of_int (2 * i)) in
  Array.init size (fun i ->
    a *. x.(i) +. b.(i))

let verify_results result expected =
  let size = Array.length expected in
  let errors = ref 0 in
  for i = 0 to size - 1 do
    let diff = abs_float (result.(i) -. expected.(i)) in
    if diff > 0.001 then begin
      if !errors < 5 then
        Printf.printf
          "  Mismatch at %d: expected %.2f, got %.2f\n"
          i
          expected.(i)
          result.(i) ;
      incr errors
    end
  done ;
  !errors = 0

let run_test dev size block_size =
  (* Standard runtime path for all devices *)
  let _, kirc = saxpy in
  let ir =
    match kirc.Sarek.Kirc_types.body_ir with
    | Some ir -> ir
    | None -> failwith "No IR"
  in

  let a = Vector.create Vector.float32 1 in
  let x = Vector.create Vector.float32 size in
  let b = Vector.create Vector.float32 size in
  let y = Vector.create Vector.float32 size in

  Vector.set a 0 (float_of_int 3);
  for i = 0 to size - 1 do
    Vector.set x i (float_of_int (size-i)) ;
    Vector.set b i (float_of_int (i * 2)) ;
    Vector.set y i (float_of_int (-999)) ;
  done ;

  let block_sz = block_size in
  let grid_sz = (size + block_sz - 1) / block_sz in
  let block = Execute.dims1d block_sz in
  let grid = Execute.dims1d grid_sz in

  (* Warmup *)
  Execute.run_vectors
    ~device:dev
    ~ir
    ~args:[Vec a; Vec x; Vec b; Vec y; Int size]
    ~block
    ~grid
    () ;
  Transfer.flush dev ;

  let t0 = Unix.gettimeofday () in
  Execute.run_vectors
    ~device:dev
    ~ir
    ~args:[Vec a; Vec x; Vec b; Vec y; Int size]
    ~block
    ~grid
    () ;
  Transfer.flush dev ;
  let t1 = Unix.gettimeofday () in

  let result = Vector.to_array y in
  ((t1 -. t0) *. 1000.0, result)

let () =
  Benchmarks.run
    ~baseline:compute_expected
    ~verify:verify_results
    "SAXPY Test"
    run_test ;
  Benchmarks.exit ()
