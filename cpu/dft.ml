open Unix

let dft_par_k start_k end_k a b c d size =
  for k = start_k to end_k - 1 do
    let sum_a = ref 0.0 in
    let sum_b = ref 0.0 in
    for j = 0 to size - 1 do
      let angle = 2.0 *. Float.pi *. float_of_int (k * j) /. float_of_int size in
      sum_a := !sum_a +. a.(j) *. cos angle;
      sum_b := !sum_b -. b.(j) *. sin angle;
    done;
    c.(k) <- !sum_a;
    d.(k) <- !sum_b
  done

let dft_par a b c d size =
  let num_domains = Domain.recommended_domain_count () in
  let chunk_size = (size + num_domains - 1) / num_domains in
  let domains = ref [] in

  for t = 0 to num_domains - 1 do
    let start_k = min (t * chunk_size) size in
    let end_k = min ((t + 1) * chunk_size) size in

    let domain = Domain.spawn (fun () -> dft_par_k start_k end_k a b c d size) in
    domains := domain :: !domains;
  done;
  List.iter Domain.join !domains

let dft a b c d size =
  let const = 2.0 *. Float.pi /. float_of_int size in

  for t = 0 to size - 1 do
    let sum_a = ref 0.0 in
    let sum_b = ref 0.0 in
    for k = 0 to size - 1 do
      let angle = const *. float_of_int t *. float_of_int k in
      sum_a := !sum_a +. a.(k) *. cos(angle);
      sum_b := !sum_b -. b.(k) *. sin(angle);
    done;
    c.(t) <- !sum_a;
    d.(t) <- !sum_b
  done

let main () =
  let size = 5000 in
  let a1 = Array.init size (fun i -> float_of_int i) in
  let b1 = Array.init size (fun i -> float_of_int i) in
  let c1 = Array.make size 0.0 in
  let d1 = Array.make size 0.0 in

  let a2 = Array.init size (fun i -> float_of_int i) in
  let b2 = Array.init size (fun i -> float_of_int i) in
  let c2 = Array.make size 0.0 in
  let d2 = Array.make size 0.0 in

  let t1 = Unix.gettimeofday() in
  dft a1 b1 c1 d1 size;
  let t2 = Unix.gettimeofday () in
  dft_par a2 b2 c2 d2 size;
  let t3 = Unix.gettimeofday () in

  Printf.printf "classic: %f, parallel: %f\n" (t2 -. t1) (t3 -. t2);

  let errors = ref 0 in
  let epsilon = 0.001 in
  for i = 0 to size - 1 do
    let diff = abs_float (c2.(i) -. c1.(i)) in
    if diff > epsilon then begin
      if !errors < 5 then
        Printf.printf
          "  Mismatch at %d: expected %.2f, got %.2f\n"
          i
          c1.(i)
          c2.(i) ;
      incr errors
    end
  done;
  for i = 0 to size - 1 do
    let diff = abs_float (d2.(i) -. d1.(i)) in
    if diff > epsilon then begin
      if !errors < 5 then
        Printf.printf
          "  Mismatch at %d: expected %.2f, got %.2f\n"
          i
          d1.(i)
          d2.(i) ;
      incr errors
    end
  done

let _ = main ()
