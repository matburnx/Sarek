(** Test file for the floatml library with assertions *)

open Floatml

(** Tolerance for floating-point comparisons *)
let epsilon = 1e-6

let epsilon_f16 = 1e-2 (* F16 has lower precision *)

(** Check if float is infinity *)
let is_inf x = x = infinity || x = neg_infinity

(** Check if float is NaN *)
let is_nan x = x <> x

(** Test counter *)
let tests_run = ref 0

let tests_passed = ref 0

(** Assert helper that tracks test results *)
let assert_true msg condition =
  incr tests_run;
  if condition then begin
    incr tests_passed;
    Printf.printf "  PASS: %s\n" msg
  end
  else begin
    Printf.printf "  FAIL: %s\n" msg;
    failwith (Printf.sprintf "Assertion failed: %s" msg)
  end

(** Assert float equality within tolerance *)
let assert_float_eq ?(eps = epsilon) msg expected actual =
  let diff = Float.abs (expected -. actual) in
  assert_true
    (Printf.sprintf "%s (expected %f, got %f)" msg expected actual)
    (diff < eps)

(** Assert int equality *)
let assert_int_eq msg expected actual =
  assert_true
    (Printf.sprintf "%s (expected %d, got %d)" msg expected actual)
    (expected = actual)

(** Assert int32 equality *)
let assert_int32_eq msg expected actual =
  assert_true
    (Printf.sprintf "%s (expected 0x%08lx, got 0x%08lx)" msg expected actual)
    (expected = actual)

(** Assert int64 equality *)
let assert_int64_eq msg expected actual =
  assert_true
    (Printf.sprintf "%s (expected 0x%016Lx, got 0x%016Lx)" msg expected actual)
    (expected = actual)

(** Assert Z.t equality *)
let assert_z_eq msg expected actual =
  assert_true
    (Printf.sprintf "%s (expected %s, got %s)" msg (Z.to_string expected)
       (Z.to_string actual))
    (Z.equal expected actual)

let test_f16 () =
  print_endline "\n=== Testing F16 (half-precision) ===";

  let a = F16.of_string "3.5" in
  let b = F16.of_string "1.5" in

  (* Test basic values *)
  assert_float_eq ~eps:epsilon_f16 "F16 parse 3.5" 3.5 (F16.to_float a);
  assert_float_eq ~eps:epsilon_f16 "F16 parse 1.5" 1.5 (F16.to_float b);

  (* Test arithmetic *)
  assert_float_eq ~eps:epsilon_f16 "F16 add" 5.0 (F16.to_float (F16.add a b));
  assert_float_eq ~eps:epsilon_f16 "F16 sub" 2.0 (F16.to_float (F16.sub a b));
  assert_float_eq ~eps:epsilon_f16 "F16 mul" 5.25 (F16.to_float (F16.mul a b));
  assert_float_eq ~eps:epsilon_f16 "F16 div" 2.333 (F16.to_float (F16.div a b));
  assert_float_eq ~eps:epsilon_f16 "F16 rem" 0.5 (F16.to_float (F16.rem a b));

  (* Test bit representation - 3.5 in F16 is 0x4300 *)
  assert_int_eq "F16 to_bits 3.5" 0x4300 (F16.to_bits a);
  assert_z_eq "F16 to_z 3.5" (Z.of_int 0x4300) (F16.to_z a);

  (* Test round-trip *)
  let bits = F16.to_bits a in
  let a' = F16.of_bits bits in
  assert_float_eq ~eps:epsilon_f16 "F16 round-trip" (F16.to_float a)
    (F16.to_float a');

  (* Test infix operators *)
  let open F16 in
  assert_float_eq ~eps:epsilon_f16 "F16 infix +" 5.0 (to_float (a + b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix -" 2.0 (to_float (a - b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix *" 5.25 (to_float (a * b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix /" 2.333 (to_float (a / b))

(** Test IEEE-754 compliance for F16 *)
let test_f16_ieee754 () =
  print_endline "\n=== Testing F16 IEEE-754 compliance ===";

  (* Test well-known bit patterns *)

  (* Zero: 0x0000 *)
  let zero = F16.of_bits 0x0000 in
  assert_float_eq "F16 +0 value" 0.0 (F16.to_float zero);
  assert_int_eq "F16 +0 bits" 0x0000 (F16.to_bits zero);

  (* Negative zero: 0x8000 *)
  let neg_zero = F16.of_bits 0x8000 in
  assert_float_eq "F16 -0 value" 0.0 (F16.to_float neg_zero);
  assert_int_eq "F16 -0 bits" 0x8000 (F16.to_bits neg_zero);

  (* One: 0x3C00 (exp=15, mant=0 -> 2^0 * 1.0 = 1.0) *)
  let one = F16.of_bits 0x3C00 in
  assert_float_eq "F16 1.0 value" 1.0 (F16.to_float one);
  assert_int_eq "F16 1.0 bits" 0x3C00 (F16.to_bits one);

  (* Negative one: 0xBC00 *)
  let neg_one = F16.of_bits 0xBC00 in
  assert_float_eq "F16 -1.0 value" (-1.0) (F16.to_float neg_one);
  assert_int_eq "F16 -1.0 bits" 0xBC00 (F16.to_bits neg_one);

  (* Two: 0x4000 (exp=16, mant=0 -> 2^1 * 1.0 = 2.0) *)
  let two = F16.of_bits 0x4000 in
  assert_float_eq "F16 2.0 value" 2.0 (F16.to_float two);
  assert_int_eq "F16 2.0 bits" 0x4000 (F16.to_bits two);

  (* Infinity: 0x7C00 *)
  let inf = F16.of_bits 0x7C00 in
  assert_true "F16 +inf is infinity" (is_inf (F16.to_float inf));
  assert_int_eq "F16 +inf bits" 0x7C00 (F16.to_bits inf);

  (* Negative infinity: 0xFC00 *)
  let neg_inf = F16.of_bits 0xFC00 in
  assert_true "F16 -inf is infinity" (is_inf (F16.to_float neg_inf));
  assert_int_eq "F16 -inf bits" 0xFC00 (F16.to_bits neg_inf);

  (* NaN: 0x7E00 (quiet NaN) *)
  let nan_val = F16.of_bits 0x7E00 in
  assert_true "F16 NaN is NaN" (is_nan (F16.to_float nan_val));

  (* Smallest positive normal: 0x0400 (exp=1, mant=0 -> 2^-14) *)
  let min_normal = F16.of_bits 0x0400 in
  let expected_min_normal = ldexp 1.0 (-14) in
  (* 2^-14 ≈ 6.103515625e-05 *)
  assert_float_eq ~eps:1e-10 "F16 min normal value" expected_min_normal
    (F16.to_float min_normal);
  assert_int_eq "F16 min normal bits" 0x0400 (F16.to_bits min_normal);

  (* Largest normal: 0x7BFF (exp=30, mant=0x3FF -> (2 - 2^-10) * 2^15 = 65504) *)
  let max_normal = F16.of_bits 0x7BFF in
  assert_float_eq ~eps:1.0 "F16 max normal value" 65504.0
    (F16.to_float max_normal);
  assert_int_eq "F16 max normal bits" 0x7BFF (F16.to_bits max_normal);

  (* Smallest positive denormal: 0x0001 (exp=0, mant=1 -> 2^-24) *)
  let min_denormal = F16.of_bits 0x0001 in
  let expected_min_denormal = ldexp 1.0 (-24) in
  (* 2^-24 ≈ 5.96e-08 *)
  assert_float_eq ~eps:1e-12 "F16 min denormal value" expected_min_denormal
    (F16.to_float min_denormal);
  assert_int_eq "F16 min denormal bits" 0x0001 (F16.to_bits min_denormal);

  (* Largest denormal: 0x03FF (exp=0, mant=0x3FF -> (2^-14) * (1 - 2^-10)) *)
  let max_denormal = F16.of_bits 0x03FF in
  let expected_max_denormal = ldexp 1.0 (-14) *. (1.0 -. ldexp 1.0 (-10)) in
  assert_float_eq ~eps:1e-10 "F16 max denormal value" expected_max_denormal
    (F16.to_float max_denormal);
  assert_int_eq "F16 max denormal bits" 0x03FF (F16.to_bits max_denormal);

  (* Test rounding: round to nearest, ties to even *)
  (* 1.0 + 2^-11 should round to 1.0 (round down, ties to even) *)
  (* 1.0 + 2^-11 + 2^-12 should round to 1.0 + 2^-10 (round up) *)

  (* Test that 1.5 is exact: 0x3E00 *)
  let one_half = F16.of_string "1.5" in
  assert_int_eq "F16 1.5 bits" 0x3E00 (F16.to_bits one_half);

  (* Test that 0.5 is exact: 0x3800 *)
  let half = F16.of_string "0.5" in
  assert_int_eq "F16 0.5 bits" 0x3800 (F16.to_bits half);

  (* Test overflow to infinity *)
  let big = F16.of_string "100000.0" in
  assert_true "F16 overflow to inf" (is_inf (F16.to_float big));

  (* Test underflow to zero *)
  let tiny = F16.of_string "1e-10" in
  assert_float_eq "F16 underflow to zero" 0.0 (F16.to_float tiny);

  (* Test arithmetic with special values *)
  let inf16 = F16.of_bits 0x7C00 in
  let zero16 = F16.of_bits 0x0000 in
  let one16 = F16.of_bits 0x3C00 in

  (* inf + 1 = inf *)
  assert_true "F16 inf + 1 = inf" (is_inf (F16.to_float (F16.add inf16 one16)));

  (* inf - inf = NaN *)
  assert_true "F16 inf - inf = NaN"
    (is_nan (F16.to_float (F16.sub inf16 inf16)));

  (* 0 * inf = NaN *)
  assert_true "F16 0 * inf = NaN" (is_nan (F16.to_float (F16.mul zero16 inf16)));

  (* 1 / 0 = inf *)
  assert_true "F16 1 / 0 = inf" (is_inf (F16.to_float (F16.div one16 zero16)));

  (* 0 / 0 = NaN *)
  assert_true "F16 0 / 0 = NaN" (is_nan (F16.to_float (F16.div zero16 zero16)))

let test_f32 () =
  print_endline "\n=== Testing F32 (single-precision) ===";

  let a = F32.of_string "3.5" in
  let b = F32.of_string "1.5" in
  let c = F32.of_string "0.0" in
  let f32_pi = F32.of_string (Float.to_string Float.pi) in

  (* Test basic values *)
  assert_float_eq "F32 parse 3.5" 3.5 (F32.to_float a);
  assert_float_eq "F32 parse 1.5" 1.5 (F32.to_float b);

  (* Test arithmetic *)
  assert_float_eq "F32 add" 5.0 (F32.to_float (F32.add a b));
  assert_float_eq "F32 sub" 2.0 (F32.to_float (F32.sub a b));
  assert_float_eq "F32 mul" 5.25 (F32.to_float (F32.mul a b));
  assert_float_eq "F32 div" 2.333333 (F32.to_float (F32.div a b));
  assert_float_eq "F32 rem" 0.5 (F32.to_float (F32.rem a b));

  assert_float_eq "F32 cos" 1.0 (F32.to_float (F32.cos c));
  assert_float_eq "F32 cos" 0.0 (F32.to_float (F32.cos (F32.div f32_pi (F32.of_string "2.0"))));
  assert_float_eq "F32 sin" 0.0 (F32.to_float (F32.sin c));
  assert_float_eq "F32 sin" 1.0 (F32.to_float (F32.sin (F32.div f32_pi (F32.of_string "2.0"))));

  (* Test bit representation - 3.5 in F32 is 0x40600000 *)
  assert_int32_eq "F32 to_bits 3.5" 0x40600000l (F32.to_bits a);
  assert_z_eq "F32 to_z 3.5" (Z.of_int32 0x40600000l) (F32.to_z a);

  (* Test round-trip *)
  let bits = F32.to_bits a in
  let a' = F32.of_bits bits in
  assert_float_eq "F32 round-trip" (F32.to_float a) (F32.to_float a');

  (* Test infix operators *)
  let open F32 in
  assert_float_eq "F32 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F32 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F32 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F32 infix /" 2.333333 (to_float (a / b))

let test_f64 () =
  print_endline "\n=== Testing F64 (double-precision) ===";

  let a = F64.of_string "3.5" in
  let b = F64.of_string "1.5" in

  (* Test basic values *)
  assert_float_eq "F64 parse 3.5" 3.5 (F64.to_float a);
  assert_float_eq "F64 parse 1.5" 1.5 (F64.to_float b);

  (* Test arithmetic *)
  assert_float_eq "F64 add" 5.0 (F64.to_float (F64.add a b));
  assert_float_eq "F64 sub" 2.0 (F64.to_float (F64.sub a b));
  assert_float_eq "F64 mul" 5.25 (F64.to_float (F64.mul a b));
  assert_float_eq "F64 div" 2.333333333333333 (F64.to_float (F64.div a b));
  assert_float_eq "F64 rem" 0.5 (F64.to_float (F64.rem a b));

  (* Test bit representation - 3.5 in F64 is 0x400C000000000000 *)
  assert_int64_eq "F64 to_bits 3.5" 0x400C000000000000L (F64.to_bits a);
  assert_z_eq "F64 to_z 3.5" (Z.of_int64 0x400C000000000000L) (F64.to_z a);

  (* Test round-trip *)
  let bits = F64.to_bits a in
  let a' = F64.of_bits bits in
  assert_float_eq "F64 round-trip" (F64.to_float a) (F64.to_float a');

  (* Test infix operators *)
  let open F64 in
  assert_float_eq "F64 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F64 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F64 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F64 infix /" 2.333333333333333 (to_float (a / b))

let test_f128 () =
  print_endline "\n=== Testing F128 (quad-precision) ===";

  let a = F128.of_string "3.5" in
  let b = F128.of_string "1.5" in

  (* Test basic values - note: may lose precision when converting to float *)
  assert_float_eq "F128 parse 3.5" 3.5 (F128.to_float a);
  assert_float_eq "F128 parse 1.5" 1.5 (F128.to_float b);

  (* Test arithmetic *)
  assert_float_eq "F128 add" 5.0 (F128.to_float (F128.add a b));
  assert_float_eq "F128 sub" 2.0 (F128.to_float (F128.sub a b));
  assert_float_eq "F128 mul" 5.25 (F128.to_float (F128.mul a b));
  assert_float_eq "F128 div" 2.333333333333333 (F128.to_float (F128.div a b));
  assert_float_eq "F128 rem" 0.5 (F128.to_float (F128.rem a b));

  (* Test round-trip *)
  let low, high = F128.to_bits a in
  let a' = F128.of_bits (low, high) in
  assert_float_eq "F128 round-trip" (F128.to_float a) (F128.to_float a');

  (* Test to_z produces non-zero value *)
  let z = F128.to_z a in
  assert_true "F128 to_z is non-zero" (not (Z.equal z Z.zero));

  (* Test infix operators *)
  let open F128 in
  assert_float_eq "F128 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F128 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F128 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F128 infix /" 2.333333333333333 (to_float (a / b))

let test_special_values () =
  print_endline "\n=== Testing special values ===";

  (* Test zero *)
  let zero32 = F32.of_string "0.0" in
  assert_float_eq "F32 zero" 0.0 (F32.to_float zero32);
  assert_int32_eq "F32 zero bits" 0x00000000l (F32.to_bits zero32);

  (* Test negative zero *)
  let neg_zero32 = F32.of_string "-0.0" in
  assert_float_eq "F32 -0.0 value" 0.0 (F32.to_float neg_zero32);
  assert_int32_eq "F32 -0.0 bits" 0x80000000l (F32.to_bits neg_zero32);

  (* Test one *)
  let one32 = F32.of_string "1.0" in
  assert_float_eq "F32 one" 1.0 (F32.to_float one32);
  assert_int32_eq "F32 one bits" 0x3F800000l (F32.to_bits one32);

  (* Test infinity *)
  let inf32 = F32.of_string "inf" in
  assert_true "F32 infinity is infinity" (is_inf (F32.to_float inf32));
  assert_int32_eq "F32 infinity bits" 0x7F800000l (F32.to_bits inf32);

  (* Test negative infinity *)
  let neg_inf32 = F32.of_string "-inf" in
  assert_true "F32 -infinity is infinity" (is_inf (F32.to_float neg_inf32));
  assert_int32_eq "F32 -infinity bits" 0xFF800000l (F32.to_bits neg_inf32);

  (* Test NaN *)
  let nan32 = F32.of_string "nan" in
  assert_true "F32 nan is nan" (is_nan (F32.to_float nan32))

let test_z_conversions () =
  print_endline "\n=== Testing Z.t conversions ===";

  (* Test that Z.t values are correct bit representations *)
  let f32 = F32.of_string "1.0" in
  let z32 = F32.to_z f32 in
  assert_z_eq "F32 1.0 to Z" (Z.of_string "1065353216") z32;

  (* 0x3F800000 *)
  let f64 = F64.of_string "1.0" in
  let z64 = F64.to_z f64 in
  assert_z_eq "F64 1.0 to Z" (Z.of_string "4607182418800017408") z64;

  (* 0x3FF0000000000000 *)

  (* Test F16 *)
  let f16 = F16.of_string "1.0" in
  let z16 = F16.to_z f16 in
  assert_z_eq "F16 1.0 to Z" (Z.of_int 0x3C00) z16

(** Assert fpclass equality *)
let assert_fpclass_eq msg expected actual =
  let fpclass_to_string = function
    | FP_normal -> "Normal"
    | FP_subnormal -> "Subnormal"
    | FP_zero -> "Zero"
    | FP_infinite -> "Infinite"
    | FP_nan -> "NaN"
  in
  assert_true
    (Printf.sprintf "%s (expected %s, got %s)" msg
       (fpclass_to_string expected)
       (fpclass_to_string actual))
    (expected = actual)

let test_fpclass () =
  print_endline "\n=== Testing fpclass (IEEE 754 classification) ===";

  (* Test F16 fpclass *)
  print_endline "  F16 fpclass:";
  assert_fpclass_eq "F16 fpclass 1.0" FP_normal
    (F16.fpclass (F16.of_string "1.0"));
  assert_fpclass_eq "F16 fpclass 0.0" FP_zero (F16.fpclass (F16.of_bits 0x0000));
  assert_fpclass_eq "F16 fpclass -0.0" FP_zero
    (F16.fpclass (F16.of_bits 0x8000));
  assert_fpclass_eq "F16 fpclass inf" FP_infinite
    (F16.fpclass (F16.of_bits 0x7C00));
  assert_fpclass_eq "F16 fpclass -inf" FP_infinite
    (F16.fpclass (F16.of_bits 0xFC00));
  assert_fpclass_eq "F16 fpclass NaN" FP_nan (F16.fpclass (F16.of_bits 0x7E00));
  assert_fpclass_eq "F16 fpclass denormal" FP_subnormal
    (F16.fpclass (F16.of_bits 0x0001));

  (* Test F32 fpclass *)
  print_endline "  F32 fpclass:";
  assert_fpclass_eq "F32 fpclass 1.0" FP_normal
    (F32.fpclass (F32.of_string "1.0"));
  assert_fpclass_eq "F32 fpclass 0.0" FP_zero
    (F32.fpclass (F32.of_bits 0x00000000l));
  assert_fpclass_eq "F32 fpclass -0.0" FP_zero
    (F32.fpclass (F32.of_bits 0x80000000l));
  assert_fpclass_eq "F32 fpclass inf" FP_infinite
    (F32.fpclass (F32.of_bits 0x7F800000l));
  assert_fpclass_eq "F32 fpclass -inf" FP_infinite
    (F32.fpclass (F32.of_bits 0xFF800000l));
  assert_fpclass_eq "F32 fpclass NaN" FP_nan (F32.fpclass (F32.of_string "nan"));
  assert_fpclass_eq "F32 fpclass denormal" FP_subnormal
    (F32.fpclass (F32.of_bits 0x00000001l));

  (* Test F64 fpclass *)
  print_endline "  F64 fpclass:";
  assert_fpclass_eq "F64 fpclass 1.0" FP_normal
    (F64.fpclass (F64.of_string "1.0"));
  assert_fpclass_eq "F64 fpclass 0.0" FP_zero
    (F64.fpclass (F64.of_bits 0x0000000000000000L));
  assert_fpclass_eq "F64 fpclass inf" FP_infinite
    (F64.fpclass (F64.of_bits 0x7FF0000000000000L));
  assert_fpclass_eq "F64 fpclass NaN" FP_nan (F64.fpclass (F64.of_string "nan"));
  assert_fpclass_eq "F64 fpclass denormal" FP_subnormal
    (F64.fpclass (F64.of_bits 0x0000000000000001L));

  (* Test F128 fpclass *)
  print_endline "  F128 fpclass:";
  assert_fpclass_eq "F128 fpclass 1.0" FP_normal
    (F128.fpclass (F128.of_string "1.0"));
  assert_fpclass_eq "F128 fpclass 0.0" FP_zero
    (F128.fpclass (F128.of_string "0.0"));
  assert_fpclass_eq "F128 fpclass inf" FP_infinite
    (F128.fpclass (F128.of_string "inf"));
  assert_fpclass_eq "F128 fpclass NaN" FP_nan
    (F128.fpclass (F128.of_string "nan"));
  assert_fpclass_eq "F128 fpclass denormal" FP_subnormal
    (F128.fpclass (F128.of_bits (0x0000000000000001L, 0x0000000000000000L)))

let test_abs () =
  print_endline "\n=== Testing abs (absolute value) ===";

  (* Test F16 abs *)
  print_endline "  F16 abs:";
  assert_float_eq ~eps:epsilon_f16 "F16 abs 3.5" 3.5
    (F16.to_float (F16.abs (F16.of_string "3.5")));
  assert_float_eq ~eps:epsilon_f16 "F16 abs -3.5" 3.5
    (F16.to_float (F16.abs (F16.of_string "-3.5")));
  assert_float_eq "F16 abs 0.0" 0.0
    (F16.to_float (F16.abs (F16.of_bits 0x0000)));
  assert_int_eq "F16 abs -0.0 bits" 0x0000
    (F16.to_bits (F16.abs (F16.of_bits 0x8000)));
  assert_true "F16 abs inf"
    (is_inf (F16.to_float (F16.abs (F16.of_bits 0x7C00))));
  assert_true "F16 abs -inf"
    (is_inf (F16.to_float (F16.abs (F16.of_bits 0xFC00))));
  assert_int_eq "F16 abs -inf -> +inf" 0x7C00
    (F16.to_bits (F16.abs (F16.of_bits 0xFC00)));

  (* Test F32 abs *)
  print_endline "  F32 abs:";
  assert_float_eq "F32 abs 3.5" 3.5
    (F32.to_float (F32.abs (F32.of_string "3.5")));
  assert_float_eq "F32 abs -3.5" 3.5
    (F32.to_float (F32.abs (F32.of_string "-3.5")));
  assert_int32_eq "F32 abs -0.0 bits" 0x00000000l
    (F32.to_bits (F32.abs (F32.of_bits 0x80000000l)));

  (* Test F64 abs *)
  print_endline "  F64 abs:";
  assert_float_eq "F64 abs 3.5" 3.5
    (F64.to_float (F64.abs (F64.of_string "3.5")));
  assert_float_eq "F64 abs -3.5" 3.5
    (F64.to_float (F64.abs (F64.of_string "-3.5")));

  (* Test F128 abs *)
  print_endline "  F128 abs:";
  assert_float_eq "F128 abs 3.5" 3.5
    (F128.to_float (F128.abs (F128.of_string "3.5")));
  assert_float_eq "F128 abs -3.5" 3.5
    (F128.to_float (F128.abs (F128.of_string "-3.5")))

let test_round () =
  print_endline "\n=== Testing round (rounding modes) ===";

  (* Test F16 round *)
  print_endline "  F16 round:";
  let f16_2_5 = F16.of_string "2.5" in
  let f16_3_5 = F16.of_string "3.5" in
  let f16_neg_2_5 = F16.of_string "-2.5" in

  (* NearestTiesToEven: 2.5 -> 2, 3.5 -> 4 (ties to even) *)
  assert_float_eq ~eps:epsilon_f16 "F16 round NearestTiesToEven 2.5" 2.0
    (F16.to_float (F16.round NearestTiesToEven f16_2_5));
  assert_float_eq ~eps:epsilon_f16 "F16 round NearestTiesToEven 3.5" 4.0
    (F16.to_float (F16.round NearestTiesToEven f16_3_5));

  (* Truncate (truncate) *)
  assert_float_eq ~eps:epsilon_f16 "F16 round Truncate 2.5" 2.0
    (F16.to_float (F16.round Truncate f16_2_5));
  assert_float_eq ~eps:epsilon_f16 "F16 round Truncate -2.5" (-2.0)
    (F16.to_float (F16.round Truncate f16_neg_2_5));

  (* Ceil (ceiling) *)
  assert_float_eq ~eps:epsilon_f16 "F16 round Ceil 2.5" 3.0
    (F16.to_float (F16.round Ceil f16_2_5));
  assert_float_eq ~eps:epsilon_f16 "F16 round Ceil -2.5" (-2.0)
    (F16.to_float (F16.round Ceil f16_neg_2_5));

  (* Floor (floor) *)
  assert_float_eq ~eps:epsilon_f16 "F16 round Floor 2.5" 2.0
    (F16.to_float (F16.round Floor f16_2_5));
  assert_float_eq ~eps:epsilon_f16 "F16 round Floor -2.5" (-3.0)
    (F16.to_float (F16.round Floor f16_neg_2_5));

  (* NearestTiesToAway: ties away from zero *)
  assert_float_eq ~eps:epsilon_f16 "F16 round NearestTiesToAway 2.5" 3.0
    (F16.to_float (F16.round NearestTiesToAway f16_2_5));
  assert_float_eq ~eps:epsilon_f16 "F16 round NearestTiesToAway -2.5" (-3.0)
    (F16.to_float (F16.round NearestTiesToAway f16_neg_2_5));

  (* Test F32 round *)
  print_endline "  F32 round:";
  let f32_2_5 = F32.of_string "2.5" in
  assert_float_eq "F32 round Truncate 2.5" 2.0
    (F32.to_float (F32.round Truncate f32_2_5));
  assert_float_eq "F32 round Ceil 2.5" 3.0
    (F32.to_float (F32.round Ceil f32_2_5));
  assert_float_eq "F32 round Floor 2.5" 2.0
    (F32.to_float (F32.round Floor f32_2_5));

  (* Test F64 round *)
  print_endline "  F64 round:";
  let f64_2_5 = F64.of_string "2.5" in
  assert_float_eq "F64 round Truncate 2.5" 2.0
    (F64.to_float (F64.round Truncate f64_2_5));
  assert_float_eq "F64 round Ceil 2.5" 3.0
    (F64.to_float (F64.round Ceil f64_2_5));
  assert_float_eq "F64 round Floor 2.5" 2.0
    (F64.to_float (F64.round Floor f64_2_5));

  (* Test F128 round *)
  print_endline "  F128 round:";
  let f128_2_5 = F128.of_string "2.5" in
  assert_float_eq "F128 round Truncate 2.5" 2.0
    (F128.to_float (F128.round Truncate f128_2_5));
  assert_float_eq "F128 round Ceil 2.5" 3.0
    (F128.to_float (F128.round Ceil f128_2_5));
  assert_float_eq "F128 round Floor 2.5" 2.0
    (F128.to_float (F128.round Floor f128_2_5));

  (* Test that special values are preserved *)
  print_endline "  Special values:";
  let f32_inf = F32.of_bits 0x7F800000l in
  let f32_nan = F32.of_string "nan" in
  assert_true "F32 round preserves inf"
    (is_inf (F32.to_float (F32.round NearestTiesToEven f32_inf)));
  assert_true "F32 round preserves NaN"
    (is_nan (F32.to_float (F32.round NearestTiesToEven f32_nan)))

let test_comparison () =
  print_endline "\n=== Testing comparison operators (eq, lt, le) ===";

  (* Test F16 comparisons *)
  print_endline "  F16 comparisons:";
  let f16_1 = F16.of_string "1.0" in
  let f16_2 = F16.of_string "2.0" in
  let f16_1b = F16.of_string "1.0" in
  let f16_nan = F16.of_bits 0x7E00 in
  let f16_zero = F16.of_bits 0x0000 in
  let f16_neg_zero = F16.of_bits 0x8000 in

  (* Equality tests *)
  assert_true "F16 1.0 = 1.0" (F16.eq f16_1 f16_1b);
  assert_true "F16 1.0 <> 2.0" (not (F16.eq f16_1 f16_2));
  assert_true "F16 NaN <> NaN" (not (F16.eq f16_nan f16_nan));
  assert_true "F16 +0 = -0" (F16.eq f16_zero f16_neg_zero);

  (* Less than tests *)
  assert_true "F16 1.0 < 2.0" (F16.lt f16_1 f16_2);
  assert_true "F16 not (2.0 < 1.0)" (not (F16.lt f16_2 f16_1));
  assert_true "F16 not (1.0 < 1.0)" (not (F16.lt f16_1 f16_1b));
  assert_true "F16 not (NaN < 1.0)" (not (F16.lt f16_nan f16_1));
  assert_true "F16 not (1.0 < NaN)" (not (F16.lt f16_1 f16_nan));

  (* Less than or equal tests *)
  assert_true "F16 1.0 <= 2.0" (F16.le f16_1 f16_2);
  assert_true "F16 1.0 <= 1.0" (F16.le f16_1 f16_1b);
  assert_true "F16 not (2.0 <= 1.0)" (not (F16.le f16_2 f16_1));
  assert_true "F16 not (NaN <= 1.0)" (not (F16.le f16_nan f16_1));

  (* Infix operator tests *)
  let open F16 in
  assert_true "F16 infix 1.0 = 1.0" (f16_1 = f16_1b);
  assert_true "F16 infix 1.0 < 2.0" (f16_1 < f16_2);
  assert_true "F16 infix 1.0 <= 1.0" (f16_1 <= f16_1b);

  (* Test F32 comparisons *)
  print_endline "  F32 comparisons:";
  let f32_1 = F32.of_string "1.0" in
  let f32_2 = F32.of_string "2.0" in
  let f32_nan = F32.of_string "nan" in

  assert_true "F32 1.0 = 1.0" (F32.eq f32_1 f32_1);
  assert_true "F32 1.0 < 2.0" (F32.lt f32_1 f32_2);
  assert_true "F32 1.0 <= 2.0" (F32.le f32_1 f32_2);
  assert_true "F32 NaN <> NaN" (not (F32.eq f32_nan f32_nan));
  assert_true "F32 not (NaN < NaN)" (not (F32.lt f32_nan f32_nan));

  (* Test F64 comparisons *)
  print_endline "  F64 comparisons:";
  let f64_1 = F64.of_string "1.0" in
  let f64_2 = F64.of_string "2.0" in
  let f64_nan = F64.of_string "nan" in

  assert_true "F64 1.0 = 1.0" (F64.eq f64_1 f64_1);
  assert_true "F64 1.0 < 2.0" (F64.lt f64_1 f64_2);
  assert_true "F64 1.0 <= 2.0" (F64.le f64_1 f64_2);
  assert_true "F64 NaN <> NaN" (not (F64.eq f64_nan f64_nan));

  (* Test F128 comparisons *)
  print_endline "  F128 comparisons:";
  let f128_1 = F128.of_string "1.0" in
  let f128_2 = F128.of_string "2.0" in
  let f128_nan = F128.of_string "nan" in

  assert_true "F128 1.0 = 1.0" (F128.eq f128_1 f128_1);
  assert_true "F128 1.0 < 2.0" (F128.lt f128_1 f128_2);
  assert_true "F128 1.0 <= 2.0" (F128.le f128_1 f128_2);
  assert_true "F128 NaN <> NaN" (not (F128.eq f128_nan f128_nan));

  (* Test infinity comparisons *)
  print_endline "  Infinity comparisons:";
  let f32_inf = F32.of_bits 0x7F800000l in
  let f32_neg_inf = F32.of_bits 0xFF800000l in
  assert_true "F32 1.0 < inf" (F32.lt f32_1 f32_inf);
  assert_true "F32 -inf < 1.0" (F32.lt f32_neg_inf f32_1);
  assert_true "F32 inf = inf" (F32.eq f32_inf f32_inf);
  assert_true "F32 -inf < inf" (F32.lt f32_neg_inf f32_inf)

let test_float2int () =
  print_endline "\n=== Testing float2int (float to integer conversion) ===";

  (* Test F32 float2int - basic conversions *)
  print_endline "  F32 float2int:";

  (* Simple positive integer *)
  let f32_42 = F32.of_string "42.0" in
  let result = F32.float2int f32_42 Int8 Truncate ~signed:true in
  assert_true "F32 42.0 -> i8 signed" (result = Some (Z.of_int 42));

  let result = F32.float2int f32_42 Int8 Truncate ~signed:false in
  assert_true "F32 42.0 -> u8 unsigned" (result = Some (Z.of_int 42));

  (* Negative integer (signed) *)
  let f32_neg42 = F32.of_string "-42.0" in
  let result = F32.float2int f32_neg42 Int8 Truncate ~signed:true in
  assert_true "F32 -42.0 -> i8 signed" (result = Some (Z.of_int (-42)));

  (* Negative integer (unsigned) - should fail *)
  let result = F32.float2int f32_neg42 Int8 Truncate ~signed:false in
  assert_true "F32 -42.0 -> u8 unsigned fails" (result = None);

  (* Rounding tests *)
  let f32_2_7 = F32.of_string "2.7" in
  let result = F32.float2int f32_2_7 Int8 Truncate ~signed:true in
  assert_true "F32 2.7 Truncate -> 2" (result = Some (Z.of_int 2));

  let result = F32.float2int f32_2_7 Int8 Ceil ~signed:true in
  assert_true "F32 2.7 Ceil -> 3" (result = Some (Z.of_int 3));

  let result = F32.float2int f32_2_7 Int8 Floor ~signed:true in
  assert_true "F32 2.7 Floor -> 2" (result = Some (Z.of_int 2));

  let f32_neg2_7 = F32.of_string "-2.7" in
  let result = F32.float2int f32_neg2_7 Int8 Truncate ~signed:true in
  assert_true "F32 -2.7 Truncate -> -2" (result = Some (Z.of_int (-2)));

  let result = F32.float2int f32_neg2_7 Int8 Floor ~signed:true in
  assert_true "F32 -2.7 Floor -> -3" (result = Some (Z.of_int (-3)));

  (* Overflow tests *)
  let f32_200 = F32.of_string "200.0" in
  let result = F32.float2int f32_200 Int8 Truncate ~signed:true in
  assert_true "F32 200.0 -> i8 signed overflow" (result = None);

  let result = F32.float2int f32_200 Int8 Truncate ~signed:false in
  assert_true "F32 200.0 -> u8 unsigned ok" (result = Some (Z.of_int 200));

  let f32_300 = F32.of_string "300.0" in
  let result = F32.float2int f32_300 Int8 Truncate ~signed:false in
  assert_true "F32 300.0 -> u8 unsigned overflow" (result = None);

  (* NaN and Inf *)
  let f32_nan = F32.of_string "nan" in
  let f32_inf = F32.of_bits 0x7F800000l in
  let result = F32.float2int f32_nan Int32 Truncate ~signed:true in
  assert_true "F32 NaN -> int fails" (result = None);

  let result = F32.float2int f32_inf Int32 Truncate ~signed:true in
  assert_true "F32 Inf -> int fails" (result = None);

  (* Larger integer sizes *)
  let f32_big = F32.of_string "100000.0" in
  let result = F32.float2int f32_big Int32 Truncate ~signed:true in
  assert_true "F32 100000.0 -> i32" (result = Some (Z.of_int 100000));

  (* Test F16 float2int *)
  print_endline "  F16 float2int:";
  let f16_10 = F16.of_string "10.0" in
  let result = F16.float2int f16_10 Int8 Truncate ~signed:true in
  assert_true "F16 10.0 -> i8" (result = Some (Z.of_int 10));

  (* Test F64 float2int *)
  print_endline "  F64 float2int:";
  let f64_1000000 = F64.of_string "1000000.0" in
  let result = F64.float2int f64_1000000 Int32 Truncate ~signed:true in
  assert_true "F64 1000000.0 -> i32" (result = Some (Z.of_int 1000000));

  (* Test F128 float2int *)
  print_endline "  F128 float2int:";
  let f128_999 = F128.of_string "999.0" in
  let result = F128.float2int f128_999 Int16 Truncate ~signed:true in
  assert_true "F128 999.0 -> i16" (result = Some (Z.of_int 999))

let test_int2float () =
  print_endline "\n=== Testing int2float (integer to float conversion) ===";

  (* Test F32 int2float - basic conversions *)
  print_endline "  F32 int2float:";

  (* Simple positive integer *)
  let f32 = F32.int2float (Z.of_int 42) Int8 NearestTiesToEven ~signed:true in
  assert_float_eq "F32 i8 42 -> 42.0" 42.0 (F32.to_float f32);

  (* Negative signed integer *)
  let f32 =
    F32.int2float (Z.of_int (-42)) Int8 NearestTiesToEven ~signed:true
  in
  assert_float_eq "F32 i8 -42 -> -42.0" (-42.0) (F32.to_float f32);

  (* Unsigned interpretation of negative value (two's complement) *)
  (* -1 as signed i8 = 255 as unsigned u8 *)
  let f32_signed =
    F32.int2float (Z.of_int (-1)) Int8 NearestTiesToEven ~signed:true
  in
  assert_float_eq "F32 i8 -1 signed" (-1.0) (F32.to_float f32_signed);

  (* Larger integers *)
  let f32 =
    F32.int2float (Z.of_int 1000000) Int32 NearestTiesToEven ~signed:true
  in
  assert_float_eq "F32 i32 1000000" 1000000.0 (F32.to_float f32);

  (* Test F16 int2float *)
  print_endline "  F16 int2float:";
  let f16 = F16.int2float (Z.of_int 100) Int8 NearestTiesToEven ~signed:false in
  assert_float_eq ~eps:epsilon_f16 "F16 u8 100" 100.0 (F16.to_float f16);

  (* Test F64 int2float *)
  print_endline "  F64 int2float:";
  let f64 =
    F64.int2float (Z.of_int 123456789) Int32 NearestTiesToEven ~signed:true
  in
  assert_float_eq "F64 i32 123456789" 123456789.0 (F64.to_float f64);

  (* Test F128 int2float *)
  print_endline "  F128 int2float:";
  let f128 =
    F128.int2float (Z.of_int 999999) Int32 NearestTiesToEven ~signed:true
  in
  assert_float_eq "F128 i32 999999" 999999.0 (F128.to_float f128);

  (* Test 64-bit integers *)
  print_endline "  64-bit integers:";
  let big_int = Z.of_string "9223372036854775807" in
  (* max int64 *)
  let f64 = F64.int2float big_int Int64 NearestTiesToEven ~signed:true in
  (* Note: F64 cannot represent max int64 exactly, so we just check it's close *)
  assert_true "F64 max_int64 is positive" (F64.to_float f64 > 0.0);

  (* Test unsigned 64-bit *)
  let unsigned_big = Z.of_string "18446744073709551615" in
  (* max uint64 *)
  let f64 = F64.int2float unsigned_big Int64 NearestTiesToEven ~signed:false in
  assert_true "F64 max_uint64 is positive" (F64.to_float f64 > 0.0)

let test_float_int_roundtrip () =
  print_endline "\n=== Testing float-int roundtrip ===";

  (* Test that integer -> float -> integer roundtrips correctly for exact values *)
  let test_roundtrip_f32 n =
    let z = Z.of_int n in
    let f = F32.int2float z Int32 NearestTiesToEven ~signed:true in
    match F32.float2int f Int32 Truncate ~signed:true with
    | Some z' -> Z.equal z z'
    | None -> false
  in

  assert_true "F32 roundtrip 0" (test_roundtrip_f32 0);
  assert_true "F32 roundtrip 1" (test_roundtrip_f32 1);
  assert_true "F32 roundtrip -1" (test_roundtrip_f32 (-1));
  assert_true "F32 roundtrip 1000" (test_roundtrip_f32 1000);
  assert_true "F32 roundtrip -1000" (test_roundtrip_f32 (-1000));

  (* Test F64 roundtrip with larger exact integers *)
  let test_roundtrip_f64 n =
    let z = Z.of_int n in
    let f = F64.int2float z Int64 NearestTiesToEven ~signed:true in
    match F64.float2int f Int64 Truncate ~signed:true with
    | Some z' -> Z.equal z z'
    | None -> false
  in

  assert_true "F64 roundtrip 1000000" (test_roundtrip_f64 1000000);
  assert_true "F64 roundtrip -1000000" (test_roundtrip_f64 (-1000000))

(* ============================================================================
 * COMPREHENSIVE EDGE CASE TESTS
 * ============================================================================ *)

(** Test F16 edge cases - denormals, boundary values, precision limits *)
let test_f16_edge_cases () =
  print_endline "\n=== Testing F16 edge cases ===";

  (* Denormal arithmetic *)
  print_endline "  Denormal arithmetic:";
  let min_denorm = F16.of_bits 0x0001 in
  (* smallest denormal *)
  let max_denorm = F16.of_bits 0x03FF in
  (* largest denormal *)
  let min_normal = F16.of_bits 0x0400 in
  (* smallest normal *)

  (* Denormal + Denormal *)
  let two_min_denorm = F16.add min_denorm min_denorm in
  assert_int_eq "F16 denorm + denorm" 0x0002 (F16.to_bits two_min_denorm);

  (* Max denormal + min denormal = min normal (approximately) *)
  let sum = F16.add max_denorm min_denorm in
  assert_int_eq "F16 max_denorm + min_denorm = min_normal" 0x0400
    (F16.to_bits sum);

  (* Denormal * 2 *)
  let two = F16.of_string "2.0" in
  let doubled = F16.mul min_denorm two in
  assert_int_eq "F16 min_denorm * 2" 0x0002 (F16.to_bits doubled);

  (* Denormal / 2 - underflow to zero *)
  let half = F16.of_string "0.5" in
  let halved = F16.mul min_denorm half in
  assert_int_eq "F16 min_denorm / 2 -> 0" 0x0000 (F16.to_bits halved);

  (* Normal to denormal transition *)
  let almost_denorm = F16.mul min_normal half in
  assert_fpclass_eq "F16 min_normal/2 is subnormal" FP_subnormal
    (F16.fpclass almost_denorm);

  (* Boundary precision tests *)
  print_endline "  Precision boundaries:";

  (* Test that 1 + epsilon != 1 for F16 epsilon (2^-10) *)
  let one = F16.of_bits 0x3C00 in
  let eps16 = F16.of_bits 0x1400 in
  (* 2^-10 = machine epsilon for F16 *)
  let one_plus_eps = F16.add one eps16 in
  assert_true "F16 1 + eps != 1" (F16.to_bits one_plus_eps <> F16.to_bits one);

  (* Test that 1 + eps/2 rounds to 1 or 1+eps (ties to even) *)
  let half_eps = F16.of_bits 0x1000 in
  (* 2^-11 *)
  let one_plus_half_eps = F16.add one half_eps in
  (* Should round to 1.0 (ties to even, 1.0 has even mantissa) *)
  assert_int_eq "F16 1 + eps/2 rounds to 1" 0x3C00
    (F16.to_bits one_plus_half_eps);

  (* Overflow at boundary *)
  print_endline "  Overflow boundaries:";
  let max_normal = F16.of_bits 0x7BFF in
  (* 65504 *)
  let small_inc = F16.of_string "16.0" in
  let overflow_result = F16.add max_normal small_inc in
  assert_true "F16 max + 16 = inf" (is_inf (F16.to_float overflow_result));

  (* Just below overflow *)
  let tiny_inc = F16.of_string "1.0" in
  let no_overflow = F16.add max_normal tiny_inc in
  (* 65504 + 1 should still be 65504 due to precision limits *)
  assert_true "F16 max + 1 no overflow"
    (not (is_inf (F16.to_float no_overflow)));

  (* Negative overflow *)
  let neg_max = F16.of_bits 0xFBFF in
  (* -65504 *)
  let neg_overflow = F16.sub neg_max small_inc in
  assert_true "F16 -max - 16 = -inf" (is_inf (F16.to_float neg_overflow));

  (* Signed zero operations *)
  print_endline "  Signed zero:";
  let pos_zero = F16.of_bits 0x0000 in
  let neg_zero = F16.of_bits 0x8000 in

  (* +0 + -0 = +0 *)
  let zero_sum = F16.add pos_zero neg_zero in
  assert_int_eq "F16 +0 + -0 = +0" 0x0000 (F16.to_bits zero_sum);

  (* -0 + -0 = -0 *)
  let neg_zero_sum = F16.add neg_zero neg_zero in
  assert_int_eq "F16 -0 + -0 = -0" 0x8000 (F16.to_bits neg_zero_sum);

  (* 1 * -0 = -0 *)
  let one_times_neg_zero = F16.mul one neg_zero in
  assert_int_eq "F16 1 * -0 = -0" 0x8000 (F16.to_bits one_times_neg_zero);

  (* -1 * -0 = +0 *)
  let neg_one = F16.of_bits 0xBC00 in
  let neg_one_times_neg_zero = F16.mul neg_one neg_zero in
  assert_int_eq "F16 -1 * -0 = +0" 0x0000 (F16.to_bits neg_one_times_neg_zero);

  (* NaN propagation *)
  print_endline "  NaN propagation:";
  let nan16 = F16.of_bits 0x7E00 in
  let nan_plus_one = F16.add nan16 one in
  assert_true "F16 NaN + 1 = NaN" (is_nan (F16.to_float nan_plus_one));
  let nan_times_zero = F16.mul nan16 pos_zero in
  assert_true "F16 NaN * 0 = NaN" (is_nan (F16.to_float nan_times_zero));
  let nan_div_nan = F16.div nan16 nan16 in
  assert_true "F16 NaN / NaN = NaN" (is_nan (F16.to_float nan_div_nan));

  (* Different NaN payloads *)
  let snan = F16.of_bits 0x7C01 in
  (* signaling NaN *)
  let qnan = F16.of_bits 0x7E00 in
  (* quiet NaN *)
  assert_true "F16 sNaN is NaN" (is_nan (F16.to_float snan));
  assert_true "F16 qNaN is NaN" (is_nan (F16.to_float qnan));
  assert_fpclass_eq "F16 sNaN fpclass" FP_nan (F16.fpclass snan);
  assert_fpclass_eq "F16 qNaN fpclass" FP_nan (F16.fpclass qnan)

(** Test F32 edge cases *)
let test_f32_edge_cases () =
  print_endline "\n=== Testing F32 edge cases ===";

  (* Well-known F32 constants *)
  print_endline "  IEEE 754 constants:";
  let f32_min_normal = F32.of_bits 0x00800000l in
  (* 2^-126 *)
  let f32_max_normal = F32.of_bits 0x7F7FFFFFl in
  (* (2 - 2^-23) * 2^127 *)
  let f32_min_denorm = F32.of_bits 0x00000001l in
  (* 2^-149 *)
  let f32_max_denorm = F32.of_bits 0x007FFFFFl in

  assert_fpclass_eq "F32 min_normal class" FP_normal
    (F32.fpclass f32_min_normal);
  assert_fpclass_eq "F32 max_normal class" FP_normal
    (F32.fpclass f32_max_normal);
  assert_fpclass_eq "F32 min_denorm class" FP_subnormal
    (F32.fpclass f32_min_denorm);
  assert_fpclass_eq "F32 max_denorm class" FP_subnormal
    (F32.fpclass f32_max_denorm);

  (* Denormal arithmetic *)
  print_endline "  Denormal arithmetic:";
  let doubled = F32.add f32_min_denorm f32_min_denorm in
  assert_int32_eq "F32 min_denorm * 2" 0x00000002l (F32.to_bits doubled);

  (* Underflow to zero *)
  let half = F32.of_string "0.5" in
  let underflow = F32.mul f32_min_denorm half in
  assert_int32_eq "F32 min_denorm / 2 -> 0" 0x00000000l (F32.to_bits underflow);

  (* Overflow *)
  print_endline "  Overflow:";
  let two = F32.of_string "2.0" in
  let overflow = F32.mul f32_max_normal two in
  assert_true "F32 max * 2 = inf" (is_inf (F32.to_float overflow));

  (* Signed zeros *)
  print_endline "  Signed zeros:";
  let pos_zero = F32.of_bits 0x00000000l in
  let neg_zero = F32.of_bits 0x80000000l in
  let one = F32.of_bits 0x3F800000l in
  let neg_one = F32.of_bits 0xBF800000l in

  assert_int32_eq "F32 +0 + -0 = +0" 0x00000000l
    (F32.to_bits (F32.add pos_zero neg_zero));
  assert_int32_eq "F32 -0 + -0 = -0" 0x80000000l
    (F32.to_bits (F32.add neg_zero neg_zero));
  assert_int32_eq "F32 1 * -0 = -0" 0x80000000l
    (F32.to_bits (F32.mul one neg_zero));
  assert_int32_eq "F32 -1 * -0 = +0" 0x00000000l
    (F32.to_bits (F32.mul neg_one neg_zero));

  (* Division by zero *)
  print_endline "  Division by zero:";
  assert_int32_eq "F32 1/+0 = +inf" 0x7F800000l
    (F32.to_bits (F32.div one pos_zero));
  assert_int32_eq "F32 1/-0 = -inf" 0xFF800000l
    (F32.to_bits (F32.div one neg_zero));
  assert_int32_eq "F32 -1/+0 = -inf" 0xFF800000l
    (F32.to_bits (F32.div neg_one pos_zero));
  assert_int32_eq "F32 -1/-0 = +inf" 0x7F800000l
    (F32.to_bits (F32.div neg_one neg_zero));

  (* Infinity arithmetic *)
  print_endline "  Infinity arithmetic:";
  let inf = F32.of_bits 0x7F800000l in
  let neg_inf = F32.of_bits 0xFF800000l in

  assert_true "F32 inf + inf = inf" (is_inf (F32.to_float (F32.add inf inf)));
  assert_true "F32 inf + 1 = inf" (is_inf (F32.to_float (F32.add inf one)));
  assert_true "F32 inf * 2 = inf" (is_inf (F32.to_float (F32.mul inf two)));
  assert_true "F32 inf - inf = NaN" (is_nan (F32.to_float (F32.sub inf inf)));
  assert_true "F32 inf / inf = NaN" (is_nan (F32.to_float (F32.div inf inf)));
  assert_true "F32 0 * inf = NaN" (is_nan (F32.to_float (F32.mul pos_zero inf)));
  assert_int32_eq "F32 inf + -inf = NaN"
    (F32.to_bits (F32.add inf neg_inf))
    (F32.to_bits (F32.add inf neg_inf));
  (* Just check it's NaN *)
  assert_true "F32 inf + -inf is NaN"
    (is_nan (F32.to_float (F32.add inf neg_inf)));

  (* Precision edge cases *)
  print_endline "  Precision:";
  let f32_eps = F32.of_bits 0x34000000l in
  (* 2^-23, machine epsilon *)
  let one_plus_eps = F32.add one f32_eps in
  assert_true "F32 1 + eps != 1" (F32.to_bits one_plus_eps <> F32.to_bits one);

  (* Remainder edge cases *)
  print_endline "  Remainder:";
  let five = F32.of_string "5.0" in
  let three = F32.of_string "3.0" in
  assert_float_eq "F32 5 rem 3 = 2" 2.0 (F32.to_float (F32.rem five three));
  assert_float_eq "F32 -5 rem 3 = -2" (-2.0)
    (F32.to_float (F32.rem (F32.of_string "-5.0") three));
  assert_true "F32 inf rem 1 = NaN" (is_nan (F32.to_float (F32.rem inf one)));
  assert_true "F32 1 rem 0 = NaN" (is_nan (F32.to_float (F32.rem one pos_zero)))

(** Test F64 edge cases *)
let test_f64_edge_cases () =
  print_endline "\n=== Testing F64 edge cases ===";

  (* Well-known F64 constants *)
  print_endline "  IEEE 754 constants:";
  let f64_min_normal = F64.of_bits 0x0010000000000000L in
  (* 2^-1022 *)
  let f64_max_normal = F64.of_bits 0x7FEFFFFFFFFFFFFFL in
  let f64_min_denorm = F64.of_bits 0x0000000000000001L in
  (* 2^-1074 *)

  assert_fpclass_eq "F64 min_normal class" FP_normal
    (F64.fpclass f64_min_normal);
  assert_fpclass_eq "F64 max_normal class" FP_normal
    (F64.fpclass f64_max_normal);
  assert_fpclass_eq "F64 min_denorm class" FP_subnormal
    (F64.fpclass f64_min_denorm);

  (* Denormal arithmetic *)
  print_endline "  Denormal arithmetic:";
  let doubled = F64.add f64_min_denorm f64_min_denorm in
  assert_int64_eq "F64 min_denorm * 2" 0x0000000000000002L (F64.to_bits doubled);

  (* Overflow *)
  print_endline "  Overflow:";
  let two = F64.of_string "2.0" in
  let overflow = F64.mul f64_max_normal two in
  assert_true "F64 max * 2 = inf" (is_inf (F64.to_float overflow));

  (* Signed zeros *)
  print_endline "  Signed zeros:";
  let pos_zero = F64.of_bits 0x0000000000000000L in
  let neg_zero = F64.of_bits 0x8000000000000000L in
  let one = F64.of_bits 0x3FF0000000000000L in
  let neg_one = F64.of_bits 0xBFF0000000000000L in

  assert_int64_eq "F64 +0 + -0 = +0" 0x0000000000000000L
    (F64.to_bits (F64.add pos_zero neg_zero));
  assert_int64_eq "F64 -0 + -0 = -0" 0x8000000000000000L
    (F64.to_bits (F64.add neg_zero neg_zero));
  assert_int64_eq "F64 1 * -0 = -0" 0x8000000000000000L
    (F64.to_bits (F64.mul one neg_zero));
  assert_int64_eq "F64 -1 * -0 = +0" 0x0000000000000000L
    (F64.to_bits (F64.mul neg_one neg_zero));

  (* Infinity operations *)
  print_endline "  Infinity:";
  let inf = F64.of_bits 0x7FF0000000000000L in
  let neg_inf = F64.of_bits 0xFFF0000000000000L in

  assert_true "F64 inf + inf = inf" (is_inf (F64.to_float (F64.add inf inf)));
  assert_true "F64 inf - inf = NaN" (is_nan (F64.to_float (F64.sub inf inf)));
  assert_true "F64 inf * 0 = NaN" (is_nan (F64.to_float (F64.mul inf pos_zero)));
  assert_true "F64 -inf < inf" (F64.lt neg_inf inf);

  (* High precision values *)
  print_endline "  Precision:";
  let f64_eps = F64.of_bits 0x3CB0000000000000L in
  (* 2^-52, machine epsilon *)
  let one_plus_eps = F64.add one f64_eps in
  assert_true "F64 1 + eps != 1" (F64.to_bits one_plus_eps <> F64.to_bits one);

  (* Large exact integers *)
  let large_int = F64.of_string "9007199254740992.0" in
  (* 2^53, largest exact integer *)
  let large_int_plus_one = F64.add large_int one in
  (* 2^53 + 1 should round to 2^53 due to precision limits *)
  assert_float_eq "F64 2^53 + 1 precision" 9007199254740992.0
    (F64.to_float large_int_plus_one)

(** Test F128 edge cases *)
let test_f128_edge_cases () =
  print_endline "\n=== Testing F128 edge cases ===";

  (* Basic operations with special values *)
  print_endline "  Special values:";
  let zero = F128.of_string "0.0" in
  let neg_zero = F128.of_string "-0.0" in
  let one = F128.of_string "1.0" in
  let inf = F128.of_string "inf" in
  let neg_inf = F128.of_string "-inf" in
  let nan_val = F128.of_string "nan" in

  (* Infinity arithmetic *)
  assert_true "F128 inf + 1 = inf" (is_inf (F128.to_float (F128.add inf one)));
  assert_true "F128 inf - inf = NaN" (is_nan (F128.to_float (F128.sub inf inf)));
  assert_true "F128 inf * 0 = NaN" (is_nan (F128.to_float (F128.mul inf zero)));
  assert_true "F128 1 / 0 = inf" (is_inf (F128.to_float (F128.div one zero)));
  assert_true "F128 0 / 0 = NaN" (is_nan (F128.to_float (F128.div zero zero)));

  (* NaN propagation *)
  assert_true "F128 NaN + 1 = NaN"
    (is_nan (F128.to_float (F128.add nan_val one)));
  assert_true "F128 NaN * 0 = NaN"
    (is_nan (F128.to_float (F128.mul nan_val zero)));

  (* Signed zero *)
  print_endline "  Signed zero:";
  let sum = F128.add zero neg_zero in
  assert_float_eq "F128 +0 + -0 value" 0.0 (F128.to_float sum);

  (* Large values *)
  print_endline "  Large values:";
  let big = F128.of_string "1e308" in
  let bigger = F128.mul big (F128.of_string "10.0") in
  assert_true "F128 1e308 * 10 is finite or inf"
    (let v = F128.to_float bigger in
     is_inf v || v > 0.0);

  (* Comparison edge cases *)
  print_endline "  Comparisons:";
  assert_true "F128 inf = inf" (F128.eq inf inf);
  assert_true "F128 -inf = -inf" (F128.eq neg_inf neg_inf);
  assert_true "F128 +0 = -0" (F128.eq zero neg_zero);
  assert_true "F128 NaN != NaN" (not (F128.eq nan_val nan_val));
  assert_true "F128 !(NaN < NaN)" (not (F128.lt nan_val nan_val));
  assert_true "F128 !(NaN <= NaN)" (not (F128.le nan_val nan_val));
  assert_true "F128 -inf < inf" (F128.lt neg_inf inf)

(** Test float2int edge cases - boundary values, rounding, overflow *)
let test_float2int_edge_cases () =
  print_endline "\n=== Testing float2int edge cases ===";

  (* Int8 boundaries: -128 to 127 (signed), 0 to 255 (unsigned) *)
  print_endline "  Int8 boundaries:";

  (* Exact boundary values *)
  let f32_127 = F32.of_string "127.0" in
  let f32_128 = F32.of_string "128.0" in
  let f32_neg128 = F32.of_string "-128.0" in
  let f32_neg129 = F32.of_string "-129.0" in
  let f32_255 = F32.of_string "255.0" in
  let f32_256 = F32.of_string "256.0" in

  assert_true "F32 127 -> i8 ok"
    (F32.float2int f32_127 Int8 Truncate ~signed:true = Some (Z.of_int 127));
  assert_true "F32 128 -> i8 overflow"
    (F32.float2int f32_128 Int8 Truncate ~signed:true = None);
  assert_true "F32 -128 -> i8 ok"
    (F32.float2int f32_neg128 Int8 Truncate ~signed:true
    = Some (Z.of_int (-128)));
  assert_true "F32 -129 -> i8 overflow"
    (F32.float2int f32_neg129 Int8 Truncate ~signed:true = None);
  assert_true "F32 255 -> u8 ok"
    (F32.float2int f32_255 Int8 Truncate ~signed:false = Some (Z.of_int 255));
  assert_true "F32 256 -> u8 overflow"
    (F32.float2int f32_256 Int8 Truncate ~signed:false = None);

  (* Int16 boundaries *)
  print_endline "  Int16 boundaries:";
  let f32_32767 = F32.of_string "32767.0" in
  let f32_32768 = F32.of_string "32768.0" in
  let f32_neg32768 = F32.of_string "-32768.0" in
  let f32_neg32769 = F32.of_string "-32769.0" in
  let f32_65535 = F32.of_string "65535.0" in
  let f32_65536 = F32.of_string "65536.0" in

  assert_true "F32 32767 -> i16 ok"
    (F32.float2int f32_32767 Int16 Truncate ~signed:true = Some (Z.of_int 32767));
  assert_true "F32 32768 -> i16 overflow"
    (F32.float2int f32_32768 Int16 Truncate ~signed:true = None);
  assert_true "F32 -32768 -> i16 ok"
    (F32.float2int f32_neg32768 Int16 Truncate ~signed:true
    = Some (Z.of_int (-32768)));
  assert_true "F32 -32769 -> i16 overflow"
    (F32.float2int f32_neg32769 Int16 Truncate ~signed:true = None);
  assert_true "F32 65535 -> u16 ok"
    (F32.float2int f32_65535 Int16 Truncate ~signed:false
    = Some (Z.of_int 65535));
  assert_true "F32 65536 -> u16 overflow"
    (F32.float2int f32_65536 Int16 Truncate ~signed:false = None);

  (* Int32 boundaries (use F64 for precision) *)
  print_endline "  Int32 boundaries:";
  let f64_max_i32 = F64.of_string "2147483647.0" in
  let f64_over_i32 = F64.of_string "2147483648.0" in
  let f64_min_i32 = F64.of_string "-2147483648.0" in
  let f64_under_i32 = F64.of_string "-2147483649.0" in
  let f64_max_u32 = F64.of_string "4294967295.0" in
  let f64_over_u32 = F64.of_string "4294967296.0" in

  assert_true "F64 max_i32 -> i32 ok"
    (F64.float2int f64_max_i32 Int32 Truncate ~signed:true
    = Some (Z.of_string "2147483647"));
  assert_true "F64 max_i32+1 -> i32 overflow"
    (F64.float2int f64_over_i32 Int32 Truncate ~signed:true = None);
  assert_true "F64 min_i32 -> i32 ok"
    (F64.float2int f64_min_i32 Int32 Truncate ~signed:true
    = Some (Z.of_string "-2147483648"));
  assert_true "F64 min_i32-1 -> i32 overflow"
    (F64.float2int f64_under_i32 Int32 Truncate ~signed:true = None);
  assert_true "F64 max_u32 -> u32 ok"
    (F64.float2int f64_max_u32 Int32 Truncate ~signed:false
    = Some (Z.of_string "4294967295"));
  assert_true "F64 max_u32+1 -> u32 overflow"
    (F64.float2int f64_over_u32 Int32 Truncate ~signed:false = None);

  (* Rounding near boundaries *)
  print_endline "  Rounding near boundaries:";
  let f32_127_5 = F32.of_string "127.5" in
  let f32_neg128_5 = F32.of_string "-128.5" in

  (* 127.5 Truncate -> 127 (ok), Ceil -> 128 (overflow) *)
  assert_true "F32 127.5 Truncate -> i8 ok"
    (F32.float2int f32_127_5 Int8 Truncate ~signed:true = Some (Z.of_int 127));
  assert_true "F32 127.5 Ceil -> i8 overflow"
    (F32.float2int f32_127_5 Int8 Ceil ~signed:true = None);

  (* -128.5 Truncate -> -128 (ok), Floor -> -129 (overflow) *)
  assert_true "F32 -128.5 Truncate -> i8 ok"
    (F32.float2int f32_neg128_5 Int8 Truncate ~signed:true
    = Some (Z.of_int (-128)));
  assert_true "F32 -128.5 Floor -> i8 overflow"
    (F32.float2int f32_neg128_5 Int8 Floor ~signed:true = None);

  (* Zero handling *)
  print_endline "  Zero handling:";
  let f32_pos_zero = F32.of_bits 0x00000000l in
  let f32_neg_zero = F32.of_bits 0x80000000l in
  assert_true "F32 +0 -> int = 0"
    (F32.float2int f32_pos_zero Int32 Truncate ~signed:true = Some Z.zero);
  assert_true "F32 -0 -> int = 0"
    (F32.float2int f32_neg_zero Int32 Truncate ~signed:true = Some Z.zero);

  (* Very small values that round to zero *)
  let f32_tiny = F32.of_string "0.1" in
  assert_true "F32 0.1 Truncate -> 0"
    (F32.float2int f32_tiny Int8 Truncate ~signed:true = Some Z.zero);
  assert_true "F32 0.1 Ceil -> 1"
    (F32.float2int f32_tiny Int8 Ceil ~signed:true = Some Z.one);
  assert_true "F32 0.1 Floor -> 0"
    (F32.float2int f32_tiny Int8 Floor ~signed:true = Some Z.zero);

  let f32_neg_tiny = F32.of_string "-0.1" in
  assert_true "F32 -0.1 Truncate -> 0"
    (F32.float2int f32_neg_tiny Int8 Truncate ~signed:true = Some Z.zero);
  assert_true "F32 -0.1 Ceil -> 0"
    (F32.float2int f32_neg_tiny Int8 Ceil ~signed:true = Some Z.zero);
  assert_true "F32 -0.1 Floor -> -1"
    (F32.float2int f32_neg_tiny Int8 Floor ~signed:true = Some (Z.of_int (-1)));

  (* NearestTiesToEven rounding *)
  print_endline "  NearestTiesToEven rounding:";
  let f32_0_5 = F32.of_string "0.5" in
  let f32_1_5 = F32.of_string "1.5" in
  let f32_2_5 = F32.of_string "2.5" in
  let f32_neg_0_5 = F32.of_string "-0.5" in
  let f32_neg_1_5 = F32.of_string "-1.5" in

  (* 0.5 -> 0 (even), 1.5 -> 2 (even), 2.5 -> 2 (even) *)
  assert_true "F32 0.5 NearestTiesToEven -> 0"
    (F32.float2int f32_0_5 Int8 NearestTiesToEven ~signed:true = Some Z.zero);
  assert_true "F32 1.5 NearestTiesToEven -> 2"
    (F32.float2int f32_1_5 Int8 NearestTiesToEven ~signed:true
    = Some (Z.of_int 2));
  assert_true "F32 2.5 NearestTiesToEven -> 2"
    (F32.float2int f32_2_5 Int8 NearestTiesToEven ~signed:true
    = Some (Z.of_int 2));
  assert_true "F32 -0.5 NearestTiesToEven -> 0"
    (F32.float2int f32_neg_0_5 Int8 NearestTiesToEven ~signed:true = Some Z.zero);
  assert_true "F32 -1.5 NearestTiesToEven -> -2"
    (F32.float2int f32_neg_1_5 Int8 NearestTiesToEven ~signed:true
    = Some (Z.of_int (-2)));

  (* NearestTiesToAway rounding *)
  print_endline "  NearestTiesToAway rounding:";
  assert_true "F32 0.5 NearestTiesToAway -> 1"
    (F32.float2int f32_0_5 Int8 NearestTiesToAway ~signed:true = Some Z.one);
  assert_true "F32 -0.5 NearestTiesToAway -> -1"
    (F32.float2int f32_neg_0_5 Int8 NearestTiesToAway ~signed:true
    = Some (Z.of_int (-1)))

(** Test int2float edge cases *)
let test_int2float_edge_cases () =
  print_endline "\n=== Testing int2float edge cases ===";

  (* Boundary values *)
  print_endline "  Int8 boundaries:";
  let z_127 = Z.of_int 127 in
  let z_neg128 = Z.of_int (-128) in
  let z_255 = Z.of_int 255 in

  assert_float_eq "i8 127 -> F32" 127.0
    (F32.to_float (F32.int2float z_127 Int8 NearestTiesToEven ~signed:true));
  assert_float_eq "i8 -128 -> F32" (-128.0)
    (F32.to_float (F32.int2float z_neg128 Int8 NearestTiesToEven ~signed:true));
  assert_float_eq "u8 255 -> F32" 255.0
    (F32.to_float (F32.int2float z_255 Int8 NearestTiesToEven ~signed:false));

  (* Int16 boundaries *)
  print_endline "  Int16 boundaries:";
  let z_32767 = Z.of_int 32767 in
  let z_neg32768 = Z.of_int (-32768) in
  let z_65535 = Z.of_int 65535 in

  assert_float_eq "i16 32767 -> F32" 32767.0
    (F32.to_float (F32.int2float z_32767 Int16 NearestTiesToEven ~signed:true));
  assert_float_eq "i16 -32768 -> F32" (-32768.0)
    (F32.to_float
       (F32.int2float z_neg32768 Int16 NearestTiesToEven ~signed:true));
  assert_float_eq "u16 65535 -> F32" 65535.0
    (F32.to_float (F32.int2float z_65535 Int16 NearestTiesToEven ~signed:false));

  (* Int32 boundaries *)
  print_endline "  Int32 boundaries:";
  let z_max_i32 = Z.of_string "2147483647" in
  let z_min_i32 = Z.of_string "-2147483648" in
  let z_max_u32 = Z.of_string "4294967295" in

  (* F64 can represent these exactly *)
  assert_float_eq "i32 max -> F64" 2147483647.0
    (F64.to_float
       (F64.int2float z_max_i32 Int32 NearestTiesToEven ~signed:true));
  assert_float_eq "i32 min -> F64" (-2147483648.0)
    (F64.to_float
       (F64.int2float z_min_i32 Int32 NearestTiesToEven ~signed:true));
  assert_float_eq "u32 max -> F64" 4294967295.0
    (F64.to_float
       (F64.int2float z_max_u32 Int32 NearestTiesToEven ~signed:false));

  (* Zero values *)
  print_endline "  Zero:";
  assert_float_eq "i8 0 -> F32" 0.0
    (F32.to_float (F32.int2float Z.zero Int8 NearestTiesToEven ~signed:true));
  assert_float_eq "u8 0 -> F32" 0.0
    (F32.to_float (F32.int2float Z.zero Int8 NearestTiesToEven ~signed:false));

  (* Large values that lose precision in F32 *)
  print_endline "  Precision loss in F32:";
  let z_large = Z.of_int 16777217 in
  (* 2^24 + 1, not exactly representable in F32 *)
  let f32_large = F32.int2float z_large Int32 NearestTiesToEven ~signed:true in
  (* Should round to 16777216 or 16777218 *)
  assert_true "i32 2^24+1 -> F32 rounds"
    (let v = F32.to_float f32_large in
     v = 16777216.0 || v = 16777218.0);

  (* Same value in F64 is exact *)
  let f64_large = F64.int2float z_large Int32 NearestTiesToEven ~signed:true in
  assert_float_eq "i32 2^24+1 -> F64 exact" 16777217.0 (F64.to_float f64_large);

  (* Power of 2 values (always exact in floating point) *)
  print_endline "  Powers of 2:";
  let z_pow2 = Z.of_int 1024 in
  (* 2^10 *)
  assert_float_eq "i16 1024 -> F16" 1024.0
    (F16.to_float (F16.int2float z_pow2 Int16 NearestTiesToEven ~signed:true))

(** Test comparison edge cases *)
let test_comparison_edge_cases () =
  print_endline "\n=== Testing comparison edge cases ===";

  (* NaN comparisons *)
  print_endline "  NaN comparisons:";
  let nan32 = F32.of_string "nan" in
  let one32 = F32.of_string "1.0" in
  let inf32 = F32.of_bits 0x7F800000l in

  assert_true "F32 NaN != NaN" (not (F32.eq nan32 nan32));
  assert_true "F32 NaN != 1" (not (F32.eq nan32 one32));
  assert_true "F32 !(NaN < 1)" (not (F32.lt nan32 one32));
  assert_true "F32 !(1 < NaN)" (not (F32.lt one32 nan32));
  assert_true "F32 !(NaN <= NaN)" (not (F32.le nan32 nan32));
  assert_true "F32 !(NaN < inf)" (not (F32.lt nan32 inf32));
  assert_true "F32 !(inf < NaN)" (not (F32.lt inf32 nan32));

  (* Infinity comparisons *)
  print_endline "  Infinity comparisons:";
  let neg_inf32 = F32.of_bits 0xFF800000l in
  let max32 = F32.of_bits 0x7F7FFFFFl in
  let min32 = F32.of_bits 0xFF7FFFFFl in

  assert_true "F32 inf = inf" (F32.eq inf32 inf32);
  assert_true "F32 -inf = -inf" (F32.eq neg_inf32 neg_inf32);
  assert_true "F32 inf != -inf" (not (F32.eq inf32 neg_inf32));
  assert_true "F32 -inf < max" (F32.lt neg_inf32 max32);
  assert_true "F32 min < inf" (F32.lt min32 inf32);
  assert_true "F32 max < inf" (F32.lt max32 inf32);
  assert_true "F32 -inf < -max" (F32.lt neg_inf32 min32);

  (* Zero comparisons *)
  print_endline "  Zero comparisons:";
  let pos_zero32 = F32.of_bits 0x00000000l in
  let neg_zero32 = F32.of_bits 0x80000000l in

  assert_true "F32 +0 = -0" (F32.eq pos_zero32 neg_zero32);
  assert_true "F32 -0 = +0" (F32.eq neg_zero32 pos_zero32);
  assert_true "F32 !(+0 < -0)" (not (F32.lt pos_zero32 neg_zero32));
  assert_true "F32 !(-0 < +0)" (not (F32.lt neg_zero32 pos_zero32));
  assert_true "F32 +0 <= -0" (F32.le pos_zero32 neg_zero32);
  assert_true "F32 -0 <= +0" (F32.le neg_zero32 pos_zero32);

  (* Denormal comparisons *)
  print_endline "  Denormal comparisons:";
  let min_denorm32 = F32.of_bits 0x00000001l in
  let max_denorm32 = F32.of_bits 0x007FFFFFl in
  let min_normal32 = F32.of_bits 0x00800000l in

  assert_true "F32 +0 < min_denorm" (F32.lt pos_zero32 min_denorm32);
  assert_true "F32 min_denorm < max_denorm" (F32.lt min_denorm32 max_denorm32);
  assert_true "F32 max_denorm < min_normal" (F32.lt max_denorm32 min_normal32);

  (* Same tests for F64 *)
  print_endline "  F64 comparisons:";
  let nan64 = F64.of_string "nan" in
  let inf64 = F64.of_bits 0x7FF0000000000000L in
  let neg_inf64 = F64.of_bits 0xFFF0000000000000L in
  let pos_zero64 = F64.of_bits 0x0000000000000000L in
  let neg_zero64 = F64.of_bits 0x8000000000000000L in

  assert_true "F64 NaN != NaN" (not (F64.eq nan64 nan64));
  assert_true "F64 inf = inf" (F64.eq inf64 inf64);
  assert_true "F64 +0 = -0" (F64.eq pos_zero64 neg_zero64);
  assert_true "F64 -inf < inf" (F64.lt neg_inf64 inf64)

(** Test rounding edge cases *)
let test_rounding_edge_cases () =
  print_endline "\n=== Testing rounding edge cases ===";

  (* Test all rounding modes with various values *)
  print_endline "  All rounding modes:";

  (* Values: 0.5, 1.5, 2.5, -0.5, -1.5, -2.5 *)
  let test_values =
    [
      ("0.5", 0.5);
      ("1.5", 1.5);
      ("2.5", 2.5);
      ("-0.5", -0.5);
      ("-1.5", -1.5);
      ("-2.5", -2.5);
      ("0.1", 0.1);
      ("0.9", 0.9);
      ("-0.1", -0.1);
      ("-0.9", -0.9);
    ]
  in

  List.iter
    (fun (name, v) ->
      let f32 = F32.of_string (Printf.sprintf "%f" v) in

      (* Truncate *)
      let r_tz = F32.to_float (F32.round Truncate f32) in
      assert_float_eq
        (Printf.sprintf "F32 round Truncate %s" name)
        (Float.trunc v) r_tz;

      (* Ceil *)
      let r_up = F32.to_float (F32.round Ceil f32) in
      assert_float_eq
        (Printf.sprintf "F32 round Ceil %s" name)
        (Float.ceil v) r_up;

      (* Floor *)
      let r_dn = F32.to_float (F32.round Floor f32) in
      assert_float_eq
        (Printf.sprintf "F32 round Floor %s" name)
        (Float.floor v) r_dn;

      (* NearestTiesToAway *)
      let r_na = F32.to_float (F32.round NearestTiesToAway f32) in
      assert_true
        (Printf.sprintf "F32 round NearestTiesToAway %s is int" name)
        (Float.is_integer r_na))
    test_values;

  (* Special values *)
  print_endline "  Special values:";
  let inf32 = F32.of_bits 0x7F800000l in
  let neg_inf32 = F32.of_bits 0xFF800000l in
  let nan32 = F32.of_string "nan" in
  let pos_zero32 = F32.of_bits 0x00000000l in
  let neg_zero32 = F32.of_bits 0x80000000l in

  (* Rounding infinities and NaN should preserve them *)
  assert_true "F32 round inf = inf"
    (is_inf (F32.to_float (F32.round NearestTiesToEven inf32)));
  assert_true "F32 round -inf = -inf"
    (is_inf (F32.to_float (F32.round NearestTiesToEven neg_inf32)));
  assert_true "F32 round NaN = NaN"
    (is_nan (F32.to_float (F32.round NearestTiesToEven nan32)));

  (* Rounding zeros should preserve sign *)
  assert_int32_eq "F32 round +0 = +0" 0x00000000l
    (F32.to_bits (F32.round NearestTiesToEven pos_zero32));
  assert_int32_eq "F32 round -0 = -0" 0x80000000l
    (F32.to_bits (F32.round NearestTiesToEven neg_zero32));

  (* Large values that are already integers *)
  print_endline "  Large integers:";
  let big32 = F32.of_string "16777216.0" in
  (* 2^24, exactly representable *)
  assert_float_eq "F32 round 2^24 = 2^24" 16777216.0
    (F32.to_float (F32.round NearestTiesToEven big32));

  (* Denormals *)
  print_endline "  Denormals:";
  let min_denorm32 = F32.of_bits 0x00000001l in
  let rounded_denorm = F32.round NearestTiesToEven min_denorm32 in
  assert_int32_eq "F32 round min_denorm = 0" 0x00000000l
    (F32.to_bits rounded_denorm);

  let neg_min_denorm32 = F32.of_bits 0x80000001l in
  let rounded_neg_denorm = F32.round NearestTiesToEven neg_min_denorm32 in
  assert_int32_eq "F32 round -min_denorm = -0" 0x80000000l
    (F32.to_bits rounded_neg_denorm)

(** Test abs edge cases *)
let test_abs_edge_cases () =
  print_endline "\n=== Testing abs edge cases ===";

  (* F32 abs *)
  print_endline "  F32 abs:";
  let pos_zero32 = F32.of_bits 0x00000000l in
  let neg_zero32 = F32.of_bits 0x80000000l in
  let inf32 = F32.of_bits 0x7F800000l in
  let neg_inf32 = F32.of_bits 0xFF800000l in
  let nan32 = F32.of_bits 0x7FC00000l in
  let neg_nan32 = F32.of_bits 0xFFC00000l in
  let min_denorm32 = F32.of_bits 0x00000001l in
  let neg_min_denorm32 = F32.of_bits 0x80000001l in
  let max32 = F32.of_bits 0x7F7FFFFFl in
  let neg_max32 = F32.of_bits 0xFF7FFFFFl in

  assert_int32_eq "F32 abs +0" 0x00000000l (F32.to_bits (F32.abs pos_zero32));
  assert_int32_eq "F32 abs -0" 0x00000000l (F32.to_bits (F32.abs neg_zero32));
  assert_int32_eq "F32 abs +inf" 0x7F800000l (F32.to_bits (F32.abs inf32));
  assert_int32_eq "F32 abs -inf" 0x7F800000l (F32.to_bits (F32.abs neg_inf32));
  assert_true "F32 abs NaN is NaN" (is_nan (F32.to_float (F32.abs nan32)));
  assert_true "F32 abs -NaN is NaN" (is_nan (F32.to_float (F32.abs neg_nan32)));
  assert_int32_eq "F32 abs min_denorm" 0x00000001l
    (F32.to_bits (F32.abs min_denorm32));
  assert_int32_eq "F32 abs -min_denorm" 0x00000001l
    (F32.to_bits (F32.abs neg_min_denorm32));
  assert_int32_eq "F32 abs max" 0x7F7FFFFFl (F32.to_bits (F32.abs max32));
  assert_int32_eq "F32 abs -max" 0x7F7FFFFFl (F32.to_bits (F32.abs neg_max32));

  (* F64 abs *)
  print_endline "  F64 abs:";
  let neg_zero64 = F64.of_bits 0x8000000000000000L in
  let neg_inf64 = F64.of_bits 0xFFF0000000000000L in

  assert_int64_eq "F64 abs -0" 0x0000000000000000L
    (F64.to_bits (F64.abs neg_zero64));
  assert_int64_eq "F64 abs -inf" 0x7FF0000000000000L
    (F64.to_bits (F64.abs neg_inf64));

  (* F16 abs *)
  print_endline "  F16 abs:";
  let neg_zero16 = F16.of_bits 0x8000 in
  let neg_inf16 = F16.of_bits 0xFC00 in
  let neg_min_denorm16 = F16.of_bits 0x8001 in

  assert_int_eq "F16 abs -0" 0x0000 (F16.to_bits (F16.abs neg_zero16));
  assert_int_eq "F16 abs -inf" 0x7C00 (F16.to_bits (F16.abs neg_inf16));
  assert_int_eq "F16 abs -min_denorm" 0x0001
    (F16.to_bits (F16.abs neg_min_denorm16))

let () =
  print_endline "Running floatml library tests...";

  test_f16 ();
  test_f16_ieee754 ();
  test_f32 ();
  test_f64 ();
  test_f128 ();
  test_special_values ();
  test_z_conversions ();
  test_fpclass ();
  test_abs ();
  test_round ();
  test_comparison ();
  test_float2int ();
  test_int2float ();
  test_float_int_roundtrip ();
  test_f16_edge_cases ();
  test_f32_edge_cases ();
  test_f64_edge_cases ();
  test_f128_edge_cases ();
  test_float2int_edge_cases ();
  test_int2float_edge_cases ();
  test_comparison_edge_cases ();
  test_rounding_edge_cases ();
  test_abs_edge_cases ();

  Printf.printf "\n=== Results: %d/%d tests passed ===\n" !tests_passed
    !tests_run;
  if !tests_passed = !tests_run then print_endline "All tests passed!"
  else begin
    Printf.printf "FAILED: %d tests failed\n" (!tests_run - !tests_passed);
    exit 1
  end
