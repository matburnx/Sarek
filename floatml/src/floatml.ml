(** OCaml bindings for f16, f32, f64, f128 floating-point types *)

(** IEEE 754 rounding modes *)
type rounding_mode =
  | NearestTiesToEven
      (** Round to nearest, ties to even (default IEEE 754 mode) *)
  | Truncate  (** Round toward zero *)
  | Ceil  (** Round toward +infinity *)
  | Floor  (** Round toward -infinity *)
  | NearestTiesToAway  (** Round to nearest, ties away from zero *)

(** Integer sizes for float-int conversions *)
type int_size =
  | Int8  (** 8-bit integer *)
  | Int16  (** 16-bit integer *)
  | Int32  (** 32-bit integer *)
  | Int64  (** 64-bit integer *)
  | Int128  (** 128-bit integer *)

(* Convert fpclass int from C to OCaml type *)
let fpclass_of_int = function
  | 0 -> FP_normal
  | 1 -> FP_subnormal
  | 2 -> FP_zero
  | 3 -> FP_infinite
  | _ -> FP_nan

(* Convert rounding_mode to int for C *)
let int_of_rounding_mode = function
  | NearestTiesToEven -> 0
  | Truncate -> 1
  | Ceil -> 2
  | Floor -> 3
  | NearestTiesToAway -> 4

(* Convert int_size to int for C *)
let int_of_int_size = function
  | Int8 -> 0
  | Int16 -> 1
  | Int32 -> 2
  | Int64 -> 3
  | Int128 -> 4

(* Get the number of bits for an int_size *)
let bits_of_int_size = function
  | Int8 -> 8
  | Int16 -> 16
  | Int32 -> 32
  | Int64 -> 64
  | Int128 -> 128

(* Convert Z.t to (low, high) int64 pair for C, with proper handling of signedness *)
let z_to_int64_pair z size ~signed =
  let bits = bits_of_int_size size in
  if bits <= 64 then
    if signed then
      (* For signed values, just pass the Z.t value directly as signed int64 *)
      (* The C code will interpret it correctly *)
      let low = Z.to_int64 z in
      let high = if Z.sign z < 0 then -1L else 0L in
      (low, high)
    else
      (* For unsigned values, mask to bit width and extract lower 64 bits *)
      let mask = Z.pred (Z.shift_left Z.one bits) in
      let masked = Z.logand z mask in
      (* Extract lower 32 bits and upper 32 bits separately to avoid overflow *)
      let low32 = Z.to_int64 (Z.logand masked (Z.of_int 0xFFFFFFFF)) in
      let high32 =
        Z.to_int64 (Z.logand (Z.shift_right masked 32) (Z.of_int 0xFFFFFFFF))
      in
      let low = Int64.logor low32 (Int64.shift_left high32 32) in
      (low, 0L)
  else
    (* 128-bit case *)
    let mask = Z.pred (Z.shift_left Z.one bits) in
    let masked =
      if signed && Z.sign z < 0 then Z.logand z mask else Z.logand z mask
    in
    (* Extract in 32-bit chunks to avoid overflow *)
    let low32_0 = Z.to_int64 (Z.logand masked (Z.of_int 0xFFFFFFFF)) in
    let low32_1 =
      Z.to_int64 (Z.logand (Z.shift_right masked 32) (Z.of_int 0xFFFFFFFF))
    in
    let low = Int64.logor low32_0 (Int64.shift_left low32_1 32) in
    let high32_0 =
      Z.to_int64 (Z.logand (Z.shift_right masked 64) (Z.of_int 0xFFFFFFFF))
    in
    let high32_1 =
      Z.to_int64 (Z.logand (Z.shift_right masked 96) (Z.of_int 0xFFFFFFFF))
    in
    let high = Int64.logor high32_0 (Int64.shift_left high32_1 32) in
    (low, high)

(* Convert (low, high) int64 pair from C to Z.t, with proper handling of signedness *)
let int64_pair_to_z low high size ~signed =
  let bits = bits_of_int_size size in
  if bits > 64 then
    (* For 128-bit: combine low (unsigned) and high (signed for sign extension) *)
    let z_low = Z.of_int64_unsigned low in
    let z_high = Z.of_int64 high in
    let combined = Z.add (Z.shift_left z_high 64) z_low in
    if signed && Z.testbit combined (bits - 1) then
      (* Negative: subtract 2^bits to get the signed value *)
      Z.sub combined (Z.shift_left Z.one bits)
    else combined
  else if signed then
    (* For signed values <= 64 bits, C returns the value directly as signed int64 *)
    Z.of_int64 low
  else
    (* For unsigned values <= 64 bits, interpret as unsigned *)
    Z.of_int64_unsigned low

(* ============================================================================
 * F16 - Half-precision (16-bit) floating-point
 * Uses native _Float16 when available, IEEE-754 compliant software fallback otherwise
 * ============================================================================ *)
module F16 = struct
  type t
  type bits = int

  external of_string : string -> t = "caml_f16_of_string"
  external add : t -> t -> t = "caml_f16_add"
  external sub : t -> t -> t = "caml_f16_sub"
  external mul : t -> t -> t = "caml_f16_mul"
  external div : t -> t -> t = "caml_f16_div"
  external rem : t -> t -> t = "caml_f16_rem"
  external to_bits : t -> int = "caml_f16_to_bits"
  external of_bits : int -> t = "caml_f16_of_bits"
  external to_float : t -> float = "caml_f16_to_float"
  external fpclass_raw : t -> int = "caml_f16_fpclass"
  external abs : t -> t = "caml_f16_abs"
  external round_raw : t -> int -> t = "caml_f16_round"
  external cos : t -> t = "caml_f16_cos"
  external sin : t -> t = "caml_f16_sin"

  let of_bits_z z = of_bits (Z.to_int z)
  let to_z t = Z.of_int (to_bits t)
  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f16_eq"
  external lt : t -> t -> bool = "caml_f16_lt"
  external le : t -> t -> bool = "caml_f16_le"

  let gt = Fun.flip lt
  let ge = Fun.flip le
  let equal = eq
  let compare l r = if lt l r then -1 else if gt l r then 1 else 0
  let bits_equal l r = to_bits l = to_bits r

  (* Float to integer conversion *)
  external to_int_raw : t -> int -> int -> bool -> int64 * int64 * bool
    = "caml_f16_to_int"

  let float2int t size mode ~signed =
    let low, high, success =
      to_int_raw t (int_of_int_size size) (int_of_rounding_mode mode) signed
    in
    if success then Some (int64_pair_to_z low high size ~signed) else None

  (* Integer to float conversion *)
  external of_int_raw : int64 -> int64 -> int -> int -> bool -> t
    = "caml_f16_of_int"

  let int2float z size mode ~signed =
    let low, high = z_to_int64_pair z size ~signed in
    of_int_raw low high (int_of_int_size size)
      (int_of_rounding_mode mode)
      signed

  let pp ft t = Format.fprintf ft "%f" (to_float t)
  let show t = Format.asprintf "%f" (to_float t)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

(* ============================================================================
 * F32 - Single-precision (32-bit) floating-point
 * ============================================================================ *)
module F32 = struct
  type t
  type bits = int32

  external of_string : string -> t = "caml_f32_of_string"
  external add : t -> t -> t = "caml_f32_add"
  external sub : t -> t -> t = "caml_f32_sub"
  external mul : t -> t -> t = "caml_f32_mul"
  external div : t -> t -> t = "caml_f32_div"
  external rem : t -> t -> t = "caml_f32_rem"
  external to_bits : t -> int32 = "caml_f32_to_bits"
  external of_bits : int32 -> t = "caml_f32_of_bits"
  external to_float : t -> float = "caml_f32_to_float"
  external fpclass_raw : t -> int = "caml_f32_fpclass"
  external abs : t -> t = "caml_f32_abs"
  external round_raw : t -> int -> t = "caml_f32_round"
  external cos : t -> t = "caml_f32_cos"
  external sin : t -> t = "caml_f32_sin"

  let of_bits_z z = of_bits (Z.to_int32_unsigned z)
  let to_z t = Z.of_int32 (to_bits t)
  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f32_eq"
  external lt : t -> t -> bool = "caml_f32_lt"
  external le : t -> t -> bool = "caml_f32_le"

  let gt = Fun.flip lt
  let ge = Fun.flip le
  let equal = eq
  let compare l r = if lt l r then -1 else if gt l r then 1 else 0
  let bits_equal l r = to_bits l = to_bits r

  (* Float to integer conversion *)
  external to_int_raw : t -> int -> int -> bool -> int64 * int64 * bool
    = "caml_f32_to_int"

  let float2int t size mode ~signed =
    let low, high, success =
      to_int_raw t (int_of_int_size size) (int_of_rounding_mode mode) signed
    in
    if success then Some (int64_pair_to_z low high size ~signed) else None

  (* Integer to float conversion *)
  external of_int_raw : int64 -> int64 -> int -> int -> bool -> t
    = "caml_f32_of_int"

  let int2float z size mode ~signed =
    let low, high = z_to_int64_pair z size ~signed in
    of_int_raw low high (int_of_int_size size)
      (int_of_rounding_mode mode)
      signed

  let pp ft t = Format.fprintf ft "%f" (to_float t)
  let show t = Format.asprintf "%f" (to_float t)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

(* ============================================================================
 * F64 - Double-precision (64-bit) floating-point
 * ============================================================================ *)
module F64 = struct
  type t
  type bits = int64

  external of_string : string -> t = "caml_f64_of_string"
  external add : t -> t -> t = "caml_f64_add"
  external sub : t -> t -> t = "caml_f64_sub"
  external mul : t -> t -> t = "caml_f64_mul"
  external div : t -> t -> t = "caml_f64_div"
  external rem : t -> t -> t = "caml_f64_rem"
  external to_bits : t -> int64 = "caml_f64_to_bits"
  external of_bits : int64 -> t = "caml_f64_of_bits"
  external to_float : t -> float = "caml_f64_to_float"
  external fpclass_raw : t -> int = "caml_f64_fpclass"
  external abs : t -> t = "caml_f64_abs"
  external round_raw : t -> int -> t = "caml_f64_round"
  external cos : t -> t = "caml_f64_cos"
  external sin : t -> t = "caml_f64_sin"

  let of_bits_z z = of_bits (Z.to_int64_unsigned z)
  let to_z t = Z.of_int64 (to_bits t)
  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f64_eq"
  external lt : t -> t -> bool = "caml_f64_lt"
  external le : t -> t -> bool = "caml_f64_le"

  let gt = Fun.flip lt
  let ge = Fun.flip le
  let equal = eq
  let compare l r = if lt l r then -1 else if gt l r then 1 else 0
  let bits_equal l r = to_bits l = to_bits r

  (* Float to integer conversion *)
  external to_int_raw : t -> int -> int -> bool -> int64 * int64 * bool
    = "caml_f64_to_int"

  let float2int t size mode ~signed =
    let low, high, success =
      to_int_raw t (int_of_int_size size) (int_of_rounding_mode mode) signed
    in
    if success then Some (int64_pair_to_z low high size ~signed) else None

  (* Integer to float conversion *)
  external of_int_raw : int64 -> int64 -> int -> int -> bool -> t
    = "caml_f64_of_int"

  let int2float z size mode ~signed =
    let low, high = z_to_int64_pair z size ~signed in
    of_int_raw low high (int_of_int_size size)
      (int_of_rounding_mode mode)
      signed

  let pp ft t = Format.fprintf ft "%f" (to_float t)
  let show t = Format.asprintf "%f" (to_float t)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

(* ============================================================================
 * F128 - Quad-precision (128-bit) floating-point
 * ============================================================================ *)
module F128 = struct
  type t
  type bits = int64 * int64 (* low, high *)

  external of_string : string -> t = "caml_f128_of_string"
  external add : t -> t -> t = "caml_f128_add"
  external sub : t -> t -> t = "caml_f128_sub"
  external mul : t -> t -> t = "caml_f128_mul"
  external div : t -> t -> t = "caml_f128_div"
  external rem : t -> t -> t = "caml_f128_rem"
  external to_bits_low : t -> int64 = "caml_f128_to_bits_low"
  external to_bits_high : t -> int64 = "caml_f128_to_bits_high"
  external of_bits_raw : int64 -> int64 -> t = "caml_f128_of_bits"
  external to_float : t -> float = "caml_f128_to_float"
  external fpclass_raw : t -> int = "caml_f128_fpclass"
  external abs : t -> t = "caml_f128_abs"
  external round_raw : t -> int -> t = "caml_f128_round"
  external cos : t -> t = "caml_f128_cos"
  external sin : t -> t = "caml_f128_sin"

  let to_bits t = (to_bits_low t, to_bits_high t)
  let of_bits (low, high) = of_bits_raw low high

  let of_bits_z z =
    let low, high = z_to_int64_pair z Int128 ~signed:false in
    of_bits_raw low high

  (** Convert to Z.t representing the full 128-bit value. The result is: high *
      2^64 + low (treating low as unsigned) *)
  let to_z t =
    let low = to_bits_low t in
    let high = to_bits_high t in
    (* Convert low as unsigned 64-bit integer *)
    let z_low = Z.of_int64_unsigned low in
    (* high can be treated as signed since it's the upper bits *)
    let z_high = Z.of_int64 high in
    Z.add (Z.shift_left z_high 64) z_low

  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f128_eq"
  external lt : t -> t -> bool = "caml_f128_lt"
  external le : t -> t -> bool = "caml_f128_le"

  let gt = Fun.flip lt
  let ge = Fun.flip le
  let equal = eq
  let compare l r = if lt l r then -1 else if gt l r then 1 else 0

  let bits_equal l r =
    let ll, lh = to_bits l in
    let rl, rh = to_bits r in
    ll = rl && lh = rh

  (* Float to integer conversion *)
  external to_int_raw : t -> int -> int -> bool -> int64 * int64 * bool
    = "caml_f128_to_int"

  let float2int t size mode ~signed =
    let low, high, success =
      to_int_raw t (int_of_int_size size) (int_of_rounding_mode mode) signed
    in
    if success then Some (int64_pair_to_z low high size ~signed) else None

  (* Integer to float conversion *)
  external of_int_raw : int64 -> int64 -> int -> int -> bool -> t
    = "caml_f128_of_int"

  let int2float z size mode ~signed =
    let low, high = z_to_int64_pair z size ~signed in
    of_int_raw low high (int_of_int_size size)
      (int_of_rounding_mode mode)
      signed

  let pp ft t = Format.fprintf ft "%f" (to_float t)
  let show t = Format.asprintf "%f" (to_float t)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

module AnyFloat = struct
  type precision = F16 | F32 | F64 | F128
  type t = F16 of F16.t | F32 of F32.t | F64 of F64.t | F128 of F128.t

  type bits =
    | BitsF16 of F16.bits
    | BitsF32 of F32.bits
    | BitsF64 of F64.bits
    | BitsF128 of F128.bits

  let f16 x = F16 x
  let f32 x = F32 x
  let f64 x = F64 x
  let f128 x = F128 x

  let[@inline] unop_raw ~f16 ~f32 ~f64 ~f128 = function
    | F16 x -> f16 x
    | F32 x -> f32 x
    | F64 x -> f64 x
    | F128 x -> f128 x

  let[@inline] binop_raw ~f16 ~f32 ~f64 ~f128 a b =
    match (a, b) with
    | F16 x, F16 y -> f16 x y
    | F32 x, F32 y -> f32 x y
    | F64 x, F64 y -> f64 x y
    | F128 x, F128 y -> f128 x y
    | _ -> invalid_arg "Mismatched float precisions"

  let[@inline] unop ~f16 ~f32 ~f64 ~f128 =
    unop_raw
      ~f16:(fun x -> F16 (f16 x))
      ~f32:(fun x -> F32 (f32 x))
      ~f64:(fun x -> F64 (f64 x))
      ~f128:(fun x -> F128 (f128 x))

  let[@inline] binop ~f16 ~f32 ~f64 ~f128 a b =
    binop_raw
      ~f16:(fun x y -> F16 (f16 x y))
      ~f32:(fun x y -> F32 (f32 x y))
      ~f64:(fun x y -> F64 (f64 x y))
      ~f128:(fun x y -> F128 (f128 x y))
      a b

  let of_string : precision -> string -> t = function
    | F16 -> fun s -> F16 (F16.of_string s)
    | F32 -> fun s -> F32 (F32.of_string s)
    | F64 -> fun s -> F64 (F64.of_string s)
    | F128 -> fun s -> F128 (F128.of_string s)

  let precision : t -> precision = function
    | F16 _ -> F16
    | F32 _ -> F32
    | F64 _ -> F64
    | F128 _ -> F128

  let add = binop ~f16:F16.add ~f32:F32.add ~f64:F64.add ~f128:F128.add
  let sub = binop ~f16:F16.sub ~f32:F32.sub ~f64:F64.sub ~f128:F128.sub
  let mul = binop ~f16:F16.mul ~f32:F32.mul ~f64:F64.mul ~f128:F128.mul
  let div = binop ~f16:F16.div ~f32:F32.div ~f64:F64.div ~f128:F128.div
  let rem = binop ~f16:F16.rem ~f32:F32.rem ~f64:F64.rem ~f128:F128.rem
  let abs = unop ~f16:F16.abs ~f32:F32.abs ~f64:F64.abs ~f128:F128.abs
  let cos = unop ~f16:F16.cos ~f32:F32.cos ~f64:F64.cos ~f128:F128.cos
  let sin = unop ~f16:F16.sin ~f32:F32.sin ~f64:F64.sin ~f128:F128.sin

  let round mode =
    unop ~f16:(F16.round mode) ~f32:(F32.round mode) ~f64:(F64.round mode)
      ~f128:(F128.round mode)

  let of_bits : bits -> t = function
    | BitsF16 b -> F16 (F16.of_bits b)
    | BitsF32 b -> F32 (F32.of_bits b)
    | BitsF64 b -> F64 (F64.of_bits b)
    | BitsF128 b -> F128 (F128.of_bits b)

  let of_bits_z : precision -> Z.t -> t = function
    | F16 -> fun z -> F16 (F16.of_bits_z z)
    | F32 -> fun z -> F32 (F32.of_bits_z z)
    | F64 -> fun z -> F64 (F64.of_bits_z z)
    | F128 -> fun z -> F128 (F128.of_bits_z z)

  let to_bits : t -> bits = function
    | F16 x -> BitsF16 (F16.to_bits x)
    | F32 x -> BitsF32 (F32.to_bits x)
    | F64 x -> BitsF64 (F64.to_bits x)
    | F128 x -> BitsF128 (F128.to_bits x)

  let to_z : t -> Z.t =
    unop_raw ~f16:F16.to_z ~f32:F32.to_z ~f64:F64.to_z ~f128:F128.to_z

  let to_float : t -> float =
    unop_raw ~f16:F16.to_float ~f32:F32.to_float ~f64:F64.to_float
      ~f128:F128.to_float

  let fpclass : t -> fpclass =
    unop_raw ~f16:F16.fpclass ~f32:F32.fpclass ~f64:F64.fpclass
      ~f128:F128.fpclass

  let float2int t size mode ~signed =
    match t with
    | F16 x -> F16.float2int x size mode ~signed
    | F32 x -> F32.float2int x size mode ~signed
    | F64 x -> F64.float2int x size mode ~signed
    | F128 x -> F128.float2int x size mode ~signed

  let int2float z size (precision : precision) mode ~signed =
    match precision with
    | F16 -> F16 (F16.int2float z size mode ~signed)
    | F32 -> F32 (F32.int2float z size mode ~signed)
    | F64 -> F64 (F64.int2float z size mode ~signed)
    | F128 -> F128 (F128.int2float z size mode ~signed)

  let eq = binop_raw ~f16:F16.eq ~f32:F32.eq ~f64:F64.eq ~f128:F128.eq
  let lt = binop_raw ~f16:F16.lt ~f32:F32.lt ~f64:F64.lt ~f128:F128.lt
  let le = binop_raw ~f16:F16.le ~f32:F32.le ~f64:F64.le ~f128:F128.le
  let gt = binop_raw ~f16:F16.gt ~f32:F32.gt ~f64:F64.gt ~f128:F128.gt
  let ge = binop_raw ~f16:F16.ge ~f32:F32.ge ~f64:F64.ge ~f128:F128.ge

  let equal =
    binop_raw ~f16:F16.equal ~f32:F32.equal ~f64:F64.equal ~f128:F128.equal

  let compare =
    binop_raw ~f16:F16.compare ~f32:F32.compare ~f64:F64.compare
      ~f128:F128.compare

  let bits_equal =
    binop_raw ~f16:F16.bits_equal ~f32:F32.bits_equal ~f64:F64.bits_equal
      ~f128:F128.bits_equal

  let pp ft t =
    match t with
    | F16 x -> F16.pp ft x
    | F32 x -> F32.pp ft x
    | F64 x -> F64.pp ft x
    | F128 x -> F128.pp ft x

  let show = unop_raw ~f16:F16.show ~f32:F32.show ~f64:F64.show ~f128:F128.show
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end
