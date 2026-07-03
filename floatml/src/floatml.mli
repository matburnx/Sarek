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

module type FloatType := sig
  type t
  type bits

  (** Convert a string to an F16 value *)
  val of_string : string -> t

  (** Addition *)
  val add : t -> t -> t

  (** Subtraction *)
  val sub : t -> t -> t

  (** Multiplication *)
  val mul : t -> t -> t

  (** Division *)
  val div : t -> t -> t

  (** Remainder *)
  val rem : t -> t -> t

  (** Cosine *)
  val cos : t -> t

  (* Sine *)
  val sin : t -> t

  (** Convert from some bit representation *)
  val of_bits : bits -> t

  (** Convert from a bit representation in a Z.t *)
  val of_bits_z : Z.t -> t

  (** Convert to some bit representation *)
  val to_bits : t -> bits

  (** Convert to Z.t (exact bit representation) *)
  val to_z : t -> Z.t

  (** Convert to OCaml float for display *)
  val to_float : t -> float

  (** Classify a floating-point value according to IEEE 754 *)
  val fpclass : t -> Stdlib.fpclass

  (** Absolute value (IEEE 754 compliant - clears sign bit) *)
  val abs : t -> t

  (** Round to integer value according to rounding mode *)
  val round : rounding_mode -> t -> t

  (** Equality comparison (IEEE 754: NaN != NaN) *)
  val eq : t -> t -> bool

  (** Alias for [eq] *)
  val equal : t -> t -> bool

  (** Bitwise equality comparison (NaNs are equal if their bit patterns match)
  *)
  val bits_equal : t -> t -> bool

  (** Less than comparison (IEEE 754: NaN comparisons return false) *)
  val lt : t -> t -> bool

  (** Less than or equal comparison (IEEE 754: NaN comparisons return false) *)
  val le : t -> t -> bool

  (** Greater than comparison (IEEE 754: NaN comparisons return false) *)
  val gt : t -> t -> bool

  (** Greater than or equal comparison (IEEE 754: NaN comparisons return false)
  *)
  val ge : t -> t -> bool

  (** Total ordering comparison; returns -1 if x < y, 1 if x > y, and 0 otherwise.
    {bThis is not IEEE 754 compliant, it is provided for convenience only.} *)
  val compare : t -> t -> int

  (** Convert float to integer with specified size, rounding mode, and
      signedness. Similar to SMT-LIB's fp.to_sbv (signed) and fp.to_ubv
      (unsigned). Returns None if the value is NaN, Inf, or out of range for the
      target type. *)
  val float2int : t -> int_size -> rounding_mode -> signed:bool -> Z.t option

  (** Convert integer to float with specified size, rounding mode, and
      signedness. Similar to SMT-LIB's to_fp from signed/unsigned bitvector. The
      integer is interpreted according to the specified size and signedness. *)
  val int2float : Z.t -> int_size -> rounding_mode -> signed:bool -> t

  (** Pretty-printing *)
  val pp : Format.formatter -> t -> unit

  val show : t -> string

  (* Infix operators *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

(** F16 - IEEE 754 half-precision (16-bit) floating-point type

    Uses native _Float16 hardware support when available (modern ARM64, x86 with
    AVX512-FP16). Falls back to IEEE-754 compliant software emulation with
    proper rounding otherwise. *)
module F16 : FloatType with type bits = int

(** F32 - IEEE 754 single-precision (32-bit) floating-point type *)
module F32 : FloatType with type bits = int32

(** F64 - IEEE 754 double-precision (64-bit) floating-point type *)
module F64 : FloatType with type bits = int64

(** F128 - IEEE 754 quad-precision (128-bit) floating-point type Note:
    Implementation may use long double if native __float128 is unavailable *)
module F128 : FloatType with type bits = int64 * int64

(** AnyFloat - A variant type that can hold any of the supported float types *)
module AnyFloat : sig
  type precision = F16 | F32 | F64 | F128
  type t = F16 of F16.t | F32 of F32.t | F64 of F64.t | F128 of F128.t

  type bits =
    | BitsF16 of F16.bits
    | BitsF32 of F32.bits
    | BitsF64 of F64.bits
    | BitsF128 of F128.bits

  include FloatType with type t := t and type bits := bits

  val f16 : F16.t -> t
  val f32 : F32.t -> t
  val f64 : F64.t -> t
  val f128 : F128.t -> t
  val of_string : precision -> string -> t
  val of_bits_z : precision -> Z.t -> t

  val int2float :
    Z.t -> int_size -> precision -> rounding_mode -> signed:bool -> t

  (** Get the precision of the float type *)
  val precision : t -> precision
end
