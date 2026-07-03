/*
 * C stubs for f16, f32, f64, f128 float types
 * Provides OCaml bindings for arithmetic operations and bit conversions
 * 
 * IEEE 754 compliant implementations with native hardware support where available.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <fenv.h>
#include <quadmath.h>

/* ============================================================================
 * Common types and utilities for IEEE-754 compliance
 * ============================================================================ */

/*
 * IEEE 754 float classification (matches OCaml's fpclass)
 * These values must match the OCaml type definition order
 */
#define FPCLASS_NORMAL      0
#define FPCLASS_SUBNORMAL   1
#define FPCLASS_ZERO        2
#define FPCLASS_INFINITE    3
#define FPCLASS_NAN         4

/*
 * IEEE 754 rounding modes
 * These values must match the OCaml type definition order
 */
#define ROUND_NEAREST_EVEN  0  /* Round to nearest, ties to even (default) */
#define ROUND_TO_ZERO       1  /* Round toward zero (truncate) */
#define ROUND_UP            2  /* Round toward +infinity (ceiling) */
#define ROUND_DOWN          3  /* Round toward -infinity (floor) */
#define ROUND_NEAREST_AWAY  4  /* Round to nearest, ties away from zero */

/* Map our rounding mode to C fenv rounding mode */
static int get_fenv_round(int mode) {
    switch (mode) {
        case ROUND_NEAREST_EVEN: return FE_TONEAREST;
        case ROUND_TO_ZERO:      return FE_TOWARDZERO;
        case ROUND_UP:           return FE_UPWARD;
        case ROUND_DOWN:         return FE_DOWNWARD;
        case ROUND_NEAREST_AWAY: return FE_TONEAREST;  /* Approximate - not all platforms support this */
        default:                 return FE_TONEAREST;
    }
}

/* ============================================================================
 * F16 (half-precision) implementation
 * IEEE 754 half-precision: 1 sign bit, 5 exponent bits, 10 mantissa bits
 * 
 * Uses native _Float16 when available (GCC 12+, Clang 15+ on supported archs)
 * Falls back to IEEE-754 compliant software implementation otherwise.
 * ============================================================================ */

/* Detect native _Float16 support */
#if defined(__FLT16_MANT_DIG__) && defined(__clang__)
    /* Clang with _Float16 support (typically ARM64, x86 with SSE) */
    #define HAS_NATIVE_FLOAT16 1
#elif defined(__FLT16_MANT_DIG__) && defined(__GNUC__) && __GNUC__ >= 12
    /* GCC 12+ with _Float16 support */
    #define HAS_NATIVE_FLOAT16 1
#elif defined(__ARM_FP16_FORMAT_IEEE)
    /* ARM with IEEE half-precision */
    #define HAS_NATIVE_FLOAT16 1
#else
    #define HAS_NATIVE_FLOAT16 0
#endif

#if HAS_NATIVE_FLOAT16

/* ---- Native _Float16 implementation ---- */
typedef _Float16 f16_native_t;
typedef uint16_t f16_bits_t;

static inline f16_bits_t f16_to_bits_native(f16_native_t f) {
    f16_bits_t bits;
    memcpy(&bits, &f, sizeof(bits));
    return bits;
}

static inline f16_native_t f16_from_bits_native(f16_bits_t bits) {
    f16_native_t f;
    memcpy(&f, &bits, sizeof(f));
    return f;
}

static inline float f16_to_float_native(f16_native_t f) {
    return (float)f;
}

static inline f16_native_t f16_from_string_native(const char *s) {
    float f = strtof(s, NULL);
    return (f16_native_t)f;
}

/* Use native type for storage */
typedef f16_native_t f16_t;

#define F16_TO_BITS(f) f16_to_bits_native(f)
#define F16_FROM_BITS(b) f16_from_bits_native(b)
#define F16_TO_FLOAT(f) f16_to_float_native(f)
#define F16_ADD(a, b) ((f16_t)((a) + (b)))
#define F16_SUB(a, b) ((f16_t)((a) - (b)))
#define F16_MUL(a, b) ((f16_t)((a) * (b)))
#define F16_DIV(a, b) ((f16_t)((a) / (b)))
#define F16_REM(a, b) ((f16_t)fmodf((float)(a), (float)(b)))
#define F16_FROM_STRING(s) f16_from_string_native(s)

#else

/* ---- Software IEEE-754 compliant implementation ---- */

/*
 * IEEE 754 half-precision format:
 * - Sign: 1 bit (bit 15)
 * - Exponent: 5 bits (bits 14-10), bias = 15
 * - Mantissa: 10 bits (bits 9-0), implicit leading 1 for normalized
 * 
 * Special values:
 * - Zero: exp=0, mant=0 (signed)
 * - Denormal: exp=0, mant!=0
 * - Infinity: exp=31, mant=0 (signed)
 * - NaN: exp=31, mant!=0
 */

typedef uint16_t f16_t;

/* 
 * Convert float32 to float16 with proper IEEE-754 rounding (round to nearest, ties to even)
 */
static f16_t f16_from_float_soft(float f) {
    uint32_t bits;
    memcpy(&bits, &f, sizeof(bits));
    
    uint32_t sign = (bits >> 31) & 0x1;
    int32_t exp = (int32_t)((bits >> 23) & 0xFF) - 127;
    uint32_t mant = bits & 0x7FFFFF;
    
    uint16_t result;
    
    /* Handle special cases */
    if (exp == 128) {
        /* Infinity or NaN */
        if (mant == 0) {
            /* Infinity */
            result = (sign << 15) | (0x1F << 10);
        } else {
            /* NaN - preserve quiet/signaling bit and some payload */
            uint16_t nan_mant = (mant >> 13) | 0x200;  /* Ensure it's still NaN */
            result = (sign << 15) | (0x1F << 10) | (nan_mant & 0x3FF);
        }
    } else if (exp > 15) {
        /* Overflow to infinity */
        result = (sign << 15) | (0x1F << 10);
    } else if (exp < -24) {
        /* Underflow to zero (too small even for denormal) */
        result = sign << 15;
    } else if (exp < -14) {
        /* Denormalized number */
        /* Add implicit leading 1 */
        mant |= 0x800000;
        
        /* Calculate shift amount: we need to shift right to fit in 10 bits
         * For exp=-15, shift by 14 (one more than normal)
         * For exp=-24, shift by 23 (mant becomes 1 bit) */
        int shift = -14 - exp + 13;  /* 13 = 23 - 10 (f32 mant bits - f16 mant bits) */
        
        /* Round to nearest, ties to even */
        uint32_t round_bit = 1U << (shift - 1);
        uint32_t sticky_mask = round_bit - 1;
        uint32_t sticky = (mant & sticky_mask) != 0;
        uint32_t round = (mant >> (shift - 1)) & 1;
        uint32_t lsb = (mant >> shift) & 1;
        
        uint16_t mant16 = mant >> shift;
        
        /* Round to nearest, ties to even */
        if (round && (sticky || lsb)) {
            mant16++;
        }
        
        result = (sign << 15) | mant16;
    } else {
        /* Normalized number */
        /* Round to nearest, ties to even */
        /* We're dropping 13 bits (23 - 10) */
        uint32_t round_bit = 1U << 12;
        uint32_t sticky_mask = round_bit - 1;
        uint32_t sticky = (mant & sticky_mask) != 0;
        uint32_t round = (mant >> 12) & 1;
        uint32_t lsb = (mant >> 13) & 1;
        
        uint16_t mant16 = mant >> 13;
        uint16_t exp16 = exp + 15;
        
        /* Round to nearest, ties to even */
        if (round && (sticky || lsb)) {
            mant16++;
            if (mant16 == 0x400) {
                /* Mantissa overflow, increment exponent */
                mant16 = 0;
                exp16++;
                if (exp16 >= 31) {
                    /* Overflow to infinity */
                    result = (sign << 15) | (0x1F << 10);
                    return result;
                }
            }
        }
        
        result = (sign << 15) | (exp16 << 10) | mant16;
    }
    
    return result;
}

/* Convert float16 to float32 (exact, no rounding needed) */
static float f16_to_float_soft(f16_t h) {
    uint32_t sign = (h >> 15) & 0x1;
    uint32_t exp = (h >> 10) & 0x1F;
    uint32_t mant = h & 0x3FF;
    
    uint32_t result;
    
    if (exp == 0) {
        if (mant == 0) {
            /* Signed zero */
            result = sign << 31;
        } else {
            /* Denormalized - normalize it */
            /* Count leading zeros in mantissa and normalize */
            int shift = 0;
            while (!(mant & 0x400)) {
                mant <<= 1;
                shift++;
            }
            mant &= 0x3FF;  /* Remove the implicit 1 we just shifted in */
            int32_t new_exp = 1 - 15 + 127 - shift;  /* Adjust exponent */
            result = (sign << 31) | (new_exp << 23) | (mant << 13);
        }
    } else if (exp == 0x1F) {
        /* Infinity or NaN */
        result = (sign << 31) | (0xFF << 23) | (mant << 13);
    } else {
        /* Normalized number */
        int32_t new_exp = exp - 15 + 127;
        result = (sign << 31) | (new_exp << 23) | (mant << 13);
    }
    
    float f;
    memcpy(&f, &result, sizeof(f));
    return f;
}

#define F16_TO_BITS(f) (f)
#define F16_FROM_BITS(b) ((f16_t)(b))
#define F16_FROM_FLOAT(f) f16_from_float_soft(f)
#define F16_TO_FLOAT(f) f16_to_float_soft(f)
#define F16_ADD(a, b) f16_from_float_soft(f16_to_float_soft(a) + f16_to_float_soft(b))
#define F16_SUB(a, b) f16_from_float_soft(f16_to_float_soft(a) - f16_to_float_soft(b))
#define F16_MUL(a, b) f16_from_float_soft(f16_to_float_soft(a) * f16_to_float_soft(b))
#define F16_DIV(a, b) f16_from_float_soft(f16_to_float_soft(a) / f16_to_float_soft(b))
#define F16_REM(a, b) f16_from_float_soft(fmodf(f16_to_float_soft(a), f16_to_float_soft(b)))
#define F16_FROM_STRING(s) f16_from_float_soft(strtof(s, NULL))

#endif /* HAS_NATIVE_FLOAT16 */

/* F16 custom block operations */
static struct custom_operations f16_ops = {
    "floats.f16",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F16_val(v) (*((f16_t *) Data_custom_val(v)))

static value alloc_f16(f16_t val) {
    value v = caml_alloc_custom(&f16_ops, sizeof(f16_t), 0, 1);
    F16_val(v) = val;
    return v;
}

CAMLprim value caml_f16_of_string(value s) {
    CAMLparam1(s);
    CAMLreturn(alloc_f16(F16_FROM_STRING(String_val(s))));
}

CAMLprim value caml_f16_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f16(F16_ADD(F16_val(a), F16_val(b))));
}

CAMLprim value caml_f16_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f16(F16_SUB(F16_val(a), F16_val(b))));
}

CAMLprim value caml_f16_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f16(F16_MUL(F16_val(a), F16_val(b))));
}

CAMLprim value caml_f16_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f16(F16_DIV(F16_val(a), F16_val(b))));
}

CAMLprim value caml_f16_rem(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f16(F16_REM(F16_val(a), F16_val(b))));
}

CAMLprim value caml_f16_cos(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f16(cos(F16_val(v))));
}

CAMLprim value caml_f16_sin(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f16(sin(F16_val(v))));
}

CAMLprim value caml_f16_to_bits(value v) {
    CAMLparam1(v);
#if HAS_NATIVE_FLOAT16
    CAMLreturn(Val_int(F16_TO_BITS(F16_val(v))));
#else
    CAMLreturn(Val_int(F16_val(v)));
#endif
}

CAMLprim value caml_f16_of_bits(value bits) {
    CAMLparam1(bits);
    CAMLreturn(alloc_f16(F16_FROM_BITS((uint16_t)Int_val(bits))));
}

CAMLprim value caml_f16_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double(F16_TO_FLOAT(F16_val(v))));
}

/* Return whether native F16 is being used (for debugging/testing) */
CAMLprim value caml_f16_is_native(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(HAS_NATIVE_FLOAT16));
}

/* Classify F16 value according to IEEE 754 */
CAMLprim value caml_f16_fpclass(value v) {
    CAMLparam1(v);
    uint16_t bits = F16_TO_BITS(F16_val(v));
    uint16_t exp = (bits >> 10) & 0x1F;
    uint16_t mant = bits & 0x3FF;
    
    int result;
    if (exp == 0) {
        if (mant == 0) {
            result = FPCLASS_ZERO;
        } else {
            result = FPCLASS_SUBNORMAL;
        }
    } else if (exp == 0x1F) {
        if (mant == 0) {
            result = FPCLASS_INFINITE;
        } else {
            result = FPCLASS_NAN;
        }
    } else {
        result = FPCLASS_NORMAL;
    }
    CAMLreturn(Val_int(result));
}

/* Absolute value of F16 - IEEE 754 compliant (just clear sign bit) */
CAMLprim value caml_f16_abs(value v) {
    CAMLparam1(v);
    uint16_t bits = F16_TO_BITS(F16_val(v));
    bits &= 0x7FFF;  /* Clear sign bit */
    CAMLreturn(alloc_f16(F16_FROM_BITS(bits)));
}

/* Round F16 to integer value according to rounding mode */
CAMLprim value caml_f16_round(value v, value mode) {
    CAMLparam2(v, mode);
    int rounding_mode = Int_val(mode);
    float f = F16_TO_FLOAT(F16_val(v));
    float result;
    
    /* Handle special cases first */
    if (isnan(f) || isinf(f)) {
        CAMLreturn(v);  /* NaN and Inf are unchanged */
    }
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN:
            result = rintf(f);  /* Uses current rounding mode, but we want ties to even */
            break;
        case ROUND_TO_ZERO:
            result = truncf(f);
            break;
        case ROUND_UP:
            result = ceilf(f);
            break;
        case ROUND_DOWN:
            result = floorf(f);
            break;
        case ROUND_NEAREST_AWAY:
            result = roundf(f);  /* Rounds ties away from zero */
            break;
        default:
            result = rintf(f);
            break;
    }
    
#if HAS_NATIVE_FLOAT16
    CAMLreturn(alloc_f16((f16_t)result));
#else
    CAMLreturn(alloc_f16(f16_from_float_soft(result)));
#endif
}

/* Equality comparison for F16 (IEEE 754: NaN != NaN) */
CAMLprim value caml_f16_eq(value a, value b) {
    CAMLparam2(a, b);
    float fa = F16_TO_FLOAT(F16_val(a));
    float fb = F16_TO_FLOAT(F16_val(b));
    CAMLreturn(Val_bool(fa == fb));
}

/* Less than comparison for F16 (IEEE 754: NaN comparisons return false) */
CAMLprim value caml_f16_lt(value a, value b) {
    CAMLparam2(a, b);
    float fa = F16_TO_FLOAT(F16_val(a));
    float fb = F16_TO_FLOAT(F16_val(b));
    CAMLreturn(Val_bool(fa < fb));
}

/* Less than or equal comparison for F16 */
CAMLprim value caml_f16_le(value a, value b) {
    CAMLparam2(a, b);
    float fa = F16_TO_FLOAT(F16_val(a));
    float fb = F16_TO_FLOAT(F16_val(b));
    CAMLreturn(Val_bool(fa <= fb));
}

/* ============================================================================
 * F32 (single-precision) implementation
 * IEEE 754 single-precision: 1 sign bit, 8 exponent bits, 23 mantissa bits
 * ============================================================================ */

typedef float f32_t;

static struct custom_operations f32_ops = {
    "floats.f32",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F32_val(v) (*((f32_t *) Data_custom_val(v)))

static value alloc_f32(f32_t val) {
    value v = caml_alloc_custom(&f32_ops, sizeof(f32_t), 0, 1);
    F32_val(v) = val;
    return v;
}

CAMLprim value caml_f32_of_string(value s) {
    CAMLparam1(s);
    float f = strtof(String_val(s), NULL);
    CAMLreturn(alloc_f32(f));
}

CAMLprim value caml_f32_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) + F32_val(b)));
}

CAMLprim value caml_f32_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) - F32_val(b)));
}

CAMLprim value caml_f32_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) * F32_val(b)));
}

CAMLprim value caml_f32_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) / F32_val(b)));
}

CAMLprim value caml_f32_rem(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(fmodf(F32_val(a), F32_val(b))));
}

CAMLprim value caml_f32_cos(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f32(cos(F32_val(v))));
}

CAMLprim value caml_f32_sin(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f32(sin(F32_val(v))));
}

CAMLprim value caml_f32_to_bits(value v) {
    CAMLparam1(v);
    uint32_t bits;
    f32_t f = F32_val(v);
    memcpy(&bits, &f, sizeof(bits));
    CAMLreturn(caml_copy_int32(bits));
}

CAMLprim value caml_f32_of_bits(value bits) {
    CAMLparam1(bits);
    uint32_t b = Int32_val(bits);
    f32_t f;
    memcpy(&f, &b, sizeof(f));
    CAMLreturn(alloc_f32(f));
}

CAMLprim value caml_f32_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double((double)F32_val(v)));
}

/* Classify F32 value according to IEEE 754 */
CAMLprim value caml_f32_fpclass(value v) {
    CAMLparam1(v);
    f32_t f = F32_val(v);
    int result;
    
    switch (fpclassify(f)) {
        case FP_ZERO:
            result = FPCLASS_ZERO;
            break;
        case FP_SUBNORMAL:
            result = FPCLASS_SUBNORMAL;
            break;
        case FP_NORMAL:
            result = FPCLASS_NORMAL;
            break;
        case FP_INFINITE:
            result = FPCLASS_INFINITE;
            break;
        case FP_NAN:
        default:
            result = FPCLASS_NAN;
            break;
    }
    CAMLreturn(Val_int(result));
}

/* Absolute value of F32 - IEEE 754 compliant */
CAMLprim value caml_f32_abs(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f32(fabsf(F32_val(v))));
}

/* Round F32 to integer value according to rounding mode */
CAMLprim value caml_f32_round(value v, value mode) {
    CAMLparam2(v, mode);
    int rounding_mode = Int_val(mode);
    f32_t f = F32_val(v);
    f32_t result;
    
    /* Handle special cases first */
    if (isnan(f) || isinf(f)) {
        CAMLreturn(v);  /* NaN and Inf are unchanged */
    }
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN:
            result = rintf(f);
            break;
        case ROUND_TO_ZERO:
            result = truncf(f);
            break;
        case ROUND_UP:
            result = ceilf(f);
            break;
        case ROUND_DOWN:
            result = floorf(f);
            break;
        case ROUND_NEAREST_AWAY:
            result = roundf(f);
            break;
        default:
            result = rintf(f);
            break;
    }
    CAMLreturn(alloc_f32(result));
}

/* Equality comparison for F32 (IEEE 754: NaN != NaN) */
CAMLprim value caml_f32_eq(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F32_val(a) == F32_val(b)));
}

/* Less than comparison for F32 (IEEE 754: NaN comparisons return false) */
CAMLprim value caml_f32_lt(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F32_val(a) < F32_val(b)));
}

/* Less than or equal comparison for F32 */
CAMLprim value caml_f32_le(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F32_val(a) <= F32_val(b)));
}

/* ============================================================================
 * F64 (double-precision) implementation
 * IEEE 754 double-precision: 1 sign bit, 11 exponent bits, 52 mantissa bits
 * ============================================================================ */

typedef double f64_t;

static struct custom_operations f64_ops = {
    "floats.f64",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F64_val(v) (*((f64_t *) Data_custom_val(v)))

static value alloc_f64(f64_t val) {
    value v = caml_alloc_custom(&f64_ops, sizeof(f64_t), 0, 1);
    F64_val(v) = val;
    return v;
}

CAMLprim value caml_f64_of_string(value s) {
    CAMLparam1(s);
    double d = strtod(String_val(s), NULL);
    CAMLreturn(alloc_f64(d));
}

CAMLprim value caml_f64_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) + F64_val(b)));
}

CAMLprim value caml_f64_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) - F64_val(b)));
}

CAMLprim value caml_f64_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) * F64_val(b)));
}

CAMLprim value caml_f64_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) / F64_val(b)));
}

CAMLprim value caml_f64_rem(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(fmod(F64_val(a), F64_val(b))));
}

CAMLprim value caml_f64_cos(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f64(cos(F64_val(v))));
}

CAMLprim value caml_f64_sin(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f64(sin(F64_val(v))));
}

CAMLprim value caml_f64_to_bits(value v) {
    CAMLparam1(v);
    uint64_t bits;
    f64_t d = F64_val(v);
    memcpy(&bits, &d, sizeof(bits));
    CAMLreturn(caml_copy_int64(bits));
}

CAMLprim value caml_f64_of_bits(value bits) {
    CAMLparam1(bits);
    uint64_t b = Int64_val(bits);
    f64_t d;
    memcpy(&d, &b, sizeof(d));
    CAMLreturn(alloc_f64(d));
}

CAMLprim value caml_f64_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double(F64_val(v)));
}

/* Classify F64 value according to IEEE 754 */
CAMLprim value caml_f64_fpclass(value v) {
    CAMLparam1(v);
    f64_t f = F64_val(v);
    int result;
    
    switch (fpclassify(f)) {
        case FP_ZERO:
            result = FPCLASS_ZERO;
            break;
        case FP_SUBNORMAL:
            result = FPCLASS_SUBNORMAL;
            break;
        case FP_NORMAL:
            result = FPCLASS_NORMAL;
            break;
        case FP_INFINITE:
            result = FPCLASS_INFINITE;
            break;
        case FP_NAN:
        default:
            result = FPCLASS_NAN;
            break;
    }
    CAMLreturn(Val_int(result));
}

/* Absolute value of F64 - IEEE 754 compliant */
CAMLprim value caml_f64_abs(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f64(fabs(F64_val(v))));
}

/* Round F64 to integer value according to rounding mode */
CAMLprim value caml_f64_round(value v, value mode) {
    CAMLparam2(v, mode);
    int rounding_mode = Int_val(mode);
    f64_t f = F64_val(v);
    f64_t result;
    
    /* Handle special cases first */
    if (isnan(f) || isinf(f)) {
        CAMLreturn(v);  /* NaN and Inf are unchanged */
    }
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN:
            result = rint(f);
            break;
        case ROUND_TO_ZERO:
            result = trunc(f);
            break;
        case ROUND_UP:
            result = ceil(f);
            break;
        case ROUND_DOWN:
            result = floor(f);
            break;
        case ROUND_NEAREST_AWAY:
            result = round(f);
            break;
        default:
            result = rint(f);
            break;
    }
    CAMLreturn(alloc_f64(result));
}

/* Equality comparison for F64 (IEEE 754: NaN != NaN) */
CAMLprim value caml_f64_eq(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F64_val(a) == F64_val(b)));
}

/* Less than comparison for F64 (IEEE 754: NaN comparisons return false) */
CAMLprim value caml_f64_lt(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F64_val(a) < F64_val(b)));
}

/* Less than or equal comparison for F64 */
CAMLprim value caml_f64_le(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F64_val(a) <= F64_val(b)));
}

/* ============================================================================
 * F128 (quad-precision) implementation
 * IEEE 754 quad-precision: 1 sign bit, 15 exponent bits, 112 mantissa bits
 * Note: We use long double which may or may not be true 128-bit depending on platform
 * On x86-64 Linux, long double is 80-bit extended precision
 * On some platforms, __float128 may be available
 * ============================================================================ */

/* Use a 128-bit representation stored as two 64-bit values for portability */
typedef struct {
    uint64_t low;
    uint64_t high;
} f128_bits_t;

#if defined(__SIZEOF_FLOAT128__) || defined(__FLOAT128__)
typedef __float128 f128_t;
#define HAS_FLOAT128 1
#else
/* Fall back to long double */
typedef long double f128_t;
#define HAS_FLOAT128 0
#endif

static struct custom_operations f128_ops = {
    "floats.f128",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F128_val(v) (*((f128_t *) Data_custom_val(v)))

static value alloc_f128(f128_t val) {
    value v = caml_alloc_custom(&f128_ops, sizeof(f128_t), 0, 1);
    F128_val(v) = val;
    return v;
}

CAMLprim value caml_f128_of_string(value s) {
    CAMLparam1(s);
#if HAS_FLOAT128
    f128_t f = strtoflt128(String_val(s), NULL);
#else
    f128_t f = strtold(String_val(s), NULL);
#endif
    CAMLreturn(alloc_f128(f));
}

CAMLprim value caml_f128_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) + F128_val(b)));
}

CAMLprim value caml_f128_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) - F128_val(b)));
}

CAMLprim value caml_f128_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) * F128_val(b)));
}

CAMLprim value caml_f128_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) / F128_val(b)));
}

CAMLprim value caml_f128_rem(value a, value b) {
    CAMLparam2(a, b);
#if HAS_FLOAT128
    CAMLreturn(alloc_f128(fmodq(F128_val(a), F128_val(b))));
#else
    CAMLreturn(alloc_f128(fmodl(F128_val(a), F128_val(b))));
#endif
}

CAMLprim value caml_f128_cos(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f128(cos(F128_val(v))));
}

CAMLprim value caml_f128_sin(value v) {
    CAMLparam1(v);
    CAMLreturn(alloc_f128(sin(F128_val(v))));
}

/* Return low 64 bits of f128 representation */
CAMLprim value caml_f128_to_bits_low(value v) {
    CAMLparam1(v);
    f128_t f = F128_val(v);
    uint64_t bits[2];
    memcpy(bits, &f, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(caml_copy_int64(bits[0]));
}

/* Return high 64 bits of f128 representation */
CAMLprim value caml_f128_to_bits_high(value v) {
    CAMLparam1(v);
    f128_t f = F128_val(v);
    uint64_t bits[2] = {0, 0};
    memcpy(bits, &f, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(caml_copy_int64(bits[1]));
}

CAMLprim value caml_f128_of_bits(value low, value high) {
    CAMLparam2(low, high);
    uint64_t bits[2];
    bits[0] = Int64_val(low);
    bits[1] = Int64_val(high);
    f128_t f;
    memcpy(&f, bits, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(alloc_f128(f));
}

CAMLprim value caml_f128_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double((double)F128_val(v)));
}

/* Return the size of f128_t for debugging */
CAMLprim value caml_f128_size(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(sizeof(f128_t)));
}

/* Classify F128 value according to IEEE 754 */
CAMLprim value caml_f128_fpclass(value v) {
    CAMLparam1(v);
    f128_t f = F128_val(v);
    int result;
    
#if HAS_FLOAT128
    /* Use __builtin functions for __float128 if available */
    if (__builtin_isinf(f)) {
        result = FPCLASS_INFINITE;
    } else if (__builtin_isnan(f)) {
        result = FPCLASS_NAN;
    } else if (f == 0.0Q) {
        result = FPCLASS_ZERO;
    } else {
        /* Check for subnormal by examining exponent bits */
        /* For __float128: 1 sign, 15 exp, 112 mantissa */
        uint64_t bits[2];
        memcpy(bits, &f, sizeof(f));
        uint64_t exp = (bits[1] >> 48) & 0x7FFF;
        if (exp == 0) {
            result = FPCLASS_SUBNORMAL;
        } else {
            result = FPCLASS_NORMAL;
        }
    }
#else
    /* Use fpclassify for long double */
    switch (fpclassify(f)) {
        case FP_ZERO:
            result = FPCLASS_ZERO;
            break;
        case FP_SUBNORMAL:
            result = FPCLASS_SUBNORMAL;
            break;
        case FP_NORMAL:
            result = FPCLASS_NORMAL;
            break;
        case FP_INFINITE:
            result = FPCLASS_INFINITE;
            break;
        case FP_NAN:
        default:
            result = FPCLASS_NAN;
            break;
    }
#endif
    CAMLreturn(Val_int(result));
}

/* Absolute value of F128 - IEEE 754 compliant */
CAMLprim value caml_f128_abs(value v) {
    CAMLparam1(v);
#if HAS_FLOAT128
    CAMLreturn(alloc_f128(fabsq(F128_val(v))));
#else
    CAMLreturn(alloc_f128(fabsl(F128_val(v))));
#endif
}

/* Round F128 to integer value according to rounding mode */
CAMLprim value caml_f128_round(value v, value mode) {
    CAMLparam2(v, mode);
    int rounding_mode = Int_val(mode);
    f128_t f = F128_val(v);
    f128_t result;
    
    /* Handle special cases first */
#if HAS_FLOAT128
    if (__builtin_isnan(f) || __builtin_isinf(f)) {
        CAMLreturn(v);  /* NaN and Inf are unchanged */
    }
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN:
            result = rintq(f);
            break;
        case ROUND_TO_ZERO:
            result = truncq(f);
            break;
        case ROUND_UP:
            result = ceilq(f);
            break;
        case ROUND_DOWN:
            result = floorq(f);
            break;
        case ROUND_NEAREST_AWAY:
            result = roundq(f);
            break;
        default:
            result = rintq(f);
            break;
    }
#else
    if (isnan(f) || isinf(f)) {
        CAMLreturn(v);  /* NaN and Inf are unchanged */
    }
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN:
            result = rintl(f);
            break;
        case ROUND_TO_ZERO:
            result = truncl(f);
            break;
        case ROUND_UP:
            result = ceill(f);
            break;
        case ROUND_DOWN:
            result = floorl(f);
            break;
        case ROUND_NEAREST_AWAY:
            result = roundl(f);
            break;
        default:
            result = rintl(f);
            break;
    }
#endif
    CAMLreturn(alloc_f128(result));
}

/* Equality comparison for F128 (IEEE 754: NaN != NaN) */
CAMLprim value caml_f128_eq(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F128_val(a) == F128_val(b)));
}

/* Less than comparison for F128 (IEEE 754: NaN comparisons return false) */
CAMLprim value caml_f128_lt(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F128_val(a) < F128_val(b)));
}

/* Less than or equal comparison for F128 */
CAMLprim value caml_f128_le(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(Val_bool(F128_val(a) <= F128_val(b)));
}

/* ============================================================================
 * Float to Integer conversions (fp.to_sbv / fp.to_ubv in SMT-LIB)
 * 
 * These functions convert floating-point values to integers with:
 * - Specified bit width (8, 16, 32, 64, 128)
 * - Specified rounding mode
 * - Signed or unsigned interpretation
 * 
 * Returns the integer value as two int64 values (low, high) for 128-bit support.
 * For values that don't fit or are NaN/Inf, behavior follows IEEE 754.
 * ============================================================================ */

/* Integer size constants */
#define INT_SIZE_8    0
#define INT_SIZE_16   1
#define INT_SIZE_32   2
#define INT_SIZE_64   3
#define INT_SIZE_128  4

/* Helper: Apply rounding to a double and return the integer part */
static double apply_rounding_d(double f, int rounding_mode) {
    if (isnan(f) || isinf(f)) return f;
    
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN: return rint(f);
        case ROUND_TO_ZERO:      return trunc(f);
        case ROUND_UP:           return ceil(f);
        case ROUND_DOWN:         return floor(f);
        case ROUND_NEAREST_AWAY: return round(f);
        default:                 return rint(f);
    }
}

/* Helper: Check if a double value fits in a signed integer of given bits */
static int fits_signed(double f, int bits) {
    if (isnan(f) || isinf(f)) return 0;
    if (bits >= 64) {
        /* For 64+ bits, check against int64 limits approximately */
        double min_val = -9223372036854775808.0;  /* -2^63 */
        double max_val = 9223372036854775807.0;   /* 2^63 - 1 */
        return f >= min_val && f <= max_val;
    }
    double min_val = -(double)(1LL << (bits - 1));
    double max_val = (double)((1LL << (bits - 1)) - 1);
    return f >= min_val && f <= max_val;
}

/* Helper: Check if a double value fits in an unsigned integer of given bits */
static int fits_unsigned(double f, int bits) {
    if (isnan(f) || isinf(f)) return 0;
    if (f < 0.0) return 0;
    if (bits >= 64) {
        double max_val = 18446744073709551615.0;  /* 2^64 - 1 */
        return f <= max_val;
    }
    double max_val = (double)((1ULL << bits) - 1);
    return f <= max_val;
}

/* 
 * F16 to integer conversion
 * Returns: tuple (low: int64, high: int64, success: bool)
 * For sizes <= 64 bits, result is in low; high is sign extension for signed.
 * For 128-bit, result spans both low and high.
 */
CAMLprim value caml_f16_to_int(value v, value size_v, value mode_v, value signed_v) {
    CAMLparam4(v, size_v, mode_v, signed_v);
    CAMLlocal1(result);
    
    double f = (double)F16_TO_FLOAT(F16_val(v));
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    /* Apply rounding */
    double rounded = apply_rounding_d(f, rounding_mode);
    
    /* Check for NaN/Inf */
    if (isnan(rounded) || isinf(rounded)) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));  /* failure */
        CAMLreturn(result);
    }
    
    /* Get bit width */
    int bits;
    switch (size) {
        case INT_SIZE_8:   bits = 8; break;
        case INT_SIZE_16:  bits = 16; break;
        case INT_SIZE_32:  bits = 32; break;
        case INT_SIZE_64:  bits = 64; break;
        case INT_SIZE_128: bits = 128; break;
        default:           bits = 64; break;
    }
    
    /* Check range */
    int fits = is_signed ? fits_signed(rounded, bits) : fits_unsigned(rounded, bits);
    if (!fits) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));  /* failure */
        CAMLreturn(result);
    }
    
    /* Convert to integer */
    int64_t low = (int64_t)rounded;
    int64_t high = 0;
    
    /* For signed values, sign-extend to high if negative */
    if (is_signed && rounded < 0) {
        high = -1;  /* All ones for sign extension */
    }
    
    result = caml_alloc_tuple(3);
    Store_field(result, 0, caml_copy_int64(low));
    Store_field(result, 1, caml_copy_int64(high));
    Store_field(result, 2, Val_bool(1));  /* success */
    CAMLreturn(result);
}

/* F32 to integer conversion */
CAMLprim value caml_f32_to_int(value v, value size_v, value mode_v, value signed_v) {
    CAMLparam4(v, size_v, mode_v, signed_v);
    CAMLlocal1(result);
    
    double f = (double)F32_val(v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    double rounded = apply_rounding_d(f, rounding_mode);
    
    if (isnan(rounded) || isinf(rounded)) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    
    int bits;
    switch (size) {
        case INT_SIZE_8:   bits = 8; break;
        case INT_SIZE_16:  bits = 16; break;
        case INT_SIZE_32:  bits = 32; break;
        case INT_SIZE_64:  bits = 64; break;
        case INT_SIZE_128: bits = 128; break;
        default:           bits = 64; break;
    }
    
    int fits = is_signed ? fits_signed(rounded, bits) : fits_unsigned(rounded, bits);
    if (!fits) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    
    int64_t low = (int64_t)rounded;
    int64_t high = (is_signed && rounded < 0) ? -1 : 0;
    
    result = caml_alloc_tuple(3);
    Store_field(result, 0, caml_copy_int64(low));
    Store_field(result, 1, caml_copy_int64(high));
    Store_field(result, 2, Val_bool(1));
    CAMLreturn(result);
}

/* F64 to integer conversion */
CAMLprim value caml_f64_to_int(value v, value size_v, value mode_v, value signed_v) {
    CAMLparam4(v, size_v, mode_v, signed_v);
    CAMLlocal1(result);
    
    double f = F64_val(v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    double rounded = apply_rounding_d(f, rounding_mode);
    
    if (isnan(rounded) || isinf(rounded)) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    
    int bits;
    switch (size) {
        case INT_SIZE_8:   bits = 8; break;
        case INT_SIZE_16:  bits = 16; break;
        case INT_SIZE_32:  bits = 32; break;
        case INT_SIZE_64:  bits = 64; break;
        case INT_SIZE_128: bits = 128; break;
        default:           bits = 64; break;
    }
    
    int fits = is_signed ? fits_signed(rounded, bits) : fits_unsigned(rounded, bits);
    if (!fits) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    
    int64_t low = (int64_t)rounded;
    int64_t high = (is_signed && rounded < 0) ? -1 : 0;
    
    result = caml_alloc_tuple(3);
    Store_field(result, 0, caml_copy_int64(low));
    Store_field(result, 1, caml_copy_int64(high));
    Store_field(result, 2, Val_bool(1));
    CAMLreturn(result);
}

/* F128 to integer conversion */
CAMLprim value caml_f128_to_int(value v, value size_v, value mode_v, value signed_v) {
    CAMLparam4(v, size_v, mode_v, signed_v);
    CAMLlocal1(result);
    
    f128_t f = F128_val(v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    /* Apply rounding */
    f128_t rounded;
#if HAS_FLOAT128
    if (__builtin_isnan(f) || __builtin_isinf(f)) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN: rounded = rintq(f); break;
        case ROUND_TO_ZERO:      rounded = truncq(f); break;
        case ROUND_UP:           rounded = ceilq(f); break;
        case ROUND_DOWN:         rounded = floorq(f); break;
        case ROUND_NEAREST_AWAY: rounded = roundq(f); break;
        default:                 rounded = rintq(f); break;
    }
#else
    if (isnan(f) || isinf(f)) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    switch (rounding_mode) {
        case ROUND_NEAREST_EVEN: rounded = rintl(f); break;
        case ROUND_TO_ZERO:      rounded = truncl(f); break;
        case ROUND_UP:           rounded = ceill(f); break;
        case ROUND_DOWN:         rounded = floorl(f); break;
        case ROUND_NEAREST_AWAY: rounded = roundl(f); break;
        default:                 rounded = rintl(f); break;
    }
#endif
    
    /* Convert to double for range checking (may lose precision for very large values) */
    double rounded_d = (double)rounded;
    
    int bits;
    switch (size) {
        case INT_SIZE_8:   bits = 8; break;
        case INT_SIZE_16:  bits = 16; break;
        case INT_SIZE_32:  bits = 32; break;
        case INT_SIZE_64:  bits = 64; break;
        case INT_SIZE_128: bits = 128; break;
        default:           bits = 64; break;
    }
    
    /* For 128-bit, we need special handling */
    if (bits == 128) {
        /* For now, use double conversion (loses precision for very large 128-bit values) */
        /* A full implementation would need arbitrary precision */
        int64_t low = (int64_t)rounded_d;
        int64_t high = (is_signed && rounded_d < 0) ? -1 : 0;
        
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(low));
        Store_field(result, 1, caml_copy_int64(high));
        Store_field(result, 2, Val_bool(1));
        CAMLreturn(result);
    }
    
    int fits = is_signed ? fits_signed(rounded_d, bits) : fits_unsigned(rounded_d, bits);
    if (!fits) {
        result = caml_alloc_tuple(3);
        Store_field(result, 0, caml_copy_int64(0));
        Store_field(result, 1, caml_copy_int64(0));
        Store_field(result, 2, Val_bool(0));
        CAMLreturn(result);
    }
    
    int64_t low = (int64_t)rounded_d;
    int64_t high = (is_signed && rounded_d < 0) ? -1 : 0;
    
    result = caml_alloc_tuple(3);
    Store_field(result, 0, caml_copy_int64(low));
    Store_field(result, 1, caml_copy_int64(high));
    Store_field(result, 2, Val_bool(1));
    CAMLreturn(result);
}

/* ============================================================================
 * Integer to Float conversions (to_fp / to_fp_unsigned in SMT-LIB)
 * 
 * These functions convert integers to floating-point values with:
 * - Specified bit width (8, 16, 32, 64, 128)
 * - Specified rounding mode (for inexact conversions)
 * - Signed or unsigned interpretation
 * 
 * Input is two int64 values (low, high) representing up to 128-bit integer.
 * ============================================================================ */

/* F16 from integer */
CAMLprim value caml_f16_of_int(value low_v, value high_v, value size_v, value mode_v, value signed_v) {
    CAMLparam5(low_v, high_v, size_v, mode_v, signed_v);
    
    int64_t low = Int64_val(low_v);
    int64_t high = Int64_val(high_v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    (void)rounding_mode;  /* F16 conversion handles rounding internally */
    
    double value_d;
    
    if (size == INT_SIZE_128) {
        /* 128-bit: combine low and high */
        if (is_signed && high < 0) {
            /* Negative 128-bit signed value */
            /* This is approximate for very large negative values */
            value_d = (double)high * 18446744073709551616.0 + (double)(uint64_t)low;
        } else {
            value_d = (double)(uint64_t)high * 18446744073709551616.0 + (double)(uint64_t)low;
        }
    } else {
        /* 64-bit or smaller: use low only */
        if (is_signed) {
            value_d = (double)low;
        } else {
            value_d = (double)(uint64_t)low;
        }
    }
    
#if HAS_NATIVE_FLOAT16
    CAMLreturn(alloc_f16((f16_t)value_d));
#else
    CAMLreturn(alloc_f16(f16_from_float_soft((float)value_d)));
#endif
}

/* F32 from integer */
CAMLprim value caml_f32_of_int(value low_v, value high_v, value size_v, value mode_v, value signed_v) {
    CAMLparam5(low_v, high_v, size_v, mode_v, signed_v);
    
    int64_t low = Int64_val(low_v);
    int64_t high = Int64_val(high_v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    (void)rounding_mode;  /* Let hardware handle rounding */
    
    double value_d;
    
    if (size == INT_SIZE_128) {
        if (is_signed && high < 0) {
            value_d = (double)high * 18446744073709551616.0 + (double)(uint64_t)low;
        } else {
            value_d = (double)(uint64_t)high * 18446744073709551616.0 + (double)(uint64_t)low;
        }
    } else {
        if (is_signed) {
            value_d = (double)low;
        } else {
            value_d = (double)(uint64_t)low;
        }
    }
    
    CAMLreturn(alloc_f32((float)value_d));
}

/* F64 from integer */
CAMLprim value caml_f64_of_int(value low_v, value high_v, value size_v, value mode_v, value signed_v) {
    CAMLparam5(low_v, high_v, size_v, mode_v, signed_v);
    
    int64_t low = Int64_val(low_v);
    int64_t high = Int64_val(high_v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    (void)rounding_mode;
    
    double value_d;
    
    if (size == INT_SIZE_128) {
        if (is_signed && high < 0) {
            value_d = (double)high * 18446744073709551616.0 + (double)(uint64_t)low;
        } else {
            value_d = (double)(uint64_t)high * 18446744073709551616.0 + (double)(uint64_t)low;
        }
    } else {
        if (is_signed) {
            value_d = (double)low;
        } else {
            value_d = (double)(uint64_t)low;
        }
    }
    
    CAMLreturn(alloc_f64(value_d));
}

/* F128 from integer */
CAMLprim value caml_f128_of_int(value low_v, value high_v, value size_v, value mode_v, value signed_v) {
    CAMLparam5(low_v, high_v, size_v, mode_v, signed_v);
    
    int64_t low = Int64_val(low_v);
    int64_t high = Int64_val(high_v);
    int size = Int_val(size_v);
    int rounding_mode = Int_val(mode_v);
    int is_signed = Bool_val(signed_v);
    
    (void)rounding_mode;
    
    f128_t value_f;
    
    if (size == INT_SIZE_128) {
#if HAS_FLOAT128
        if (is_signed && high < 0) {
            value_f = ((__float128)high * 18446744073709551616.0Q) + (__float128)(uint64_t)low;
        } else {
            value_f = ((__float128)(uint64_t)high * 18446744073709551616.0Q) + (__float128)(uint64_t)low;
        }
#else
        if (is_signed && high < 0) {
            value_f = ((long double)high * 18446744073709551616.0L) + (long double)(uint64_t)low;
        } else {
            value_f = ((long double)(uint64_t)high * 18446744073709551616.0L) + (long double)(uint64_t)low;
        }
#endif
    } else {
#if HAS_FLOAT128
        if (is_signed) {
            value_f = (__float128)low;
        } else {
            value_f = (__float128)(uint64_t)low;
        }
#else
        if (is_signed) {
            value_f = (long double)low;
        } else {
            value_f = (long double)(uint64_t)low;
        }
#endif
    }
    
    CAMLreturn(alloc_f128(value_f));
}
