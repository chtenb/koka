/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#include "kklib.h"

#include <stdio.h>
#include <math.h>   // INFINITY

/*----------------------------------------------------------------------
Big naturals. For our purposes, we need an implementation that does not
have to be the fastest possible; we instead aim for portable, simple,
well performing, and with fast bitwise operations.
Still, it performs quite respectable and does have various optimizations
including Karatsuba multiplication.

  Big naturals are arrays of `digits` with a `count` flag.
  For a number `n` we have:

  n = (digits[0]*(BASE^0) + digits[1]*(BASE^1) + ... + digits[count-1]*(BASE^(count-1)))

  For any `count>0`, we have `digits[count-1] != 0`.
  We use 32-bit or 64-bit integers for the digits depending on the platform, this way:
  - we can use base 2^30 or 2^62
  - it can hold `2*BASE + 1` which allows for efficient addition with
    portable overflow detection.
  - a double digit `kk_ddigit_t` of 64/128-bit can hold a full multiply
    of `BASE*BASE + BASE + 1` which allows efficient multiplication with
    portable overflow detection.
----------------------------------------------------------------------*/

#if (KK_INTPTR_SIZE>=8) && defined(_MSC_VER) && (_MSC_VER >= 1920) && !defined(__clang_msvc__) /* not clang-cl or we get link errors */
// Use 64-bit digits on Microsoft VisualC
#define BASE          KK_I64(4611686018427387904) // 2^62
#define DIGIT_BITS    (64)
typedef uint64_t      kk_digit_t;     // 2*BASE + 1 < kk_digit_t_max

typedef struct kk_ddigit_s {
  uint64_t hi;
  uint64_t lo;
} kk_ddigit_t;

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  kk_assert_internal(divisor >= 0);
  kk_digit_t tmp;
  if (rem==NULL) rem = &tmp;
  if (d.hi==0 && d.lo < divisor) {  // common case
    *rem = (kk_digit_t)d.lo;
    return 0;
  }
  return _udiv128(d.hi, d.lo, divisor, rem);
}

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  kk_ddigit_t r;
  r.lo = _umul128(x, y, &r.hi);
  if (z > 0) {
    if (r.lo > (UINT64_MAX - z)) {
      r.hi++;
    }
    r.lo += z;
  }
  return r;
}

#elif (KK_INTPTR_SIZE >= 8) && defined(__GNUC__) && defined(__SIZEOF_INT128__)
// Use 64-bit digits with gcc/clang/icc
#define BASE          KK_I64(1000000000000000000)
#define DIGIT_BITS    (64)
typedef uint64_t      kk_digit_t;     // 2*BASE + 1 < kk_digit_t_max

__extension__ typedef unsigned __int128 kk_ddigit_t;

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  if (d < divisor) {
    if (rem!=NULL) *rem = (kk_digit_t)d;
    return 0;
  }
  if (rem!=NULL) *rem = (kk_digit_t)(d%divisor);
  return (kk_digit_t)(d/divisor);
}

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  return ((kk_ddigit_t)x * y) + z;
}

#else
// Default: use 32-bit digits
#if KK_INTPTR_SIZE > 4
#pragma message("using 32-bit digits for large natural number arithmetic")
#endif

#define BASE          KK_I32(1000000000)
#define DIGIT_BITS    (32)
typedef uint32_t      kk_digit_t;       // 2*BASE + 1 < kk_digit_t_max

typedef uint64_t      kk_ddigit_t;      // double digit for multiplies

static inline kk_ddigit_t ddigit_mul_add(kk_digit_t x, kk_digit_t y, kk_digit_t z) {
  return ((kk_ddigit_t)x * y) + z;
}

static inline kk_digit_t ddigit_cdiv(kk_ddigit_t d, kk_digit_t divisor, kk_digit_t* rem) {
  if (d < divisor) {
    if (rem!=NULL) *rem = (kk_digit_t)d;
    return 0;
  }
  if (rem!=NULL) *rem = (kk_digit_t)(d%divisor);
  return (kk_digit_t)(d/divisor);
}

#endif


typedef int16_t kk_extra_t;
#define MAX_EXTRA           (INT16_MAX)

// FIXNOW: why are the counts signed?
typedef struct kk_bignat_s {
  kk_block_t  _block;
#if KK_INTPTR_SIZE>=8
  int64_t     count  :48;      // count of digits in the number
  kk_extra_t  extra;      // extra digits available: `sizeof(digits) == (count+extra)*sizeof(kk_digit_t)`
#else
  int32_t     count;
  kk_extra_t  extra;
#endif
  kk_digit_t  digits[1];      // digits from least-significant to most significant.
} kk_bignat_t;

static kk_ssize_t bignat_count_(const kk_bignat_t* b) {
  return b->count;
}

static kk_ssize_t bignat_available_(const kk_bignat_t* b) {
  return (b->count + b->extra);
}

static kk_digit_t bignat_last_digit_(const kk_bignat_t* b) {
  return b->digits[b->count-1];
}

static kk_natural_t natural_bignat(kk_bignat_t* x, kk_context_t* ctx);


/*----------------------------------------------------------------------
  allocation, ref counts, trim
  functions ending in "_" do not change reference counts of input arguments
----------------------------------------------------------------------*/

static kk_ptr_t bignat_ptr_(kk_bignat_t* x) {
  return &x->_block;
}

// FIXNOW: we should either not use KK_TAG_VALUE for nats, or make sure it works everywhere (I'm pretty sure our masks only work with KK_TAG_VALUE=1)
static kk_natural_t bignat_as_natural_(kk_bignat_t* x, kk_context_t* ctx) {
  kk_natural_t i = { kk_ptr_encode(bignat_ptr_(x), ctx) };
#if KK_TAG_VALUE!=KK_TAG_VALUE
  i.ibox = i.ibox ^ 1;
#endif
  return i;
}

static bool bignat_is_unique_(kk_bignat_t* x) {
  return kk_block_is_unique(bignat_ptr_(x));
}

static kk_bignat_t* dup_bignat(kk_bignat_t* x, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_block_assert(kk_bignat_t*, kk_block_dup(&x->_block), KK_TAG_BIGnat);
}

static void drop_bignat(kk_bignat_t* x, kk_context_t* ctx) {
  kk_block_drop_assert(&x->_block,KK_TAG_BIGnat,ctx);
}

// FIXNOW: why must count always be even?
static kk_ssize_t bignat_roundup_count(kk_ssize_t count) {
  if (count*kk_ssizeof(kk_digit_t) < 16) return (16/kk_ssizeof(kk_digit_t));    // minimal size of 128-bit (= 16 bytes)
  else if ((count & 1) == 1) return (count+1);  // always even
  else return count;
}

static kk_bignat_t* bignat_alloc(kk_ssize_t count, kk_context_t* ctx) {
  kk_ssize_t dcount = bignat_roundup_count(count);
  kk_bignat_t* b = (kk_bignat_t*)kk_block_alloc_any(kk_ssizeof(kk_bignat_t) - kk_ssizeof(kk_digit_t) + dcount*kk_ssizeof(kk_digit_t), 0, KK_TAG_BIGINT, ctx);
  b->extra = (kk_extra_t)(dcount - count);
  b->count = count;
  return b;
}

static kk_bignat_t* bignat_alloc_zero(kk_ssize_t count, kk_context_t* ctx) {
  kk_bignat_t* b = bignat_alloc(count, ctx);
  kk_memset(b->digits, 0, kk_ssizeof(kk_digit_t) * bignat_available_(b));
  return b;
}

static kk_bignat_t* kk_bignat_trim_realloc_(kk_bignat_t* x, kk_ssize_t count, kk_context_t* ctx) {
  kk_assert_internal(bigint_is_unique_(x));
  kk_ssize_t dcount = bignat_roundup_count(count);
  kk_ssize_t xcount = bignat_available_(x);
  kk_bignat_t* b;
  if ((dcount <= xcount) && (xcount-dcount) < (16/kk_ssizeof(kk_digit_t))) {
    b = x; // avoid realloc if shrinking by less than 128 bits
    dcount = xcount;
  }
  else {
    b = (kk_bignat_t*)kk_block_realloc(bignat_ptr_(x), kk_ssizeof(kk_bignat_t) - kk_ssizeof(kk_digit_t) + dcount*kk_ssizeof(kk_digit_t), ctx);
  }
  b->count = count;
  b->extra = (kk_extra_t)(dcount - count);
  return b;
}

static kk_bignat_t* kk_bignat_trim_to(kk_bignat_t* x, kk_ssize_t count, bool allow_realloc, kk_context_t* ctx) {
  kk_ssize_t d = bignat_available_(x) - count;
  kk_assert_internal(d >= 0 && bignat_is_unique_(x));
  if (d < 0) {
    return x;
  }
  else if (d > MAX_EXTRA) {
    if (allow_realloc) {
      return kk_bignat_trim_realloc_(x, count, ctx);
    }
    else {
      x->count = count;
      x->extra = MAX_EXTRA;  // if we cannot realloc and extra > MAX_EXTRA, we may lose space :(
      return x;
    }
  }
  else {
    x->count = count;
    x->extra = (kk_extra_t)d;
    return x;
  }
}

static kk_bignat_t* kk_bignat_trim(kk_bignat_t* x, bool allow_realloc, kk_context_t* ctx) {
  kk_assert_internal(bignat_is_unique_(x));
  kk_ssize_t i = bignat_count_(x);
  while ((i > 0) && (x->digits[i-1] == 0)) { i--; }  // skip top zero's
  return kk_bignat_trim_to(x, i, allow_realloc, ctx);
}

static kk_bignat_t* bignat_alloc_reuse_(kk_bignat_t* x, kk_ssize_t count, kk_context_t* ctx) {
  kk_ssize_t d = (bignat_available_(x) - count);
  if (d >= 0 && d <= MAX_EXTRA && bignat_is_unique_(x)) {   // reuse?
    return kk_bignat_trim_to(x, count, false /* no realloc */, ctx);
  }
  else {
    return bignat_alloc(count, ctx);
  }
}

static kk_bignat_t* bignat_copy(kk_bignat_t* x, kk_ssize_t extra, kk_context_t* ctx) {
  kk_assert_internal(extra <= MAX_EXTRA);
  if (extra > MAX_EXTRA) extra = MAX_EXTRA;
  kk_bignat_t* z = bignat_alloc(x->count + extra, ctx);
  z = kk_bignat_trim_to(z, x->count, false, ctx);
  kk_memcpy(z->digits, x->digits, x->count * kk_ssizeof(kk_digit_t) );
  drop_bignat(x,ctx);
  return z;
}

static kk_bignat_t* bignat_ensure_unique(kk_bignat_t* x, kk_context_t* ctx) {
  return (bignat_is_unique_(x) ? x : bignat_copy(x,0,ctx));
}

static kk_bignat_t* bignat_push(kk_bignat_t* x, kk_digit_t d, kk_context_t* ctx) {
  if (x->extra == 0) { x = bignat_copy(x, MAX_EXTRA, ctx); }
  x->digits[x->count] = d;
  x->count++;
  x->extra--;
  return x;
}

/*----------------------------------------------------------------------
  Conversion from numbers
----------------------------------------------------------------------*/

// Bignat to natural. Possibly converting to a small int.
static kk_natural_t natural_bignat(kk_bignat_t* x, kk_context_t* ctx) {
  if (x->count==0) {
    return kk_natural_zero;
  }
  else if (x->count==1
#if (DIGIT_BITS >= KK_SMALLNAT_BITS-2)
    && x->digits[0] <= KK_SMALLNAT_MAX
#endif
  ) {
    // make it a small int
    kk_intf_t i = (kk_intf_t)(x->digits[0]);
    drop_bignat(x,ctx);
    return kk_natural_from_small(i);
  }
  else {
    return bignat_as_natural_(x,ctx);
  }
}

// create a bignat from an kk_int_t
static kk_bignat_t* bignat_from_uint(kk_uintx_t i, kk_context_t* ctx) {
  kk_bignat_t* b = bignat_alloc(0, ctx); // will reserve at least 4 digits
  do {
    b = bignat_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

// create a bignat from a uint64_t
static kk_bignat_t* bignat_from_uint64(uint64_t i, kk_context_t* ctx) {
  kk_bignat_t* b = bignat_alloc(0, false, ctx); // will reserve at least 4 digits
  do {
    b = bignat_push(b, i%BASE, ctx);
    i /= BASE;
  } while (i > 0);
  return b;
}

kk_natural_t kk_natural_from_big(kk_uintx_t i, kk_context_t* ctx) {
  return bignat_as_natural_(bignat_from_uint(i, ctx),ctx);
}

kk_natural_t kk_natural_from_bigu64(uint64_t i, kk_context_t* ctx) {
  return bignat_as_natural_(bignat_from_uint64(i, ctx),ctx);
}


/*----------------------------------------------------------------------
  To string
----------------------------------------------------------------------*/
