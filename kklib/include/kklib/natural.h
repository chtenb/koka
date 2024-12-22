#pragma once
#ifndef KK_NATURAL_H
#define KK_NATURAL_H
/*---------------------------------------------------------------------------
  Copyright 2020-2022, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------------------
Naturals are either a pointer to a `kk_bignat_t` (with lowest bit == 0),  
or a _small_ int (with lowest bit == 1).

The small nat is restricted in size to KK_SMALLNAT_BITS such that we can do
efficient arithmetic on the representation of a small nat `n` directly where
`boxed(n) == 4*n + 1`. The `kk_smallnat_t` size is chosen to allow efficient overflow detection.
By using `4*n + 1` we always have the lowest two bits of a pointer as `00` 
while those of a kk_smallnat_t are always `01`.

--------------------------------------------------------------------------------------------------*/

typedef kk_intf_t kk_smallnat_t;
#define KK_SMALLNAT_BITS  (KK_INTF_BOX_BITS(1))
#define KK_SMALLNAT_MAX   (KK_INTF_BOX_MAX(1))

#define KK_SMALLNAT_SIZE  (KK_SMALLNAT_BITS/8)

typedef struct kk_natural_s {
  kk_intb_t ibox;
} kk_natural_t;


static inline bool kk_is_smallnat(kk_natural_t i) {
  return kk_is_value(i.ibox);
}

static inline bool kk_is_bignat(kk_natural_t i) {
  return kk_is_ptr(i.ibox);
}

static inline kk_ptr_t _kk_natural_ptr(kk_natural_t i, kk_context_t* ctx) {
  kk_assert_internal(kk_is_bignat(i));
  return kk_ptr_decode(i.ibox,ctx);  
}

static inline kk_intf_t kk_smallnat_from_natural(kk_natural_t i) {  // use for known small nats
  kk_assert_internal(kk_is_smallnat(i));
  return kk_intf_decode(i.ibox,1);
}

static inline bool kk_is_natural(kk_natural_t i) {
  return ((kk_is_smallnat(i) && kk_smallnat_from_natural(i) <= KK_SMALLNAT_MAX) 
         || (kk_is_bignat(i) && kk_block_tag(_kk_natural_ptr(i,kk_get_context())) == KK_TAG_BIGINT));
}

static inline bool kk_are_smallnats(kk_natural_t i, kk_natural_t j) {
  kk_assert_internal(kk_is_natural(i) && kk_is_natural(j));
  #if KK_TAG_VALUE == 1
  return kk_is_value(i.ibox & j.ibox);
  #else
  return (kk_is_smallnat(i) && kk_is_smallnat(j));
  #endif
}

static inline bool kk_natural_small_eq(kk_natural_t x, kk_natural_t y) {
  kk_assert_internal(kk_are_smallnats(x, y));
  return (x.ibox == y.ibox);
}

#define kk_natural_zero     (kk_natural_from_small(0))
#define kk_natural_one      (kk_natural_from_small(1))

static inline bool kk_natural_is_zero_borrow(kk_natural_t x) {
  if kk_likely(kk_is_smallnat(x)) return kk_natural_small_eq(x,kk_natural_zero);
  return false;
}

static inline bool kk_natural_is_one_borrow(kk_natural_t x) {
  if kk_likely(kk_is_smallnat(x)) return kk_natural_small_eq(x, kk_natural_one);
  return false;
}


/*---------------------------------------------------------------------------------
  Generic operations on naturals
-----------------------------------------------------------------------------------*/

// Isomorphic with boxed values
static inline kk_box_t kk_natural_box(kk_natural_t i, kk_context_t* ctx) { 
  kk_unused(ctx);
  kk_box_t b = { i.ibox };
  return b;
}
static inline kk_natural_t kk_natural_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_natural_t i = { b.box };
  return i;
}

static inline kk_natural_t kk_natural_dup(kk_natural_t i, kk_context_t* ctx) {
  if kk_unlikely(kk_is_bignat(i)) { kk_block_dup(_kk_natural_ptr(i,ctx)); }
  return i;
}

static inline void kk_natural_drop(kk_natural_t i, kk_context_t* ctx) {
  if kk_unlikely(kk_is_bignat(i)) { kk_block_drop(_kk_natural_ptr(i,ctx), ctx); }
}

kk_decl_export bool          kk_natural_parse(const char* num, kk_natural_t* result, kk_context_t* ctx);
kk_decl_export bool          kk_natural_hex_parse(const char* s, kk_natural_t* res, kk_context_t* ctx);
kk_decl_export kk_natural_t  kk_natural_from_str(const char* num, kk_context_t* ctx); // for known correct string number (returns 0 on wrong string)

kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_from_big(kk_intx_t i, kk_context_t* ctx);         // for possibly large i
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_from_big64(int64_t i, kk_context_t* ctx);     // for possibly large i
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_from_bigu64(uint64_t i, kk_context_t* ctx);   // for possibly large i
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_from_double(double d, kk_context_t* ctx);     // round d and convert to natural (0 for NaN/Inf)

kk_decl_export kk_decl_noinline uint32_t    kk_natural_clamp32_generic(kk_natural_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline uint64_t    kk_natural_clamp64_generic(kk_natural_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline size_t     kk_natural_clamp_size_t_generic(kk_natural_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline double     kk_natural_as_double_generic(kk_natural_t i, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_add_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_sub_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_mul_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);


kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_div_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_mod_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_div_mod_generic(kk_natural_t x, kk_natural_t y, kk_natural_t* mod, kk_context_t* ctx);

kk_decl_export kk_decl_noinline int           kk_natural_cmp_generic(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int           kk_natural_cmp_generic_borrow(kk_natural_t x, kk_natural_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_neg_generic(kk_natural_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_sqr_generic(kk_natural_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_pow(kk_natural_t x, kk_natural_t p, kk_context_t* ctx);

kk_decl_export kk_decl_noinline bool          kk_natural_is_even_generic(kk_natural_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int           kk_natural_signum_generic_bigint(kk_natural_t x, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_ctz(kk_natural_t x, kk_context_t* ctx);           // count trailing zero digits
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_count_digits(kk_natural_t x, kk_context_t* ctx);  // count decimal digits
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_mul_pow10(kk_natural_t x, kk_natural_t p, kk_context_t* ctx);  // x*(10^p)
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_cdiv_pow10(kk_natural_t x, kk_natural_t p, kk_context_t* ctx);  // x/(10^p)
kk_decl_export kk_decl_noinline kk_natural_t  kk_natural_div_pow10(kk_natural_t x, kk_natural_t p, kk_context_t* ctx);  // x/(10^p)

kk_decl_export void   kk_natural_fprint(FILE* f, kk_natural_t x, kk_context_t* ctx);
kk_decl_export void   kk_natural_print(kk_natural_t x, kk_context_t* ctx);
kk_decl_export double kk_double_round_even(double d, kk_context_t* ctx);


/*---------------------------------------------------------------------------------
  Conversion from fixed size integers
-----------------------------------------------------------------------------------*/

static inline kk_natural_t kk_natural_from_uint8(uint8_t u, kk_context_t* ctx) {
  #if (KK_SMALLNAT_MAX >= UINT8_MAX)
    kk_unused(ctx);
    return kk_natural_from_small((kk_intf_t)u);
  #else  
    return kk_likely(u <= KK_SMALLNAT_MAX) ? kk_natural_from_small((kk_intf_t)u) : kk_natural_from_big(u, ctx);
  #endif
}

static inline kk_natural_t kk_natural_from_int8(int8_t i, kk_context_t* ctx) {
  #if (KK_SMALLNAT_MAX >= INT8_MAX)
    kk_unused(ctx);
    return kk_natural_from_small(i);
  #else
    return kk_likely(i >= KK_SMALLNAT_MIN && i <= KK_SMALLNAT_MAX) ? kk_natural_from_small(i) : kk_natural_from_big(i, ctx);
  #endif
}

static inline kk_natural_t kk_natural_from_int16(int16_t i, kk_context_t* ctx) {
  #if (KK_SMALLNAT_MAX >= INT16_MAX)
    kk_unused(ctx);
    return kk_natural_from_small(i);
  #else
    return kk_likely(i >= KK_SMALLNAT_MIN && i <= KK_SMALLNAT_MAX) ? kk_natural_from_small(i) : kk_natural_from_big(i, ctx);
  #endif
}

static inline kk_natural_t kk_natural_from_int32(int32_t i, kk_context_t* ctx) {
  #if (KK_SMALLNAT_MAX >= INT32_MAX)
    kk_unused(ctx);
    return kk_natural_from_small(i);
  #else
    return kk_likely(i >= KK_SMALLNAT_MIN && i <= KK_SMALLNAT_MAX) ? kk_natural_from_small(i) : kk_natural_from_big(i, ctx);
  #endif
}

static inline kk_natural_t kk_natural_from_uint32(uint32_t u, kk_context_t* ctx) {
  #if (KK_SMALLNAT_MAX >= UINT32_MAX)
    kk_unused(ctx);
    return kk_natural_from_small((kk_intf_t)u);
  #else  
    return kk_likely(u <= KK_SMALLNAT_MAX) ? kk_natural_from_small((kk_intf_t)u) : kk_natural_from_big(u, ctx);
  #endif 
}

static inline kk_natural_t kk_natural_from_int64(int64_t i, kk_context_t* ctx) {
  return kk_likely(i >= KK_SMALLNAT_MIN && i <= KK_SMALLNAT_MAX) ? kk_natural_from_small((kk_intf_t)i) : kk_natural_from_big64(i, ctx);
}

static inline kk_natural_t kk_natural_from_uint64(uint64_t i, kk_context_t* ctx) {
  return kk_likely(i <= KK_SMALLNAT_MAX) ? kk_natural_from_small((kk_intf_t)i) : kk_natural_from_bigu64(i, ctx);
}

static inline kk_natural_t kk_natural_from_int(kk_intx_t i, kk_context_t* ctx) {
  return kk_likely(i >= KK_SMALLNAT_MIN && i <= KK_SMALLNAT_MAX) ? kk_natural_from_small((kk_intf_t)i) : kk_natural_from_big(i, ctx);
}

#if (KK_INTX_SIZE <= 4)
static inline kk_natural_t kk_natural_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return kk_natural_from_uint32(i,ctx);
}
#elif (KK_INTX_SIZE <= 8)
static inline kk_natural_t kk_natural_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return kk_natural_from_uint64(i, ctx);
}
#else
# error "define kk_natural_from_uintx_t for this platform"
#endif

static inline kk_natural_t kk_natural_from_size_t(size_t i, kk_context_t* ctx) {
  return kk_natural_from_uintx_t(i, ctx);
}

static inline kk_natural_t kk_natural_from_ssize_t(kk_ssize_t i, kk_context_t* ctx) {
  return kk_natural_from_int(i, ctx);
}

static inline kk_natural_t kk_natural_from_ptrdiff_t(ptrdiff_t i, kk_context_t* ctx) {
  return kk_natural_from_int(i, ctx);
}

static inline kk_natural_t kk_natural_from_intptr_t(intptr_t i, kk_context_t* ctx) {
  return kk_natural_from_int(i, ctx);
}

static inline kk_natural_t kk_natural_from_byte(uint8_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_natural_from_small(i);
}
