#ifndef __COMPILE_VALUE_HEADER__
#define __COMPILE_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define faslwrite_value_nil_ _n(faslwrite_value_nil_)
#define faslwrite_value_t_ _n(faslwrite_value_t_)
#define faslwrite_value_clos_ _n(faslwrite_value_clos_)
#define faslwrite_value_cons_ _n(faslwrite_value_cons_)
#define faslwrite_value_vector_ _n(faslwrite_value_vector_)
#define faslwrite_value_character_ _n(faslwrite_value_character_)
#define faslwrite_value_string_ _n(faslwrite_value_string_)
#define faslwrite_value_symbol_ _n(faslwrite_value_symbol_)
#define faslwrite_value_hashtable_ _n(faslwrite_value_hashtable_)
#define faslwrite_value_fixnum_ _n(faslwrite_value_fixnum_)
#define faslwrite_value_bignum_ _n(faslwrite_value_bignum_)
#define faslwrite_value_ratio_ _n(faslwrite_value_ratio_)
#define faslwrite_value_single_float_ _n(faslwrite_value_single_float_)
#define faslwrite_value_double_float_ _n(faslwrite_value_double_float_)
#define faslwrite_value_long_float_ _n(faslwrite_value_long_float_)
#define faslwrite_value_complex_ _n(faslwrite_value_complex_)
#define faslwrite_value_callname_ _n(faslwrite_value_callname_)
#define faslwrite_value_index_ _n(faslwrite_value_index_)
#define faslwrite_value_package_ _n(faslwrite_value_package_)
#define faslwrite_value_random_state_ _n(faslwrite_value_random_state_)
#define faslwrite_value_pathname_ _n(faslwrite_value_pathname_)
#define faslwrite_value_quote_ _n(faslwrite_value_quote_)
#define faslwrite_value_bitvector_ _n(faslwrite_value_bitvector_)
#define faslwrite_value_load_time_value_ _n(faslwrite_value_load_time_value_)

#define faslread_value_nil_ _n(faslread_value_nil_)
#define faslread_value_t_ _n(faslread_value_t_)
#define faslread_value_clos_ _n(faslread_value_clos_)
#define faslread_value_cons_ _n(faslread_value_cons_)
#define faslread_value_vector2_ _n(faslread_value_vector2_)
#define faslread_value_vector4_ _n(faslread_value_vector4_)
#define faslread_value_vector8_ _n(faslread_value_vector8_)
#define faslread_value_character_ _n(faslread_value_character_)
#define faslread_value_character7_ _n(faslread_value_character7_)
#define faslread_value_string_ _n(faslread_value_string_)
#define faslread_value_string7_ _n(faslread_value_string7_)
#define faslread_value_gensym_ _n(faslread_value_gensym_)
#define faslread_value_symbol_ _n(faslread_value_symbol_)
#define faslread_value_hashtable_ _n(faslread_value_hashtable_)
#define faslread_value_fixnum_ _n(faslread_value_fixnum_)
#define faslread_value_bignum_ _n(faslread_value_bignum_)
#define faslread_value_ratio_ _n(faslread_value_ratio_)
#define faslread_value_single_float_ _n(faslread_value_single_float_)
#define faslread_value_double_float_ _n(faslread_value_double_float_)
#define faslread_value_long_float_ _n(faslread_value_long_float_)
#define faslread_value_complex_ _n(faslread_value_complex_)
#define faslread_value_callname_ _n(faslread_value_callname_)
#define faslread_value_index_ _n(faslread_value_index_)
#define faslread_value_package_ _n(faslread_value_package_)
#define faslread_value_random_state_ _n(faslread_value_random_state_)
#define faslread_value_pathname_ _n(faslread_value_pathname_)
#define faslread_value_quote_ _n(faslread_value_quote_)
#define faslread_value_bitvector_ _n(faslread_value_bitvector_)
#define faslread_value_load_time_value_ _n(faslread_value_load_time_value_)

int faslwrite_value_nil_(Execute ptr, addr stream, addr pos);
int faslwrite_value_t_(Execute ptr, addr stream, addr pos);
int faslwrite_value_clos_(Execute ptr, addr stream, addr pos);
int faslwrite_value_cons_(Execute ptr, addr stream, addr pos);
int faslwrite_value_vector_(Execute ptr, addr stream, addr pos);
int faslwrite_value_character_(Execute ptr, addr stream, addr pos);
int faslwrite_value_string_(Execute ptr, addr stream, addr pos);
int faslwrite_value_symbol_(Execute ptr, addr stream, addr pos);
int faslwrite_value_hashtable_(Execute ptr, addr stream, addr pos);
int faslwrite_value_fixnum_(Execute ptr, addr stream, addr pos);
int faslwrite_value_bignum_(Execute ptr, addr stream, addr pos);
int faslwrite_value_ratio_(Execute ptr, addr stream, addr pos);
int faslwrite_value_single_float_(Execute ptr, addr stream, addr pos);
int faslwrite_value_double_float_(Execute ptr, addr stream, addr pos);
int faslwrite_value_long_float_(Execute ptr, addr stream, addr pos);
int faslwrite_value_complex_(Execute ptr, addr stream, addr pos);
int faslwrite_value_callname_(Execute ptr, addr stream, addr pos);
int faslwrite_value_index_(Execute ptr, addr stream, addr pos);
int faslwrite_value_package_(Execute ptr, addr stream, addr pos);
int faslwrite_value_random_state_(Execute ptr, addr stream, addr pos);
int faslwrite_value_pathname_(Execute ptr, addr stream, addr pos);
int faslwrite_value_quote_(Execute ptr, addr stream, addr pos);
int faslwrite_value_bitvector_(Execute ptr, addr stream, addr pos);
int faslwrite_value_load_time_value_(Execute ptr, addr stream, addr pos);

int faslread_value_nil_(Execute ptr, addr stream, addr *ret);
int faslread_value_t_(Execute ptr, addr stream, addr *ret);
int faslread_value_clos_(Execute ptr, addr stream, addr *ret);
int faslread_value_cons_(Execute ptr, addr stream, addr *ret);
int faslread_value_vector2_(Execute ptr, addr stream, addr *ret);
int faslread_value_vector4_(Execute ptr, addr stream, addr *ret);
#ifdef LISP_ARCH_64BIT
int faslread_value_vector8_(Execute ptr, addr stream, addr *ret);
#endif
int faslread_value_character_(Execute ptr, addr stream, addr *ret);
int faslread_value_character7_(Execute ptr, addr stream, addr *ret);
int faslread_value_string_(Execute ptr, addr stream, addr *ret);
int faslread_value_string7_(Execute ptr, addr stream, addr *ret);
int faslread_value_gensym_(Execute ptr, addr stream, addr *ret);
int faslread_value_symbol_(Execute ptr, addr stream, addr *ret);
int faslread_value_hashtable_(Execute ptr, addr stream, addr *ret);
int faslread_value_fixnum_(Execute ptr, addr stream, addr *ret);
int faslread_value_bignum_(Execute ptr, addr stream, addr *ret);
int faslread_value_ratio_(Execute ptr, addr stream, addr *ret);
int faslread_value_single_float_(Execute ptr, addr stream, addr *ret);
int faslread_value_double_float_(Execute ptr, addr stream, addr *ret);
int faslread_value_long_float_(Execute ptr, addr stream, addr *ret);
int faslread_value_complex_(Execute ptr, addr stream, addr *ret);
int faslread_value_callname_(Execute ptr, addr stream, addr *ret);
int faslread_value_index_(Execute ptr, addr stream, addr *ret);
int faslread_value_package_(Execute ptr, addr stream, addr *ret);
int faslread_value_random_state_(Execute ptr, addr stream, addr *ret);
int faslread_value_pathname_(Execute ptr, addr stream, addr *ret);
int faslread_value_quote_(Execute ptr, addr stream, addr *ret);
int faslread_value_bitvector_(Execute ptr, addr stream, addr *ret);
int faslread_value_load_time_value_(Execute ptr, addr stream, addr *ret);

#endif

