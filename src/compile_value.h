#ifndef __COMPILE_VALUE_HEADER__
#define __COMPILE_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define faslwrite_value_nil _n(faslwrite_value_nil)
#define faslwrite_value_t _n(faslwrite_value_t)
#define faslwrite_value_cons _n(faslwrite_value_cons)
#define faslwrite_value_vector _n(faslwrite_value_vector)
#define faslwrite_value_character _n(faslwrite_value_character)
#define faslwrite_value_string _n(faslwrite_value_string)
#define faslwrite_value_symbol _n(faslwrite_value_symbol)
#define faslwrite_value_hashtable _n(faslwrite_value_hashtable)
#define faslwrite_value_fixnum _n(faslwrite_value_fixnum)
#define faslwrite_value_bignum _n(faslwrite_value_bignum)
#define faslwrite_value_ratio _n(faslwrite_value_ratio)
#define faslwrite_value_single_float _n(faslwrite_value_single_float)
#define faslwrite_value_double_float _n(faslwrite_value_double_float)
#define faslwrite_value_long_float _n(faslwrite_value_long_float)
#define faslwrite_value_complex _n(faslwrite_value_complex)
#define faslwrite_value_callname _n(faslwrite_value_callname)
#define faslwrite_value_index _n(faslwrite_value_index)
#define faslwrite_value_package _n(faslwrite_value_package)
#define faslwrite_value_random_state _n(faslwrite_value_random_state)
#define faslwrite_value_pathname _n(faslwrite_value_pathname)
#define faslwrite_value_quote _n(faslwrite_value_quote)
#define faslwrite_value_bitvector _n(faslwrite_value_bitvector)
#define faslwrite_value_load_time_value _n(faslwrite_value_load_time_value)
#define faslread_value_nil _n(faslread_value_nil)
#define faslread_value_t _n(faslread_value_t)
#define faslread_value_cons _n(faslread_value_cons)
#define faslread_value_vector2 _n(faslread_value_vector2)
#define faslread_value_vector4 _n(faslread_value_vector4)
#define faslread_value_vector8 _n(faslread_value_vector8)
#define faslread_value_character _n(faslread_value_character)
#define faslread_value_string _n(faslread_value_string)
#define faslread_value_gensym _n(faslread_value_gensym)
#define faslread_value_symbol _n(faslread_value_symbol)
#define faslread_value_hashtable _n(faslread_value_hashtable)
#define faslread_value_fixnum _n(faslread_value_fixnum)
#define faslread_value_bignum _n(faslread_value_bignum)
#define faslread_value_ratio _n(faslread_value_ratio)
#define faslread_value_single_float _n(faslread_value_single_float)
#define faslread_value_double_float _n(faslread_value_double_float)
#define faslread_value_long_float _n(faslread_value_long_float)
#define faslread_value_complex _n(faslread_value_complex)
#define faslread_value_callname _n(faslread_value_callname)
#define faslread_value_index _n(faslread_value_index)
#define faslread_value_package _n(faslread_value_package)
#define faslread_value_random_state _n(faslread_value_random_state)
#define faslread_value_pathname _n(faslread_value_pathname)
#define faslread_value_quote _n(faslread_value_quote)
#define faslread_value_bitvector _n(faslread_value_bitvector)
#define faslread_value_load_time_value _n(faslread_value_load_time_value)

int faslwrite_value_nil(Execute ptr, addr stream, addr pos);
int faslwrite_value_t(Execute ptr, addr stream, addr pos);
int faslwrite_value_cons(Execute ptr, addr stream, addr pos);
int faslwrite_value_vector(Execute ptr, addr stream, addr pos);
int faslwrite_value_character(Execute ptr, addr stream, addr pos);
int faslwrite_value_string(Execute ptr, addr stream, addr pos);
int faslwrite_value_symbol(Execute ptr, addr stream, addr pos);
int faslwrite_value_hashtable(Execute ptr, addr stream, addr pos);
int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos);
int faslwrite_value_bignum(Execute ptr, addr stream, addr pos);
int faslwrite_value_ratio(Execute ptr, addr stream, addr pos);
int faslwrite_value_single_float(Execute ptr, addr stream, addr pos);
int faslwrite_value_double_float(Execute ptr, addr stream, addr pos);
int faslwrite_value_long_float(Execute ptr, addr stream, addr pos);
int faslwrite_value_complex(Execute ptr, addr stream, addr pos);
int faslwrite_value_callname(Execute ptr, addr stream, addr pos);
int faslwrite_value_index(Execute ptr, addr stream, addr pos);
int faslwrite_value_package(Execute ptr, addr stream, addr pos);
int faslwrite_value_random_state(Execute ptr, addr stream, addr pos);
int faslwrite_value_pathname(Execute ptr, addr stream, addr pos);
int faslwrite_value_quote(Execute ptr, addr stream, addr pos);
int faslwrite_value_bitvector(Execute ptr, addr stream, addr pos);
int faslwrite_value_load_time_value(Execute ptr, addr stream, addr pos);

int faslread_value_nil(Execute ptr, addr stream, addr *ret);
int faslread_value_t(Execute ptr, addr stream, addr *ret);
int faslread_value_cons(Execute ptr, addr stream, addr *ret);
int faslread_value_vector2(Execute ptr, addr stream, addr *ret);
int faslread_value_vector4(Execute ptr, addr stream, addr *ret);
#ifdef LISP_ARCH_64BIT
int faslread_value_vector8(Execute ptr, addr stream, addr *ret);
#endif
int faslread_value_character(Execute ptr, addr stream, addr *ret);
int faslread_value_string(Execute ptr, addr stream, addr *ret);
int faslread_value_gensym(Execute ptr, addr stream, addr *ret);
int faslread_value_symbol(Execute ptr, addr stream, addr *ret);
int faslread_value_hashtable(Execute ptr, addr stream, addr *ret);
int faslread_value_fixnum(Execute ptr, addr stream, addr *ret);
int faslread_value_bignum(Execute ptr, addr stream, addr *ret);
int faslread_value_ratio(Execute ptr, addr stream, addr *ret);
int faslread_value_single_float(Execute ptr, addr stream, addr *ret);
int faslread_value_double_float(Execute ptr, addr stream, addr *ret);
int faslread_value_long_float(Execute ptr, addr stream, addr *ret);
int faslread_value_complex(Execute ptr, addr stream, addr *ret);
int faslread_value_callname(Execute ptr, addr stream, addr *ret);
int faslread_value_index(Execute ptr, addr stream, addr *ret);
int faslread_value_package(Execute ptr, addr stream, addr *ret);
int faslread_value_random_state(Execute ptr, addr stream, addr *ret);
int faslread_value_pathname(Execute ptr, addr stream, addr *ret);
int faslread_value_quote(Execute ptr, addr stream, addr *ret);
int faslread_value_bitvector(Execute ptr, addr stream, addr *ret);
int faslread_value_load_time_value(Execute ptr, addr stream, addr *ret);

#endif

