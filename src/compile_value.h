#ifndef __COMPILE_VALUE_HEADER__
#define __COMPILE_VALUE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int faslwrite_value_nil(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_t(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_cons(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_vector(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_character(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_string(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_symbol(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_bignum(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_ratio(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_single_float(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_double_float(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_long_float(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_complex(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_index(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_package(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_random_state(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_pathname(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_bitvector(Execute ptr, addr stream, addr pos);
_g int faslwrite_value_load_time_value(Execute ptr, addr stream, addr pos);

_g int faslread_value_nil(Execute ptr, addr stream, addr *ret);
_g int faslread_value_t(Execute ptr, addr stream, addr *ret);
_g int faslread_value_cons(Execute ptr, addr stream, addr *ret);
_g int faslread_value_vector2(Execute ptr, addr stream, addr *ret);
_g int faslread_value_vector4(Execute ptr, addr stream, addr *ret);
#ifdef LISP_ARCH_64BIT
_g int faslread_value_vector8(Execute ptr, addr stream, addr *ret);
#endif
_g int faslread_value_character(Execute ptr, addr stream, addr *ret);
_g int faslread_value_string(Execute ptr, addr stream, addr *ret);
_g int faslread_value_symbol(Execute ptr, addr stream, addr *ret);
_g int faslread_value_fixnum(Execute ptr, addr stream, addr *ret);
_g int faslread_value_bignum(Execute ptr, addr stream, addr *ret);
_g int faslread_value_ratio(Execute ptr, addr stream, addr *ret);
_g int faslread_value_single_float(Execute ptr, addr stream, addr *ret);
_g int faslread_value_double_float(Execute ptr, addr stream, addr *ret);
_g int faslread_value_long_float(Execute ptr, addr stream, addr *ret);
_g int faslread_value_complex(Execute ptr, addr stream, addr *ret);
_g int faslread_value_index(Execute ptr, addr stream, addr *ret);
_g int faslread_value_package(Execute ptr, addr stream, addr *ret);
_g int faslread_value_random_state(Execute ptr, addr stream, addr *ret);
_g int faslread_value_pathname(Execute ptr, addr stream, addr *ret);
_g int faslread_value_bitvector(Execute ptr, addr stream, addr *ret);
_g int faslread_value_load_time_value(Execute ptr, addr stream, addr *ret);

#endif

