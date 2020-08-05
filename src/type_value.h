#ifndef __TYPE_VALUE_HEADER__
#define __TYPE_VALUE_HEADER__

#include "typedef.h"

/*  function type_value
 *     input: object
 *    output: type
 */
_g void type_value_nil(addr *ret);
_g void type_value_t(addr *ret);
_g void type_value_type(addr *ret);
_g int type_value_clos_(addr *ret, addr value);
_g void type_value_array(addr *ret, addr value);
_g void type_value_vector(addr *ret, addr value);
_g void type_value_character(addr *ret, addr value);

_g void type_value_integer(addr *ret, addr value);
_g void type_value_rational(addr *ret, addr value);
_g void type_value_bitvector(addr *ret, addr value);
_g void type_value_float(addr *ret, addr value);
_g int type_value_complex_(addr *ret, addr value);
_g void type_value_random_state(addr *ret, addr value);
_g void type_value_pathname(addr *ret, addr value);
_g void type_value_environment(addr *ret, addr value);

_g int type_value_(addr *ret, addr value);
_g void init_type_value(void);

#endif

