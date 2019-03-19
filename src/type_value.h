#ifndef __TYPE_VALUE_HEADER__
#define __TYPE_VALUE_HEADER__

#include "typedef.h"

/*  function type_value
 *     input: object
 *    output: type
 */
int type_name_p(addr *ret, addr value);
void type_value_nil(addr *ret);
void type_value_t(addr *ret);
void type_value_integer(addr *ret, addr value);
void type_value_rational(addr *ret, addr value);
void type_value_character(addr *ret, addr value);
void type_value_vector(addr *ret, addr value);
void type_value_bitvector(addr *ret, addr value);
void type_value_string(addr *ret, addr value);
void type_value_array(addr *ret, addr value);
void type_value_float(addr *ret, addr value);
void type_value_complex(addr *ret, addr value);
void type_value_pathname(addr *ret, addr value);
void type_value(addr *ret, addr value);
void init_type_value(void);

#endif

