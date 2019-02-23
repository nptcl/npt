#ifndef __HEADER_TYPE_VALUE__
#define __HEADER_TYPE_VALUE__

#include "local.h"
#include "typedef.h"

/* type-value */
void type_value_nil(LocalRoot local, addr *ret);
void type_value_t(LocalRoot local, addr *ret);
void type_value_integer(LocalRoot local, addr *ret, addr value);
void type_value_rational(LocalRoot local, addr *ret, addr value);
void type_value_real(LocalRoot local, addr *ret, addr value);
void type_value_character(LocalRoot local, addr *ret, addr value);
void type_value_vector(LocalRoot local, addr *ret, addr value);
void type_value_bitvector(LocalRoot local, addr *ret, addr value);
void type_value_string(LocalRoot local, addr *ret, addr value);
void type_value_array(LocalRoot local, addr *ret, addr value);
void type_value_symbol(LocalRoot local, addr *ret, addr value);
void type_value_float(LocalRoot local, addr *ret, addr value);
void type_value_complex(LocalRoot local, addr *ret, addr value);
void type_value_function(LocalRoot local, addr *ret, addr value);
void type_value_pathname(LocalRoot local, addr *ret, addr value);

void type_value_alloc(LocalRoot local, addr *ret, addr value);
void type_value_local(LocalRoot local, addr *ret, addr value);
void type_value_heap(addr *ret, addr value);

/* type-name */
int type_name_check(addr *ret, addr value);
void type_name(addr *ret, addr value);

/* type-object */
void type_object(LocalRoot local, addr *ret, addr value);

/* initialize */
void init_type_value();

#endif

