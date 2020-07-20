#ifndef __TYPE_UPGRADED_HEADER__
#define __TYPE_UPGRADED_HEADER__

#include "execute.h"
#include "typedef.h"

/*
 *  upgraded-array-element-type
 */
/* upgraded-type check */
_g int upgraded_array0_equal(addr left, addr right);
/* type -> upgraded -> value */
_g void upgraded_array_value(addr type, enum ARRAY_TYPE *ret, int *size);
/* value, size -> type */
_g void upgraded_array_object(enum ARRAY_TYPE type, int size, addr *ret);
/* type -> upgraded -> type */
_g void upgraded_array_type(addr type, addr *ret);
/* value -> object */
_g void upgraded_array_const(enum ARRAY_TYPE type, int size, addr *ret);
/* object -> upgraded -> object */
_g int upgraded_array_common(Execute ptr, addr env, addr pos, addr *ret);
/* make local */
_g void upgraded_array_t_local(LocalRoot local, addr *ret);
_g void upgraded_array_bit_local(LocalRoot local, addr *ret);
_g void upgraded_array_character_local(LocalRoot local, addr *ret);


/*
 *  upgraded-complex-part-type
 */
/* type -> upgraded -> type */
_g int upgraded_complex_type_(addr type, addr *ret);
/* object -> upgraded -> object */
_g int upgraded_complex_common(Execute ptr, addr env, addr pos, addr *ret);
/* build */
_g void build_type_upgraded(void);

#endif

