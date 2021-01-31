#ifndef __TYPE_UPGRADED_HEADER__
#define __TYPE_UPGRADED_HEADER__

#include "execute.h"
#include "typedef.h"

#define upgraded_array0_equal _n(upgraded_array0_equal)
#define upgraded_array_value_ _n(upgraded_array_value_)
#define upgraded_array_object _n(upgraded_array_object)
#define upgraded_array_type_ _n(upgraded_array_type_)
#define upgraded_array_const _n(upgraded_array_const)
#define upgraded_array_common _n(upgraded_array_common)
#define upgraded_array_t_local _n(upgraded_array_t_local)
#define upgraded_array_bit_local _n(upgraded_array_bit_local)
#define upgraded_array_character_local _n(upgraded_array_character_local)
#define upgraded_complex_type_ _n(upgraded_complex_type_)
#define upgraded_complex_common _n(upgraded_complex_common)
#define build_type_upgraded _n(build_type_upgraded)

/*
 *  upgraded-array-element-type
 */
/* upgraded-type check */
int upgraded_array0_equal(addr left, addr right);
/* type -> upgraded -> value */
int upgraded_array_value_(addr type, enum ARRAY_TYPE *ret, int *size);
/* value, size -> type */
void upgraded_array_object(enum ARRAY_TYPE type, int size, addr *ret);
/* type -> upgraded -> type */
int upgraded_array_type_(addr type, addr *ret);
/* value -> object */
void upgraded_array_const(enum ARRAY_TYPE type, int size, addr *ret);
/* object -> upgraded -> object */
int upgraded_array_common(Execute ptr, addr env, addr pos, addr *ret);
/* make local */
void upgraded_array_t_local(LocalRoot local, addr *ret);
void upgraded_array_bit_local(LocalRoot local, addr *ret);
void upgraded_array_character_local(LocalRoot local, addr *ret);


/*
 *  upgraded-complex-part-type
 */
/* type -> upgraded -> type */
int upgraded_complex_type_(Execute ptr, addr env, addr type, addr *ret);
/* object -> upgraded -> object */
int upgraded_complex_common(Execute ptr, addr env, addr pos, addr *ret);
/* build */
void build_type_upgraded(void);

#endif

