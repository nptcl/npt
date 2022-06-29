#ifndef __TYPE_OBJECT_HEADER__
#define __TYPE_OBJECT_HEADER__

#include "typedef.h"

#define type_object_ _n(type_object_)
#define init_type_object _n(init_type_object)

/*  function type_object
 *     input: type
 *    output: (or symbol cons)
 */
int type_object_(Execute ptr, addr *ret, addr pos);
void init_type_object(void);

#endif

