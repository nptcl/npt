#ifndef __TYPE_OBJECT_HEADER__
#define __TYPE_OBJECT_HEADER__

#include "typedef.h"

/*  function type_object
 *     input: type
 *    output: (or symbol cons)
 */
_g int type_object_(addr *ret, addr value);
_g void init_type_object(void);

#endif

