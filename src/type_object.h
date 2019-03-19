#ifndef __TYPE_OBJECT_HEADER__
#define __TYPE_OBJECT_HEADER__

#include "typedef.h"

/*  function type_object
 *     input: type
 *    output: (or symbol cons)
 */
void type_object(addr *ret, addr value);
void init_type_object(void);

#endif

