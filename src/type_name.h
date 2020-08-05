#ifndef __TYPE_NAME_HEADER__
#define __TYPE_NAME_HEADER__

#include "typedef.h"

/*  function type_name
 *     input: type
 *    output: symbol  [not cons]
 */
_g int type_name_p_(addr pos, addr *value, int *ret);
_g int type_name_(addr pos, addr *value);
_g void init_type_name(void);

#endif

