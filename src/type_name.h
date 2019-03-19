#ifndef __TYPE_NAME_HEADER__
#define __TYPE_NAME_HEADER__

#include "typedef.h"

/*  function type_name
 *     input: type
 *    output: symbol  [not cons]
 */
int type_name_p(addr *ret, addr value);
void type_name(addr *ret, addr value);
void init_type_name(void);

#endif

