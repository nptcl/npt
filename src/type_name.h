#ifndef __TYPE_NAME_HEADER__
#define __TYPE_NAME_HEADER__

#include "execute.h"
#include "typedef.h"

#define type_name_p_ _n(type_name_p_)
#define type_name_ _n(type_name_)
#define init_type_name _n(init_type_name)

/*  function type_name
 *     input: type
 *    output: symbol  [not cons]
 */
int type_name_p_(Execute ptr, addr pos, addr *value, int *ret);
int type_name_(Execute ptr, addr pos, addr *value);
void init_type_name(void);

#endif

