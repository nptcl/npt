#ifndef __LOOP_VARIABLES_HEADER__
#define __LOOP_VARIABLES_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define loop_filter_initially _n(loop_filter_initially)
#define loop_filter_finally _n(loop_filter_finally)
#define loop_filter_with_ _n(loop_filter_with_)
#define loop_variables_with_ _n(loop_variables_with_)
#define loop_push_for_as_ _n(loop_push_for_as_)
#define loop_variables_for_as_ _n(loop_variables_for_as_)

_g void loop_filter_initially(addr *form, addr *list);
_g void loop_filter_finally(addr *form, addr *list);
_g int loop_filter_with_(Execute ptr, addr *form, addr *list);
_g int loop_variables_with_(Execute ptr, addr *form, addr list);
_g int loop_push_for_as_(Execute ptr, addr *expr1, addr *expr2, addr list);
_g int loop_variables_for_as_(addr *form, addr list);

#endif

