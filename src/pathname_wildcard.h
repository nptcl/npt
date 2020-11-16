#ifndef __PATHNAME_WILDCARD_HEADER__
#define __PATHNAME_WILDCARD_HEADER__

#include "local.h"
#include "typedef.h"

#define wild_pathname_boolean_ _n(wild_pathname_boolean_)
#define wildcard_stringp_p_ _n(wildcard_stringp_p_)
#define wildcard_string_pathname_ _n(wildcard_string_pathname_)
#define wildcard_eq_pathname_ _n(wildcard_eq_pathname_)
#define wildcard_pathname_ _n(wildcard_pathname_)

int wild_pathname_boolean_(addr file, addr field, int *ret);
int wildcard_stringp_p_(addr pos, int *ret);
int wildcard_string_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret);
int wildcard_eq_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret);
int wildcard_pathname_(addr a, addr b, int wild, int *ret);

#endif

