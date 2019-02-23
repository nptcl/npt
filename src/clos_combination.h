#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"

int check_qualifiers_equal(Execute ptr, addr combination, addr qualifiers);
void method_combination_qualifiers_count(addr combination, size_t *ret);
int qualifiers_position_nil(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
void qualifiers_position(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
void build_clos_combination(Execute ptr);

#endif

