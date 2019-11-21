#ifndef __PRINT_HEADER__
#define __PRINT_HEADER__

#include <stdarg.h>
#include "typedef.h"

enum PrintCase {
	PrintCase_unread = 0,
	PrintCase_upcase,
	PrintCase_downcase,
	PrintCase_capitalize,
	PrintCase_preserve,
	PrintCase_invert,
	PrintCase_escape,
	PrintCase_size
};
typedef int (*calltype_print)(Execute ptr, addr stream, addr object);

_g int array_print(Execute ptr);
_g unsigned base_print(Execute ptr);
_g int radix_print(Execute ptr);
_g enum PrintCase case_print(Execute ptr);
_g int circle_print(Execute ptr);
_g int escape_print(Execute ptr);
_g int gensym_print(Execute ptr);
_g int readably_print(Execute ptr);
_g int pretty_print(Execute ptr);
_g int level_print(Execute ptr, size_t *ret);
_g int length_print(Execute ptr, size_t *ret);
_g int lines_print(Execute ptr, size_t *ret);
_g int miser_width_print(Execute ptr, size_t *ret);
_g void right_margin_print(Execute ptr, size_t *ret);
_g void pprint_dispatch_print(Execute ptr, addr *ret);

_g void push_array_print(Execute ptr, int value);
_g void push_base_print(Execute ptr, unsigned base);
_g void push_radix_print(Execute ptr, int value);
_g void push_case_print(Execute ptr, enum PrintCase pcase);
_g void push_circle_print(Execute ptr, int value);
_g void push_escape_print(Execute ptr, int value);
_g void push_gensym_print(Execute ptr, int value);
_g void push_readably_print(Execute ptr, int value);
_g void push_pretty_print(Execute ptr, int value);
_g void push_level_print(Execute ptr, size_t value);
_g void push_level_nil_print(Execute ptr);
_g void push_length_print(Execute ptr, size_t value);
_g void push_length_nil_print(Execute ptr);
_g void push_lines_print(Execute ptr, size_t value);
_g void push_lines_nil_print(Execute ptr);
_g void push_miser_width_print(Execute ptr, size_t value);
_g void push_miser_width_nil_print(Execute ptr);
_g void push_right_margin_print(Execute ptr, size_t value);
_g void push_right_margin_nil_print(Execute ptr);
_g void push_pprint_dispatch(Execute ptr, addr value);

_g int print_unreadable_object(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call);
_g int print_unreadable_common(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body);

/* initialize */
_g void build_print(Execute ptr);
_g void init_print(void);

#endif

