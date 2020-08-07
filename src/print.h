#ifndef __PRINT_HEADER__
#define __PRINT_HEADER__

#include <stdarg.h>
#include "typedef.h"

#define PRINT_DEFAULT_WIDTH		72

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

_g int array_print_(Execute ptr, int *ret);
_g int base_print_(Execute ptr, unsigned *ret);
_g int radix_print_(Execute ptr, int *ret);
_g int case_print_(Execute ptr, enum PrintCase *ret);
_g int circle_print_(Execute ptr, int *ret);
_g int escape_print_(Execute ptr, int *ret);
_g int gensym_print_(Execute ptr, int *ret);
_g int readably_print_(Execute ptr, int *ret);
_g int pretty_print_(Execute ptr, int *ret);
_g int level_print_(Execute ptr, size_t *value, int *ret);
_g int length_print_(Execute ptr, size_t *value, int *ret);
_g int lines_print_(Execute ptr, size_t *value, int *ret);
_g int miser_width_print_(Execute ptr, size_t *value, int *ret);
_g int right_margin_print_(Execute ptr, addr stream, size_t *ret);
_g int pprint_dispatch_print_(Execute ptr, addr *ret);

_g void push_array_print(Execute ptr, int value);
_g void push_base_print(Execute ptr, unsigned base);
_g void push_radix_print(Execute ptr, int value);
_g int push_case_print_(Execute ptr, enum PrintCase pcase);
_g void push_circle_print(Execute ptr, int value);
_g void push_escape_print(Execute ptr, int value);
_g void push_gensym_print(Execute ptr, int value);
_g void push_readably_print(Execute ptr, int value);
_g void push_pretty_print(Execute ptr, int value);
_g int push_level_print_(Execute ptr, size_t value);
_g void push_level_nil_print(Execute ptr);
_g int push_length_print_(Execute ptr, size_t value);
_g void push_length_nil_print(Execute ptr);
_g int push_lines_print_(Execute ptr, size_t value);
_g void push_lines_nil_print(Execute ptr);
_g int push_miser_width_print_(Execute ptr, size_t value);
_g void push_miser_width_nil_print(Execute ptr);
_g int push_right_margin_print_(Execute ptr, size_t value);
_g void push_right_margin_nil_print(Execute ptr);
_g void push_pprint_dispatch(Execute ptr, addr value);

_g int print_unreadable_object_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call);
_g int print_unreadable_common_(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body);

/* initialize */
_g void build_print(Execute ptr);
_g void init_print(void);

#endif

