#ifndef __PRINT_HEADER__
#define __PRINT_HEADER__

#include <stdarg.h>
#include "typedef.h"

#define array_print_ _n(array_print_)
#define base_print_ _n(base_print_)
#define radix_print_ _n(radix_print_)
#define case_print_ _n(case_print_)
#define circle_print_ _n(circle_print_)
#define escape_print_ _n(escape_print_)
#define gensym_print_ _n(gensym_print_)
#define readably_print_ _n(readably_print_)
#define pretty_print_ _n(pretty_print_)
#define level_print_ _n(level_print_)
#define length_print_ _n(length_print_)
#define lines_print_ _n(lines_print_)
#define miser_width_print_ _n(miser_width_print_)
#define right_margin_print_ _n(right_margin_print_)
#define pprint_dispatch_print_ _n(pprint_dispatch_print_)
#define push_array_print _n(push_array_print)
#define push_base_print _n(push_base_print)
#define push_radix_print _n(push_radix_print)
#define push_case_print_ _n(push_case_print_)
#define push_circle_print _n(push_circle_print)
#define push_escape_print _n(push_escape_print)
#define push_gensym_print _n(push_gensym_print)
#define push_readably_print _n(push_readably_print)
#define push_pretty_print _n(push_pretty_print)
#define push_level_print_ _n(push_level_print_)
#define push_level_nil_print _n(push_level_nil_print)
#define push_length_print_ _n(push_length_print_)
#define push_length_nil_print _n(push_length_nil_print)
#define push_lines_print_ _n(push_lines_print_)
#define push_lines_nil_print _n(push_lines_nil_print)
#define push_miser_width_print_ _n(push_miser_width_print_)
#define push_miser_width_nil_print _n(push_miser_width_nil_print)
#define push_right_margin_print_ _n(push_right_margin_print_)
#define push_right_margin_nil_print _n(push_right_margin_nil_print)
#define push_pprint_dispatch _n(push_pprint_dispatch)
#define print_unreadable_object_ _n(print_unreadable_object_)
#define print_unreadable_common_ _n(print_unreadable_common_)
#define build_print _n(build_print)
#define init_print _n(init_print)

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

int array_print_(Execute ptr, int *ret);
int base_print_(Execute ptr, unsigned *ret);
int radix_print_(Execute ptr, int *ret);
int case_print_(Execute ptr, enum PrintCase *ret);
int circle_print_(Execute ptr, int *ret);
int escape_print_(Execute ptr, int *ret);
int gensym_print_(Execute ptr, int *ret);
int readably_print_(Execute ptr, int *ret);
int pretty_print_(Execute ptr, int *ret);
int level_print_(Execute ptr, size_t *value, int *ret);
int length_print_(Execute ptr, size_t *value, int *ret);
int lines_print_(Execute ptr, size_t *value, int *ret);
int miser_width_print_(Execute ptr, size_t *value, int *ret);
int right_margin_print_(Execute ptr, addr stream, size_t *ret);
int pprint_dispatch_print_(Execute ptr, addr *ret);

void push_array_print(Execute ptr, int value);
void push_base_print(Execute ptr, unsigned base);
void push_radix_print(Execute ptr, int value);
int push_case_print_(Execute ptr, enum PrintCase pcase);
void push_circle_print(Execute ptr, int value);
void push_escape_print(Execute ptr, int value);
void push_gensym_print(Execute ptr, int value);
void push_readably_print(Execute ptr, int value);
void push_pretty_print(Execute ptr, int value);
int push_level_print_(Execute ptr, size_t value);
void push_level_nil_print(Execute ptr);
int push_length_print_(Execute ptr, size_t value);
void push_length_nil_print(Execute ptr);
int push_lines_print_(Execute ptr, size_t value);
void push_lines_nil_print(Execute ptr);
int push_miser_width_print_(Execute ptr, size_t value);
void push_miser_width_nil_print(Execute ptr);
int push_right_margin_print_(Execute ptr, size_t value);
void push_right_margin_nil_print(Execute ptr);
void push_pprint_dispatch(Execute ptr, addr value);

int print_unreadable_object_(Execute ptr, addr stream, addr pos,
		int type, int identity, calltype_print call);
int print_unreadable_common_(Execute ptr, addr stream, addr pos,
		int type, int identity, addr body);

/* initialize */
void build_print(Execute ptr);
void init_print(void);

#endif

