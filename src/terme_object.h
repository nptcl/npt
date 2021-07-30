#ifndef __TERME_OBJECT_HEADER__
#define __TERME_OBJECT_HEADER__

#include "prompt.h"
#include "typedef.h"

#define terme_pointer _n(terme_pointer)
#define terme_get _n(terme_get)
#define terme_set _n(terme_set)
#define terme_get_type _n(terme_get_type)
#define terme_set_type _n(terme_set_type)
#define termep _n(termep)
#define terme_root_p _n(terme_root_p)
#define terme_data_p _n(terme_data_p)
#define terme_string_p _n(terme_string_p)
#define terme_screen_p _n(terme_screen_p)
#define terme_display_p _n(terme_display_p)
#define terme_line_p _n(terme_line_p)
#define terme_history_p _n(terme_history_p)
#define terme_root_build _n(terme_root_build)
#define terme_root_data_ _n(terme_root_data_)
#define terme_root_screen_ _n(terme_root_screen_)
#define terme_root_display_ _n(terme_root_display_)
#define terme_root_history_ _n(terme_root_history_)

#define terme_prompt_set_ _n(terme_prompt_set_)
#define terme_prompt_get_ _n(terme_prompt_get_)

enum terme_root_index {
	terme_root_prompt,
	terme_root_data,
	terme_root_screen,
	terme_root_display,
	terme_root_history,
	terme_root_size
};

enum terme_type {
	terme_type_root,
	terme_type_data,
	terme_type_string,
	terme_type_screen,
	terme_type_display,
	terme_type_line,
	terme_type_history,
	terme_type_size
};

byte *terme_pointer(addr pos);
void terme_get(addr pos, size_t index, addr *ret);
void terme_set(addr pos, size_t index, addr value);
enum terme_type terme_get_type(addr pos);
void terme_set_type(addr pos, enum terme_type type);
int termep(addr pos);
int terme_root_p(addr pos);
int terme_data_p(addr pos);
int terme_string_p(addr pos);
int terme_screen_p(addr pos);
int terme_display_p(addr pos);
int terme_line_p(addr pos);
int terme_history_p(addr pos);
void terme_root_build(addr *ret);
int terme_root_data_(Execute ptr, addr *ret);
int terme_root_screen_(Execute ptr, addr *ret);
int terme_root_display_(Execute ptr, addr *ret);
int terme_root_history_(Execute ptr, addr *ret);

int terme_prompt_set_(Execute ptr, addr value, enum prompt_mode mode);
int terme_prompt_get_(Execute ptr, addr *value, enum prompt_mode *mode);

#endif

