#ifndef __FORMAT_PARSE_HEADER__
#define __FORMAT_PARSE_HEADER__

#include <stddef.h>
#include "execute.h"
#include "local.h"
#include "typedef.h"

#define format_abort_ _n(format_abort_)
#define formatp _n(formatp)
#define format_pointer _n(format_pointer)
#define format_bytesize _n(format_bytesize)
#define format_parse_local_ _n(format_parse_local_)
#define format_parse_heap_ _n(format_parse_heap_)
#define format_string_alloc_ _n(format_string_alloc_)
#define format_string_local_ _n(format_string_local_)
#define format_string_heap_ _n(format_string_heap_)
#define init_format_parse _n(init_format_parse)

int format_abort_(addr format, size_t position, const char *str, va_list args);
int formatp(addr pos);
void *format_pointer(addr pos);
size_t format_bytesize(size_t count);
int format_parse_local_(LocalRoot local, addr *ret, addr format);
int format_parse_heap_(LocalRoot local, addr *ret, addr format);
int format_string_alloc_(LocalRoot local, addr *ret, addr format);
int format_string_local_(LocalRoot local, addr *ret, addr format);
int format_string_heap_(addr *ret, addr format);
void init_format_parse(void);

#endif

