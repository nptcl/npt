#ifndef __FORMAT_PARSE_HEADER__
#define __FORMAT_PARSE_HEADER__

#include <stddef.h>
#include "execute.h"
#include "local.h"
#include "typedef.h"

_g void format_abort(addr format, size_t position, const char *str, va_list args);
_g int formatp(addr pos);
_g void *format_pointer(addr pos);
_g size_t format_bytesize(size_t count);
_g void format_parse_local(LocalRoot local, addr *ret, addr format);
_g void format_parse_heap(LocalRoot local, addr *ret, addr format);
_g void format_string_alloc(LocalRoot local, addr *ret, addr format);
_g void format_string_local(LocalRoot local, addr *ret, addr format);
_g void format_string_heap(addr *ret, addr format);
_g void init_format_parse(void);

#endif

