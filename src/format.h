#ifndef __FORMAT_HEADER__
#define __FORMAT_HEADER__

#include "execute.h"
#include "typedef.h"

_g int format_stream_lisp(Execute ptr, addr stream, addr format, addr args);
_g int format_string_lisp(Execute ptr, addr format, addr args, addr *ret);
_g int format_lisp(Execute ptr, addr stream, addr format, addr args, addr *ret);

_g int format_stream(Execute ptr, addr stream, const char *str, ...);
_g int format_string(Execute ptr, addr *ret, const char *str, ...);
_g int format_stdout(Execute ptr, const char *str, ...);

_g void init_format(void);

#endif

