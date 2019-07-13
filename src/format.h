#ifndef __FORMAT_HEADER__
#define __FORMAT_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "local.h"
#include "typedef.h"

/* format */
_g void init_format(void);
_g int format_stream_args(Execute ptr, addr stream, addr format, addr args, addr *tail);
_g int format_string_args(Execute ptr, addr format, addr args, addr *ret, addr *tail);
_g int format_args(Execute ptr,
		addr stream, addr format, addr args, addr *ret, addr *tail);
_g int format_stream_lisp(Execute ptr, addr stream, addr format, addr args);
_g int format_string_lisp(Execute ptr, addr format, addr args, addr *ret);
_g int format_lisp(Execute ptr, addr stream, addr format, addr args, addr *ret);

/* format clang */
_g int format_stdarg(addr stream, const char *str, va_list args);
_g int format_stream(addr stream, const char *str, ...);
_g int format(const char *str, ...) ;
_g addr format_string_stdarg(LocalRoot local, const char *str, va_list args);
_g addr format_alloc(LocalRoot local, const char *str, ...);
_g addr format_local(LocalRoot local, const char *str, ...);
_g addr format_heap(const char *str, ...);
#define fmts format_stream
#define fmta format_alloc
#define fmth format_heap
_g addr fmtl(const char *str, ...);

/* printf -> string */
_g addr allocf_stdarg(LocalRoot local, const char *str, va_list args);
_g addr localf_stdarg(LocalRoot local, const char *str, va_list args);
_g addr heapf_stdarg(const char *str, va_list args);
_g addr allocf(LocalRoot local, const char *str, ...);
_g addr localf(const char *str, ...);
_g addr heapf(const char *str, ...);

#endif

