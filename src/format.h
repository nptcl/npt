#ifndef __FORMAT_HEADER__
#define __FORMAT_HEADER__

#include <stdarg.h>
#include "typedef.h"

/* format */
void init_format(void);
int format_stream_args(Execute ptr, addr stream, addr format, addr args, addr *tail);
int format_string_args(Execute ptr, addr format, addr args, addr *ret, addr *tail);
int format_args(Execute ptr,
		addr stream, addr format, addr args, addr *ret, addr *tail);
int format_stream_lisp(Execute ptr, addr stream, addr format, addr args);
int format_string_lisp(Execute ptr, addr format, addr args, addr *ret);
int format_lisp(Execute ptr, addr stream, addr format, addr args, addr *ret);

/* format clang */
int format_stdarg(addr stream, const char *str, va_list args);
int format_stream(addr stream, const char *str, ...);
int format(const char *str, ...) ;
addr format_string_stdarg(LocalRoot local, const char *str, va_list args);
addr format_alloc(LocalRoot local, const char *str, ...);
addr format_local(LocalRoot local, const char *str, ...);
addr format_heap(const char *str, ...);
#define fmts format_stream
#define fmta format_alloc
#define fmth format_heap
addr fmtl(const char *str, ...);

/* printf -> string */
addr allocf_stdarg(LocalRoot local, const char *str, va_list args);
addr localf_stdarg(LocalRoot local, const char *str, va_list args);
addr heapf_stdarg(const char *str, va_list args);
addr allocf(LocalRoot local, const char *str, ...);
addr localf(const char *str, ...);
addr heapf(const char *str, ...);

#endif

