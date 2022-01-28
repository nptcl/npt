#ifndef __FORMAT_HEADER__
#define __FORMAT_HEADER__

#include "execute.h"
#include "typedef.h"

#define format_stream_lisp_ _n(format_stream_lisp_)
#define format_string_lisp_ _n(format_string_lisp_)
#define format_lisp_ _n(format_lisp_)
#define format_stream_ _n(format_stream_)
#define format_string_ _n(format_string_)
#define format_stdout_ _n(format_stdout_)
#define formatf _n(formatf)
#define init_format _n(init_format)

int format_stream_lisp_(Execute ptr, addr stream, addr format, addr args);
int format_string_lisp_(Execute ptr, addr format, addr args, addr *ret);
int format_lisp_(Execute ptr, addr stream, addr format, addr args, addr *ret);

int format_stream_(Execute ptr, addr stream, const char *str, ...);
int format_string_(Execute ptr, addr *ret, const char *str, ...);
int format_stdout_(Execute ptr, const char *str, ...);

void formatf(const char *str, ...);

void init_format(void);

#endif

