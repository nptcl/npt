#ifndef __STREAM_PRETTY_HEADER__
#define __STREAM_PRETTY_HEADER__

#include "execute.h"
#include "typedef.h"

/* pretty-stream object */
_g void open_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix);
_g int listp_pretty_stream(addr stream);
_g int discard_pretty_stream(addr stream);
_g size_t length_pretty_stream(addr stream);
_g int first_pretty_stream(addr stream);
_g void stream_pretty_stream(addr stream, addr *ret);
_g void gensym_pretty_stream(addr stream, addr *ret);
_g void root_pretty_stream(addr stream, addr *ret);
_g void prefix_pretty_stream(addr stream, addr *ret);
_g void perline_pretty_stream(addr stream, addr *ret);
_g void suffix_pretty_stream(addr stream, addr *ret);
_g void result_pretty_stream(addr stream, addr *ret);
_g void sharp_pretty_stream(addr stream, addr *ret);
_g void setsharp_pretty_stream(addr stream, addr value);
/* pretty-stream function */
_g void close_pretty_stream(Execute ptr, addr stream);
_g void push_pretty_stream(addr stream, addr pos);
_g int pop_pretty_stream(addr stream, addr *ret);
_g int call_pretty_stream(Execute ptr, addr stream, addr call);
/* stream function */
_g void init_stream_pretty(void);

#endif

