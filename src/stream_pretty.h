#ifndef __STREAM_PRETTY_HEADER__
#define __STREAM_PRETTY_HEADER__

#include "typedef.h"

_g void open_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix);
_g void get_pretty_stream(addr stream, addr *ret);
_g void set_pretty_stream(addr stream, addr pos);
_g void catch_pretty_stream(addr stream, addr *ret);
_g void object_pretty_stream(addr stream, addr *ret);
_g void suffix_pretty_stream(addr stream, addr *ret);
_g void close_pretty_stream(addr stream);
_g void next_pretty_stream(addr stream);
_g int first_pretty_stream(addr stream);
_g int pop_pretty_stream(addr stream, addr *ret);
_g size_t length_pretty_stream(addr stream);
_g void init_stream_pretty(void);

#endif

