#ifndef __STREAM_CONCAT_HEADER__
#define __STREAM_CONCAT_HEADER__

#include "typedef.h"

#define open_concatenated_stream_ _n(open_concatenated_stream_)
#define push_concatenated_stream _n(push_concatenated_stream)
#define get_concatenated_stream _n(get_concatenated_stream)
#define init_stream_concatenated _n(init_stream_concatenated)

_g int open_concatenated_stream_(addr *stream, addr list);
_g void push_concatenated_stream(addr stream, addr input);
_g void get_concatenated_stream(addr stream, addr *ret);
_g void init_stream_concatenated(void);

#endif

