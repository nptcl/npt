#ifndef __STREAM_SYNONYM_HEADER__
#define __STREAM_SYNONYM_HEADER__

#include "typedef.h"

#define open_synonym_stream_ _n(open_synonym_stream_)
#define get_synonym_stream _n(get_synonym_stream)
#define set_synonym_stream _n(set_synonym_stream)
#define init_stream_synonym _n(init_stream_synonym)

_g int open_synonym_stream_(addr *stream, addr symbol);
_g void get_synonym_stream(addr stream, addr *ret);
_g void set_synonym_stream(addr stream, addr symbol);
_g void init_stream_synonym(void);

#endif

