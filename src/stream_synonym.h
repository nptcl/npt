#ifndef __STREAM_SYNONYM_HEADER__
#define __STREAM_SYNONYM_HEADER__

#include "typedef.h"

_g int open_synonym_stream_(addr *stream, addr symbol);
_g void get_synonym_stream(addr stream, addr *ret);
_g void set_synonym_stream(addr stream, addr symbol);
_g void init_stream_synonym(void);

#endif

