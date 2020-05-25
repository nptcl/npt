#ifndef __READER_LABEL_HEADER__
#define __READER_LABEL_HEADER__

#include "execute.h"
#include "typedef.h"

_g void dotqueue_readlabel(Execute ptr, addr queue, addr pos);
_g void pushqueue_readlabel(Execute ptr, addr queue, addr pos);
_g int find_readlabel(addr key, addr list, addr *ret);
_g void pushlabel_readinfo(Execute ptr, addr value, addr *ret);
_g void closelabel_readlabel(Execute ptr, addr label, addr pos);
_g void vector_readlabel(Execute ptr, addr pos);
_g void array_readlabel(Execute ptr, addr pos);

#endif
