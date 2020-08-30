#ifndef __READER_LABEL_HEADER__
#define __READER_LABEL_HEADER__

#include "execute.h"
#include "typedef.h"

#define dotqueue_readlabel _n(dotqueue_readlabel)
#define pushqueue_readlabel _n(pushqueue_readlabel)
#define find_readlabel _n(find_readlabel)
#define pushlabel_readinfo_ _n(pushlabel_readinfo_)
#define closelabel_readlabel_ _n(closelabel_readlabel_)
#define vector_readlabel _n(vector_readlabel)
#define array_readlabel_ _n(array_readlabel_)

_g void dotqueue_readlabel(Execute ptr, addr queue, addr pos);
_g void pushqueue_readlabel(Execute ptr, addr queue, addr pos);
_g int find_readlabel(addr key, addr list, addr *ret);
_g int pushlabel_readinfo_(Execute ptr, addr value, addr *ret);
_g int closelabel_readlabel_(Execute ptr, addr label, addr pos);
_g void vector_readlabel(Execute ptr, addr pos);
_g int array_readlabel_(Execute ptr, addr pos);

#endif

