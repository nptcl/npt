#ifndef __STREAM_PRETTY_HEADER__
#define __STREAM_PRETTY_HEADER__

#include "execute.h"
#include "typedef.h"

#define open_pretty_stream_ _n(open_pretty_stream_)
#define setlistp_pretty_stream _n(setlistp_pretty_stream)
#define listp_pretty_stream _n(listp_pretty_stream)
#define setdiscard_pretty_stream _n(setdiscard_pretty_stream)
#define discard_pretty_stream _n(discard_pretty_stream)
#define length_pretty_stream_ _n(length_pretty_stream_)
#define first_pretty_stream_ _n(first_pretty_stream_)
#define stream_pretty_stream _n(stream_pretty_stream)
#define gensym_pretty_stream_ _n(gensym_pretty_stream_)
#define root_pretty_stream_ _n(root_pretty_stream_)
#define setroot_pretty_stream_ _n(setroot_pretty_stream_)
#define object_pretty_stream_ _n(object_pretty_stream_)
#define prefix_pretty_stream _n(prefix_pretty_stream)
#define perline_pretty_stream _n(perline_pretty_stream)
#define suffix_pretty_stream _n(suffix_pretty_stream)
#define result_pretty_stream _n(result_pretty_stream)
#define sharp_pretty_stream _n(sharp_pretty_stream)
#define setsharp_pretty_stream _n(setsharp_pretty_stream)
#define setdepth_pretty_stream _n(setdepth_pretty_stream)
#define close_pretty_stream_ _n(close_pretty_stream_)
#define push_pretty_stream_ _n(push_pretty_stream_)
#define pop_pretty_stream_ _n(pop_pretty_stream_)
#define push_pretty_stream_p _n(push_pretty_stream_p)
#define call_pretty_stream _n(call_pretty_stream)
#define init_stream_pretty _n(init_stream_pretty)

/* pretty-stream object */
_g int open_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix);
_g void setlistp_pretty_stream(addr stream, int value);
_g int listp_pretty_stream(addr stream);
_g void setdiscard_pretty_stream(addr stream, int value);
_g int discard_pretty_stream(addr stream);
_g int length_pretty_stream_(addr stream, size_t *ret);
_g int first_pretty_stream_(addr stream, int *ret);
_g void stream_pretty_stream(addr stream, addr *ret);
_g int gensym_pretty_stream_(addr stream, addr *ret);
_g int root_pretty_stream_(addr stream, addr *ret);
_g int setroot_pretty_stream_(addr stream, addr value);
_g int object_pretty_stream_(addr stream, addr *ret);
_g void prefix_pretty_stream(addr stream, addr *ret);
_g void perline_pretty_stream(addr stream, addr *ret);
_g void suffix_pretty_stream(addr stream, addr *ret);
_g void result_pretty_stream(addr stream, addr *ret);
_g void sharp_pretty_stream(addr stream, addr *ret);
_g void setsharp_pretty_stream(addr stream, addr value);
/* pretty-stream function */
_g void setdepth_pretty_stream(Execute ptr, addr stream, size_t inc);
_g int close_pretty_stream_(Execute ptr, addr stream);
_g int push_pretty_stream_(addr stream, addr pos);
_g int pop_pretty_stream_(addr stream, addr *value, int *ret);
_g int push_pretty_stream_p(addr stream);
_g int call_pretty_stream(Execute ptr, addr stream, addr call);
/* stream function */
_g void init_stream_pretty(void);

#endif

