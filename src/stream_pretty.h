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
#define close_pretty_stream_unwind_protect_ _n(close_pretty_stream_unwind_protect_)
#define push_pretty_stream_ _n(push_pretty_stream_)
#define pop_pretty_stream_ _n(pop_pretty_stream_)
#define push_pretty_stream_p _n(push_pretty_stream_p)
#define call_pretty_stream _n(call_pretty_stream)
#define init_stream_pretty _n(init_stream_pretty)

/* pretty-stream object */
int open_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix);
void setlistp_pretty_stream(addr stream, int value);
int listp_pretty_stream(addr stream);
void setdiscard_pretty_stream(addr stream, int value);
int discard_pretty_stream(addr stream);
int length_pretty_stream_(addr stream, size_t *ret);
int first_pretty_stream_(addr stream, int *ret);
void stream_pretty_stream(addr stream, addr *ret);
int gensym_pretty_stream_(addr stream, addr *ret);
int root_pretty_stream_(addr stream, addr *ret);
int setroot_pretty_stream_(addr stream, addr value);
int object_pretty_stream_(addr stream, addr *ret);
void prefix_pretty_stream(addr stream, addr *ret);
void perline_pretty_stream(addr stream, addr *ret);
void suffix_pretty_stream(addr stream, addr *ret);
void result_pretty_stream(addr stream, addr *ret);
void sharp_pretty_stream(addr stream, addr *ret);
void setsharp_pretty_stream(addr stream, addr value);
/* pretty-stream function */
void setdepth_pretty_stream(Execute ptr, addr stream, size_t inc);
int close_pretty_stream_(Execute ptr, addr stream);
int close_pretty_stream_unwind_protect_(Execute ptr, addr stream);
int push_pretty_stream_(addr stream, addr pos);
int pop_pretty_stream_(addr stream, addr *value, int *ret);
int push_pretty_stream_p(addr stream);
int call_pretty_stream(Execute ptr, addr stream, addr call);
/* stream function */
void init_stream_pretty(void);

#endif

