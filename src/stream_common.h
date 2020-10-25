#ifndef __STREAM_COMMON_HEADER__
#define __STREAM_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

#define read_binary_stream_ _n(read_binary_stream_)
#define write_binary_stream_ _n(write_binary_stream_)
#define terpri_stream_ _n(terpri_stream_)
#define fresh_line_stream_ _n(fresh_line_stream_)
#define peek_char_stream_ _n(peek_char_stream_)
#define read_line_stream_ _n(read_line_stream_)
#define write_string_stream _n(write_string_stream)
#define read_sequence_stream _n(read_sequence_stream)
#define write_sequence_stream _n(write_sequence_stream)

_g int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret);
_g int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret);
_g int terpri_stream_(addr stream);
_g int fresh_line_stream_(addr stream, int *ret);
_g int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp);
_g int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp);
_g int write_string_stream(Execute ptr, addr string, addr rest, addr *ret);
_g int read_sequence_stream(addr *ret, addr seq, addr stream, size_t start, size_t end);
_g int write_sequence_stream(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end);

#endif

