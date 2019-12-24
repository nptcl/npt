#ifndef __STREAM_STRING_HEADER__
#define __STREAM_STRING_HEADER__

#include "local.h"
#include "typedef.h"

_g void open_input_string_stream(addr *stream, addr string);
_g void open_input_string_stream1(addr *stream, addr string, size_t start);
_g void open_input_string_stream2(addr *stream, addr string, size_t start, size_t end);
_g void open_input_char_stream(addr *stream, const char *str);
_g void null_input_string_stream(addr *stream);
_g void getindex_input_stream(addr stream, size_t *ret);

_g void setvalue_input_string_stream(addr stream, addr value);
_g void clear_input_string_stream(addr stream);

_g void open_output_string_stream(addr *stream, size_t size);
_g void copy_terminal_width_string_stream(addr stream, addr src);
_g void string_stream_alloc(LocalRoot local, addr stream, addr *string);
_g void string_stream_local(LocalRoot local, addr stream, addr *string);
_g void string_stream_heap(addr stream, addr *string);
_g void clear_output_string_stream(addr stream);
_g void open_extend_output_stream(addr *stream, addr array);

_g void init_stream_string_input(void);
_g void init_stream_string_output(void);

#endif

