#ifndef __STREAM_STRING_HEADER__
#define __STREAM_STRING_HEADER__

#include "local.h"
#include "typedef.h"

void open_input_string_stream(addr *stream, addr string);
void open_input_string_stream1(addr *stream, addr string, size_t start);
void open_input_string_stream2(addr *stream, addr string, size_t start, size_t end);
void open_input_char_stream(addr *stream, const char *str);
void null_input_string_stream(addr *stream);
void getindex_input_stream(addr stream, size_t *ret);

void setvalue_input_string_stream(addr stream, addr value);
void clear_input_string_stream(addr stream);

void open_output_string_stream(addr *stream, size_t size);
void string_stream_alloc(LocalRoot local, addr stream, addr *string);
void string_stream_local(LocalRoot local, addr stream, addr *string);
void string_stream_heap(addr stream, addr *string);
void clear_output_string_stream(addr stream);
void open_extend_output_stream(addr *stream, addr array);

void init_stream_string_input(void);
void init_stream_string_output(void);

#endif

