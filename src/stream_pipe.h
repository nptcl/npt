#ifndef __STREAM_PIPE_HEADER__
#define __STREAM_PIPE_HEADER__

#include "stream_object.h"
#include "typedef.h"

#define open_pipe_stream _n(open_pipe_stream)
#define get_type_pipe_stream _n(get_type_pipe_stream)
#define set_type_pipe_stream _n(set_type_pipe_stream)

void open_pipe_stream(addr *stream, enum StreamPipe type);
enum StreamPipe get_type_pipe_stream(addr stream);
void set_type_pipe_stream(addr stream, enum StreamPipe type);

#endif

