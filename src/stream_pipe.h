#ifndef __STREAM_PIPE_HEADER__
#define __STREAM_PIPE_HEADER__

#include "stream_object.h"
#include "typedef.h"

#define open_pipe_stream _n(open_pipe_stream)
#define get_type_pipe_stream _n(get_type_pipe_stream)
#define set_type_pipe_stream _n(set_type_pipe_stream)
#define get_name_pipe_stream _n(get_name_pipe_stream)
#define set_name_pipe_stream _n(set_name_pipe_stream)
#define init_stream_pipe _n(init_stream_pipe)

enum pipe_stream_name {
	pipe_stream_default,
	pipe_stream_input,
	pipe_stream_output,
	pipe_stream_error
};

void open_pipe_stream(addr *stream, enum StreamPipe type);
enum StreamPipe get_type_pipe_stream(addr stream);
void set_type_pipe_stream(addr stream, enum StreamPipe type);
const char *get_name_pipe_stream(addr stream);
void set_name_pipe_stream(addr stream, enum pipe_stream_name type);
void init_stream_pipe(void);

#endif

