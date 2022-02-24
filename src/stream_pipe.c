#include "stream_object.h"
#include "stream_pipe.h"
#include "typedef.h"

struct stream_pipe_struct {
	enum StreamPipe type;
};

#define CheckPipeStream(stream) { \
	Check(! pipe_stream_p(stream), "type error"); \
}
#define PtrStreamPipeStruct(pos) ((struct stream_pipe_struct *)PtrDataStream(pos))

void open_pipe_stream(addr *stream, enum StreamPipe type)
{
	addr pos;
	struct stream_pipe_struct *str;

	stream_heap(&pos, StreamType_Pipe, sizeoft(struct stream_pipe_struct));
	str = PtrStreamPipeStruct(pos);
	str->type = type;
	force_open_stream(pos);
	*stream = pos;
}

enum StreamPipe get_type_pipe_stream(addr stream)
{
	struct stream_pipe_struct *str;

	CheckPipeStream(stream);
	str = PtrStreamPipeStruct(stream);
	return str->type;
}

void set_type_pipe_stream(addr stream, enum StreamPipe type)
{
	struct stream_pipe_struct *str;

	CheckPipeStream(stream);
	str = PtrStreamPipeStruct(stream);
	str->type = type;
}

