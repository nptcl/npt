#include "condition.h"
#include "stream_object.h"
#include "stream_pipe.h"
#include "stream_variable.h"
#include "typedef.h"

struct stream_pipe_struct {
	enum StreamPipe type;
	const char *name;
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
	str->name = NULL;
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

const char *get_name_pipe_stream(addr stream)
{
	struct stream_pipe_struct *str;

	CheckPipeStream(stream);
	str = PtrStreamPipeStruct(stream);
	return str->name;
}

void set_name_pipe_stream(addr stream, const char *name)
{
	struct stream_pipe_struct *str;

	CheckPipeStream(stream);
	str = PtrStreamPipeStruct(stream);
	str->name = name;
}

static int getstream_pipe_(addr stream, int *ret)
{
	struct stream_pipe_struct *str;
	enum StreamPipe type;

	CheckPipeStream(stream);
	str = PtrStreamPipeStruct(stream);
	type = str->type;
	*ret = StreamPipe_Index(type);

	return 0;
}

#define getstream_pipe_call(stream, index, call, name) { \
	Return(getstream_pipe_(stream, &index)); \
	call = Stream_##name[index]; \
	if (call == NULL) { \
		return fmte_("Stream call error, ~S.", stream, NULL); \
	} \
}

static int close_Pipe(addr stream, addr *ret)
{
	int index;
	lisp_streamtype_close call;
	getstream_pipe_call(stream, index, call, close);
	return (*call)(stream, ret);
}

static int read_byte_Pipe(addr stream, addr *value, int *ret)
{
	int index;
	lisp_streamtype_read_byte call;
	getstream_pipe_call(stream, index, call, read_byte);
	return (*call)(stream, value, ret);
}

static int unread_byte_Pipe(addr stream, byte c)
{
	int index;
	lisp_streamtype_unread_byte call;
	getstream_pipe_call(stream, index, call, unread_byte);
	return (*call)(stream, c);
}

static int write_byte_Pipe(addr stream, addr pos)
{
	int index;
	lisp_streamtype_write_byte call;
	getstream_pipe_call(stream, index, call, write_byte);
	return (*call)(stream, pos);
}

static int read_char_Pipe(addr stream, unicode *c, int *ret)
{
	int index;
	lisp_streamtype_read_char call;
	getstream_pipe_call(stream, index, call, read_char);
	return (*call)(stream, c, ret);
}

static int read_hang_Pipe(addr stream, unicode *c, int *hang, int *ret)
{
	int index;
	lisp_streamtype_read_hang call;
	getstream_pipe_call(stream, index, call, read_hang);
	return (*call)(stream, c, hang, ret);
}

static int unread_char_Pipe(addr stream, unicode c)
{
	int index;
	lisp_streamtype_unread_char call;
	getstream_pipe_call(stream, index, call, unread_char);
	return (*call)(stream, c);
}

static int write_char_Pipe(addr stream, unicode c)
{
	int index;
	lisp_streamtype_write_char call;
	getstream_pipe_call(stream, index, call, write_char);
	return (*call)(stream, c);
}

static int getleft_Pipe(addr stream, size_t *ret)
{
	int index;
	lisp_streamtype_getleft call;
	getstream_pipe_call(stream, index, call, getleft);
	return (*call)(stream, ret);
}

static int setleft_Pipe(addr stream, size_t value)
{
	int index;
	lisp_streamtype_setleft call;
	getstream_pipe_call(stream, index, call, setleft);
	return (*call)(stream, value);
}

static int inputp_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_inputp call;
	getstream_pipe_call(stream, index, call, inputp);
	return (*call)(stream, ret);
}

static int outputp_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_outputp call;
	getstream_pipe_call(stream, index, call, outputp);
	return (*call)(stream, ret);
}

static int interactivep_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_interactivep call;
	getstream_pipe_call(stream, index, call, interactivep);
	return (*call)(stream, ret);
}

static int characterp_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_characterp call;
	getstream_pipe_call(stream, index, call, characterp);
	return (*call)(stream, ret);
}

static int binaryp_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_binaryp call;
	getstream_pipe_call(stream, index, call, binaryp);
	return (*call)(stream, ret);
}

static int element_type_Pipe(addr stream, addr *ret)
{
	int index;
	lisp_streamtype_element_type call;
	getstream_pipe_call(stream, index, call, element_type);
	return (*call)(stream, ret);
}

static int external_format_Pipe(addr stream, addr *ret)
{
	int index;
	lisp_streamtype_external_format call;
	getstream_pipe_call(stream, index, call, external_format);
	return (*call)(stream, ret);
}

static int file_length_Pipe(addr stream, addr *ret)
{
	int index;
	lisp_streamtype_file_length call;
	getstream_pipe_call(stream, index, call, file_length);
	return (*call)(stream, ret);
}

static int file_position_Pipe(addr stream, size_t *value, int *ret)
{
	int index;
	lisp_streamtype_file_position call;
	getstream_pipe_call(stream, index, call, file_position);
	return (*call)(stream, value, ret);
}

static int file_position_start_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_file_position_start call;
	getstream_pipe_call(stream, index, call, file_position_start);
	return (*call)(stream, ret);
}

static int file_position_end_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_file_position_end call;
	getstream_pipe_call(stream, index, call, file_position_end);
	return (*call)(stream, ret);
}

static int file_position_set_Pipe(addr stream, size_t value, int *ret)
{
	int index;
	lisp_streamtype_file_position_set call;
	getstream_pipe_call(stream, index, call, file_position_set);
	return (*call)(stream, value, ret);
}

static int file_charlen_Pipe(addr stream, unicode c, size_t *value, int *ret)
{
	int index;
	lisp_streamtype_file_charlen call;
	getstream_pipe_call(stream, index, call, file_charlen);
	return (*call)(stream, c, value, ret);
}

static int file_strlen_Pipe(addr stream, addr pos, size_t *value, int *ret)
{
	int index;
	lisp_streamtype_file_strlen call;
	getstream_pipe_call(stream, index, call, file_strlen);
	return (*call)(stream, pos, value, ret);
}

static int listen_Pipe(addr stream, int *ret)
{
	int index;
	lisp_streamtype_listen call;
	getstream_pipe_call(stream, index, call, listen);
	return (*call)(stream, ret);
}

static int clear_input_Pipe(addr stream)
{
	int index;
	lisp_streamtype_clear_input call;
	getstream_pipe_call(stream, index, call, clear_input);
	return (*call)(stream);
}

static int finish_output_Pipe(addr stream)
{
	int index;
	lisp_streamtype_finish_output call;
	getstream_pipe_call(stream, index, call, finish_output);
	return (*call)(stream);
}

static int force_output_Pipe(addr stream)
{
	int index;
	lisp_streamtype_force_output call;
	getstream_pipe_call(stream, index, call, force_output);
	return (*call)(stream);
}

static int clear_output_Pipe(addr stream)
{
	int index;
	lisp_streamtype_clear_output call;
	getstream_pipe_call(stream, index, call, clear_output);
	return (*call)(stream);
}

static int exitpoint_Pipe(addr stream)
{
	int index;
	lisp_streamtype_exitpoint call;
	getstream_pipe_call(stream, index, call, exitpoint);
	return (*call)(stream);
}

static int termsize_Pipe(addr stream, size_t *value, int *ret)
{
	int index;
	lisp_streamtype_termsize call;
	getstream_pipe_call(stream, index, call, termsize);
	return (*call)(stream, value, ret);
}

void init_stream_pipe(void)
{
	DefineStreamSet(Pipe, close);
	DefineStreamSet(Pipe, read_byte);
	DefineStreamSet(Pipe, unread_byte);
	DefineStreamSet(Pipe, write_byte);
	DefineStreamSet(Pipe, read_char);
	DefineStreamSet(Pipe, read_hang);
	DefineStreamSet(Pipe, unread_char);
	DefineStreamSet(Pipe, write_char);
	DefineStreamSet(Pipe, getleft);
	DefineStreamSet(Pipe, setleft);
	DefineStreamSet(Pipe, inputp);
	DefineStreamSet(Pipe, outputp);
	DefineStreamSet(Pipe, interactivep);
	DefineStreamSet(Pipe, characterp);
	DefineStreamSet(Pipe, binaryp);
	DefineStreamSet(Pipe, element_type);
	DefineStreamSet(Pipe, external_format);
	DefineStreamSet(Pipe, file_length);
	DefineStreamSet(Pipe, file_position);
	DefineStreamSet(Pipe, file_position_start);
	DefineStreamSet(Pipe, file_position_end);
	DefineStreamSet(Pipe, file_position_set);
	DefineStreamSet(Pipe, file_charlen);
	DefineStreamSet(Pipe, file_strlen);
	DefineStreamSet(Pipe, listen);
	DefineStreamSet(Pipe, clear_input);
	DefineStreamSet(Pipe, finish_output);
	DefineStreamSet(Pipe, force_output);
	DefineStreamSet(Pipe, clear_output);
	DefineStreamSet(Pipe, exitpoint);
	DefineStreamSet(Pipe, termsize);
}

