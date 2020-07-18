#include "condition.h"
#include "object.h"
#include "stream_echo.h"
#include "stream_error.h"
#include "stream.h"

#define CheckEchoStream(stream) { \
	Check(! echo_stream_p(stream), "type error"); \
}

_g void open_echo_stream(addr *stream, addr input, addr output)
{
	addr pos;

	CheckType(input, LISPTYPE_STREAM);
	CheckType(output, LISPTYPE_STREAM);
	stream_heap(&pos, StreamType_Echo, 0);
	SetInputStream(pos, input);
	SetOutputStream(pos, output);
	force_open_stream(pos);
	*stream = pos;
}

_g void get_echo_input_stream(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetInputStream(stream, ret);
}

_g void set_echo_input_stream(addr stream, addr input)
{
	CheckEchoStream(stream);
	SetInputStream(stream, input);
}

_g void get_echo_output_stream(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetOutputStream(stream, ret);
}

_g void set_echo_output_stream(addr stream, addr output)
{
	CheckEchoStream(stream);
	SetOutputStream(stream, output);
}

static void input_Echo(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetInputStream(stream, ret);
}

static void output_Echo(addr stream, addr *ret)
{
	CheckEchoStream(stream);
	GetOutputStream(stream, ret);
}

static void io_Echo(addr stream, addr *input, addr *output)
{
	CheckEchoStream(stream);
	GetInputStream(stream, input);
	GetOutputStream(stream, output);
}

static int read_binary_Echo(addr stream, void *pos, size_t size, size_t *ret)
{
	addr input, output;
	size_t value;

	io_Echo(stream, &input, &output);
	Return(read_binary_stream_(input, pos, size, &size));
	if (size) {
		Return(write_binary_stream_(output, pos, size, &value));
	}

	return Result(ret, size);
}

static int readf_binary_Echo(addr stream, void *pos, size_t size, size_t *ret)
{
	addr input, output;
	size_t value;

	io_Echo(stream, &input, &output);
	Return(readf_binary_stream_(stream, pos, size, &size));
	if (size) {
		Return(write_binary_stream_(stream, pos, size, &value));
	}

	return Result(ret, size);
}

static int read_byte_Echo(addr stream, byte *c, int *ret)
{
	int check;
	addr input, output;

	io_Echo(stream, &input, &output);
	Return(read_byte_stream_(input, c, &check));
	if (check == 0) {
		Return(write_byte_stream_(output, *c));
	}

	return Result(ret, check);
}

static int unread_byte_Echo(addr stream, byte c)
{
	input_Echo(stream, &stream);
	return unread_byte_stream_(stream, c);
}

static int write_binary_Echo(addr stream, const void *pos, size_t size, size_t *ret)
{
	output_Echo(stream, &stream);
	return write_binary_stream_(stream, pos, size, ret);
}

static int write_byte_Echo(addr stream, byte c)
{
	output_Echo(stream, &stream);
	return write_byte_stream_(stream, c);
}

static int read_char_Echo(addr stream, unicode *u, int *ret)
{
	int check;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	Return(read_char_stream_(input, u, &check));
	if (check == 0 && ptr->unread_check == 0) {
		Return(write_char_stream_(output, *u));
	}
	ptr->unread_check = 0;

	return Result(ret, check);
}

static int read_hang_Echo(addr stream, unicode *u, int *hang, int *ret)
{
	int check, value;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	Return(read_hang_stream_(input, u, &value, &check));
	if (check == 0) {
		if (value == 0 && ptr->unread_check == 0) {
			Return(write_char_stream_(output, *u));
		}
		*hang = value;
	}
	ptr->unread_check = 0;

	return Result(ret, check);
}

static int unread_char_Echo(addr stream, unicode c)
{
	addr input;

	input_Echo(stream, &input);
	Return(unread_char_stream_(input, c));
	PtrStructStream(stream)->unread_check = 1;

	return 0;
}

static int write_char_Echo(addr stream, unicode u)
{
	output_Echo(stream, &stream);
	return write_char_stream_(stream, u);
}

static int terpri_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return terpri_stream_(stream);
}

static int getleft_Echo(addr stream, size_t *ret)
{
	output_Echo(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_Echo(addr stream, size_t value)
{
	output_Echo(stream, &stream);
	return setleft_stream_(stream, value);
}

static int fresh_line_Echo(addr stream, int *ret)
{
	output_Echo(stream, &stream);
	return fresh_line_stream_(stream, ret);
}

static int characterp_Echo(addr stream)
{
	addr input, output;
	io_Echo(stream, &input, &output);
	return characterp_stream(input) && characterp_stream(output);
}

static int binaryp_Echo(addr stream)
{
	addr input, output;
	io_Echo(stream, &input, &output);
	return binaryp_stream(input) && binaryp_stream(output);
}

static int file_charlen_Echo(addr stream, unicode u, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Echo(addr stream, addr pos, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Echo(addr stream, int *ret)
{
	input_Echo(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_Echo(addr stream)
{
	input_Echo(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return clear_output_stream_(stream);
}

static int exitpoint_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return exitpoint_stream_(stream);
}

static int termsize_Echo(addr stream, size_t *value, int *ret)
{
	output_Echo(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

_g void init_stream_echo(void)
{
	DefineStreamDef(Echo, close);
	DefineStreamSet(Echo, read_binary);
	DefineStreamSet(Echo, readf_binary);
	DefineStreamSet(Echo, read_byte);
	DefineStreamSet(Echo, unread_byte);
	DefineStreamSet(Echo, write_binary);
	DefineStreamSet(Echo, write_byte);
	DefineStreamSet(Echo, read_char);
	DefineStreamSet(Echo, read_hang);
	DefineStreamSet(Echo, unread_char);
	DefineStreamSet(Echo, write_char);
	DefineStreamSet(Echo, terpri);
	DefineStreamSet(Echo, getleft);
	DefineStreamSet(Echo, setleft);
	DefineStreamSet(Echo, fresh_line);
	DefineStreamChk(Echo, inputp, true);
	DefineStreamChk(Echo, outputp, true);
	DefineStreamChk(Echo, interactivep, false);
	DefineStreamSet(Echo, characterp);
	DefineStreamSet(Echo, binaryp);
	DefineStreamLet(Echo, element_type, io_stream);
	DefineStream___(Echo, file_length);
	DefineStreamDef(Echo, file_position);
	DefineStreamDef(Echo, file_position_start);
	DefineStreamDef(Echo, file_position_end);
	DefineStreamDef(Echo, file_position_set);
	DefineStreamSet(Echo, file_charlen);
	DefineStreamSet(Echo, file_strlen);
	DefineStreamSet(Echo, listen);
	DefineStreamSet(Echo, clear_input);
	DefineStreamSet(Echo, finish_output);
	DefineStreamSet(Echo, force_output);
	DefineStreamSet(Echo, clear_output);
	DefineStreamSet(Echo, exitpoint);
	DefineStreamSet(Echo, termsize);
}

