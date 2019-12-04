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
	int result;
	addr input, output;
	size_t temp;

	io_Echo(stream, &input, &output);
	result = read_binary_stream(input, pos, size, ret);
	if (result == 0)
		write_binary_stream(output, pos, *ret, &temp);

	return result;
}

static int readforce_binary_Echo(addr stream, void *pos, size_t size, size_t *ret)
{
	int result;
	addr input, output;
	size_t temp;

	io_Echo(stream, &input, &output);
	result = readforce_binary_stream(stream, pos, size, ret);
	if (result == 0)
		write_binary_stream(stream, pos, *ret, &temp);

	return result;
}

static int read_byte_Echo(addr stream, byte *c)
{
	int result;
	addr input, output;

	io_Echo(stream, &input, &output);
	result = read_byte_stream(input, c);
	if (result == 0)
		write_byte_stream(output, *c);

	return result;
}

static int unread_byte_Echo(addr stream, byte c)
{
	input_Echo(stream, &stream);
	return unread_byte_stream(stream, c);
}

static int write_binary_Echo(addr stream, const void *pos, size_t size, size_t *ret)
{
	output_Echo(stream, &stream);
	return write_binary_stream(stream, pos, size, ret);
}

static int write_byte_Echo(addr stream, byte c)
{
	output_Echo(stream, &stream);
	return write_byte_stream(stream, c);
}

static int read_char_Echo(addr stream, unicode *u)
{
	int result;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	result = read_char_stream(input, u);
	if (result == 0 && ptr->unread_check == 0)
		write_char_stream(output, *u);
	ptr->unread_check = 0;

	return result;
}

static int read_hang_Echo(addr stream, unicode *u, int *hang)
{
	int result, check;
	addr input, output;
	struct StructStream *ptr;

	io_Echo(stream, &input, &output);
	ptr = PtrStructStream(stream);
	result = read_hang_stream(input, u, &check);
	if (result == 0) {
		if (check == 0 && ptr->unread_check == 0)
			write_char_stream(output, *u);
		*hang = check;
	}
	ptr->unread_check = 0;

	return result;
}

static void unread_char_Echo(addr stream, unicode u)
{
	addr input;

	input_Echo(stream, &input);
	unread_char_stream(input, u);
	PtrStructStream(stream)->unread_check = 1;;
}

static void write_char_Echo(addr stream, unicode u)
{
	output_Echo(stream, &stream);
	write_char_stream(stream, u);
}

static void terpri_Echo(addr stream)
{
	output_Echo(stream, &stream);
	terpri_stream(stream);
}

static int fresh_line_Echo(addr stream)
{
	output_Echo(stream, &stream);
	return fresh_line_stream(stream);
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

static int file_character_length_Echo(addr stream, unicode u, size_t *ret)
{
	output_Echo(stream, &stream);
	return file_character_length_stream(stream, u, ret);
}

static int file_string_length_Echo(addr stream, addr pos, size_t *ret)
{
	output_Echo(stream, &stream);
	return file_string_length_stream(stream, pos, ret);
}

static int listen_Echo(addr stream)
{
	input_Echo(stream, &stream);
	return listen_stream(stream);
}

static void clear_input_Echo(addr stream)
{
	input_Echo(stream, &stream);
	clear_input_stream(stream);
}

static void finish_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	finish_output_stream(stream);
}

static void force_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	force_output_stream(stream);
}

static void clear_output_Echo(addr stream)
{
	output_Echo(stream, &stream);
	clear_output_stream(stream);
}

static void exitpoint_Echo(addr stream)
{
	output_Echo(stream, &stream);
	exitpoint_stream(stream);
}

_g void init_stream_echo(void)
{
	DefineStreamDef(Echo, close);
	DefineStreamSet(Echo, read_binary);
	DefineStreamSet(Echo, readforce_binary);
	DefineStreamSet(Echo, read_byte);
	DefineStreamSet(Echo, unread_byte);
	DefineStreamSet(Echo, write_binary);
	DefineStreamSet(Echo, write_byte);
	DefineStreamSet(Echo, read_char);
	DefineStreamSet(Echo, read_hang);
	DefineStreamSet(Echo, unread_char);
	DefineStreamSet(Echo, write_char);
	DefineStreamSet(Echo, terpri);
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
	DefineStreamSet(Echo, file_character_length);
	DefineStreamSet(Echo, file_string_length);
	DefineStreamSet(Echo, listen);
	DefineStreamSet(Echo, clear_input);
	DefineStreamSet(Echo, finish_output);
	DefineStreamSet(Echo, force_output);
	DefineStreamSet(Echo, clear_output);
	DefineStreamSet(Echo, exitpoint);
}

