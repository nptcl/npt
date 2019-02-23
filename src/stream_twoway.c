#include "condition.h"
#include "object.h"
#include "stream_error.h"
#include "stream_twoway.h"
#include "stream.h"

#define CheckTwoWayStream(stream) { \
	Check(! twoway_stream_p(stream), "type error"); \
}

void open_twoway_stream(addr *stream, addr input, addr output)
{
	addr pos;

	CheckType(input, LISPTYPE_STREAM);
	CheckType(output, LISPTYPE_STREAM);
	stream_heap(&pos, StreamType_TwoWay, 0);
	SetInputStream(pos, input);
	SetOutputStream(pos, output);
	*stream = pos;
}

void get_twoway_input_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetInputStream(stream, ret);
}

void set_twoway_input_stream(addr stream, addr input)
{
	CheckTwoWayStream(stream);
	SetInputStream(stream, input);
}

void get_twoway_output_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetOutputStream(stream, ret);
}

void set_twoway_output_stream(addr stream, addr output)
{
	CheckTwoWayStream(stream);
	SetOutputStream(stream, output);
}

static void input_twoway(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetInputStream(stream, ret);
}

static void output_twoway(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetOutputStream(stream, ret);
}

static int read_binary_TwoWay(addr stream, void *pos, size_t size, size_t *ret)
{
	input_twoway(stream, &stream);
	return read_binary_stream(stream, pos, size, ret);
}

static int readforce_binary_TwoWay(addr stream, void *pos, size_t size, size_t *ret)
{
	input_twoway(stream, &stream);
	return readforce_binary_stream(stream, pos, size, ret);
}

static int read_byte_TwoWay(addr stream, byte *c)
{
	input_twoway(stream, &stream);
	return read_byte_stream(stream, c);
}

static int unread_byte_TwoWay(addr stream, byte c)
{
	input_twoway(stream, &stream);
	return unread_byte_stream(stream, c);
}

static int write_binary_TwoWay(addr stream, const void *pos, size_t size, size_t *ret)
{
	output_twoway(stream, &stream);
	return write_binary_stream(stream, pos, size, ret);
}

static int write_byte_TwoWay(addr stream, byte c)
{
	output_twoway(stream, &stream);
	return write_byte_stream(stream, c);
}

static int read_char_TwoWay(addr stream, unicode *u)
{
	input_twoway(stream, &stream);
	return read_char_stream(stream, u);
}

static int read_hang_TwoWay(addr stream, unicode *u, int *hang)
{
	input_twoway(stream, &stream);
	return read_hang_stream(stream, u, hang);
}

static void unread_char_TwoWay(addr stream, unicode u)
{
	input_twoway(stream, &stream);
	unread_char_stream(stream, u);
}

static void write_char_TwoWay(addr stream, unicode u)
{
	output_twoway(stream, &stream);
	write_char_stream(stream, u);
}

static int fresh_line_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return fresh_line_stream(stream);
}

static int interactivep_TwoWay(addr stream)
{
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);

	return interactivep_stream(input) && interactivep_stream(output);
}

static int file_character_length_TwoWay(addr stream, unicode u, size_t *ret)
{
	output_twoway(stream, &stream);
	return file_character_length_stream(stream, u, ret);
}

static int file_string_length_TwoWay(addr stream, addr pos, size_t *ret)
{
	output_twoway(stream, &stream);
	return file_string_length_stream(stream, pos, ret);
}

static int listen_TwoWay(addr stream)
{
	input_twoway(stream, &stream);
	return listen_stream(stream);
}

static void clear_input_TwoWay(addr stream)
{
	input_twoway(stream, &stream);
	clear_input_stream(stream);
}

static void finish_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	finish_output_stream(stream);
}

static void force_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	force_output_stream(stream);
}

static void clear_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	clear_output_stream(stream);
}

void init_stream_twoway(void)
{
	DefineStreamDef(TwoWay, close);
	DefineStreamSet(TwoWay, read_binary);
	DefineStreamSet(TwoWay, readforce_binary);
	DefineStreamSet(TwoWay, read_byte);
	DefineStreamSet(TwoWay, unread_byte);
	DefineStreamSet(TwoWay, write_binary);
	DefineStreamSet(TwoWay, write_byte);
	DefineStreamSet(TwoWay, read_char);
	DefineStreamSet(TwoWay, read_hang);
	DefineStreamSet(TwoWay, unread_char);
	DefineStreamSet(TwoWay, write_char);
	DefineStreamSet(TwoWay, fresh_line);
	DefineStreamChk(TwoWay, inputp, true);
	DefineStreamChk(TwoWay, outputp, true);
	DefineStreamSet(TwoWay, interactivep);
	DefineStreamLet(TwoWay, element_type, io_stream);
	DefineStream___(TwoWay, file_length);
	DefineStreamDef(TwoWay, file_position);
	DefineStreamDef(TwoWay, file_position_start);
	DefineStreamDef(TwoWay, file_position_end);
	DefineStreamDef(TwoWay, file_position_set);
	DefineStreamSet(TwoWay, file_character_length);
	DefineStreamSet(TwoWay, file_string_length);
	DefineStreamSet(TwoWay, listen);
	DefineStreamSet(TwoWay, clear_input);
	DefineStreamSet(TwoWay, finish_output);
	DefineStreamSet(TwoWay, force_output);
	DefineStreamSet(TwoWay, clear_output);
}

