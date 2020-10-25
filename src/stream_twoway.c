#include "condition.h"
#include "object.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_twoway.h"
#include "stream_variable.h"

#define CheckTwoWayStream(stream) { \
	Check(! twoway_stream_p(stream), "type error"); \
}

_g void open_twoway_stream(addr *stream, addr input, addr output)
{
	addr pos;

	CheckType(input, LISPTYPE_STREAM);
	CheckType(output, LISPTYPE_STREAM);
	stream_heap(&pos, StreamType_TwoWay, 0);
	SetInputStream(pos, input);
	SetOutputStream(pos, output);
	force_open_stream(pos);
	*stream = pos;
}

_g void get_twoway_input_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetInputStream(stream, ret);
}

_g void set_twoway_input_stream(addr stream, addr input)
{
	CheckTwoWayStream(stream);
	SetInputStream(stream, input);
}

_g void get_twoway_output_stream(addr stream, addr *ret)
{
	CheckTwoWayStream(stream);
	GetOutputStream(stream, ret);
}

_g void set_twoway_output_stream(addr stream, addr output)
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

static int read_byte_TwoWay(addr stream, addr *value, int *ret)
{
	input_twoway(stream, &stream);
	return read_byte_stream_(stream, value, ret);
}

static int unread_byte_TwoWay(addr stream, byte c)
{
	input_twoway(stream, &stream);
	return unread_byte_stream_(stream, c);
}

static int write_byte_TwoWay(addr stream, addr pos)
{
	output_twoway(stream, &stream);
	return write_byte_stream_(stream, pos);
}

static int read_char_TwoWay(addr stream, unicode *u, int *ret)
{
	input_twoway(stream, &stream);
	return read_char_stream_(stream, u, ret);
}

static int read_hang_TwoWay(addr stream, unicode *u, int *hang, int *ret)
{
	input_twoway(stream, &stream);
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_TwoWay(addr stream, unicode c)
{
	input_twoway(stream, &stream);
	return unread_char_stream_(stream, c);
}

static int write_char_TwoWay(addr stream, unicode u)
{
	output_twoway(stream, &stream);
	return write_char_stream_(stream, u);
}

static int getleft_TwoWay(addr stream, size_t *ret)
{
	output_twoway(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_TwoWay(addr stream, size_t value)
{
	output_twoway(stream, &stream);
	return setleft_stream_(stream, value);
}

static int interactivep_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(interactivep_stream_(input, &check1));
	Return(interactivep_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int characterp_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(characterp_stream_(input, &check1));
	Return(characterp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int binaryp_TwoWay(addr stream, int *ret)
{
	int check1, check2;
	addr input, output;

	CheckTwoWayStream(stream);
	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(binaryp_stream_(input, &check1));
	Return(binaryp_stream_(output, &check2));

	return Result(ret, check1 && check2);
}

static int file_charlen_TwoWay(addr stream, unicode u, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_TwoWay(addr stream, addr pos, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_TwoWay(addr stream, int *ret)
{
	input_twoway(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_TwoWay(addr stream)
{
	input_twoway(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return clear_output_stream_(stream);
}

static int exitpoint_TwoWay(addr stream)
{
	output_twoway(stream, &stream);
	return exitpoint_stream_(stream);
}

static int termsize_TwoWay(addr stream, size_t *value, int *ret)
{
	output_twoway(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

_g void init_stream_twoway(void)
{
	DefineStreamDef(TwoWay, close);
	DefineStreamSet(TwoWay, read_byte);
	DefineStreamSet(TwoWay, unread_byte);
	DefineStreamSet(TwoWay, write_byte);
	DefineStreamSet(TwoWay, read_char);
	DefineStreamSet(TwoWay, read_hang);
	DefineStreamSet(TwoWay, unread_char);
	DefineStreamSet(TwoWay, write_char);
	DefineStreamSet(TwoWay, getleft);
	DefineStreamSet(TwoWay, setleft);
	DefineStreamChk(TwoWay, inputp, true);
	DefineStreamChk(TwoWay, outputp, true);
	DefineStreamSet(TwoWay, interactivep);
	DefineStreamSet(TwoWay, characterp);
	DefineStreamSet(TwoWay, binaryp);
	DefineStreamLet(TwoWay, element_type, io_stream);
	DefineStream___(TwoWay, file_length);
	DefineStreamDef(TwoWay, file_position);
	DefineStreamDef(TwoWay, file_position_start);
	DefineStreamDef(TwoWay, file_position_end);
	DefineStreamDef(TwoWay, file_position_set);
	DefineStreamSet(TwoWay, file_charlen);
	DefineStreamSet(TwoWay, file_strlen);
	DefineStreamSet(TwoWay, listen);
	DefineStreamSet(TwoWay, clear_input);
	DefineStreamSet(TwoWay, finish_output);
	DefineStreamSet(TwoWay, force_output);
	DefineStreamSet(TwoWay, clear_output);
	DefineStreamSet(TwoWay, exitpoint);
	DefineStreamSet(TwoWay, termsize);
}

