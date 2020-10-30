#include "condition.h"
#include "cons.h"
#include "object.h"
#include "stream_concat.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_variable.h"
#include "stream.h"

#define CheckConcatenatedStream(stream) { \
	Check(! concatenated_stream_p(stream), "type error"); \
}

_g int open_concatenated_stream_(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		return TypeError_(list, LIST);
	stream_heap(&pos, StreamType_Concatenated, 0);
	SetInfoStream(pos, list);
	force_open_stream(pos);

	return Result(stream, pos);
}

_g void push_concatenated_stream(addr stream, addr input)
{
	addr list;

	CheckConcatenatedStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, input, list);
	SetInfoStream(stream, list);
}

_g void get_concatenated_stream(addr stream, addr *ret)
{
	CheckConcatenatedStream(stream);
	GetInfoStream(stream, ret);
}

static void current_concatenated(addr stream, addr *ret)
{
	CheckConcatenatedStream(stream);
	GetInfoStream(stream, &stream);
	if (stream == Nil) {
		*ret = Nil;
	}
	else {
		GetCar(stream, ret);
	}
}

static int read_byte_Concatenated(addr stream, addr *value, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_byte_stream_(pos, value, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int unread_byte_Concatenated(addr stream, byte c)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 0;
	return unread_byte_stream_(stream, c);
}

static int read_char_Concatenated(addr stream, unicode *u, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_char_stream_(pos, u, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int read_hang_Concatenated(addr stream, unicode *u, int *hang, int *ret)
{
	int check;
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return Result(ret, 1);
		Return_getcons(list, &pos, &list);
		Return(read_hang_stream_(pos, u, hang, &check));
		if (! check)
			break;
		SetInfoStream(stream, list);
	}

	return Result(ret, 0);
}

static int unread_char_Concatenated(addr stream, unicode c)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return unread_char_stream_(stream, c);

	return 0;
}

static int interactivep_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 0);
	else
		return interactivep_stream_(stream, ret);
}

static int characterp_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 1);
	else
		return characterp_stream_(stream, ret);
}

static int binaryp_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 1);
	else
		return binaryp_stream_(stream, ret);
}

static int element_type_Concatenated(addr stream, addr *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, Nil);
	else
		return element_type_stream_(stream, ret);
}

static int external_format_Concatenated(addr stream, addr *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil) {
		GetConst(KEYWORD_DEFAULT, ret);
		return 0;
	}

	return external_format_stream_(stream, ret);
}

static int listen_Concatenated(addr stream, int *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return Result(ret, 0);
	else
		return listen_stream_(stream, ret);
}

static int clear_input_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return clear_input_stream_(stream);
	return 0;
}

static int exitpoint_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		return exitpoint_stream_(stream);
	return 0;
}

_g void init_stream_concatenated(void)
{
	DefineStreamDef(Concatenated, close);
	DefineStreamSet(Concatenated, read_byte);
	DefineStreamSet(Concatenated, unread_byte);
	DefineStream___(Concatenated, write_byte);
	DefineStreamSet(Concatenated, read_char);
	DefineStreamSet(Concatenated, read_hang);
	DefineStreamSet(Concatenated, unread_char);
	DefineStream___(Concatenated, write_char);
	DefineStream___(Concatenated, getleft);
	DefineStream___(Concatenated, setleft);
	DefineStreamChk(Concatenated, inputp, true);
	DefineStreamChk(Concatenated, outputp, false);
	DefineStreamSet(Concatenated, interactivep);
	DefineStreamSet(Concatenated, characterp);
	DefineStreamSet(Concatenated, binaryp);
	DefineStreamSet(Concatenated, element_type);
	DefineStreamSet(Concatenated, external_format);
	DefineStream___(Concatenated, file_length);
	DefineStreamDef(Concatenated, file_position);
	DefineStreamDef(Concatenated, file_position_start);
	DefineStreamDef(Concatenated, file_position_end);
	DefineStreamDef(Concatenated, file_position_set);
	DefineStream___(Concatenated, file_charlen);
	DefineStream___(Concatenated, file_strlen);
	DefineStreamSet(Concatenated, listen);
	DefineStreamSet(Concatenated, clear_input);
	DefineStream___(Concatenated, finish_output);
	DefineStream___(Concatenated, force_output);
	DefineStream___(Concatenated, clear_output);
	DefineStreamSet(Concatenated, exitpoint);
	DefineStream___(Concatenated, termsize);
}

