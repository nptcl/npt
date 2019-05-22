#include "condition.h"
#include "cons.h"
#include "object.h"
#include "stream_concat.h"
#include "stream_error.h"
#include "stream.h"

#define CheckConcatenatedStream(stream) { \
	Check(! concatenated_stream_p(stream), "type error"); \
}

void open_concatenated_stream(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		TypeError(list, LIST);
	stream_heap(&pos, StreamType_Concatenated, 0);
	SetInfoStream(pos, list);
	*stream = pos;
}

void push_concatenated_stream(addr stream, addr input)
{
	addr list;

	CheckConcatenatedStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, input, list);
	SetInfoStream(stream, list);
}

void get_concatenated_stream(addr stream, addr *ret)
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

static int read_binary_Concatenated(addr stream, void *pos, size_t size, size_t *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 1;
	return read_binary_stream(stream, pos, size, ret);
}

static int readforce_binary_Concatenated(addr stream,
		void *pos, size_t size, size_t *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 1;
	return readforce_binary_stream(stream, pos, size, ret);
}

static int read_byte_Concatenated(addr stream, byte *c)
{
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return 1;
		getcons(list, &pos, &list);
		if (! read_byte_stream(pos, c))
			break;
		SetInfoStream(stream, list);
	}

	return 0;
}

static int unread_byte_Concatenated(addr stream, byte c)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 1;
	return unread_byte_stream(stream, c);
}

static int read_char_Concatenated(addr stream, unicode *u)
{
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return 1;
		getcons(list, &pos, &list);
		if (! read_char_stream(pos, u))
			break;
		SetInfoStream(stream, list);
	}

	return 0;
}

static int read_hang_Concatenated(addr stream, unicode *u, int *hang)
{
	addr list, pos;

	CheckConcatenatedStream(stream);
	for (;;) {
		GetInfoStream(stream, &list);
		if (list == Nil)
			return 1;
		getcons(list, &pos, &list);
		if (! read_hang_stream(pos, u, hang))
			break;
		SetInfoStream(stream, list);
	}

	return 0;
}

static void unread_char_Concatenated(addr stream, unicode u)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		unread_char_stream(stream, u);
}

static int outputp_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	return (stream == Nil)? 0: outputp_stream(stream);
}

static int interactivep_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	return (stream == Nil)? 0: interactivep_stream(stream);
}

static int characterp_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	return (stream == Nil)? 1: characterp_stream(stream);
}

static int binaryp_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	return (stream == Nil)? 1: binaryp_stream(stream);
}

static void element_type_Concatenated(addr stream, addr *ret)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		*ret = Nil;
	else
		element_type_stream(stream, ret);
}

static int listen_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream == Nil)
		return 0;
	else
		return listen_stream(stream);
}

static void clear_input_Concatenated(addr stream)
{
	current_concatenated(stream, &stream);
	if (stream != Nil)
		clear_input_stream(stream);
}

void init_stream_concatenated(void)
{
	DefineStreamDef(Concatenated, close);
	DefineStreamSet(Concatenated, read_binary);
	DefineStreamSet(Concatenated, readforce_binary);
	DefineStreamSet(Concatenated, read_byte);
	DefineStreamSet(Concatenated, unread_byte);
	DefineStream___(Concatenated, write_binary);
	DefineStream___(Concatenated, write_byte);
	DefineStreamSet(Concatenated, read_char);
	DefineStreamSet(Concatenated, read_hang);
	DefineStreamSet(Concatenated, unread_char);
	DefineStream___(Concatenated, write_char);
	DefineStream___(Concatenated, fresh_line);
	DefineStreamChk(Concatenated, inputp, true);
	DefineStreamSet(Concatenated, outputp);
	DefineStreamSet(Concatenated, interactivep);
	DefineStreamSet(Concatenated, characterp);
	DefineStreamSet(Concatenated, binaryp);
	DefineStreamSet(Concatenated, element_type);
	DefineStream___(Concatenated, file_length);
	DefineStreamDef(Concatenated, file_position);
	DefineStreamDef(Concatenated, file_position_start);
	DefineStreamDef(Concatenated, file_position_end);
	DefineStreamDef(Concatenated, file_position_set);
	DefineStream___(Concatenated, file_character_length);
	DefineStream___(Concatenated, file_string_length);
	DefineStreamSet(Concatenated, listen);
	DefineStreamSet(Concatenated, clear_input);
	DefineStream___(Concatenated, finish_output);
	DefineStream___(Concatenated, force_output);
	DefineStream___(Concatenated, clear_output);
}

