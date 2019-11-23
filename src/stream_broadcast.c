#include "condition.h"
#include "cons.h"
#include "object.h"
#include "stream_broadcast.h"
#include "stream_error.h"
#include "stream.h"

#define CheckBroadCastStream(stream) { \
	Check(! broadcast_stream_p(stream), "type error"); \
}

_g void open_broadcast_stream(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		TypeError(list, LIST);
	stream_heap(&pos, StreamType_BroadCast, 0);
	SetInfoStream(pos, list);
	*stream = pos;
}

_g void push_broadcast_stream(addr stream, addr input)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, input, list);
	SetInfoStream(stream, list);
}

_g void get_broadcast_stream(addr stream, addr *ret)
{
	CheckBroadCastStream(stream);
	GetInfoStream(stream, ret);
}

static int write_binary_BroadCast(addr stream,
		const void *ptr, size_t size, size_t *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	*ret = 0;
	for (check = 1; list != Nil; ) {
		getcons(list, &pos, &list);
		check &= write_binary_stream(stream, ptr, size, ret);
	}

	return check;
}

static int write_byte_BroadCast(addr stream, byte c)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &pos, &list);
		check &= write_byte_stream(pos, c);
	}

	return check;
}

static void write_char_BroadCast(addr stream, unicode c)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		getcons(list, &stream, &list);
		write_char_stream(stream, c);
	}
}

static int fresh_line_BroadCast(addr stream)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 0; list != Nil; ) {
		getcons(list, &stream, &list);
		check = fresh_line_stream(stream);
	}

	return check;
}

static int characterp_BroadCast(addr stream)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &stream, &list);
		check = check && characterp_stream(stream);
	}

	return check;
}

static int binaryp_BroadCast(addr stream)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &stream, &list);
		check = check && binaryp_stream(stream);
	}

	return check;
}

static int last_component_BroadCast(addr stream, addr *ret)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	if (list == Nil) {
		return 1;
	}
	else {
		while (list != Nil)
			getcons(list, ret, &list);
		return 0;
	}
}

static void element_type_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream))
		*ret = T;
	else
		element_type_stream(stream, ret);
}

static void file_length_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream))
		fixnum_heap(ret, 0);
	else
		file_length_stream(stream, ret);
}

static int file_position_BroadCast(addr stream, size_t *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*ret = 0;
		return 0;
	}
	else {
		return file_position_stream(stream, ret);
	}
}

static int file_position_start_BroadCast(addr stream)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &stream, &list);
		check = file_position_start_stream(stream);
	}

	return check;
}

static int file_position_end_BroadCast(addr stream)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &stream, &list);
		check = file_position_end_stream(stream);
	}

	return check;
}

static int file_position_set_BroadCast(addr stream, size_t pos)
{
	int check;
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		getcons(list, &stream, &list);
		check = file_position_set_stream(stream, pos);
	}

	return check;
}

static int file_character_length_BroadCast(addr stream, unicode u, size_t *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*ret = 1;
		return 0;
	}
	else {
		return file_character_length_stream(stream, u, ret);
	}
}

static int file_string_length_BroadCast(addr stream, addr pos, size_t *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*ret = 1;
		return 0;
	}
	else {
		return file_string_length_stream(stream, pos, ret);
	}
}

static void finish_output_BroadCast(addr stream)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		getcons(list, &stream, &list);
		finish_output_stream(stream);
	}
}

static void force_output_BroadCast(addr stream)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		getcons(list, &stream, &list);
		force_output_stream(stream);
	}
}

static void clear_output_BroadCast(addr stream)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		getcons(list, &stream, &list);
		clear_output_stream(stream);
	}
}

static void exitpoint_BroadCast(addr stream)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		getcons(list, &stream, &list);
		exitpoint_stream(stream);
	}
}

_g void init_stream_broadcast(void)
{
	DefineStreamDef(BroadCast, close);
	DefineStream___(BroadCast, read_binary);
	DefineStream___(BroadCast, readforce_binary);
	DefineStream___(BroadCast, read_byte);
	DefineStream___(BroadCast, unread_byte);
	DefineStreamSet(BroadCast, write_binary);
	DefineStreamSet(BroadCast, write_byte);
	DefineStream___(BroadCast, read_char);
	DefineStream___(BroadCast, read_hang);
	DefineStream___(BroadCast, unread_char);
	DefineStreamSet(BroadCast, write_char);
	DefineStreamSet(BroadCast, fresh_line);
	DefineStreamChk(BroadCast, inputp, false);
	DefineStreamChk(BroadCast, outputp, true);
	DefineStreamChk(BroadCast, interactivep, false);
	DefineStreamSet(BroadCast, characterp);
	DefineStreamSet(BroadCast, binaryp);
	DefineStreamSet(BroadCast, element_type);
	DefineStreamSet(BroadCast, file_length);
	DefineStreamSet(BroadCast, file_position);
	DefineStreamSet(BroadCast, file_position_start);
	DefineStreamSet(BroadCast, file_position_end);
	DefineStreamSet(BroadCast, file_position_set);
	DefineStreamSet(BroadCast, file_character_length);
	DefineStreamSet(BroadCast, file_string_length);
	DefineStream___(BroadCast, listen);
	DefineStream___(BroadCast, clear_input);
	DefineStreamSet(BroadCast, finish_output);
	DefineStreamSet(BroadCast, force_output);
	DefineStreamSet(BroadCast, clear_output);
	DefineStreamSet(BroadCast, exitpoint);
}

