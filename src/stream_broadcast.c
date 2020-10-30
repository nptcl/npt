#include "condition.h"
#include "cons.h"
#include "object.h"
#include "stream_broadcast.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_variable.h"
#include "stream.h"

#define CheckBroadCastStream(stream) { \
	Check(! broadcast_stream_p(stream), "type error"); \
}

_g int open_broadcast_stream_(addr *stream, addr list)
{
	addr pos;

	if (! listp(list))
		return TypeError_(list, LIST);
	stream_heap(&pos, StreamType_BroadCast, 0);
	SetInfoStream(pos, list);
	force_open_stream(pos);

	return Result(stream, pos);
}

_g void push_broadcast_stream(addr stream, addr output)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	cons_heap(&list, output, list);
	SetInfoStream(stream, list);
}

_g void get_broadcast_stream(addr stream, addr *ret)
{
	CheckBroadCastStream(stream);
	GetInfoStream(stream, ret);
}

static int write_byte_BroadCast(addr stream, addr value)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_byte_stream_(pos, value));
	}

	return 0;
}

static int write_char_BroadCast(addr stream, unicode c)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_char_stream_(pos, c));
	}
	charleft_default_stream(stream, c);

	return 0;
}

static int getleft_BroadCast(addr stream, size_t *ret)
{
	CheckBroadCastStream(stream);
	return getleft_default_stream(stream, ret);
}

static int setleft_BroadCast(addr stream, size_t value)
{
	CheckBroadCastStream(stream);
	return setleft_default_stream(stream, value);
}

static int characterp_BroadCast(addr stream, int *ret)
{
	int value, check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (value = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(characterp_stream_(pos, &check));
		value = value && check;
	}

	return Result(ret, value);
}

static int binaryp_BroadCast(addr stream, int *ret)
{
	int value, check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (value = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(binaryp_stream_(pos, &check));
		value = value && check;
	}

	return Result(ret, value);
}

static int last_component_BroadCast(addr stream, addr *ret)
{
	addr list;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	if (list == Nil)
		return 1;
	while (list != Nil) {
		if (! consp_getcons(list, ret, &list))
			break;
	}
	return 0;
}

static int element_type_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream))
		return Result(ret, T);
	else
		return element_type_stream_(stream, ret);
}

static int external_format_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		GetConst(KEYWORD_DEFAULT, ret);
		return 0;
	}

	return external_format_stream_(stream, ret);
}

static int file_length_BroadCast(addr stream, addr *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		fixnum_heap(ret, 0);
		return 0;
	}
	else {
		return file_length_stream_(stream, ret);
	}
}

static int file_position_BroadCast(addr stream, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 0;
		return Result(ret, 0);
	}
	else {
		return file_position_stream_(stream, value, ret);
	}
}

static int file_position_start_BroadCast(addr stream, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_start_stream_(pos, &check));
	}

	return Result(ret, check);
}

static int file_position_end_BroadCast(addr stream, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_end_stream_(pos, &check));
	}

	return Result(ret, check);
}

static int file_position_set_BroadCast(addr stream, size_t value, int *ret)
{
	int check;
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	for (check = 1; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(file_position_set_stream_(pos, value, &check));
	}

	return Result(ret, check);
}

static int file_charlen_BroadCast(addr stream,
		unicode u, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 1;
		return Result(ret, 0);
	}
	else {
		return file_charlen_stream_(stream, u, value, ret);
	}
}

static int file_strlen_BroadCast(addr stream, addr pos, size_t *value, int *ret)
{
	if (last_component_BroadCast(stream, &stream)) {
		*value = 1;
		return Result(ret, 0);
	}
	else {
		return file_strlen_stream_(stream, pos, value, ret);
	}
}

static int finish_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(finish_output_stream_(pos));
	}

	return 0;
}

static int force_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(force_output_stream_(pos));
	}

	return 0;
}

static int clear_output_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clear_output_stream_(pos));
	}

	return 0;
}

static int exitpoint_BroadCast(addr stream)
{
	addr list, pos;

	CheckBroadCastStream(stream);
	GetInfoStream(stream, &list);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(exitpoint_stream_(pos));
	}

	return 0;
}

_g void init_stream_broadcast(void)
{
	DefineStreamDef(BroadCast, close);
	DefineStream___(BroadCast, read_byte);
	DefineStream___(BroadCast, unread_byte);
	DefineStreamSet(BroadCast, write_byte);
	DefineStream___(BroadCast, read_char);
	DefineStream___(BroadCast, read_hang);
	DefineStream___(BroadCast, unread_char);
	DefineStreamSet(BroadCast, write_char);
	DefineStreamSet(BroadCast, getleft);
	DefineStreamSet(BroadCast, setleft);
	DefineStreamChk(BroadCast, inputp, false);
	DefineStreamChk(BroadCast, outputp, true);
	DefineStreamChk(BroadCast, interactivep, false);
	DefineStreamSet(BroadCast, characterp);
	DefineStreamSet(BroadCast, binaryp);
	DefineStreamSet(BroadCast, element_type);
	DefineStreamSet(BroadCast, external_format);
	DefineStreamSet(BroadCast, file_length);
	DefineStreamSet(BroadCast, file_position);
	DefineStreamSet(BroadCast, file_position_start);
	DefineStreamSet(BroadCast, file_position_end);
	DefineStreamSet(BroadCast, file_position_set);
	DefineStreamSet(BroadCast, file_charlen);
	DefineStreamSet(BroadCast, file_strlen);
	DefineStream___(BroadCast, listen);
	DefineStream___(BroadCast, clear_input);
	DefineStreamSet(BroadCast, finish_output);
	DefineStreamSet(BroadCast, force_output);
	DefineStreamSet(BroadCast, clear_output);
	DefineStreamSet(BroadCast, exitpoint);
	DefineStream___(BroadCast, termsize);
}

