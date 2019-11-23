#include "condition.h"
#include "memory.h"
#include "object.h"
#include "stream_error.h"
#include "stream_prompt.h"
#include "stream.h"
#include "symbol.h"

struct stream_Pretty {
	unsigned alive : 1;
	size_t length;
};

#define CheckPrettyStream(stream) { \
	Check(! pretty_stream_p(stream), "type error"); \
}
#define PtrPrettyStream(pos) ((struct stream_Pretty *)PtrDataStream(pos))

enum StreamPretty_Index {
	StreamPretty_Stream,
	StreamPretty_Root,
	StreamPretty_Object,
	StreamPretty_Prefix,
	StreamPretty_PerLine,
	StreamPretty_Suffix,
	StreamPretty_Catch,
	StreamPretty_Size,
};

static void make_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	addr pos, gensym;

	make_gensym(ptr, &gensym);
	vector2_heap(&pos, StreamPretty_Size);
	SetArrayA2(pos, StreamPretty_Stream, stream);
	SetArrayA2(pos, StreamPretty_Root, object);
	SetArrayA2(pos, StreamPretty_Object, object);
	SetArrayA2(pos, StreamPretty_Prefix, prefix);
	SetArrayA2(pos, StreamPretty_PerLine, perline);
	SetArrayA2(pos, StreamPretty_Suffix, suffix);
	SetArrayA2(pos, StreamPretty_Catch, gensym);
	*ret = pos;
}

static void setalive_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->alive = value;
}

static void inclength_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	(PtrPrettyStream(stream)->length)++;
}

static void checkalive_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	if (! PtrPrettyStream(stream)->alive)
		fmte("The stream ~S is already closed.", stream, NULL);
}

_g void open_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	addr pos, info;
	struct stream_Pretty *str;

	if (! streamp(stream))
		TypeError(stream, STREAM);
	stream_heap(&pos, StreamType_Pretty, sizeoft(struct stream_Pretty));
	make_pretty_stream(ptr, &info, stream, object, prefix, perline, suffix);
	SetInfoStream(pos, info);
	str = PtrPrettyStream(pos);
	str->alive = 1;
	str->length = 0;
	*ret = pos;
}

_g void get_pretty_stream(addr stream, addr *ret)
{
	checkalive_pretty_stream(stream);
	GetInfoStream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stream, ret);
}

_g void set_pretty_stream(addr stream, addr pos)
{
	checkalive_pretty_stream(stream);
	CheckType(pos, LISPTYPE_STREAM);
	SetInfoStream(stream, pos);
}

_g void catch_pretty_stream(addr stream, addr *ret)
{
	checkalive_pretty_stream(stream);
	GetInfoStream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Catch, ret);
}

_g void object_pretty_stream(addr stream, addr *ret)
{
	checkalive_pretty_stream(stream);
	GetInfoStream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Object, ret);
}

_g void suffix_pretty_stream(addr stream, addr *ret)
{
	checkalive_pretty_stream(stream);
	GetInfoStream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Suffix, ret);
}

_g void close_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	setalive_pretty_stream(stream, 0);
}

_g void next_pretty_stream(addr stream)
{
	addr list;
	struct stream_Pretty *str;

	CheckPrettyStream(stream);
	str = PtrPrettyStream(stream);
	str->alive = 1;
	str->length = 0;
	GetArrayA2(stream, StreamPretty_Root, &list);
	SetArrayA2(stream, StreamPretty_Object, list);
}

_g int first_pretty_stream(addr stream)
{
	checkalive_pretty_stream(stream);
	return PtrPrettyStream(stream)->length == 0;
}

_g int pop_pretty_stream(addr stream, addr *ret)
{
	addr list;

	checkalive_pretty_stream(stream);
	GetInfoStream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Object, &list);
	if (list == Nil)
		return 0;
	if (consp(list)) {
		GetCons(list, ret, &list);
	}
	else {
		*ret = list;
		list = Nil;
	}
	inclength_pretty_stream(stream);
	SetArrayA2(stream, StreamPretty_Object, list);

	return 1;
}

_g size_t length_pretty_stream(addr stream)
{
	checkalive_pretty_stream(stream);
	return PtrPrettyStream(stream)->length;
}


/*
 *  stream function
 */
static int close_Pretty(addr stream, int abort)
{
	get_pretty_stream(stream, &stream);
	return 1;
}

static int read_binary_Pretty(addr stream, void *pos, size_t size, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return read_binary_stream(stream, pos, size, ret);
}

static int readforce_binary_Pretty(addr stream, void *pos, size_t size, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return readforce_binary_stream(stream, pos, size, ret);
}

static int read_byte_Pretty(addr stream, byte *c)
{
	get_pretty_stream(stream, &stream);
	return read_byte_stream(stream, c);
}

static int unread_byte_Pretty(addr stream, byte c)
{
	get_pretty_stream(stream, &stream);
	return unread_byte_stream(stream, c);
}

static int write_binary_Pretty(addr stream, const void *pos, size_t size, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return write_binary_stream(stream, pos, size, ret);
}

static int write_byte_Pretty(addr stream, byte c)
{
	get_pretty_stream(stream, &stream);
	return write_byte_stream(stream, c);
}

static int read_char_Pretty(addr stream, unicode *u)
{
	get_pretty_stream(stream, &stream);
	return read_char_stream(stream, u);
}

static int read_hang_Pretty(addr stream, unicode *u, int *hang)
{
	get_pretty_stream(stream, &stream);
	return read_hang_stream(stream, u, hang);
}

static void unread_char_Pretty(addr stream, unicode u)
{
	get_pretty_stream(stream, &stream);
	unread_char_stream(stream, u);
}

static void write_char_Pretty(addr stream, unicode u)
{
	get_pretty_stream(stream, &stream);
	write_char_stream(stream, u);
}

static int fresh_line_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return fresh_line_stream(stream);
}

static int inputp_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return inputp_stream(stream);
}

static int outputp_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return outputp_stream(stream);
}

static int interactivep_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return interactivep_stream(stream);
}

static int characterp_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return characterp_stream(stream);
}

static int binaryp_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return binaryp_stream(stream);
}

static void element_type_Pretty(addr stream, addr *ret)
{
	get_pretty_stream(stream, &stream);
	element_type_stream(stream, ret);
}

static void file_length_Pretty(addr stream, addr *ret)
{
	get_pretty_stream(stream, &stream);
	file_length_stream(stream, ret);
}

static int file_position_Pretty(addr stream, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return file_position_stream(stream, ret);
}

static int file_position_start_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return file_position_start_stream(stream);
}

static int file_position_end_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return file_position_end_stream(stream);
}

static int file_position_set_Pretty(addr stream, size_t pos)
{
	get_pretty_stream(stream, &stream);
	return file_position_set_stream(stream, pos);
}

static int file_character_length_Pretty(addr stream, unicode u, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return file_character_length_stream(stream, u, ret);
}

static int file_string_length_Pretty(addr stream, addr pos, size_t *ret)
{
	get_pretty_stream(stream, &stream);
	return file_string_length_stream(stream, pos, ret);
}

static int listen_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	return listen_stream(stream);
}

static void clear_input_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	clear_input_stream(stream);
}

static void finish_output_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	finish_output_stream(stream);
}

static void force_output_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	force_output_stream(stream);
}

static void clear_output_Pretty(addr stream)
{
	get_pretty_stream(stream, &stream);
	clear_output_stream(stream);
}

_g void init_stream_pretty(void)
{
	DefineStreamSet(Pretty, close);
	DefineStreamSet(Pretty, read_binary);
	DefineStreamSet(Pretty, readforce_binary);
	DefineStreamSet(Pretty, read_byte);
	DefineStreamSet(Pretty, unread_byte);
	DefineStreamSet(Pretty, write_binary);
	DefineStreamSet(Pretty, write_byte);
	DefineStreamSet(Pretty, read_char);
	DefineStreamSet(Pretty, read_hang);
	DefineStreamSet(Pretty, unread_char);
	DefineStreamSet(Pretty, write_char);
	DefineStreamSet(Pretty, fresh_line);
	DefineStreamSet(Pretty, inputp);
	DefineStreamSet(Pretty, outputp);
	DefineStreamSet(Pretty, interactivep);
	DefineStreamSet(Pretty, characterp);
	DefineStreamSet(Pretty, binaryp);
	DefineStreamSet(Pretty, element_type);
	DefineStreamSet(Pretty, file_length);
	DefineStreamSet(Pretty, file_position);
	DefineStreamSet(Pretty, file_position_start);
	DefineStreamSet(Pretty, file_position_end);
	DefineStreamSet(Pretty, file_position_set);
	DefineStreamSet(Pretty, file_character_length);
	DefineStreamSet(Pretty, file_string_length);
	DefineStreamSet(Pretty, listen);
	DefineStreamSet(Pretty, clear_input);
	DefineStreamSet(Pretty, finish_output);
	DefineStreamSet(Pretty, force_output);
	DefineStreamSet(Pretty, clear_output);
	DefineStreamDef(Pretty, exitpoint);
}

