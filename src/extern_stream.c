#include "condition.h"
#include "extern_stream.h"
#include "integer.h"
#include "stream.h"
#include "stream_error.h"
#include "typedef.h"

static void lisp_stream_exnted_check(int type)
{
	if (type < 0 || LISP_STREAM_EXTEND <= type)
		_fmte("Invalid stream type ~S.", intsizeh((size_t)type), NULL);
}

addr lisp_stream_define(int type, size_t size)
{
	addr x;

	lisp_stream_exnted_check(type);
	if (0xFFFF <= size)
		_fmte("Too large stream size ~S.", intsizeh(size), NULL);
	stream_heap(&x, (enum StreamType)(StreamType_Size + type), size);
	PtrStructStream(x)->closed = 0;

	return x;
}

void *lisp_stream_memory(addr stream)
{
	if (! extend_stream_p(stream))
		_fmte("Invalid stream type ~S.", stream, NULL);
	return PtrDataStream(stream);
}

addr lisp_getinfo_stream(addr stream)
{
	if (! extend_stream_p(stream))
		_fmte("Invalid stream type ~S.", stream, NULL);
	GetInfoStream(stream, &stream);
	return stream;
}

void lisp_setinfo_stream(addr stream, addr value)
{
	if (! extend_stream_p(stream))
		_fmte("Invalid stream type ~S.", stream, NULL);
	SetInfoStream(stream, value);
}


/*
 *  extend stream
 */
#define LispStreamExtendCallType(name) \
	void lisp_stream_calltype_##name(int type, lisp_streamtype_##name call) { \
		lisp_stream_exnted_check(type); \
		Stream_##name[StreamType_Size + type] = call; \
	}
LispStreamExtendCallType(close);
LispStreamExtendCallType(read_binary);
LispStreamExtendCallType(readforce_binary);
LispStreamExtendCallType(read_byte);
LispStreamExtendCallType(unread_byte);
LispStreamExtendCallType(write_binary);
LispStreamExtendCallType(write_byte);
LispStreamExtendCallType(read_char);
LispStreamExtendCallType(read_hang);
LispStreamExtendCallType(unread_char);
LispStreamExtendCallType(write_char);
LispStreamExtendCallType(terpri);
LispStreamExtendCallType(getleft);
LispStreamExtendCallType(setleft);
LispStreamExtendCallType(fresh_line);
LispStreamExtendCallType(clear_input);
LispStreamExtendCallType(inputp);
LispStreamExtendCallType(outputp);
LispStreamExtendCallType(interactivep);
LispStreamExtendCallType(characterp);
LispStreamExtendCallType(binaryp);
LispStreamExtendCallType(element_type);
LispStreamExtendCallType(file_length);
LispStreamExtendCallType(file_position);
LispStreamExtendCallType(file_position_start);
LispStreamExtendCallType(file_position_end);
LispStreamExtendCallType(file_position_set);
LispStreamExtendCallType(file_character_length);
LispStreamExtendCallType(file_string_length);
LispStreamExtendCallType(listen);
LispStreamExtendCallType(finish_output);
LispStreamExtendCallType(force_output);
LispStreamExtendCallType(clear_output);
LispStreamExtendCallType(exitpoint);
LispStreamExtendCallType(terminal_width);

#define LispStreamExtendCallTypeError(name) \
	void lisp_stream_calltype_error_##name(int type) { \
		lisp_stream_exnted_check(type); \
		Stream_##name[StreamType_Size + type] = name##_stream_error; \
	}
LispStreamExtendCallTypeError(close);
LispStreamExtendCallTypeError(read_binary);
LispStreamExtendCallTypeError(readforce_binary);
LispStreamExtendCallTypeError(read_byte);
LispStreamExtendCallTypeError(unread_byte);
LispStreamExtendCallTypeError(write_binary);
LispStreamExtendCallTypeError(write_byte);
LispStreamExtendCallTypeError(read_char);
LispStreamExtendCallTypeError(read_hang);
LispStreamExtendCallTypeError(unread_char);
LispStreamExtendCallTypeError(write_char);
LispStreamExtendCallTypeError(terpri);
LispStreamExtendCallTypeError(getleft);
LispStreamExtendCallTypeError(setleft);
LispStreamExtendCallTypeError(fresh_line);
LispStreamExtendCallTypeError(clear_input);
LispStreamExtendCallTypeError(inputp);
LispStreamExtendCallTypeError(outputp);
LispStreamExtendCallTypeError(interactivep);
LispStreamExtendCallTypeError(characterp);
LispStreamExtendCallTypeError(binaryp);
LispStreamExtendCallTypeError(element_type);
LispStreamExtendCallTypeError(file_length);
LispStreamExtendCallTypeError(file_position);
LispStreamExtendCallTypeError(file_position_start);
LispStreamExtendCallTypeError(file_position_end);
LispStreamExtendCallTypeError(file_position_set);
LispStreamExtendCallTypeError(file_character_length);
LispStreamExtendCallTypeError(file_string_length);
LispStreamExtendCallTypeError(listen);
LispStreamExtendCallTypeError(finish_output);
LispStreamExtendCallTypeError(force_output);
LispStreamExtendCallTypeError(clear_output);
LispStreamExtendCallTypeError(exitpoint);
LispStreamExtendCallTypeError(terminal_width);

