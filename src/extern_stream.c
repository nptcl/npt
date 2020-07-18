#include "condition.h"
#include "extern_stream.h"
#include "integer.h"
#include "stream.h"
#include "stream_error.h"
#include "typedef.h"

static int lisp_stream_exnted_check_(int type)
{
	if (type < 0 || LISP_STREAM_EXTEND <= type)
		return fmte_("Invalid stream type ~S.", intsizeh((size_t)type), NULL);
	return 0;
}

int lisp_stream_define_(addr *ret, int type, size_t size)
{
	addr x;

	Return(lisp_stream_exnted_check_(type));
	if (0xFFFF <= size)
		return fmte_("Too large stream size ~S.", intsizeh(size), NULL);
	stream_heap(&x, (enum StreamType)(StreamType_Size + type), size);
	PtrStructStream(x)->closed = 0;

	return Result(ret, x);
}

int lisp_stream_memory_(addr stream, void **ret)
{
	if (! extend_stream_p(stream))
		return fmte_("Invalid stream type ~S.", stream, NULL);
	return Result(ret, PtrDataStream(stream));
}

int lisp_getinfo_stream_(addr stream, addr *ret)
{
	if (! extend_stream_p(stream))
		return fmte_("Invalid stream type ~S.", stream, NULL);
	GetInfoStream(stream, &stream);
	return Result(ret, stream);
}

int lisp_setinfo_stream_(addr stream, addr value)
{
	if (! extend_stream_p(stream))
		return fmte_("Invalid stream type ~S.", stream, NULL);
	SetInfoStream(stream, value);
	return 0;
}


/*
 *  extend stream
 */
#define LispStreamExtendCallType(name) \
	int lisp_stream_calltype_##name##_(int type, lisp_streamtype_##name call) { \
		Return(lisp_stream_exnted_check_(type)); \
		Stream_##name[StreamType_Size + type] = call; \
		return 0; \
	}
LispStreamExtendCallType(close);
LispStreamExtendCallType(read_binary);
LispStreamExtendCallType(readf_binary);
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
LispStreamExtendCallType(file_charlen);
LispStreamExtendCallType(file_strlen);
LispStreamExtendCallType(listen);
LispStreamExtendCallType(finish_output);
LispStreamExtendCallType(force_output);
LispStreamExtendCallType(clear_output);
LispStreamExtendCallType(exitpoint);
LispStreamExtendCallType(termsize);

#define LispStreamExtendCallTypeError(name) \
	int lisp_stream_calltype_error_##name##_(int type) { \
		Return(lisp_stream_exnted_check_(type)); \
		Stream_##name[StreamType_Size + type] = name##_stream_error; \
		return 0; \
	}
LispStreamExtendCallTypeError(close);
LispStreamExtendCallTypeError(read_binary);
LispStreamExtendCallTypeError(readf_binary);
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
LispStreamExtendCallTypeError(file_charlen);
LispStreamExtendCallTypeError(file_strlen);
LispStreamExtendCallTypeError(listen);
LispStreamExtendCallTypeError(finish_output);
LispStreamExtendCallTypeError(force_output);
LispStreamExtendCallTypeError(clear_output);
LispStreamExtendCallTypeError(exitpoint);
LispStreamExtendCallTypeError(termsize);

