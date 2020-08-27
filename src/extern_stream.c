#include "condition.h"
#include "extern_control.h"
#include "extern_sequence.h"
#include "extern_stream.h"
#include "format.h"
#include "integer.h"
#include "hold.h"
#include "stream.h"
#include "stream_error.h"
#include "typedef.h"

/*
 *  stream object
 */
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


/*
 *  format
 */
static int lisp_format_call_(addr stream, addr format, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	if (stream == NULL)
		stream = T;
	ptr = Execute_Thread;
	lisp_push_control(&control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, stream, format, args, NULL);
	Return(format_lisp(ptr, stream, format, args, &args));
	localhold_end(hold);

	return lisp_pop_control_(control);
}

int lisp_format8_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string8_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format16_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string16_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

int lisp_format32_(addr stream, const void *str, ...)
{
	addr format, args;
	va_list va;

	hold_value(stream, &stream);
	Return(lisp0_string32_(&format, str));
	va_start(va, str);
	lisp0_list_va(&args, va);
	va_end(va);

	return lisp_format_call_(stream, format, args);
}

