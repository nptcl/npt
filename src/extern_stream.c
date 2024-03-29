#include "condition.h"
#include "extern_control.h"
#include "extern_error.h"
#include "extern_sequence.h"
#include "extern_stream.h"
#include "integer.h"
#include "hold.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_variable.h"
#include "typedef.h"

/*
 *  stream object
 */
static void lisp_stream_exnted_check(int index)
{
	addr pos;

	if (index < 0 || LISP_POINTER_EXTEND <= index) {
		fixnum_heap(&pos, (fixnum)index);
		lisp_abortf("Invalid index value %d.", index, NULL);
	}
}

void lisp0_stream_define(addr *ret, int index, size_t size)
{
	addr pos;

	lisp_stream_exnted_check(index);
	if (0xFFFF <= size) {
		*ret = Nil;
		lisp_abortf("Too large stream size %zu.", size);
		return;
	}
	stream_heap(&pos, (enum StreamType)StreamExtend_Index(index), size);
	PtrStructStream(pos)->closed = 0;
	*ret = pos;
}

void lisp_stream_define(addr x, int index, size_t size)
{
	addr pos;
	lisp0_stream_define(&pos, index, size);
	hold_set(x, pos);
}

void lisp_stream_memory(addr stream, void **ret)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		*ret = NULL;
		lisp_abortf("Invalid stream type.");
		return;
	}
	*ret = PtrDataStream(stream);
}

void lisp0_getinfo_stream(addr *ret, addr stream)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		*ret = Nil;
		lisp_abortf("Invalid stream type.");
		return;
	}
	GetInfoStream(stream, ret);
}

void lisp_getinfo_stream(addr x, addr stream)
{
	lisp0_getinfo_stream(&stream, stream);
	hold_set(x, stream);
}

void lisp_setinfo_stream(addr stream, addr value)
{
	hold_value(stream, &stream);
	if (! extend_stream_p(stream)) {
		lisp_abortf("Invalid stream type.");
		return;
	}
	hold_value(value, &value);
	SetInfoStream(stream, value);
}


/*
 *  extend stream
 */
#define LispStreamExtendCallType(name) \
	void lisp_stream_calltype_##name(int index, lisp_streamtype_##name call) { \
		lisp_stream_exnted_check(index); \
		Stream_##name[StreamExtend_Index(index)] = call; \
	}
LispStreamExtendCallType(close);
LispStreamExtendCallType(read_byte);
LispStreamExtendCallType(unread_byte);
LispStreamExtendCallType(write_byte);
LispStreamExtendCallType(read_char);
LispStreamExtendCallType(read_hang);
LispStreamExtendCallType(unread_char);
LispStreamExtendCallType(write_char);
LispStreamExtendCallType(getleft);
LispStreamExtendCallType(setleft);
LispStreamExtendCallType(inputp);
LispStreamExtendCallType(outputp);
LispStreamExtendCallType(interactivep);
LispStreamExtendCallType(characterp);
LispStreamExtendCallType(binaryp);
LispStreamExtendCallType(element_type);
LispStreamExtendCallType(external_format);
LispStreamExtendCallType(file_length);
LispStreamExtendCallType(file_position);
LispStreamExtendCallType(file_position_start);
LispStreamExtendCallType(file_position_end);
LispStreamExtendCallType(file_position_set);
LispStreamExtendCallType(file_charlen);
LispStreamExtendCallType(file_strlen);
LispStreamExtendCallType(listen);
LispStreamExtendCallType(clear_input);
LispStreamExtendCallType(finish_output);
LispStreamExtendCallType(force_output);
LispStreamExtendCallType(clear_output);
LispStreamExtendCallType(exitpoint);
LispStreamExtendCallType(termsize);

#define LispStreamExtendCallTypeError(name) \
	void lisp_stream_calltype_error_##name(int index) { \
		lisp_stream_exnted_check(index); \
		Stream_##name[StreamExtend_Index(index)] = name##_stream_error; \
	}
LispStreamExtendCallTypeError(close);
LispStreamExtendCallTypeError(read_byte);
LispStreamExtendCallTypeError(unread_byte);
LispStreamExtendCallTypeError(write_byte);
LispStreamExtendCallTypeError(read_char);
LispStreamExtendCallTypeError(read_hang);
LispStreamExtendCallTypeError(unread_char);
LispStreamExtendCallTypeError(write_char);
LispStreamExtendCallTypeError(getleft);
LispStreamExtendCallTypeError(setleft);
LispStreamExtendCallTypeError(inputp);
LispStreamExtendCallTypeError(outputp);
LispStreamExtendCallTypeError(interactivep);
LispStreamExtendCallTypeError(characterp);
LispStreamExtendCallTypeError(binaryp);
LispStreamExtendCallTypeError(element_type);
LispStreamExtendCallTypeError(external_format);
LispStreamExtendCallTypeError(file_length);
LispStreamExtendCallTypeError(file_position);
LispStreamExtendCallTypeError(file_position_start);
LispStreamExtendCallTypeError(file_position_end);
LispStreamExtendCallTypeError(file_position_set);
LispStreamExtendCallTypeError(file_charlen);
LispStreamExtendCallTypeError(file_strlen);
LispStreamExtendCallTypeError(listen);
LispStreamExtendCallTypeError(clear_input);
LispStreamExtendCallTypeError(finish_output);
LispStreamExtendCallTypeError(force_output);
LispStreamExtendCallTypeError(clear_output);
LispStreamExtendCallTypeError(exitpoint);
LispStreamExtendCallTypeError(termsize);

