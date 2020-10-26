#include "bignum.h"
#include "buffering.h"
#include "build.h"
#include "condition.h"
#include "cons.h"
#include "integer.h"
#include "sequence.h"
#include "stream_error.h"
#include "stream_memory.h"
#include "stream_variable.h"
#include "strtype.h"
#include "typedef.h"

/*****************************************************************************
 *  MemoryInput
 *****************************************************************************/
#define INPUT_MEMORY_UNREAD_SIZE	16

struct stream_MemoryInput {
	unsigned unread_index;
	size_t index, size;
	byte unread[INPUT_MEMORY_UNREAD_SIZE];
};

#define CheckInputMemoryStream(stream) { \
	Check(! input_memory_stream_p(stream), "type error"); \
}
#define PtrMemoryInputStream(pos) ((struct stream_MemoryInput *)PtrDataStream(pos))

_g int open_input_memory_stream_(addr *ret, addr vector)
{
	struct stream_MemoryInput *input;
	addr pos;
	size_t size;

	if (! sequencep(vector))
		return TypeError_(vector, SEQUENCE);
	Return(length_sequence_(vector, 0, &size));
	stream_heap(&pos, StreamType_MemoryInput, sizeoft(struct stream_MemoryInput));
	input = PtrMemoryInputStream(pos);
	input->unread_index = 0;
	input->index = 0;
	input->size = size;
	SetInfoStream(pos, vector);
	force_open_stream(pos);

	return Result(ret, pos);
}

static int read_byte_MemoryInput(addr stream, addr *value, int *ret)
{
	addr pos;
	struct stream_MemoryInput *ptr;
	fixnum v;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);

	/* unread */
	if (ptr->unread_index) {
		fixnum_heap(value, (fixnum)ptr->unread[ptr->unread_index]);
		ptr->unread_index--;
		ptr->index++;
		return Result(ret, 0);
	}

	/* end-of-file */
	if (ptr->size <= ptr->index) {
		*value = Nil;
		return Result(ret, 1);  /* EOF */
	}

	/* read */
	GetInfoStream(stream, &pos);
	Return(getelt_sequence_(NULL, pos, ptr->index, &pos));
	Return(getfixnum_signed_(pos, &v));

	/* result */
	if (0 <= v && v <= 0xFF) {
		ptr->index++;
		*value = pos;
		return Result(ret, 0);
	}

	/* error */
	*value = Nil;
	*ret = 0;
	return fmte_("The value ~S is not (unsigned-byte 8).", pos, NULL);
}

static int unread_byte_MemoryInput(addr stream, byte c)
{
	struct stream_MemoryInput *ptr;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);

	/* unread check */
	if (INPUT_MEMORY_UNREAD_SIZE <= ptr->unread_index)
		return fmte_("The unread buffer is overflow.", NULL);

	/* index check */
	if (ptr->index == 0)
		return fmte_("The memory-stream index is underflow.", NULL);

	ptr->unread[ptr->unread_index] = c;
	ptr->unread_index++;
	ptr->index--;

	return 0;
}

static int element_type_MemoryInput(addr stream, addr *ret)
{
	addr x, y;

	CheckInputMemoryStream(stream);
	/* (unsigned-byte 8) */
	GetConst(COMMON_UNSIGNED_BYTE, &x);
	fixnum_heap(&y, 8);
	list_heap(ret, x, y, NULL);

	return 0;
}

static int file_length_MemoryInput(addr stream, addr *ret)
{
	struct stream_MemoryInput *ptr;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);
	make_index_integer_heap(ret, ptr->size);

	return 0;
}

static int file_position_MemoryInput(addr stream, size_t *value, int *ret)
{
	struct stream_MemoryInput *ptr;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);
	*value = ptr->index;
	return Result(ret, 0);
}

static int file_position_start_MemoryInput(addr stream, int *ret)
{
	struct stream_MemoryInput *ptr;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);
	ptr->index = 0;
	ptr->unread_index = 0;

	return Result(ret, 0);
}

static int file_position_end_MemoryInput(addr stream, int *ret)
{
	struct stream_MemoryInput *ptr;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);
	ptr->index = ptr->size;
	ptr->unread_index = 0;

	return Result(ret, 0);
}

static int file_position_set_MemoryInput(addr stream, size_t value, int *ret)
{
	struct stream_MemoryInput *ptr;
	addr pos;

	CheckInputMemoryStream(stream);
	ptr = PtrMemoryInputStream(stream);
	if (ptr->size < value) {
		*ret = 0;
		make_index_integer_heap(&pos, value);
		return fmte_("The position ~A is too large.", pos, NULL);
	}

	ptr->index = value;
	ptr->unread_index = 0;
	return Result(ret, 0);
}

static int listen_MemoryInput(addr stream, int *ret)
{
	CheckInputMemoryStream(stream);
	return Result(ret, 1);
}

static int clear_input_MemoryInput(addr stream)
{
	CheckInputMemoryStream(stream);
	/* Don't care unread-char */
	return 0;
}

_g void init_stream_memory_input(void)
{
	DefineStreamDef(MemoryInput, close);
	DefineStreamSet(MemoryInput, read_byte);
	DefineStreamSet(MemoryInput, unread_byte);
	DefineStream___(MemoryInput, write_byte);
	DefineStream___(MemoryInput, read_char);
	DefineStream___(MemoryInput, read_hang);
	DefineStream___(MemoryInput, unread_char);
	DefineStream___(MemoryInput, write_char);
	DefineStream___(MemoryInput, getleft);
	DefineStream___(MemoryInput, setleft);
	DefineStreamChk(MemoryInput, inputp, true);
	DefineStreamChk(MemoryInput, outputp, false);
	DefineStreamChk(MemoryInput, interactivep, false);
	DefineStreamChk(MemoryInput, characterp, false);
	DefineStreamChk(MemoryInput, binaryp, true);
	DefineStreamSet(MemoryInput, element_type);
	DefineStreamSet(MemoryInput, file_length);
	DefineStreamSet(MemoryInput, file_position);
	DefineStreamSet(MemoryInput, file_position_start);
	DefineStreamSet(MemoryInput, file_position_end);
	DefineStreamSet(MemoryInput, file_position_set);
	DefineStream___(MemoryInput, file_charlen);
	DefineStream___(MemoryInput, file_strlen);
	DefineStreamSet(MemoryInput, listen);
	DefineStreamSet(MemoryInput, clear_input);
	DefineStream___(MemoryInput, finish_output);
	DefineStream___(MemoryInput, force_output);
	DefineStream___(MemoryInput, clear_output);
	DefineStreamDef(MemoryInput, exitpoint);
	DefineStreamDef(MemoryInput, termsize);
}


/*****************************************************************************
 *  MemoryOutput
 *****************************************************************************/
#define CheckOutputMemoryStream(stream) { \
	Check(! output_memory_stream_p(stream), "type error"); \
}

_g void open_output_memory_stream(addr *ret, size_t size)
{
	addr pos, queue;

	stream_heap(&pos, StreamType_MemoryOutput, 0);
	buffering_heap(&queue, 0);
	SetInfoStream(pos, queue);
	force_open_stream(pos);
	*ret = pos;
}

_g int memory_stream_heap_(addr stream, addr *ret)
{
	addr pos;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*ret = Nil;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}
	return get_buffering_heap_(pos, ret);
}

_g void clear_output_memory_stream(addr stream)
{
	addr queue;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &queue);
	Check(queue == Nil, "stream is already closed.");
	clear_buffering(queue);
}

static int close_MemoryOutput(addr stream, addr *ret)
{
	CheckOutputMemoryStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);
	return Result(ret, T);
}

static int write_byte_MemoryOutput(addr stream, addr pos)
{
	addr queue;
	fixnum v;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		return fmte_("The stream ~S is already closed.", stream, NULL);
	Return(getfixnum_signed_(pos, &v));
	if (0 <= v && v <= 0xFF) {
		push_buffering(queue, (byte)v);
		return 0;
	}

	/* error */
	return fmte_("The argument ~S must be a (unsigned-byte 8) type.", pos, NULL);
}

static int element_type_MemoryOutput(addr stream, addr *ret)
{
	return element_type_MemoryInput(stream, ret);
}

static int file_length_MemoryOutput(addr stream, addr *ret)
{
	addr pos;
	size_t size;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*ret = Nil;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}
	get_length_buffering(pos, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

static int file_position_MemoryOutput(addr stream, size_t *value, int *ret)
{
	addr pos;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*value = 0;
		*ret = 0;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}
	get_length_buffering(pos, value);

	return Result(ret, 0);
}

static int file_position_start_MemoryOutput(addr stream, int *ret)
{
	addr pos;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*ret = 0;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}
	clear_buffering(pos);

	return Result(ret, 0);
}

static int file_position_end_MemoryOutput(addr stream, int *ret)
{
	addr pos;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*ret = 0;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}

	return Result(ret, 0);
}

static int file_position_set_MemoryOutput(addr stream, size_t value, int *ret)
{
	int check;
	addr pos;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		*ret = 0;
		return fmte_("The stream ~S is already closed.", stream, NULL);
	}
	position_buffering(pos, value, &check);
	if (check) {
		*ret = 0;
		make_index_integer_heap(&pos, value);
		return fmte_("Too large position value ~S.", pos, NULL);
	}

	return Result(ret, 0);
}

_g void init_stream_memory_output(void)
{
	DefineStreamSet(MemoryOutput, close);
	DefineStream___(MemoryOutput, read_byte);
	DefineStream___(MemoryOutput, unread_byte);
	DefineStreamSet(MemoryOutput, write_byte);
	DefineStream___(MemoryOutput, read_char);
	DefineStream___(MemoryOutput, read_hang);
	DefineStream___(MemoryOutput, unread_char);
	DefineStream___(MemoryOutput, write_char);
	DefineStream___(MemoryOutput, getleft);
	DefineStream___(MemoryOutput, setleft);
	DefineStreamChk(MemoryOutput, inputp, false);
	DefineStreamChk(MemoryOutput, outputp, true);
	DefineStreamChk(MemoryOutput, interactivep, false);
	DefineStreamChk(MemoryOutput, characterp, false);
	DefineStreamChk(MemoryOutput, binaryp, true);
	DefineStreamSet(MemoryOutput, element_type);
	DefineStreamSet(MemoryOutput, file_length);
	DefineStreamSet(MemoryOutput, file_position);
	DefineStreamSet(MemoryOutput, file_position_start);
	DefineStreamSet(MemoryOutput, file_position_end);
	DefineStreamSet(MemoryOutput, file_position_set);
	DefineStream___(MemoryOutput, file_charlen);
	DefineStream___(MemoryOutput, file_strlen);
	DefineStream___(MemoryOutput, listen);
	DefineStream___(MemoryOutput, clear_input);
	DefineStreamDef(MemoryOutput, finish_output);
	DefineStreamDef(MemoryOutput, force_output);
	DefineStreamDef(MemoryOutput, clear_output);
	DefineStreamDef(MemoryOutput, exitpoint);
	DefineStreamDef(MemoryOutput, termsize);
}

