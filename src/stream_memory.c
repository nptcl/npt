#include "bignum.h"
#include "buffering.h"
#include "build.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "integer.h"
#include "sequence.h"
#include "sequence_safe.h"
#include "stream_error.h"
#include "stream_memory.h"
#include "stream_variable.h"
#include "strtype.h"
#include "type_table.h"
#include "typedef.h"

static int close_memory_stream_p(addr stream, addr *ret)
{
	addr pos;

	Check(! memory_stream_p(stream), "type error");
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		if (ret)
			*ret = Nil;
		return 1;
	}
	if (ret)
		*ret = pos;

	return 0;
}

static int close_memory_stream_error_(addr stream, addr *ret)
{
	addr pos;

	Check(! memory_stream_p(stream), "type error");
	GetInfoStream(stream, &pos);
	if (pos == Nil) {
		if (ret)
			*ret = Nil;
		return fmte_("The stream is already closed.", pos, NULL);
	}
	if (ret)
		*ret = pos;

	return 0;
}


/*****************************************************************************
 *  MemoryInput
 *****************************************************************************/
struct stream_MemoryInput {
	unsigned unread_index;
	size_t index, size;
	byte unread[INPUT_MEMORY_UNREAD_SIZE];
};
#define PtrMemoryInputStream(pos) ((struct stream_MemoryInput *)PtrDataStream(pos))

#define CheckInputMemoryStream(stream) { \
	Check(! input_memory_stream_p(stream), "type error"); \
}

_g int open_input_memory_stream_(addr *ret, addr vector)
{
	struct stream_MemoryInput *str;
	addr pos;
	size_t size;

	if (! sequencep(vector))
		return TypeError_(vector, SEQUENCE);
	if (vector == Nil)
		vector_heap(&vector, 0);

	Return(length_sequence_(vector, 0, &size));
	stream_heap(&pos, StreamType_MemoryInput, sizeoft(struct stream_MemoryInput));
	str = PtrMemoryInputStream(pos);
	str->unread_index = 0;
	str->index = 0;
	str->size = size;
	SetInfoStream(pos, vector);
	force_open_stream(pos);

	return Result(ret, pos);
}

static int close_MemoryInput(addr stream, addr *ret)
{
	CheckInputMemoryStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);

	return Result(ret, T);
}

static int read_byte_MemoryInput(addr stream, addr *value, int *ret)
{
	addr pos;
	struct stream_MemoryInput *str;
	fixnum v;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &pos));

	/* unread */
	str = PtrMemoryInputStream(stream);
	if (str->unread_index) {
		fixnum_heap(value, (fixnum)str->unread[str->unread_index]);
		str->unread_index--;
		str->index++;
		return Result(ret, 0);
	}

	/* end-of-file */
	if (str->size <= str->index) {
		*value = Nil;
		return Result(ret, 1);  /* EOF */
	}

	/* read */
	Return(getelt_sequence_(NULL, pos, str->index, &pos));
	Return(getfixnum_signed_(pos, &v));

	/* result */
	if (IsByteSign(v)) {
		str->index++;
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
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	str = PtrMemoryInputStream(stream);
	Return(close_memory_stream_error_(stream, NULL));

	/* unread check */
	if (INPUT_MEMORY_UNREAD_SIZE <= str->unread_index)
		return fmte_("The unread buffer is overflow.", NULL);

	/* index check */
	if (str->index == 0)
		return fmte_("The memory-stream index is underflow.", NULL);

	str->unread[str->unread_index] = c;
	str->unread_index++;
	str->index--;

	return 0;
}

static int element_type_MemoryStream(addr stream, addr *ret)
{
	addr x, y;

	/* (unsigned-byte 8) */
	GetConst(COMMON_UNSIGNED_BYTE, &x);
	fixnum_heap(&y, 8);
	list_heap(ret, x, y, NULL);

	return 0;
}

static int element_type_MemoryInput(addr stream, addr *ret)
{
	CheckInputMemoryStream(stream);
	return element_type_MemoryStream(stream, ret);
}

static int file_length_MemoryInput(addr stream, addr *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	str = PtrMemoryInputStream(stream);
	make_index_integer_heap(ret, str->size);

	return 0;
}

static int file_position_MemoryInput(addr stream, size_t *value, int *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	str = PtrMemoryInputStream(stream);
	*value = str->index;
	return Result(ret, 0);
}

static int file_position_start_MemoryInput(addr stream, int *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	str = PtrMemoryInputStream(stream);
	str->index = 0;
	str->unread_index = 0;

	return Result(ret, 0);
}

static int file_position_end_MemoryInput(addr stream, int *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	str = PtrMemoryInputStream(stream);
	str->index = str->size;
	str->unread_index = 0;

	return Result(ret, 0);
}

static int file_position_set_MemoryInput(addr stream, size_t value, int *ret)
{
	struct stream_MemoryInput *str;
	addr pos;

	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	str = PtrMemoryInputStream(stream);
	if (str->size < value) {
		*ret = 0;
		make_index_integer_heap(&pos, value);
		return fmte_("The position ~A is too large.", pos, NULL);
	}

	str->index = value;
	str->unread_index = 0;
	return Result(ret, 0);
}

static int listen_MemoryInput(addr stream, int *ret)
{
	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	return Result(ret, 1);
}

static int clear_input_MemoryInput(addr stream)
{
	CheckInputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	/* Don't care unread-char */
	return 0;
}

_g void init_stream_memory_input(void)
{
	DefineStreamSet(MemoryInput, close);
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
	addr pos, page;

	stream_heap(&pos, StreamType_MemoryOutput, 0);
	buffering_heap(&page, 0);
	SetInfoStream(pos, page);
	force_open_stream(pos);
	*ret = pos;
}

_g int memory_stream_heap_(addr stream, addr *ret)
{
	addr page;

	CheckOutputMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	return make_vector_buffering_heap_(page, ret);
}

_g void clear_output_memory_stream(addr stream)
{
	addr page;

	CheckOutputMemoryStream(stream);
	GetInfoStream(stream, &page);
	Check(page == Nil, "stream is already closed.");
	clear_buffering(page);
}

static int close_MemoryOutput(addr stream, addr *ret)
{
	CheckOutputMemoryStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);

	return Result(ret, T);
}

static int write_byte_MemoryStream(addr stream, addr pos)
{
	addr page;
	fixnum v;

	Return(close_memory_stream_error_(stream, &page));
	Return(getfixnum_signed_(pos, &v));
	if (IsByteSign(v)) {
		if (putc_buffering(page, (byte)v))
			return fmte_("Too large file size.", NULL);
		return 0;
	}

	/* error */
	return fmte_("The argument ~S must be a (unsigned-byte 8) type.", pos, NULL);
}

static int write_byte_MemoryOutput(addr stream, addr pos)
{
	CheckOutputMemoryStream(stream);
	return write_byte_MemoryStream(stream, pos);
}

static int element_type_MemoryOutput(addr stream, addr *ret)
{
	CheckOutputMemoryStream(stream);
	return element_type_MemoryStream(stream, ret);
}

static int file_length_MemoryStream(addr stream, addr *ret)
{
	addr page;
	size_t size;

	Return(close_memory_stream_error_(stream, &page));
	length_buffering(page, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

static int file_length_MemoryOutput(addr stream, addr *ret)
{
	CheckOutputMemoryStream(stream);
	return file_length_MemoryStream(stream, ret);
}

static int file_position_MemoryStream(addr stream, size_t *value, int *ret)
{
	addr page;

	Return(close_memory_stream_error_(stream, &page));
	length_buffering(page, value);

	return Result(ret, 0);
}

static int file_position_MemoryOutput(addr stream, size_t *value, int *ret)
{
	CheckOutputMemoryStream(stream);
	return file_position_MemoryStream(stream, value, ret);
}

static int file_position_start_MemoryStream(addr stream, int *ret)
{
	addr page;

	Return(close_memory_stream_error_(stream, &page));
	position_start_buffering(page);

	return Result(ret, 0);
}

static int file_position_start_MemoryOutput(addr stream, int *ret)
{
	CheckOutputMemoryStream(stream);
	return file_position_start_MemoryStream(stream, ret);
}

static int file_position_end_MemoryStream(addr stream, int *ret)
{
	addr page;

	Return(close_memory_stream_error_(stream, &page));
	position_end_buffering(page);

	return Result(ret, 0);
}

static int file_position_end_MemoryOutput(addr stream, int *ret)
{
	CheckOutputMemoryStream(stream);
	return file_position_end_MemoryStream(stream, ret);
}

static int file_position_set_MemoryStream(addr stream, size_t value, int *ret)
{
	addr page;

	Return(close_memory_stream_error_(stream, &page));
	position_set_buffering(page, value);

	return Result(ret, 0);
}

static int file_position_set_MemoryOutput(addr stream, size_t value, int *ret)
{
	CheckOutputMemoryStream(stream);
	return file_position_set_MemoryStream(stream, value, ret);
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


/*****************************************************************************
 *  MemoryIO
 *****************************************************************************/
struct stream_MemoryIO {
	unsigned unread_index;
	byte unread[INPUT_MEMORY_UNREAD_SIZE];
};
#define PtrMemoryIOStream(pos) ((struct stream_MemoryIO *)PtrDataStream(pos))

#define CheckIOMemoryStream(stream) { \
	Check(! io_memory_stream_p(stream), "type error"); \
}

static void clear_unread_io_memory_stream(addr stream)
{
	struct stream_MemoryIO *str;

	CheckIOMemoryStream(stream);
	str = PtrMemoryIOStream(stream);
	str->unread_index = 0;
}

_g int open_io_memory_stream(addr *ret, addr vector, size_t cell)
{
	struct stream_MemoryIO *str;
	addr pos, file;

	if (! sequencep(vector))
		return TypeError_(vector, SEQUENCE);

	/* object */
	stream_heap(&pos, StreamType_MemoryIO, sizeoft(struct stream_MemoryIO));
	str = PtrMemoryIOStream(pos);
	str->unread_index = 0;

	/* buffering */
	buffering_heap(&file, cell);
	SetInfoStream(pos, file);
	Return(read_buffering_(file, vector));
	force_open_stream(pos);

	return Result(ret, pos);
}

static int close_MemoryIO(addr stream, addr *ret)
{
	CheckIOMemoryStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);

	return Result(ret, T);
}

static int read_byte_MemoryIO(addr stream, addr *value, int *ret)
{
	byte c;
	addr page;
	struct stream_MemoryIO *str;
	size_t index;

	CheckIOMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));

	/* unread */
	str = PtrMemoryIOStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		fixnum_heap(value, (fixnum)str->unread[str->unread_index]);
		str->unread_index--;
		position_set_buffering(page, index + 1UL);
		return Result(ret, 0);
	}

	/* read */
	if (getc_buffering(page, &c)) {
		*value = Nil;
		return Result(ret, 1);  /* EOF */
	}

	fixnum_heap(value, (fixnum)c);
	return Result(ret, 0);
}

static int unread_byte_MemoryIO(addr stream, byte c)
{
	struct stream_MemoryIO *str;
	addr page;
	size_t index;

	CheckIOMemoryStream(stream);
	str = PtrMemoryIOStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	position_get_buffering(page, &index);

	/* unread check */
	if (INPUT_MEMORY_UNREAD_SIZE <= str->unread_index)
		return fmte_("The unread buffer is overflow.", NULL);

	/* index check */
	if (index == 0)
		return fmte_("The memory-stream index is underflow.", NULL);

	str->unread[str->unread_index] = c;
	str->unread_index++;
	position_set_buffering(page, index - 1UL);

	return 0;
}

static int write_byte_MemoryIO(addr stream, addr pos)
{
	CheckIOMemoryStream(stream);
	Return(close_memory_stream_error_(stream, NULL));
	clear_unread_io_memory_stream(stream);
	return write_byte_MemoryStream(stream, pos);
}

static int element_type_MemoryIO(addr stream, addr *ret)
{
	CheckIOMemoryStream(stream);
	return element_type_MemoryStream(stream, ret);
}

static int file_length_MemoryIO(addr stream, addr *ret)
{
	CheckIOMemoryStream(stream);
	return file_length_MemoryStream(stream, ret);
}

static int file_position_MemoryIO(addr stream, size_t *value, int *ret)
{
	CheckIOMemoryStream(stream);
	return file_position_MemoryStream(stream, value, ret);
}

static int file_position_start_MemoryIO(addr stream, int *ret)
{
	CheckIOMemoryStream(stream);
	return file_position_start_MemoryStream(stream, ret);
}

static int file_position_end_MemoryIO(addr stream, int *ret)
{
	CheckIOMemoryStream(stream);
	return file_position_end_MemoryStream(stream, ret);
}

static int file_position_set_MemoryIO(addr stream, size_t value, int *ret)
{
	CheckIOMemoryStream(stream);
	return file_position_set_MemoryStream(stream, value, ret);
}

_g void init_stream_memory_io(void)
{
	DefineStreamSet(MemoryIO, close);
	DefineStreamSet(MemoryIO, read_byte);
	DefineStreamSet(MemoryIO, unread_byte);
	DefineStreamSet(MemoryIO, write_byte);
	DefineStream___(MemoryIO, read_char);
	DefineStream___(MemoryIO, read_hang);
	DefineStream___(MemoryIO, unread_char);
	DefineStream___(MemoryIO, write_char);
	DefineStream___(MemoryIO, getleft);
	DefineStream___(MemoryIO, setleft);
	DefineStreamChk(MemoryIO, inputp, true);
	DefineStreamChk(MemoryIO, outputp, true);
	DefineStreamChk(MemoryIO, interactivep, false);
	DefineStreamChk(MemoryIO, characterp, false);
	DefineStreamChk(MemoryIO, binaryp, true);
	DefineStreamSet(MemoryIO, element_type);
	DefineStreamSet(MemoryIO, file_length);
	DefineStreamSet(MemoryIO, file_position);
	DefineStreamSet(MemoryIO, file_position_start);
	DefineStreamSet(MemoryIO, file_position_end);
	DefineStreamSet(MemoryIO, file_position_set);
	DefineStream___(MemoryIO, file_charlen);
	DefineStream___(MemoryIO, file_strlen);
	DefineStream___(MemoryIO, listen);
	DefineStream___(MemoryIO, clear_input);
	DefineStreamDef(MemoryIO, finish_output);
	DefineStreamDef(MemoryIO, force_output);
	DefineStreamDef(MemoryIO, clear_output);
	DefineStreamDef(MemoryIO, exitpoint);
	DefineStreamDef(MemoryIO, termsize);
}


/*****************************************************************************
 *  file-buffering
 *****************************************************************************/
/* read-byte */
static int read_byte_input_memory(addr stream, byte *value, int *ret)
{
	addr pos;
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, &pos)) {
		*value = 0;
		*ret = 0;
		return 1;  /* already closed */
	}

	/* unread */
	str = PtrMemoryInputStream(stream);
	if (str->unread_index) {
		*value = str->unread[str->unread_index];
		str->unread_index--;
		str->index++;
		*ret = 0;
		return 0;
	}

	/* end-of-file */
	if (str->size <= str->index) {
		*value = 0;
		*ret = 1; /* EOF */
		return 0;
	}

	/* read */
	if (get_unsigned8_sequence(pos, str->index, value)) {
		*value = 0;
		*ret = 0;
		return 1; /* error */
	}

	/* result */
	str->index++;
	*ret = 0;
	return 0;
}

static int read_byte_io_memory(addr stream, byte *value, int *ret)
{
	struct stream_MemoryIO *str;
	addr page;
	size_t index;

	CheckIOMemoryStream(stream);
	if (close_memory_stream_p(stream, &page)) {
		*value = 0;
		*ret = 0;
		return 1;  /* already closed */
	}

	/* unread */
	str = PtrMemoryIOStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		*value = str->unread[str->unread_index];
		str->unread_index--;
		position_set_buffering(page, index + 1UL);
		*ret = 0;
		return 0;
	}

	/* read */
	if (getc_buffering(page, value)) {
		*value = 0;
		*ret = 1; /* EOF */
		return 0;
	}

	*ret = 0;
	return 0;
}

_g int read_byte_memory_stream(addr stream, byte *value, int *ret)
{
	if (input_memory_stream_p(stream))
		return read_byte_input_memory(stream, value, ret);
	if (io_memory_stream_p(stream))
		return read_byte_io_memory(stream, value, ret);

	return 1;
}


/* write-byte */
static int write_byte_output_memory(addr stream, byte value)
{
	addr page;

	CheckOutputMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;

	return putc_buffering(page, value);
}

static int write_byte_io_memory(addr stream, byte value)
{
	addr page;

	CheckIOMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	clear_unread_io_memory_stream(stream);

	return putc_buffering(page, value);
}

_g int write_byte_memory_stream(addr stream, byte value)
{
	if (output_memory_stream_p(stream))
		return write_byte_output_memory(stream, value);
	if (io_memory_stream_p(stream))
		return write_byte_io_memory(stream, value);

	return 1;
}


/* file-length */
static int file_length_input_memory(addr stream, size_t *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, NULL))
		return 1;
	str = PtrMemoryInputStream(stream);
	*ret = str->size;

	return 0;
}

static int file_length_write_memory(addr stream, size_t *ret)
{
	addr page;

	if (close_memory_stream_p(stream, &page))
		return 1;
	length_buffering(page, ret);
	return 0;
}

_g int file_length_memory_stream(addr stream, size_t *ret)
{
	if (input_memory_stream_p(stream))
		return file_length_input_memory(stream, ret);
	if (output_memory_stream_p(stream))
		return file_length_write_memory(stream, ret);
	if (io_memory_stream_p(stream))
		return file_length_write_memory(stream, ret);

	return 1;
}


/* file-position */
static int file_position_input_memory(addr stream, size_t *ret)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, NULL))
		return 1;
	str = PtrMemoryInputStream(stream);
	*ret = str->index;

	return 0;
}

static int file_position_write_memory(addr stream, size_t *ret)
{
	addr page;

	if (close_memory_stream_p(stream, &page))
		return 0;
	length_buffering(page, ret);

	return 0;
}

_g int file_position_memory_stream(addr stream, size_t *ret)
{
	if (input_memory_stream_p(stream))
		return file_position_input_memory(stream, ret);
	if (output_memory_stream_p(stream))
		return file_position_write_memory(stream, ret);
	if (io_memory_stream_p(stream))
		return file_position_write_memory(stream, ret);

	return 1;
}


/* file-position-start */
static int file_position_start_input_memory(addr stream)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, NULL))
		return 1;
	str = PtrMemoryInputStream(stream);
	str->index = 0;
	str->unread_index = 0;

	return 0;
}

static int file_position_start_output_memory(addr stream)
{
	addr page;

	CheckOutputMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_start_buffering(page);

	return 0;
}

static int file_position_start_io_memory(addr stream)
{
	addr page;

	CheckIOMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_start_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}

_g int file_position_start_memory_stream(addr stream)
{
	if (input_memory_stream_p(stream))
		return file_position_start_input_memory(stream);
	if (output_memory_stream_p(stream))
		return file_position_start_output_memory(stream);
	if (io_memory_stream_p(stream))
		return file_position_start_io_memory(stream);

	return 1;
}


/* file-position-end */
static int file_position_end_input_memory(addr stream)
{
	struct stream_MemoryInput *str;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, NULL))
		return 1;
	str = PtrMemoryInputStream(stream);
	str->index = str->size;
	str->unread_index = 0;

	return 0;
}

static int file_position_end_output_memory(addr stream)
{
	addr page;

	CheckOutputMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_end_buffering(page);

	return 0;
}

static int file_position_end_io_memory(addr stream)
{
	addr page;

	CheckIOMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_end_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}

_g int file_position_end_memory_stream(addr stream)
{
	if (input_memory_stream_p(stream))
		return file_position_end_input_memory(stream);
	if (output_memory_stream_p(stream))
		return file_position_end_output_memory(stream);
	if (io_memory_stream_p(stream))
		return file_position_end_io_memory(stream);

	return 1;
}


/* file-position-set */
static int file_position_set_input_memory(addr stream, size_t value)
{
	struct stream_MemoryInput *str;
	addr page;

	CheckInputMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	str = PtrMemoryInputStream(stream);
	if (str->size < value)
		return 1;
	str->index = value;
	str->unread_index = 0;

	return 0;
}

static int file_position_set_output_memory(addr stream, size_t value)
{
	addr page;

	CheckOutputMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_set_buffering(page, value);

	return 0;
}

static int file_position_set_io_memory(addr stream, size_t value)
{
	addr page;

	CheckIOMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_set_buffering(page, value);
	clear_unread_io_memory_stream(stream);

	return 0;
}

_g int file_position_set_memory_stream(addr stream, size_t value)
{
	if (input_memory_stream_p(stream))
		return file_position_set_input_memory(stream, value);
	if (output_memory_stream_p(stream))
		return file_position_set_output_memory(stream, value);
	if (io_memory_stream_p(stream))
		return file_position_set_io_memory(stream, value);

	return 1;
}

