#include "bignum.h"
#include "buffering.h"
#include "build.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "integer.h"
#include "sequence.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_memory.h"
#include "stream_variable.h"
#include "strtype.h"
#include "type_table.h"
#include "typedef.h"

#define CheckMemoryStream(stream) { \
	Check(! memory_stream_p(stream), "type error"); \
}

static int close_memory_stream_p(addr stream, addr *ret)
{
	addr pos;

	CheckMemoryStream(stream);
	if (open_stream_p(stream)) {
		GetInfoStream(stream, &pos);
		if (ret)
			*ret = pos;
		return 0;
	}

	/* close */
	if (ret)
		*ret = Nil;
	return 0;
}

static int close_memory_stream_error_(addr stream, addr *ret)
{
	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, ret))
		return fmte_("The stream is already closed.", stream, NULL);

	return 0;
}


/*****************************************************************************
 *  MemoryStream
 *****************************************************************************/
struct stream_MemoryStream {
	unsigned cache : 1;
	unsigned unread_index;
	byte unread[INPUT_MEMORY_UNREAD_SIZE];
};
#define PtrMemoryStream(pos) ((struct stream_MemoryStream *)PtrDataStream(pos))

static int open_memory_stream_(addr *ret,
		enum StreamType type, addr input, size_t cell, size_t array, int cache)
{
	struct stream_MemoryStream *str;
	addr pos, file;

	if (! sequencep(input)) {
		*ret = Nil;
		return TypeError_(input, SEQUENCE);
	}

	/* object */
	stream_heap(&pos, type, sizeoft(struct stream_MemoryStream));
	str = PtrMemoryStream(pos);
	str->unread_index = 0;
	str->cache = (cache != 0);

	/* buffering */
	buffering_heap(&file, cell, array);
	SetInfoStream(pos, file);
	Return(read_buffering_(file, input));
	force_open_stream(pos);

	return Result(ret, pos);
}

static void clear_unread_io_memory_stream(addr stream)
{
	struct stream_MemoryStream *str;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	str->unread_index = 0;
}

int memory_stream_heap_(addr stream, addr *ret)
{
	addr page;

	CheckMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	return make_vector_buffering_heap_(page, ret);
}

int clear_memory_stream_(addr stream)
{
	addr page;

	CheckMemoryStream(stream);
	Return(close_memory_stream_error_(stream, &page));
	clear_unread_io_memory_stream(stream);
	clear_buffering(page);

	return 0;
}

void getsize_memory_stream(addr stream, size_t *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	getcell_buffering(page, ret);
}

void getarray_memory_stream(addr stream, size_t *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	getwidth_buffering(page, ret);
}

int getcache_memory_stream(addr stream)
{
	struct stream_MemoryStream *str;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	return str->cache;
}

void gettype_memory_stream(addr stream, addr *ret)
{
	if (! streamp(stream)) {
		*ret = Nil;
		return;
	}

	switch (getstreamtype(stream)) {
		case StreamType_MemoryInput:
			GetConst(KEYWORD_INPUT, ret);
			break;

		case StreamType_MemoryOutput:
			GetConst(KEYWORD_OUTPUT, ret);
			break;

		case StreamType_MemoryIO:
			GetConst(KEYWORD_IO, ret);
			break;

		default:
			*ret = Nil;
			break;
	}
}

int settype_memory_stream_(addr stream, addr value)
{
	addr check;
	struct StructStream *str;

	CheckType(stream, LISPTYPE_STREAM);
	str = PtrStructStream(stream);

	/* input */
	GetConst(KEYWORD_INPUT, &check);
	if (value == check) {
		str->type = StreamType_MemoryInput;
		return 0;
	}

	/* output */
	GetConst(KEYWORD_OUTPUT, &check);
	if (value == check) {
		str->type = StreamType_MemoryOutput;
		return 0;
	}

	/* io */
	GetConst(KEYWORD_IO, &check);
	if (value == check) {
		str->type = StreamType_MemoryIO;
		return 0;
	}

	/* error */
	return fmte_("Invalid memory-stream type ~S.", value, NULL);
}


/*
 *  stream function
 */
static int close_MemoryStream(addr stream, addr *ret)
{
	CheckMemoryStream(stream);
	force_close_stream(stream);
	return Result(ret, T);
}

static int read_byte_MemoryStream(addr stream, addr *value, int *ret)
{
	byte c;
	addr page;
	struct stream_MemoryStream *str;
	size_t index;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);

	/* unread */
	str = PtrMemoryStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		str->unread_index--;
		fixnum_heap(value, (fixnum)str->unread[str->unread_index]);
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

static int unread_byte_MemoryStream(addr stream, byte c)
{
	struct stream_MemoryStream *str;
	addr page;
	size_t index;

	CheckMemoryStream(stream);
	str = PtrMemoryStream(stream);
	GetInfoStream(stream, &page);
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

static int write_byte_MemoryStream(addr stream, addr pos)
{
	addr page;
	fixnum v;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	Return(getfixnum_signed_(pos, &v));
	if (! IsByteSign(v))
		return fmte_("The argument ~S must be a (unsigned-byte 8) type.", pos, NULL);
	if (putc_buffering(page, (byte)v))
		return fmte_("Too large file size.", NULL);
	clear_unread_io_memory_stream(stream);

	return 0;
}

static int element_type_MemoryStream(addr stream, addr *ret)
{
	addr x, y;

	CheckMemoryStream(stream);
	/* (unsigned-byte 8) */
	GetConst(COMMON_UNSIGNED_BYTE, &x);
	fixnum_heap(&y, 8);
	list_heap(ret, x, y, NULL);

	return 0;
}

static int file_length_MemoryStream(addr stream, addr *ret)
{
	addr page;
	size_t size;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	length_buffering(page, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

static int file_position_MemoryStream(addr stream, size_t *value, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_get_buffering(page, value);

	return Result(ret, 0);
}

static int file_position_start_MemoryStream(addr stream, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_start_buffering(page);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int file_position_end_MemoryStream(addr stream, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_end_buffering(page);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int file_position_set_MemoryStream(addr stream, size_t value, int *ret)
{
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	position_set_buffering(page, value);
	clear_unread_io_memory_stream(stream);

	return Result(ret, 0);
}

static int listen_MemoryStream(addr stream, int *ret)
{
	int check;
	addr page;

	CheckMemoryStream(stream);
	GetInfoStream(stream, &page);
	check = end_buffering(page);

	return Result(ret, ! check);
}

static int clear_input_MemoryStream(addr stream)
{
	CheckMemoryStream(stream);
	return 0;
}


/*****************************************************************************
 *  MemoryInput
 *****************************************************************************/
int open_input_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	int ignore;
	addr pos;

	pos = Nil;
	Return(open_memory_stream_(&pos, StreamType_MemoryInput, input,
				cell, array, cache));
	Return(file_position_start_MemoryStream(pos, &ignore));

	return Result(ret, pos);
}

void init_stream_memory_input(void)
{
	DefineStreamLet(MemoryInput, close, MemoryStream);
	DefineStreamLet(MemoryInput, read_byte, MemoryStream);
	DefineStreamLet(MemoryInput, unread_byte, MemoryStream);
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
	DefineStreamLet(MemoryInput, element_type, MemoryStream);
	DefineStreamDef(MemoryInput, external_format);
	DefineStreamLet(MemoryInput, file_length, MemoryStream);
	DefineStreamLet(MemoryInput, file_position, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_start, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_end, MemoryStream);
	DefineStreamLet(MemoryInput, file_position_set, MemoryStream);
	DefineStream___(MemoryInput, file_charlen);
	DefineStream___(MemoryInput, file_strlen);
	DefineStreamLet(MemoryInput, listen, MemoryStream);
	DefineStreamLet(MemoryInput, clear_input, MemoryStream);
	DefineStream___(MemoryInput, finish_output);
	DefineStream___(MemoryInput, force_output);
	DefineStream___(MemoryInput, clear_output);
	DefineStreamDef(MemoryInput, exitpoint);
	DefineStreamDef(MemoryInput, termsize);
}


/*****************************************************************************
 *  MemoryOutput
 *****************************************************************************/
int open_output_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	return open_memory_stream_(ret, StreamType_MemoryOutput,
			input, cell, array, cache);
}

void init_stream_memory_output(void)
{
	DefineStreamLet(MemoryOutput, close, MemoryStream);
	DefineStream___(MemoryOutput, read_byte);
	DefineStream___(MemoryOutput, unread_byte);
	DefineStreamLet(MemoryOutput, write_byte, MemoryStream);
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
	DefineStreamLet(MemoryOutput, element_type, MemoryStream);
	DefineStreamDef(MemoryOutput, external_format);
	DefineStreamLet(MemoryOutput, file_length, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_start, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_end, MemoryStream);
	DefineStreamLet(MemoryOutput, file_position_set, MemoryStream);
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
int open_io_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache)
{
	return open_memory_stream_(ret, StreamType_MemoryIO,
			input, cell, array, cache);
}

void init_stream_memory_io(void)
{
	DefineStreamLet(MemoryIO, close, MemoryStream);
	DefineStreamLet(MemoryIO, read_byte, MemoryStream);
	DefineStreamLet(MemoryIO, unread_byte, MemoryStream);
	DefineStreamLet(MemoryIO, write_byte, MemoryStream);
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
	DefineStreamLet(MemoryIO, element_type, MemoryStream);
	DefineStreamDef(MemoryIO, external_format);
	DefineStreamLet(MemoryIO, file_length, MemoryStream);
	DefineStreamLet(MemoryIO, file_position, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_start, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_end, MemoryStream);
	DefineStreamLet(MemoryIO, file_position_set, MemoryStream);
	DefineStream___(MemoryIO, file_charlen);
	DefineStream___(MemoryIO, file_strlen);
	DefineStreamLet(MemoryIO, listen, MemoryStream);
	DefineStreamLet(MemoryIO, clear_input, MemoryStream);
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
static int read_byte_read_memory(addr stream, byte *value, int *ret)
{
	struct stream_MemoryStream *str;
	addr page;
	size_t index;

	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, &page)) {
		*value = 0;
		*ret = 0;
		return 1;  /* already closed */
	}

	/* unread */
	str = PtrMemoryStream(stream);
	position_get_buffering(page, &index);
	if (str->unread_index) {
		str->unread_index--;
		*value = str->unread[str->unread_index];
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

int read_byte_memory_stream(addr stream, byte *value, int *ret)
{
	if (input_memory_stream_p(stream))
		return read_byte_read_memory(stream, value, ret);
	if (io_memory_stream_p(stream))
		return read_byte_read_memory(stream, value, ret);

	return 1;
}


/* write-byte */
static int write_byte_write_memory(addr stream, byte value)
{
	addr page;

	CheckMemoryStream(stream);
	if (close_memory_stream_p(stream, &page))
		return 1;
	clear_unread_io_memory_stream(stream);

	return putc_buffering(page, value);
}

int write_byte_memory_stream(addr stream, byte value)
{
	if (output_memory_stream_p(stream))
		return write_byte_write_memory(stream, value);
	if (io_memory_stream_p(stream))
		return write_byte_write_memory(stream, value);

	return 1;
}


/* file-length */
int file_length_memory_stream(addr stream, size_t *ret)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	length_buffering(page, ret);

	return 0;
}


/* file-position */
int file_position_memory_stream(addr stream, size_t *ret)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_get_buffering(page, ret);

	return 0;
}


/* file-position-start */
int file_position_start_memory_stream(addr stream)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_start_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}


/* file-position-end */
int file_position_end_memory_stream(addr stream)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_end_buffering(page);
	clear_unread_io_memory_stream(stream);

	return 0;
}


/* file-position-set */
int file_position_set_memory_stream(addr stream, size_t value)
{
	addr page;

	if (! memory_stream_p(stream))
		return 1;
	if (close_memory_stream_p(stream, &page))
		return 1;
	position_set_buffering(page, value);
	clear_unread_io_memory_stream(stream);

	return 0;
}

