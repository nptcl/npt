#include "bignum.h"
#include "condition.h"
#include "condition_define.h"
#include "constant.h"
#include "file.h"
#include "integer.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_object.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  stream
 */
int open_stream_p(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed == 0;
}

int copyleft_stream_(addr stream, addr src)
{
	size_t size;
	Return(getleft_stream_(src, &size));
	return setleft_stream_(stream, size);
}

int pageout_stream_(addr stream)
{
	return write_char_stream_(stream, '\f');
}

int print_ascii_stream_(addr stream, const char *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *(const byte *)data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, (unicode)c));
		data++;
	}

	return 0;
}

int print_unicode_stream_(addr stream, const unicode *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, c));
		data++;
	}

	return 0;
}

int print_string_stream_(addr stream, addr pos)
{
	unicode c;
	size_t size, i;

	CheckType(stream, LISPTYPE_STREAM);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int stream_designer_(Execute ptr, addr stream, addr *ret, int inputp)
{
	addr type;

	/* default */
	if (stream == Unbound || stream == Nil) {
		if (inputp)
			return standard_input_stream_(ptr, ret);
		else
			return standard_output_stream_(ptr, ret);
	}

	/* stream */
	if (streamp(stream))
		return Result(ret, stream);

	/* boolean */
	if (stream == T)
		return terminal_io_stream_(ptr, ret);

	/* error */
	*ret = Nil;
	GetTypeTable(&type, StreamDesigner);
	return call_type_error_(ptr, stream, type);
}

int input_stream_designer_(Execute ptr, addr stream, addr *ret)
{
	return stream_designer_(ptr, stream, ret, 1);
}

int output_stream_designer_(Execute ptr, addr stream, addr *ret)
{
	return stream_designer_(ptr, stream, ret, 0);
}


/*
 *  special variable
 */
static int specialvalue_(Execute ptr, constindex index, addr *ret)
{
	addr symbol;
	GetConstant(index, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

int standard_input_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_INPUT, ret);
}

int standard_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_OUTPUT, ret);
}

int error_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_ERROR_OUTPUT, ret);
}

int trace_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TRACE_OUTPUT, ret);
}

int terminal_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TERMINAL_IO, ret);
}

int debug_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_DEBUG_IO, ret);
}

int query_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_QUERY_IO, ret);
}


/*
 *  wrapper
 */
int read_unsigned8_stream_(addr stream, byte *value, int *ret)
{
	int check;
	addr pos, type;
	fixnum v;

	Return(read_byte_stream_(stream, &pos, &check));
	if (check) {
		*value = 0;
		return Result(ret, 1);
	}
	if (GetFixnum_signed(pos, &v) || ! IsByteSign(v)) {
		Return(element_type_stream_(stream, &type));
		return call_type_error_(NULL, pos, type);
	}
	*value = (byte)v;
	return Result(ret, 0);
}

int write_unsigned8_stream_(addr stream, byte value)
{
	addr pos;

	fixnum_heap(&pos, (fixnum)value);
	Return(write_byte_stream_(stream, pos));
	return exitpoint_stream_(stream);
}

#define REDIRECT_UNSIGNED8_SIZE		4096
static int read_redirect_unsigned8_stream_(Execute ptr,
		addr stream, byte *data, int *ret)
{
	int i, check;
	byte c;

	c = 0;
	for (i = 0; i < REDIRECT_UNSIGNED8_SIZE; i++) {
		Return(read_unsigned8_stream_(stream, &c, &check));
		if (check)
			break;
		data[i] = c;
	}

	return Result(ret, i);
}

static int write_redirect_unsigned8_stream_(Execute ptr,
		addr stream, byte *data, int size)
{
	int i;
	addr value;

	for (i = 0; i < size; i++) {
		fixnum_heap(&value, (fixnum)data[i]);
		Return(write_byte_stream_(stream, value));
	}

	return 0;
}

int redirect_unsigned8_stream_(Execute ptr, addr src, addr dst)
{
	byte data[REDIRECT_UNSIGNED8_SIZE];
	int size;

	for (;;) {
		Return(read_redirect_unsigned8_stream_(ptr, src, data, &size));
		if (size == 0)
			break;
		Return(write_redirect_unsigned8_stream_(ptr, dst, data, size));
	}

	return 0;
}


/*
 *  core
 */
void update_standard_stream(void)
{
	addr pos;

	/* stdin */
	GetConst(STREAM_STDIN, &pos);
	update_standard_input(pos);
	/* stdout */
	GetConst(STREAM_STDOUT, &pos);
	update_standard_output(pos);
	/* stderr */
	GetConst(STREAM_STDERR, &pos);
	update_standard_error(pos);
}

int save_stream(addr pos)
{
	switch (PtrStructStream(pos)->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
		case StreamType_Probe:
			return save_stream_file(pos);

		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
			return save_stream_system(pos);

		default:
			break;
	}

	return 0;
}

