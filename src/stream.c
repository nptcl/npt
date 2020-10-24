#include "bignum.h"
#include "condition.h"
#include "condition_define.h"
#include "constant.h"
#include "file.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  stream
 */
_g int open_stream_p(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed == 0;
}

_g int copyleft_stream_(addr stream, addr src)
{
	size_t size;
	Return(getleft_stream_(src, &size));
	return setleft_stream_(stream, size);
}

_g int pageout_stream_(addr stream)
{
	return write_char_stream_(stream, '\f');
}

_g int print_ascii_stream_(addr stream, const char *data)
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

_g int print_unicode_stream_(addr stream, const unicode *data)
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

_g int print_string_stream_(addr stream, addr pos)
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

_g int stream_designer_(Execute ptr, addr pos, addr *ret, int inputp)
{
	addr type;
	constindex index;

	/* default */
	if (pos == Unbound) {
		index = inputp?
			CONSTANT_SPECIAL_STANDARD_INPUT:
			CONSTANT_SPECIAL_STANDARD_OUTPUT;
		GetConstant(index, &pos);
	}

	/* symbol */
	if (symbolp(pos)) {
		Return(getspecialcheck_local_(ptr, pos, &pos));
	}

	/* stream */
	if (streamp(pos))
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	GetConst(COMMON_STREAM, &type);
	return call_type_error_(ptr, pos, type);
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

_g int standard_input_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_INPUT, ret);
}

_g int standard_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_STANDARD_OUTPUT, ret);
}

_g int error_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_ERROR_OUTPUT, ret);
}

_g int trace_output_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TRACE_OUTPUT, ret);
}

_g int terminal_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_TERMINAL_IO, ret);
}

_g int debug_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_DEBUG_IO, ret);
}

_g int query_io_stream_(Execute ptr, addr *ret)
{
	return specialvalue_(ptr, CONSTANT_SPECIAL_QUERY_IO, ret);
}

_g int output_stream_designer_(Execute ptr, addr stream, addr *ret)
{
	if (stream == Unbound)
		return standard_output_stream_(ptr, ret);
	if (stream == T)
		return terminal_io_stream_(ptr, ret);
	if (stream == Nil)
		return standard_output_stream_(ptr, ret);

	return Result(ret, stream);
}


/*
 *  wrapper
 */
_g int read_unsigned8_stream_(addr stream, byte *value, int *ret)
{
	int check;
	addr pos, type;
	fixnum v;

	Return(read_byte_stream_(stream, &pos, &check));
	if (check) {
		*value = 0;
		return Result(ret, 1);
	}
	if (GetFixnum_signed(pos, &v) || v < 0 || 0xFF < v) {
		GetConst(STREAM_BINARY_TYPE, &type);
		return call_type_error_(NULL, pos, type);
	}
	*value = (byte)v;
	return Result(ret, 0);
}

_g int write_unsigned8_stream_(addr stream, byte value)
{
	addr pos;

	fixnum_heap(&pos, (fixnum)value);
	Return(write_byte_stream_(stream, pos));
	return exitpoint_stream_(stream);
}


/*
 *  core
 */
_g void update_standard_stream(void)
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

_g int save_stream(addr pos)
{
	switch (PtrStructStream(pos)->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
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

