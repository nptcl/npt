#include "bignum.h"
#include "execute_object.h"
#include "condition.h"
#include "condition_define.h"
#include "constant.h"
#include "control_object.h"
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
 *  external-format
 */
static int open_external_format_equal_(addr value,
		const char *str1, const char *str2, int *ret)
{
	int check;

	if (symbolp(value))
		GetNameSymbol(value, &value);
	if (! stringp(value))
		return Result(ret, 0);
	Return(string_equalp_char_(value, str1, &check));
	if (check)
		return Result(ret, 1);
	if (str2 == NULL)
		return Result(ret, 0);

	return string_equalp_char_(value, str2, ret);
}

static int open_external_format_symbol_(addr x, enum Stream_Open_External *ret)
{
	addr y;

	/* ascii */
	GetConst(SYSTEM_ASCII, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Ascii);

	/* utf-16 */
	GetConst(SYSTEM_UTF_16, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf16);

	/* utf-16le */
	GetConst(SYSTEM_UTF_16LE, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf16Le);

	/* utf-16be */
	GetConst(SYSTEM_UTF_16BE, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf16Be);

	/* utf-16le-bom */
	GetConst(SYSTEM_UTF_16LE_BOM, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf16LeBom);

	/* utf-16be-bom */
	GetConst(SYSTEM_UTF_16BE_BOM, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf16BeBom);

	/* utf-32 */
	GetConst(SYSTEM_UTF_32, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf32);

	/* utf-32le */
	GetConst(SYSTEM_UTF_32LE, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf32Le);

	/* utf-32be */
	GetConst(SYSTEM_UTF_32BE, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf32Be);

	/* utf-32le-bom */
	GetConst(SYSTEM_UTF_32LE_BOM, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf32LeBom);

	/* utf-32be-bom */
	GetConst(SYSTEM_UTF_32BE_BOM, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf32BeBom);

	/* error */
	return Result(ret, Stream_Open_External_Error);
}

static int open_external_format_string_(addr x, enum Stream_Open_External *ret)
{
	int check;

	/* ascii */
	Return(open_external_format_equal_(x, "ASC", "ASCII", &check));
	if (check)
		return Result(ret, Stream_Open_External_Ascii);

	/* utf16 */
	Return(open_external_format_equal_(x, "UTF16", "UTF-16", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf16);

	/* utf16le */
	Return(open_external_format_equal_(x, "UTF16LE", "UTF-16LE", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf16Le);

	/* utf16be */
	Return(open_external_format_equal_(x, "UTF16BE", "UTF-16BE", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf16Be);

	/* utf16le-bom */
	Return(open_external_format_equal_(x, "UTF16LEBOM", "UTF-16LE-BOM", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf16LeBom);

	/* utf16be-bom */
	Return(open_external_format_equal_(x, "UTF16BEBOM", "UTF-16BE-BOM", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf16BeBom);

	/* utf32 */
	Return(open_external_format_equal_(x, "UTF32", "UTF-32", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf32);

	/* utf32le */
	Return(open_external_format_equal_(x, "UTF32LE", "UTF-32LE", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf32Le);

	/* utf32be */
	Return(open_external_format_equal_(x, "UTF32BE", "UTF-32BE", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf32Be);

	/* utf32le-bom */
	Return(open_external_format_equal_(x, "UTF32LEBOM", "UTF-32LE-BOM", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf32LeBom);

	/* utf32be-bom */
	Return(open_external_format_equal_(x, "UTF32BEBOM", "UTF-32BE-BOM", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf32BeBom);

	/* error */
	return Result(ret, Stream_Open_External_Error);
}

static int open_external_format_get_(addr x, enum Stream_Open_External *ret)
{
	enum Stream_Open_External value;
	int check;
	addr y;

	/* default */
	if (x == Unbound)
		return Result(ret, Stream_Open_External_Default);

	/* :default */
	GetConst(KEYWORD_DEFAULT, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Default);

	/* utf-8 symbol */
	GetConst(SYSTEM_UTF_8, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf8);

	/* utf-8-bom symbol */
	GetConst(SYSTEM_UTF_8_BOM, &y);
	if (x == y)
		return Result(ret, Stream_Open_External_Utf8Bom);

	/* utf8 string */
	Return(open_external_format_equal_(x, "UTF8", "UTF-8", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf8);

	/* utf8-bom string */
	Return(open_external_format_equal_(x, "UTF8BOM", "UTF-8-BOM", &check));
	if (check)
		return Result(ret, Stream_Open_External_Utf8Bom);

	/* symbol */
	Return(open_external_format_symbol_(x, &value));
	if (value != Stream_Open_External_Error)
		return Result(ret, value);

	/* string */
	return open_external_format_string_(x, ret);
}

int open_external_format_(Execute ptr, addr x, enum Stream_Open_External *ret)
{
	enum Stream_Open_External value;

	Return(open_external_format_get_(x, &value));
	if (value != Stream_Open_External_Default)
		return Result(ret, value);

	/* :default */
	GetConst(SYSTEM_EXTERNAL_FORMAT, &x);
	getspecial_local(ptr, x, &x);
	Return(open_external_format_get_(x, &value));
	if (value == Stream_Open_External_Default)
		value = Stream_Open_External_Utf8;

	return Result(ret, value);
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

int close_stream_unwind_protect_(Execute ptr, addr stream)
{
	addr control, save, ignore;

	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (close_stream_(stream, &ignore))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}


/*
 *  core
 */
int update_standard_stream(void)
{
	addr pos;

	/* stdin */
	GetConst(STREAM_STDIN, &pos);
	if (update_standard_input(pos))
		return 1;
	/* stdout */
	GetConst(STREAM_STDOUT, &pos);
	if (update_standard_output(pos))
		return 1;
	/* stderr */
	GetConst(STREAM_STDERR, &pos);
	if (update_standard_error(pos))
		return 1;

	return 0;
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

