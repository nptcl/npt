#include "character.h"
#include "character_queue.h"
#include "common_header.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "file.h"
#include "integer.h"
#include "sequence.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "stream_synonym.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  binary
 */
static int get_binary_stream(addr stream, addr *ret)
{
	struct StructStream *str;

	str = PtrStructStream(stream);
	switch (str->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
		case StreamType_Probe:
			*ret = stream;
			return 1;

		case StreamType_Synonym:
			get_synonym_stream(stream, &stream);
			return get_binary_stream(stream, ret);

		default:
			*ret = stream;
			return 0;
	}
}

static int read_binary_from_byte_(addr stream, byte *pos, size_t size, size_t *ret)
{
	int check;
	byte c;
	size_t x;

	for (x = 0; x < size; x++) {
		Return(read_unsigned8_stream_(stream, &c, &check));
		if (check)
			break;
		pos[x] = c;
	}

	return Result(ret, x);
}

int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	if (get_binary_stream(stream, &stream))
		return read_binary_file_(stream, pos, size, ret);
	else
		return read_binary_from_byte_(stream, (byte *)pos, size, ret);
}

static int write_binary_from_byte_(addr stream,
		const byte *pos, size_t size, size_t *ret)
{
	size_t x;

	for (x = 0; x < size; x++) {
		Return(write_unsigned8_stream_(stream, pos[x]));
	}

	return Result(ret, x);
}

int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	if (get_binary_stream(stream, &stream))
		return write_binary_file_(stream, pos, size, ret);
	else
		return write_binary_from_byte_(stream, (const byte *)pos, size, ret);
}


/*
 *  terpri
 */
int terpri_stream_(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return write_char_stream_(stream, '\n');
}


/*
 *  fresh-line
 */
int fresh_line_stream_(addr stream, int *ret)
{
	size_t size;

	CheckType(stream, LISPTYPE_STREAM);
	Return(getleft_stream_(stream, &size));
	if (size == 0) {
		if (ret)
			*ret = 0;
		return 0;
	}
	Return(terpri_stream_(stream));
	if (ret)
		*ret = 1;
	return 0;
}


/*
 *  peek-char
 */
static int end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	return call_end_of_file_(ptr, pos);
}

static int peek_char_nil_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		if (errorp) {
			*ret = Nil;
			return end_of_file_recursive_(ptr, stream, recp);
		}
		return Result(ret, value);
	}

	Return(unread_char_stream_(stream, c));
	character_heap(ret, c);
	return 0;
}

static int peek_char_t_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (! isSpaceUnicode(c)) {
			Return(unread_char_stream_(stream, c));
			character_heap(ret, c);
			break;
		}
	}

	return 0;
}

static int peek_char_character_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c, v;

	GetCharacter(type, &v);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (v == c) {
			Return(unread_char_stream_(stream, c));
			Return(peek_char_nil_(ptr, ret, stream, errorp, value, recp));
			break;
		}
	}

	return 0;
}

int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	Return(input_stream_designer_(ptr, stream, &stream));
	if (type == Nil)
		return peek_char_nil_(ptr, ret, stream, errorp, value, recp);
	else if (type == T)
		return peek_char_t_(ptr, ret, stream, errorp, value, recp);
	else
		return peek_char_character_(ptr, ret, type, stream, errorp, value, recp);
}


/*
 *  read-line
 */
enum EndOfLine_Mode {
	EndOfLine_Auto,
	EndOfLine_CR,
	EndOfLine_LF,
	EndOfLine_CRLF
};

static int get_end_of_line_mode_(Execute ptr, enum EndOfLine_Mode *ret)
{
	addr pos, check;

	GetConst(SYSTEM_END_OF_LINE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	/* Auto */
	GetConst(SYSTEM_AUTO, &check);
	if (check == pos)
		return Result(ret, EndOfLine_Auto);
	/* CR */
	GetConst(SYSTEM_CR, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CR);
	/* LF */
	GetConst(SYSTEM_LF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_LF);
	/* CRLF */
	GetConst(SYSTEM_CRLF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CRLF);
	/* error */
	*ret = EndOfLine_Auto;
	return fmte_("Invalid *end-of-line* value ~S.", pos, NULL);
}

static int read_line_stream_auto_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == 0x0A) {
			type = 1;
			break;
		}
		if (c == 0x0D) {
			Return(read_char_stream_(pos, &c, &check));
			if (check == 0 && c != 0x0A) {
				Return(unread_char_stream_(pos, c));
			}
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);
}

static int read_line_stream_value_(LocalRoot local,
		addr pos, addr queue, unicode eol, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == eol) {
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);
}

static int read_line_stream_cr_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	return read_line_stream_value_(local, pos, queue, 0x0D, rloop, ret);
}

static int read_line_stream_lf_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	return read_line_stream_value_(local, pos, queue, 0x0A, rloop, ret);
}

static int read_line_stream_crlf_(LocalRoot local,
		addr pos, addr queue, int *rloop, int *ret)
{
	int loop, check, type;
	unicode c;

	type = 0;
	for (loop = 0; ; loop = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check) {
			type = 0;
			break;
		}
		if (c == 0x0A)
			goto error;
		if (c == 0x0D) {
			Return(read_char_stream_(pos, &c, &check));
			if (check || c != 0x0A)
				goto error;
			type = 1;
			break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

	*rloop = loop;
	return Result(ret, type);

error:
	*rloop = 0;
	*ret = 0;
	return fmte_("Invalid CR-LF code.", NULL);
}

static int read_line_stream_mode_(Execute ptr,
		addr pos, addr queue, int *loop, int *ret)
{
	enum EndOfLine_Mode mode;
	LocalRoot local;

	local = ptr->local;
	Return(get_end_of_line_mode_(ptr, &mode));
	switch (mode) {
		case EndOfLine_Auto:
			return read_line_stream_auto_(local, pos, queue, loop, ret);

		case EndOfLine_CR:
			return read_line_stream_cr_(local, pos, queue, loop, ret);

		case EndOfLine_LF:
			return read_line_stream_lf_(local, pos, queue, loop, ret);

		case EndOfLine_CRLF:
			return read_line_stream_crlf_(local, pos, queue, loop, ret);

		default:
			return read_line_stream_auto_(local, pos, queue, loop, ret);
	}
}

static int read_line_stream_loop_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	int loop, type;
	addr queue;

	charqueue_local(ptr->local, &queue, 0);
	Return(read_line_stream_mode_(ptr, pos, queue, &loop, &type));
	if (type) {
		make_charqueue_heap(queue, ret);
		return Result(miss, 0);
	}

	/* result */
	if (loop != 0) {
		make_charqueue_heap(queue, ret);
		return Result(miss, 1);
	}

	/* error */
	if (errorp) {
		*ret = Nil;
		*miss = 0;
		return end_of_file_recursive_(ptr, pos, recp);
	}
	*ret = value;
	return Result(miss, 1);
}

int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	LocalRoot local;
	LocalStack stack;

	Return(input_stream_designer_(ptr, pos, &pos));
	local = ptr->local;
	push_local(local, &stack);
	Return(read_line_stream_loop_(ptr, ret, miss, pos, errorp, value, recp));
	rollback_local(local, stack);

	return 0;
}


/*
 *  write-string
 */
int write_string_stream_(Execute ptr, addr string, addr rest, addr *ret)
{
	unicode c;
	addr stream;
	size_t size, start, end, i;

	/* argument */
	string_length(string, &size);
	if (rest == Nil) {
		Return(output_stream_designer_(ptr, Unbound, &stream));
		start = 0;
		end = size;
	}
	else {
		Return_getcons(rest, &stream, &rest);
		Return(output_stream_designer_(ptr, stream, &stream));
		Return(keyword_start_end_(size, rest, &start, &end));
	}

	for (i = start; i < end; i++) {
		Return(string_getc_(string, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return Result(ret, stream);
}


/*
 *  read-sequence
 */
static int read_sequence_character_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	unicode c;
	addr value;

	for (; start < end; start++) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		character_heap(&value, c);
		Return(setelt_sequence_(seq, start, value));
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

static int read_sequence_binary_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	addr value;

	for (; start < end; start++) {
		Return(read_byte_stream_(stream, &value, &check));
		if (check)
			break;
		Return(setelt_sequence_(seq, start, value));
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

int read_sequence_stream_(addr *ret, addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return read_sequence_character_(ret, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return read_sequence_binary_(ret, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}


/*
 *  write-sequence
 */
static int write_sequence_character_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	unicode c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		Return(getelt_sequence_(local, seq, start, &value));
		if (! characterp(value))
			return TypeError_(value, CHARACTER);
		GetCharacter(value, &c);
		rollback_local(local, stack);
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int write_sequence_binary_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		Return(getelt_sequence_(local, seq, start, &value));
		if (! fixnump(value))
			return TypeError_(value, INTEGER);
		Return(write_byte_stream_(stream, value));
		rollback_local(local, stack);
	}

	return 0;
}

int write_sequence_stream_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return write_sequence_character_(local, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return write_sequence_binary_(local, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}

