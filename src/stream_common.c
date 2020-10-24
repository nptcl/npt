#include "character.h"
#include "character_queue.h"
#include "common_header.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "integer.h"
#include "sequence.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

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

_g int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	Return(stream_designer_(ptr, stream, &stream, 1));
	if (type == Nil)
		return peek_char_nil_(ptr, ret, stream, errorp, value, recp);
	else if (type == T)
		return peek_char_t_(ptr, ret, stream, errorp, value, recp);
	else
		return peek_char_character_(ptr, ret, type, stream, errorp, value, recp);
}

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

_g int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	int docheck, check;
	enum EndOfLine_Mode mode;
	unicode c;
	addr queue;
	LocalRoot local;
	LocalStack stack;

	Return(stream_designer_(ptr, pos, &pos, 1));
	local = ptr->local;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	Return(get_end_of_line_mode_(ptr, &mode));
	for (docheck = 0; ; docheck = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check)
			goto finish_eof;
		switch (mode) {
			case EndOfLine_CR:
				if (c == 0x0D)
					goto finish_value;
				break;

			case EndOfLine_LF:
				if (c == 0x0A)
					goto finish_value;
				break;

			case EndOfLine_CRLF:
				if (c == 0x0D) {
					Return(read_char_stream_(pos, &c, &check));
					if (check || c != 0x0A)
						return fmte_("Invalid CR-LF code.", NULL);
					goto finish_value;
				}
				break;

			case EndOfLine_Auto:
			default:
				if (c == 0x0A)
					goto finish_value;
				if (c == 0x0D) {
					Return(read_char_stream_(pos, &c, &check));
					if (check == 0 && c != 0x0A) {
						Return(unread_char_stream_(pos, c));
					}
					goto finish_value;
				}
				break;
		}
		Return(push_charqueue_local_(local, queue, c));
	}

finish_eof:
	if (docheck == 0)
		goto finish_error;
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	return Result(miss, 1);

finish_value:
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	return Result(miss, 0);

finish_error:
	if (errorp)
		return end_of_file_recursive_(ptr, pos, recp);
	*ret = value;
	return Result(miss, 1);
}

_g int write_string_stream(Execute ptr, addr string, addr rest, addr *ret)
{
	unicode c;
	addr stream;
	size_t size, start, end, i;

	/* argument */
	string_length(string, &size);
	if (rest == Nil) {
		Return(stream_designer_(ptr, Unbound, &stream, 0));
		start = 0;
		end = size;
	}
	else {
		Return_getcons(rest, &stream, &rest);
		Return(stream_designer_(ptr, stream, &stream, 0));
		Return(keyword_start_end_(size, rest, &start, &end));
	}

	for (i = start; i < end; i++) {
		Return(string_getc_(string, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return Result(ret, stream);
}

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

_g int read_sequence_stream(addr *ret, addr seq, addr stream, size_t start, size_t end)
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

_g int write_sequence_stream(LocalRoot local,
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

