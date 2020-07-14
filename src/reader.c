#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "control_object.h"
#include "hold.h"
#include "reader.h"
#include "reader_dispatch.h"
#include "reader_function.h"
#include "reader_info.h"
#include "reader_table.h"
#include "reader_token.h"
#include "reader_type.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"

/*
 *  pushchar_readtable
 */
static unicode charmode_readtable(addr pos, unicode c)
{
	switch (*PtrCaseReadtable(pos)) {
		case ReadTable_upcase:
			return toUpperUnicode(c);

		case ReadTable_downcase:
			return toLowerUnicode(c);

		case ReadTable_preserve:
			return c;

		case ReadTable_invert:
			if ('a' <= c && c <= 'z') return c - 'a' + 'A';
			if ('A' <= c && c <= 'Z') return c - 'A' + 'a';
			return c;

		default:
			fmte("Unknown readtable-case type.", NULL);
			break;
	}

	return 0;
}

static int tokenmode_readtable(Execute ptr)
{
	unsigned base;
	size_t size;
	addr queue;

	/* escape */
	if (getescape_readinfo(ptr))
		return TokenType_symbol;

	/* empty (for keyword) */
	getqueue_readinfo(ptr, &queue);
	getsize_charqueue(queue, &size);
	if (size == 0)
		return TokenType_empty;

	base = getreadbase(ptr);
	return tokentype(base, queue);
}

static void setpackage_readtable(Execute ptr)
{
	int type;
	addr queue, package;

	type = tokenmode_readtable(ptr);
	switch (type) {
		case TokenType_empty: /* for keyword */
			setpackage_readinfo(ptr, T);
			break;

		case TokenType_symbol:
		case TokenType_potential:
			getqueue_readinfo(ptr, &queue);
			make_charqueue_heap(queue, &package);
			setpackage_readinfo(ptr, package);
			clear_charqueue(queue);
			break;

		default:
			fmte("Package token type error.", NULL);
			return;
	}
}

static void pushchar_readtable(Execute ptr, addr pos, unicode c, int escape)
{
	unsigned bitescape;
	enum ReadInfo_State bitstate;
	struct readinfo_struct *str;
	addr queue;

	getqueue_readinfo(ptr, &queue);
	str = getreadinfo_struct(ptr);
	bitescape = str->escape;
	bitstate = str->state;
	/* mode0 : readsymbol
	 * modd1 : read colon
	 * mode2 : read colon second
	 * mode3 : readsymol next
	 *
	 * mode0 - aaa
	 * mode1 - :
	 * mode1 - aaa:
	 * mode2 - ::
	 * mode2 - aaa::
	 * mode2 - :aaa
	 * mode2 - ::aaa
	 * mode2 - aaa:bbb
	 * mode2 - aaa::bbb
	 */
	if (escape) {
		if (bitescape == 0)
			setescape_readinfo(ptr, 1);
		if (bitstate == ReadInfo_State_Colon1)
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
		push_charqueue_local(ptr->local, queue, c);
		return;
	}

	switch (bitstate) {
		case ReadInfo_State_First:
			str->unexport = 0;
			if (c == ':') {
				setstate_readinfo(ptr, ReadInfo_State_Colon1);
				setpackage_readtable(ptr);
				return;
			}
			break;

		case ReadInfo_State_Colon1:
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
			if (c != ':') break;
			str->unexport = 1;
			return;

		case ReadInfo_State_Colon2:
		case ReadInfo_State_Gensym:
			if (c == ':') {
				fmte("colon error", NULL);
				return;
			}
			break;

		default:
			fmte("mode error", NULL);
			return;
	}

	/* push char */
	if (! escape)
		c = charmode_readtable(pos, c);
	push_charqueue_local(ptr->local, queue, c);
}


/*
 *  readtable
 */
_g enum ReadTable_Type readtable_typetable(addr pos, unicode c)
{
	readtype_readtable(pos, c, &pos);
	if (pos == Nil)
		return ReadTable_Type_illegal;
	return ReadTypeStruct(pos)->type;
}

_g enum ReadTable_Result readtable_result(Execute ptr,
		addr *ret, addr stream, addr table)
{
	enum ReadTable_Type type;
	int result;
	unicode x, y, z;

step1:
	result = read_char_stream(stream, &x);
	if (result) goto eof;

	/* step2 */
	type = readtable_typetable(table, x);
	switch (type) {
		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_whitespace:
			/* step3 */
			goto step1;

		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
			unread_char_stream(stream, x);
			goto macro; /* return one value */

		case ReadTable_Type_escape_single:
			/* step5 */
			result = read_char_stream(stream, &y);
			if (result) goto error;
			pushchar_readtable(ptr, table, y, 1);
			goto step8;

		case ReadTable_Type_escape_multiple:
			/* step6 */
			goto step9;

		case ReadTable_Type_constituent:
			/* step7 */
			pushchar_readtable(ptr, table, x, 0);
			break;

		default:
			goto error;
	}

step8:
	result = read_char_stream(stream, &y);
	if (result) goto step10;
	type = readtable_typetable(table, y);
	switch (type) {
		case ReadTable_Type_constituent:
		case ReadTable_Type_macro_nonterm:
			pushchar_readtable(ptr, table, y, 0);
			goto step8;

		case ReadTable_Type_escape_single:
			result = read_char_stream(stream, &z);
			if (result) goto error;
			pushchar_readtable(ptr, table, z, 1);
			goto step8;

		case ReadTable_Type_escape_multiple:
			goto step9;

		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_macro_term:
			unread_char_stream(stream, y);
			goto step10;

		case ReadTable_Type_whitespace:
			if (getpreserving_readinfo(ptr))
				unread_char_stream(stream, y);
			goto step10;

		default:
			goto error;
	}

step9:
	result = read_char_stream(stream, &y);
	if (result) goto error;
	type = readtable_typetable(table, y);
	switch (type) {
		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
		case ReadTable_Type_constituent:
		case ReadTable_Type_whitespace:
			pushchar_readtable(ptr, table, y, 1);
			goto step9;

		case ReadTable_Type_escape_single:
			result = read_char_stream(stream, &z);
			if (result) goto error;
			pushchar_readtable(ptr, table, z, 1);
			goto step9;

		case ReadTable_Type_escape_multiple:
			goto step8;

		case ReadTable_Type_illegal:
			goto illegal_error;

		default:
			goto error;
	}

step10:
	maketoken(ptr, ret);
	goto final;

illegal_error:
	fmte("Illegal character error", NULL);
	goto error;

error:
	fmte("readtable error", NULL);
	goto eof;

final:
	return ReadTable_Result_normal;
macro:
	return ReadTable_Result_macro;
eof:
	return ReadTable_Result_eof;
}

_g int readtable_novalue(Execute ptr, int *result, addr *ret, addr stream, addr table)
{
	int check;
	addr pos;
	unicode u;

	/* read */
	clear_readinfo(ptr);
	switch (readtable_result(ptr, &pos, stream, table)) {
		case ReadTable_Result_normal:
			*result = 0;
			*ret = pos;
			return 0;

		case ReadTable_Result_eof:
			*result = 1;
			return 0;

		default:
			break;
	}

	/* macro execute */
	if (read_char_stream(stream, &u)) {
		fmte("eof error", NULL);
		return 1;
	}

	if (macro_character_execute(ptr, &check, &pos, u, stream, table)) {
		return 1;
	}

	if (check) {
		*result = 0;
		*ret = pos;
		return 0;
	}
	else {
		/* return no value */
		*result = -1;
		return 0;
	}
}

static int readtable_front(Execute ptr,
		int *result, addr *ret, addr stream, addr table)
{
	int check;

	for (;;) {
		if (readtable_novalue(ptr, &check, ret, stream, table)) {
			*result = 1;
			return 1;
		}
		if (0 <= check)
			break;
	}
	if (read_suppress_p(ptr))
		*ret = Nil;
	*result = check;
	return 0;
}


/*
 *  read
 */
_g int read_call(Execute ptr, addr stream, int *result, addr *ret)
{
	addr table;
	getreadtable(ptr, &table);
	return readtable_front(ptr, result, ret, stream, table);
}

_g int read_stream(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	pushreadinfo(ptr, &info);
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	pushreadinfo(ptr, &info);
	ReadInfoStruct(info)->preserving = 1;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int read_from_string(Execute ptr, int *result, addr *ret, addr pos)
{
	addr stream;
	LocalHold hold;

	open_input_string_stream(&stream, pos);
	hold = LocalHold_local_push(ptr, stream);
	Return(read_stream(ptr, stream, result, ret));
	localhold_end(hold);
	close_input_string_stream(stream);

	return 0;
}

_g int readstring(addr *ret, const char *code)
{
	int result;
	addr stream;

	open_input_char_stream(&stream, code);
	if (read_stream(Execute_Thread, stream, &result, ret))
		fmte("Cannot catch a system signal.", NULL);
	close_input_string_stream(stream);

	return result;
}

_g addr readr(const char *code)
{
	addr pos;
	if (readstring(&pos, code))
		return Nil;
	else
		return pos;
}


/*****************************************************************************
 *  initialize
 *****************************************************************************/
static void build_reader_special(void)
{
	addr pos, symbol;

	readtable_heap(&pos);
	GetConst(SPECIAL_READTABLE, &symbol);
	SetValueSymbol(symbol, pos);
}

_g void build_reader(void)
{
	build_reader_dispatch();
	build_reader_special();
}

_g void init_reader(void)
{
	init_reader_dispatch();
	init_reader_function();
	init_reader_token();
}

