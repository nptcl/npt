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
static int charmode_readtable_(addr pos, unicode c, unicode *ret)
{
	switch (*PtrCaseReadtable(pos)) {
		case ReadTable_upcase:
			c = toUpperUnicode(c);
			break;

		case ReadTable_downcase:
			c =  toLowerUnicode(c);
			break;

		case ReadTable_preserve:
			break;

		case ReadTable_invert:
			if ('a' <= c && c <= 'z') {
				c = c - 'a' + 'A';
				break;
			}
			if ('A' <= c && c <= 'Z') {
				c = c - 'A' + 'a';
				break;
			}
			break;

		default:
			*ret = 0;
			return fmte_("Unknown readtable-case type.", NULL);
	}

	return Result(ret, c);
}

static int tokenmode_readtable_(Execute ptr, int *ret)
{
	unsigned base;
	size_t size;
	addr queue;

	/* escape */
	if (getescape_readinfo(ptr))
		return Result(ret, TokenType_symbol);

	/* empty (for keyword) */
	getqueue_readinfo(ptr, &queue);
	getsize_charqueue(queue, &size);
	if (size == 0)
		return Result(ret, TokenType_empty);

	Return(getreadbase_(ptr, &base));
	return Result(ret, tokentype(base, queue));
}

static int setpackage_readtable_(Execute ptr)
{
	int type;
	addr queue, package;

	Return(tokenmode_readtable_(ptr, &type));
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
			return fmte_("Package token type error.", NULL);
	}

	return 0;
}

static int pushchar_readtable_(Execute ptr, addr pos, unicode c, int escape)
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
		return push_charqueue_local_(ptr->local, queue, c);
	}

	switch (bitstate) {
		case ReadInfo_State_First:
			str->unexport = 0;
			if (c == ':') {
				setstate_readinfo(ptr, ReadInfo_State_Colon1);
				return setpackage_readtable_(ptr);
			}
			break;

		case ReadInfo_State_Colon1:
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
			if (c != ':') break;
			str->unexport = 1;
			return 0;

		case ReadInfo_State_Colon2:
		case ReadInfo_State_Gensym:
			if (c == ':')
				return fmte_("colon error", NULL);
			break;

		default:
			return fmte_("mode error", NULL);
	}

	/* push char */
	if (! escape) {
		Return(charmode_readtable_(pos, c, &c));
	}
	return push_charqueue_local_(ptr->local, queue, c);
}


/*
 *  readtable
 */
_g int readtable_typetable_(addr pos, unicode c, enum ReadTable_Type *ret)
{
	Return(readtype_readtable_(pos, c, &pos));
	if (pos == Nil)
		return Result(ret, ReadTable_Type_illegal);
	else
		return Result(ret, ReadTypeStruct(pos)->type);
}

_g int readtable_result_(Execute ptr,
		addr *token, addr stream, addr table, enum ReadTable_Result *ret)
{
	enum ReadTable_Type type;
	int check;
	unicode x, y, z;

step1:
	Return(read_char_stream_(stream, &x, &check));
	if (check)
		goto eof;

	/* step2 */
	Return(readtable_typetable_(table, x, &type));
	switch (type) {
		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_whitespace:
			/* step3 */
			goto step1;

		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
			Return(unread_char_stream_(stream, x));
			goto macro; /* return one value */

		case ReadTable_Type_escape_single:
			/* step5 */
			Return(read_char_stream_(stream, &y, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, y, 1));
			goto step8;

		case ReadTable_Type_escape_multiple:
			/* step6 */
			goto step9;

		case ReadTable_Type_constituent:
			/* step7 */
			Return(pushchar_readtable_(ptr, table, x, 0));
			break;

		default:
			goto error;
	}

step8:
	Return(read_char_stream_(stream, &y, &check));
	if (check)
		goto step10;
	Return(readtable_typetable_(table, y, &type));
	switch (type) {
		case ReadTable_Type_constituent:
		case ReadTable_Type_macro_nonterm:
			Return(pushchar_readtable_(ptr, table, y, 0));
			goto step8;

		case ReadTable_Type_escape_single:
			Return(read_char_stream_(stream, &z, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, z, 1));
			goto step8;

		case ReadTable_Type_escape_multiple:
			goto step9;

		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_macro_term:
			Return(unread_char_stream_(stream, y));
			goto step10;

		case ReadTable_Type_whitespace:
			if (getpreserving_readinfo(ptr)) {
				Return(unread_char_stream_(stream, y));
			}
			goto step10;

		default:
			goto error;
	}

step9:
	Return(read_char_stream_(stream, &y, &check));
	if (check)
		goto error;
	Return(readtable_typetable_(table, y, &type));
	switch (type) {
		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
		case ReadTable_Type_constituent:
		case ReadTable_Type_whitespace:
			Return(pushchar_readtable_(ptr, table, y, 1));
			goto step9;

		case ReadTable_Type_escape_single:
			Return(read_char_stream_(stream, &z, &check));
			if (check)
				goto error;
			Return(pushchar_readtable_(ptr, table, z, 1));
			goto step9;

		case ReadTable_Type_escape_multiple:
			goto step8;

		case ReadTable_Type_illegal:
			goto illegal_error;

		default:
			goto error;
	}

step10:
	Return(maketoken_(ptr, token));
	goto final;

illegal_error:
	return fmte_("Illegal character error", NULL);

error:
	return fmte_("readtable error", NULL);

final:
	return Result(ret, ReadTable_Result_normal);
macro:
	return Result(ret, ReadTable_Result_macro);
eof:
	return Result(ret, ReadTable_Result_eof);
}

_g int readtable_novalue(Execute ptr, int *ret, addr *token, addr stream, addr table)
{
	enum ReadTable_Result value;
	int check;
	addr pos;
	unicode u;

	/* read */
	clear_readinfo(ptr);
	Return(readtable_result_(ptr, &pos, stream, table, &value));
	switch (value) {
		case ReadTable_Result_normal:
			*token = pos;
			return Result(ret, 0);

		case ReadTable_Result_eof:
			return Result(ret, 1);

		default:
			break;
	}

	/* macro execute */
	Return(read_char_stream_(stream, &u, &check));
	if (check)
		return fmte_("eof error", NULL);

	Return(macro_character_execute(ptr, &check, &pos, u, stream, table));
	if (check) {
		*token = pos;
		return Result(ret, 0);
	}
	else {
		/* return no value */
		return Result(ret, -1);
	}
}

static int readtable_front(Execute ptr,
		int *result, addr *ret, addr stream, addr table)
{
	int check;

	for (;;) {
		Return(readtable_novalue(ptr, &check, ret, stream, table));
		if (0 <= check) {
			break;
		}
	}

	/* eof */
	if (check) {
		*result = 1;
		return 0;
	}

	/* suppress */
	Return(read_suppress_p_(ptr, &check));
	if (check)
		*ret = Nil;

	/* normal */
	return Result(result, 0);
}


/*
 *  read
 */
_g int read_call(Execute ptr, addr stream, int *result, addr *ret)
{
	addr table;
	Return(getreadtable_(ptr, &table));
	return readtable_front(ptr, result, ret, stream, table);
}

static int read_stream_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	pushreadinfo(ptr, &info);
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

_g int read_stream(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_stream_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int read_preserving_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	pushreadinfo(ptr, &info);
	ReadInfoStruct(info)->preserving = 1;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_preserving_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int read_recursive_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)read_recursive_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int read_from_string(Execute ptr, int *result, addr *ret, addr pos)
{
	addr stream;
	LocalHold hold;

	Return(open_input_string_stream_(&stream, pos));
	hold = LocalHold_local_push(ptr, stream);
	Return(read_stream(ptr, stream, result, ret));
	localhold_end(hold);
	close_input_string_stream(stream);

	return 0;
}

_g int readstring_debug(addr *ret, const char *code)
{
	int result;
	addr stream;

	open_input_char_stream(&stream, code);
	if (read_stream(Execute_Thread, stream, &result, ret))
		Abort("Cannot catch a system signal.");
	close_input_string_stream(stream);

	return result;
}

_g addr readr_debug(const char *code)
{
	addr pos;
	if (readstring_debug(&pos, code))
		return Nil;
	else
		return pos;
}


/*****************************************************************************
 *  initialize
 *****************************************************************************/
static int build_reader_special_(void)
{
	addr pos, symbol;

	Return(readtable_heap_(&pos));
	GetConst(SPECIAL_READTABLE, &symbol);
	SetValueSymbol(symbol, pos);

	return 0;
}

_g void build_reader(void)
{
	build_reader_dispatch();
	Error(build_reader_special_());
}

_g void init_reader(void)
{
	init_reader_dispatch();
	init_reader_function();
	init_reader_token();
}

