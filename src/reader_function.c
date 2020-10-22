#include "array_make.h"
#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_object.h"
#include "bit.h"
#include "character_queue.h"
#include "character.h"
#include "character_name.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "gc.h"
#include "hashtable.h"
#include "integer.h"
#include "pathname.h"
#include "quote.h"
#include "rational.h"
#include "reader.h"
#include "reader_function.h"
#include "reader_info.h"
#include "reader_label.h"
#include "reader_table.h"
#include "reader_token.h"
#include "real.h"
#include "stream.h"
#include "stream_function.h"
#include "structure.h"
#include "symbol.h"

/*****************************************************************************
 *  reader macro
 *****************************************************************************/
/*
 *  reader "
 */
_g int double_quote_reader(LocalRoot local, addr stream, addr *ret)
{
	int escape, check;
	unicode c;
	addr queue;
	LocalStack stack;

	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	escape = 0;
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return fmte_("The string token must terminate by \".", NULL);
		if (escape) {
			Return(push_charqueue_local_(local, queue, c));
			escape = 0;
			continue;
		}
		if (c == '\"') {
			break;
		}
		if (c == '\\') {
			escape = 1;
		}
		else {
			Return(push_charqueue_local_(local, queue, c));
		}
	}
	make_charqueue_heap(queue, ret);

	return 0;
}


/*
 *  reader '
 */
static int quote_macro_reader(Execute ptr, constindex index, addr stream, addr *ret)
{
	int check;
	addr pos, quote;

	/* readtable */
	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		return Result(ret, Unbound);

	/* ([index] pos) */
	GetConstant(index, &quote);
	list_heap(ret, quote, pos, NULL);

	return 0;
}

_g int single_quote_reader(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader(ptr, CONSTANT_COMMON_QUOTE, stream, ret));
	if (stream == Unbound)
		return fmte_("After character ' must be an object, but EOF.", NULL);

	return 0;
}


/*
 *  reader (
 */
static int read_delimited_read(Execute ptr, addr queue, addr dot, addr pos, int *mode)
{
	addr root;

	switch (*mode) {
		case 0:
			if (dot == pos) {
				rootqueue(queue, &root);
				if (root == Nil) /* (. */
					return 1; /* error */
				*mode = 1; /* (a b . */
				return 2; /* continue */
			}
			return 0;

		case 1:
			if (dot == pos)  /* (a b . . */
				return 1;  /* error */
			dotqueue_readlabel(ptr, queue, pos);
			*mode = 2;  /* (a b . c */
			return 2; /* continue */

		default: /* (a b . c d */
			return 1;  /* error */
	}
}

static int read_delimited_execute(Execute ptr, addr stream, unicode limit)
{
	enum ReadTable_Type type;
	int mode, check;
	unicode c;
	addr table, pos, root, dotsym, queue;
	LocalHold hold;

	mode = 0;
	GetConst(SYSTEM_READTABLE_DOT, &dotsym);
	Return(getreadtable_(ptr, &table));
	queue_heap(&queue);
	hold = LocalHold_local_push(ptr, queue);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return call_end_of_file_(ptr, stream);
		Return(readtable_typetable_(table, c, &type));
		if (type == ReadTable_Type_whitespace) {
			/* discard character */
			continue;
		}
		if (c == limit) {
			/* discard limit character */
			break;
		}
		Return(unread_char_stream_(stream, c));
		Return(readtable_novalue(ptr, &check, &pos, stream, table));
		if (0 < check)
			return fmte_("read error", NULL);
		if (check < 0)
			continue;

		/* dot */
		check = read_delimited_read(ptr, queue, dotsym, pos, &mode);
		if (check == 1)
			return fmte_("dot no allowed here", NULL);
		if (check == 2)
			continue;
		pushqueue_readlabel(ptr, queue, pos);
	}
	localhold_end(hold);

	if (mode == 1) /* (a b . ) */
		return fmte_("dot no allowed here", NULL);

	rootqueue(queue, &root);
	setresult_control(ptr, root);

	return 0;
}

static int read_delimited_list_call_(Execute ptr, addr stream, unicode limit, int recp)
{
	addr info;
	struct readinfo_struct *str;

	/* push */
	/* code */
	if (recp) {
		Return(pushreadinfo_recursive_(ptr, &info));
	}
	else {
		pushreadinfo(ptr, &info);
	}
	str = ReadInfoStruct(info);
	str->dot = 1;
	return read_delimited_execute(ptr, stream, limit);
}

_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recp)
{
	addr control;

	push_control(ptr, &control);
	(void)read_delimited_list_call_(ptr, stream, limit, recp);
	return pop_control_(ptr, control);
}

_g int parensis_open_reader(Execute ptr, addr stream)
{
	return read_delimited_list(ptr, stream, ')', 1);
}


/*
 *  reader )
 */
_g int parensis_close_reader(void)
{
	return fmte_("unmatch close parenthiesis ).", NULL);
}


/*
 *  reader ;
 */
_g int semicolon_reader_(addr stream)
{
	int check;
	unicode c;

	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check || c == '\n')
			break;
	}

	return 0;
}


/*
 *  reader `
 */
static int backquote_read_reader_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	ReadInfoStruct(info)->backquote++;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int backquote_read_reader(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)backquote_read_reader_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int backquote_reader(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(backquote_read_reader(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After backquote ` must be an object.", NULL);

	return quote_back_heap_(ret, pos);
}


/*
 *  reader ,
 */
static int comma_read_reader_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr info;

	Return(pushreadinfo_recursive_(ptr, &info));
	ReadInfoStruct(info)->backquote--;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int comma_read_reader(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)comma_read_reader_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int comma_reader(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;
	unicode c;

	/* check */
	getreadinfo(ptr, &pos);
	if (ReadInfoStruct(pos)->backquote == 0)
		return fmte_("The comma , is not inside backquote.", NULL);

	/* read */
	Return(read_char_stream_(stream, &c, &check));
	if (check)
		return fmte_("After comma , must be a character or an object.", NULL);
	if (c == '@') {
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,@ must be an object.", NULL);
		quote_atsign_heap(&pos, pos);
	}
	else if (c == '.') {
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After ,. must be an object.", NULL);
		quote_dot_heap(&pos, pos);
	}
	else {
		Return(unread_char_stream_(stream, c));
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			return fmte_("After comma , must be an object.", NULL);
		quote_comma_heap(&pos, pos);
	}

	return Result(ret, pos);
}


/*
 *  reader #
 */
static int sharp_parameter_reader_(LocalRoot local, addr stream, addr *ret, unicode *rc)
{
	int check;
	unicode c;
	addr cons;
	LocalStack stack;

	/* no digit */
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		*rc = 0;
		return Result(ret, NULL);
	}
	if (! isDigitCase(c)) {
		*rc = c;
		return Result(ret, Nil);
	}

	/* parse digit */
	push_local(local, &stack);
	bigcons_local(local, &cons);
	for (;;) {
		push_bigcons(local, cons, 10, (unsigned)(c - '0'));
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			*rc = 0;
			return Result(ret, NULL);
		}
		if (! isDigitCase(c)) {
			*rc = c;
			break;
		}
	}
	integer_cons_heap(ret, signplus_bignum, cons);
	rollback_local(local, stack);

	return 0;
}

_g int sharp_reader(Execute ptr, addr stream, addr code)
{
	addr arg, pos, code2;
	unicode x, y;

	/* #[integer][code] */
	Return(sharp_parameter_reader_(ptr->local, stream, &arg, &y));
	if (arg == NULL)
		return fmte_("Invalid dispatch character form ~S.", code, NULL);
	character_heap(&code2, y);
	y = toUpperUnicode(y);

	/* macro character */
	Return(getreadtable_(ptr, &pos));
	GetDispatchReadtable(pos, &pos);
	GetCharacter(code, &x);
	Return(findnil_character2_hashtable_(pos, x, y, &pos));
	if (pos == Nil)
		return fmte_("There is no macro character ~S-~S.", code, code2, NULL);

	return funcall_control(ptr, pos, stream, code2, arg, NULL);
}


/*****************************************************************************
 *  dispatch macro
 *****************************************************************************/
/*
 *  dispatch # whitespace
 */
/* (defun error-dispatch (stream code arg) ...) -> * */
_g int error_dispatch(addr code)
{
	return fmte_("don't allow ~S dispatch character.", code, NULL);
}


/*
 *  dispatch #n=
 */
static int equal_finalize_dispatch(Execute ptr)
{
	addr pos, value;

	getdata_control(ptr, &pos);
	GetCons(pos, &pos, &value);
	ReadInfoStruct(pos)->replace = value != Nil? 1: 0;

	return 0;
}

static int equal_read_dispatch_call_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr pos, value;
	struct readinfo_struct *str;
	LocalHold hold;

	/* readinfo */
	getreadinfo(ptr, &pos);
	str = ReadInfoStruct(pos);
	value = str->replace? T: Nil;
	str->replace = 1;
	hold = LocalHold_array(ptr, 1);
	/* finalize */
	cons_local(ptr->local, &pos, pos, value);
	setprotect_control_local(ptr, p_equal_finalize_dispatch, pos);
	/* code */
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int equal_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)equal_read_dispatch_call_(ptr, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int equal_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	int check;
	addr pos, label;

	Return(pushlabel_readinfo_(ptr, y, &label));
	Return(equal_read_dispatch(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After dispatch character ~S must be an object.", x, NULL);
	Return(closelabel_readlabel_(ptr, label, pos));

	return Result(ret, pos);
}


/*
 *  dispatch #n#
 */
_g int sharp_dispatch(Execute ptr, addr y, addr *ret)
{
	addr pos;

	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Label, &pos);
	Check(! consp(pos), "type error");
	GetCar(pos, &pos);
	if (! find_readlabel(y, pos, &pos))
		return fmte_("The #n# label ~S is not exist.", y, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #'
 */
_g int single_quote_dispatch(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader(ptr, CONSTANT_COMMON_FUNCTION, stream, ret));
	if (stream == Unbound)
		return fmte_("After character #' must be a function-designer, but EOF.", NULL);

	return 0;
}


/*
 *  dispatch #(
 */
static void parensis_open_normal_dispatch(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i, index;

	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		GetCons(cons, &pos, &cons);
		setarray(vector, index, pos);
	}
	*ret = vector;
}

static void parensis_open_limit_dispatch(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i;

	vector_heap(&vector, size);
	nreverse(&cons, cons);
	pos = Nil;
	for (i = 0; i < size; i++) {
		if (cons != Nil)
			GetCons(cons, &pos, &cons);
		setarray(vector, i, pos);
	}
	*ret = vector;
}

_g int parensis_open_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	enum ReadTable_Type type;
	int check;
	unicode c;
	addr table, root, pos;
	LocalRoot local;
	LocalStack stack;
	size_t size, limit;

	/* parameter */
	if (y != Nil && GetIndex_integer(y, &limit))
		return fmte_("Too large dispatch parameter ~S.", y, NULL);

	/* read list */
	local = ptr->local;
	push_local(local, &stack);
	Return(getreadtable_(ptr, &table));
	root = Nil;
	size = 0;
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			return call_end_of_file_(ptr, stream);
		Return(readtable_typetable_(table, c, &type));
		if (type == ReadTable_Type_whitespace)
			continue;
		if (c == ')')
			break;
		Return(unread_char_stream_(stream, c));
		Return(read_recursive(ptr, stream, &check, &pos));
		if (check)
			return fmte_("read error", NULL);
		cons_local(local, &root, pos, root);
		size++;

		/* size check */
		if (y != Nil && limit < size)
			return fmte_("Too many vector parameter.", NULL);
	}

	/* make vector */
	if (y == Nil) {
		parensis_open_normal_dispatch(root, size, &root);
	}
	else {
		if (root == Nil) {
			if (limit == 0) {
				vector_heap(&root, 0);
			}
			else {
				return fmte_("The vector don't initialize "
						"because body is empty #n().", NULL);
			}
		}
		else {
			parensis_open_limit_dispatch(root, limit, &root);
		}
	}
	vector_readlabel(ptr, root);
	rollback_local(local, stack);

	return Result(ret, root);
}


/*
 *  dispatch #)
 */
_g int parensis_close_dispatch(void)
{
	return fmte_("unmatch close parenthiesis ).", NULL);
}


/*
 *  dispatch #*
 */
static int asterisk_bitcons_dispatch_(Execute ptr, addr stream, addr code, addr *ret)
{
	int check;
	unicode c;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		if (c == '0') {
			push_bitcons(local, cons, 0);
		}
		else if (c == '1') {
			push_bitcons(local, cons, 1);
		}
		else {
			Return(unread_char_stream_(stream, c));
			break;
		}
	}
	bitmemory_cons_heap(&pos, cons);
	rollback_local(local, stack);

	return Result(ret, pos);
}

static int asterisk_size_dispatch_(Execute ptr, addr stream, size_t size, addr *ret)
{
	int last, check;
	unicode c;
	addr pos;
	size_t i;

	/* read bit-vector */
	bitmemory_heap(&pos, size);
	last = 0;
	for (i = 0; ; i++) {
		if (size <= i)
			return fmte_("Too large bit-vector.", NULL);
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		if (c == '0') {
			Return(bitmemory_setint_(pos, i, 0));
			last = 0;
		}
		else if (c == '1') {
			Return(bitmemory_setint_(pos, i, 1));
			last = 1;
		}
		else {
			Return(unread_char_stream_(stream, c));
			break;
		}
	}

	/* fill last value */
	for (; i < size; i++) {
		Return(bitmemory_setint_(pos, i, last));
	}

	/* result */
	return Result(ret, pos);
}

_g int asterisk_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	size_t size;

	if (y == Nil) {
		Return(asterisk_bitcons_dispatch_(ptr, stream, x, ret));
	}
	else {
		if (GetIndex_integer(y, &size))
			return fmte_("The index size ~S is too large.", y, NULL);
		Return(asterisk_size_dispatch_(ptr, stream, size, ret));
	}

	return 0;
}


/*
 *  dispatch #:
 */
static int colon_object_dispatch_(Execute ptr, addr stream, addr *ret)
{
	enum ReadTable_Result value;
	addr table;

	Return(getreadtable_(ptr, &table));
	setstate_readinfo(ptr, ReadInfo_State_Gensym);
	Return(readtable_result_(ptr, ret, stream, table, &value));
	switch (value) {
		case ReadTable_Result_normal:
			break;

		case ReadTable_Result_eof:
			return fmte_("After character #: must be an object, but EOF.", NULL);

		case ReadTable_Result_macro:
			return fmte_("After character #: don't allow a macro character.", NULL);

		default:
			return fmte_("Invalid result.", NULL);
	}

	return 0;
}

static int colon_dispatch_call_(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	Return(pushreadinfo_recursive_(ptr, &pos));
	return colon_object_dispatch_(ptr, stream, ret);
}

_g int colon_dispatch(Execute ptr, addr stream, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)colon_dispatch_call_(ptr, stream, ret);
	return pop_control_(ptr, control);
}


/*
 *  dispatch #<
 */
_g int less_dispatch(void)
{
	return fmte_("Cannot read #< dispatch character.", NULL);
}


/*
 *  dispatch #\
 */
_g int backslash_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr table, pos;

	Return(getreadtable_(ptr, &table));
	Return(unread_char_stream_(stream, '\\'));
	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		return fmte_("Cannot read character name.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check) {
		setresult_control(ptr, Nil);
		return 0;
	}
	if (! symbolp(pos))
		return fmte_("Invalid character type ~S.", pos, NULL);
	Return(find_name_char_(&pos, pos));
	if (pos == Nil)
		return fmte_("The character name ~S is not found.", pos, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #|
 */
_g int or_dispatch_(addr stream)
{
	int check;
	unicode u;
	size_t count;

	count = 0;
	for (;;) {
		Return(read_char_stream_(stream, &u, &check));
		if (check)
			break;
		if (u == '#') {
			Return(read_char_stream_(stream, &u, &check));
			if (check)
				break;
			if (u == '|')
				count++;
		}
		else if (u == '|') {
			Return(read_char_stream_(stream, &u, &check));
			if (check)
				break;
			if (u == '#') {
				if (count == 0)
					break;
				count--;
			}
		}
	}

	return 0;
}


/*
 *  dispatch #+
 */
static int feature_read_dispatch_call_(Execute ptr, LocalHold hold,
		addr stream, int *result, addr *ret)
{
	addr symbol, keyword;

	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_KEYWORD, &keyword);
	pushspecial_control(ptr, symbol, keyword);
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int feature_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	/* (let ((*package* (find-package "KEYWORD")))
	 *   (read))
	 */
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	feature_read_dispatch_call_(ptr, hold, stream, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int feature_eq_dispatch(addr pos, constindex index1, constindex index2)
{
	addr check;

	GetConstant(index1, &check);
	if (check == pos)
		return 1;
	GetConstant(index2, &check);
	return check == pos;
}
static int feature_not_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_NOT, CONSTANT_COMMON_NOT);
}
static int feature_and_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_AND, CONSTANT_COMMON_AND);
}
static int feature_or_dispatch(addr pos)
{
	return feature_eq_dispatch(pos, CONSTANT_KEYWORD_OR, CONSTANT_COMMON_OR);
}

static int feature_check_dispatch_(addr list, addr pos, int *ret);
static int feature_cons_dispatch_(addr list, addr cons, int *ret)
{
	int check;
	addr car, cdr;

	Return_getcons(cons, &car, &cdr);
	/* not */
	if (feature_not_dispatch(car)) {
		if (! singlep(cdr))
			return fmte_("The feature ~S must be a (not x) form.", cons, NULL);
		GetCar(cdr, &car);
		Return(feature_check_dispatch_(list, car, &check));
		return Result(ret, ! check);
	}
	/* and */
	if (feature_and_dispatch(car)) {
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			Return(feature_check_dispatch_(list, car, &check));
			if (! check)
				return Result(ret, 0);
		}
		return Result(ret, 1);
	}
	/* or */
	if (feature_or_dispatch(car)) {
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			Return(feature_check_dispatch_(list, car, &check));
			if (check)
				return Result(ret, 1);
		}
		return Result(ret, 0);
	}
	/* error */
	return fmte_("Invalid feature operator ~S.", car, NULL);
}

static int feature_check_dispatch_(addr list, addr pos, int *ret)
{
	if (symbolp(pos))
		return find_list_eq_safe_(pos, list, ret);
	else if (consp(pos))
		return feature_cons_dispatch_(list, pos, ret);
	else
		return fmte_("Invalid feature ~S.", pos, NULL);
}

static int feature_ignore_dispatch(Execute ptr, addr stream, int *result)
{
	/* (let ((*read-suppress* t)) ...) */
	addr control, symbol;

	push_control(ptr, &control);
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	pushspecial_control(ptr, symbol, T);
	(void)read_recursive(ptr, stream, result, &stream);
	return pop_control_(ptr, control);
}

_g int plus_dispatch(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #+ must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (check) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #+feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #+feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}

	return 0;
}


/*
 *  dispatch #-
 */
_g int minus_dispatch(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch(ptr, stream, &check, &feature));
	if (check)
		return fmte_("After dispatch #- must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	hold = LocalHold_local_push(ptr, feature);
	Return(feature_check_dispatch_(list, feature, &check));
	if (! check) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #-feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			return fmte_("After dispatch #-feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}

	return 0;
}


/*
 *  dispatch #.
 */
_g int dot_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr eval;
	LocalHold hold;

	GetConst(SPECIAL_READ_EVAL, &eval);
	Return(getspecialcheck_local_(ptr, eval, &eval));
	if (eval == Nil)
		return fmte_("The dispatch #. don't read when *read-eval* is nil.", NULL);
	Return(read_recursive(ptr, stream, &check, &eval));
	if (check)
		return fmte_("After dispatch #. must be a object.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);

	hold = LocalHold_local_push(ptr, eval);
	Return(eval_object(ptr, eval, &eval));
	localhold_end(hold);

	return Result(ret, eval);
}


/*
 *  dispatch #R
 */
static int radix_execute_dispatch_call_(Execute ptr, LocalHold hold,
		addr stream, fixnum base, int *result, addr *ret)
{
	addr symbol, value;

	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, base);
	pushspecial_control(ptr, symbol, value);
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);

	return 0;
}

static int radix_execute_dispatch(Execute ptr, addr stream, fixnum base,
		int *result, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)radix_execute_dispatch_call_(ptr, hold, stream, base, result, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int radix_read_dispatch(Execute ptr, addr stream, fixnum base, addr *ret)
{
	int check;
	addr pos;

	Return(radix_execute_dispatch(ptr, stream, base, &check, &pos));
	if (check)
		return fmte_("After radix dispatch #<n>r must be an integer.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! rationalp(pos))
		return fmte_("The radix value ~S must be an integer.", pos, NULL);

	return Result(ret, pos);
}

_g int radix_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	int check;
	fixnum value;

	GetFixnum_signed(y, &value);
	if (! isBaseChar(value)) {
		Return(read_suppress_p_(ptr, &check));
		if (! check)
			return fmte_("The radix ~S must be a number between 2 and 36.", y, NULL);
	}

	return radix_read_dispatch(ptr, stream, value, ret);
}


/*
 *  dispatch #B
 */
_g int binary_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 2, ret);
}


/*
 *  dispatch #O
 */
_g int octal_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 8, ret);
}


/*
 *  dispatch #X
 */
_g int hexadecimal_dispatch(Execute ptr, addr stream, addr *ret)
{
	return radix_read_dispatch(ptr, stream, 16, ret);
}


/*
 *  dispatch #C
 */
_g int complex_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr form, pos, real, imag;

	Return(read_recursive(ptr, stream, &check, &form));
	if (check)
		return fmte_("After complex dispatch must be a (real imag) form.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	pos = form;
	if (! consp(pos))
		goto error;
	GetCons(pos, &real, &pos);
	if (! consp(pos))
		goto error;
	GetCons(pos, &imag, &pos);
	if (pos != Nil)
		goto error;
	if (! realp(real))
		goto error;
	if (! realp(imag))
		goto error;
	Return(complex_heap_(&pos, real, imag));
	return Result(ret, pos);

error:
	return fmte_("The complex dispatch ~S must be a (real imag) form.", form, NULL);
}


/*
 *  dispatch #A
 */
_g int array_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	int check, ignore;
	addr form;

	Return(read_suppress_p_(ptr, &ignore));
	if (y == Nil && (! ignore))
		return fmte_("There is no rank parameter at the #<n>a dispatch.", NULL);
	Return(read_recursive(ptr, stream, &check, &form));
	if (ignore)
		return Result(ret, Nil);
	if (check)
		return fmte_("After array dispatch must be an initial-contents form.", NULL);
	Return(array_contents_heap_(&form, y, form));
	Return(array_readlabel_(ptr, form));

	return Result(ret, form);
}


/*
 *  dispatch #P
 */
_g int pathname_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		return fmte_("After #P must be a pathname-designer.", NULL);
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	Return(pathname_designer_heap_(ptr, pos, &pos));

	return Result(ret, pos);
}


/*
 *  dispatch #S
 */
_g int structure_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos, rest;
	LocalHold hold;

	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		goto error;
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);
	if (! consp(pos))
		goto error;
	GetCons(pos, &pos, &rest);

	hold = LocalHold_local_push(ptr, pos);
	Return(structure_constructor_common(ptr, pos, rest, &pos));
	localhold_end(hold);
	return Result(ret, pos);

error:
	return fmte_("After #S must be (name key value ...) form.", NULL);
}


/*****************************************************************************
 *  initialize
 *****************************************************************************/
_g void init_reader_function(void)
{
	SetPointerType(empty, equal_finalize_dispatch);
}

