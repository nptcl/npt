#include "array_make.h"
#include "bigcons.h"
#include "bignum.h"
#include "bit.h"
#include "charqueue.h"
#include "character.h"
#include "character_name.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval.h"
#include "gc.h"
#include "hashtable.h"
#include "integer.h"
#include "pathname.h"
#include "quote.h"
#include "rational.h"
#include "reader.h"
#include "reader_info.h"
#include "reader_label.h"
#include "reader_table.h"
#include "reader_token.h"
#include "stream.h"
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
	int escape;
	unicode c;
	addr queue;
	LocalStack stack;

	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	escape = 0;
	for (;;) {
		if (read_char_stream(stream, &c))
			fmte("The string token must terminate by \".", NULL);
		if (escape) {
			push_charqueue_local(local, queue, c);
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
			push_charqueue_local(local, queue, c);
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
		fmte("After character ' must be an object, but EOF.", NULL);

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
	int mode, check;
	unicode c;
	addr table, pos, root, dotsym, queue;
	LocalHold hold;

	mode = 0;
	GetConst(SYSTEM_READTABLE_DOT, &dotsym);
	getreadtable(ptr, &table);
	queue_heap(&queue);
	hold = LocalHold_local_push(ptr, queue);
	for (;;) {
		if (read_char_stream(stream, &c))
			fmte("Don't allow end-of-file in the parensis.", NULL);
		if (readtable_typetable(table, c) == ReadTable_Type_whitespace) {
			/* discard character */
			continue;
		}
		if (c == limit) {
			/* discard limit character */
			break;
		}
		unread_char_stream(stream, c);
		if (readtable_novalue(ptr, &check, &pos, stream, table))
			return 1;
		if (0 < check) {
			fmte("read error", NULL);
			return 1;
		}
		if (check < 0)
			continue;

		/* dot */
		check = read_delimited_read(ptr, queue, dotsym, pos, &mode);
		if (check == 1) {
			fmte("dot no allowed here", NULL);
			return 1;
		}
		if (check == 2)
			continue;
		pushqueue_readlabel(ptr, queue, pos);
	}
	localhold_end(hold);

	if (mode == 1) { /* (a b . ) */
		fmte("dot no allowed here", NULL);
		return 1;
	}

	rootqueue(queue, &root);
	setresult_control(ptr, root);

	return 0;
}

_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive)
{
	addr control, info;
	struct readinfo_struct *str;

	/* push */
	push_return_control(ptr, &control);
	/* code */
	if (recursive)
		pushreadinfo_recursive(ptr, &info);
	else
		pushreadinfo(ptr, &info);
	str = ReadInfoStruct(info);
	str->dot = 1;
	Return(read_delimited_execute(ptr, stream, limit));
	return free_control_(ptr, control);
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
	fmte("unmatch close parenthiesis ).", NULL);
	return 0;
}


/*
 *  reader ;
 */
_g void semicolon_reader(addr stream)
{
	int check;
	unicode c;

	for (;;) {
		check = read_char_stream(stream, &c);
		if (check || c == '\n')
			break;
	}
}


/*
 *  reader `
 */
static int backquote_read_reader(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	ReadInfoStruct(info)->backquote++;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int backquote_reader(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(backquote_read_reader(ptr, stream, &check, &pos));
	if (check)
		fmte("After backquote ` must be an object.", NULL);
	quote_back_heap(ret, pos);

	return 0;
}


/*
 *  reader ,
 */
static int comma_read_reader(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	ReadInfoStruct(info)->backquote--;
	Return(read_call(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
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
		fmte("The comma , is not inside backquote.", NULL);

	/* read */
	if (read_char_stream(stream, &c))
		fmte("After comma , must be a character or an object.", NULL);
	if (c == '@') {
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			fmte("After ,@ must be an object.", NULL);
		quote_atsign_heap(&pos, pos);
	}
	else if (c == '.') {
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			fmte("After ,. must be an object.", NULL);
		quote_dot_heap(&pos, pos);
	}
	else {
		unread_char_stream(stream, c);
		Return(comma_read_reader(ptr, stream, &check, &pos));
		if (check)
			fmte("After comma , must be an object.", NULL);
		quote_comma_heap(&pos, pos);
	}

	return Result(ret, pos);
}


/*
 *  reader #
 */
static int sharp_parameter_reader(LocalRoot local, addr stream, addr *ret, unicode *rc)
{
	unicode c;
	addr cons;
	LocalStack stack;

	/* no digit */
	if (read_char_stream(stream, &c))
		return 1;
	if (! isDigitCase(c)) {
		*ret = Nil;
		*rc = c;
		return 0;
	}

	/* parse digit */
	push_local(local, &stack);
	bigcons_local(local, &cons);
	for (;;) {
		push_bigcons(local, cons, 10, (unsigned)(c - '0'));
		if (read_char_stream(stream, &c))
			return 1;
		if (! isDigitCase(c)) {
			*rc = c;
			break;
		}
	}
	integer_cons_alloc(NULL, ret, signplus_bignum, cons);
	rollback_local(local, stack);

	return 0;
}

_g int sharp_reader(Execute ptr, addr stream, addr code)
{
	addr arg, pos, code2;
	unicode x, y;

	/* #[integer][code] */
	if (sharp_parameter_reader(ptr->local, stream, &arg, &y)) {
		fmte("Invalid dispatch character form ~S.", code, NULL);
		return 0;
	}
	character_heap(&code2, y);
	y = toUpperUnicode(y);

	/* macro character */
	getreadtable(ptr, &pos);
	GetDispatchReadtable(pos, &pos);
	GetCharacter(code, &x);
	findvalue_character2_hashtable(pos, x, y, &pos);
	if (pos == Nil) {
		fmte("There is no macro character ~S-~S.", code, code2, NULL);
		return 0;
	}

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
	fmte("don't allow ~S dispatch character.", code, NULL);
	return 0;
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

static int equal_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
{
	addr pos, value, control;
	struct readinfo_struct *str;
	LocalHold hold;

	/* readinfo */
	getreadinfo(ptr, &pos);
	str = ReadInfoStruct(pos);
	value = str->replace? T: Nil;
	str->replace = 1;
	hold = LocalHold_array(ptr, 1);
	/* finalize */
	push_close_control(ptr, &control);
	cons_local(ptr->local, &pos, pos, value);
	setprotect_control_local(ptr, p_equal_finalize_dispatch, pos);
	/* code */
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int equal_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	int check;
	addr pos, label;

	pushlabel_readinfo(ptr, y, &label);
	Return(equal_read_dispatch(ptr, stream, &check, &pos));
	if (check)
		fmte("After dispatch character ~S must be an object.", x, NULL);
	closelabel_readlabel(ptr, label, pos);

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
		fmte("The #n# label ~S is not exist.", y, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #'
 */
_g int single_quote_dispatch(Execute ptr, addr stream, addr *ret)
{
	Return(quote_macro_reader(ptr, CONSTANT_COMMON_FUNCTION, stream, ret));
	if (stream == Unbound)
		fmte("After character #' must be a function-designer, but EOF.", NULL);

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
	nreverse_list_unsafe(&cons, cons);
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
	int check;
	unicode c;
	addr table, root, pos;
	LocalRoot local;
	LocalStack stack;
	size_t size, limit;

	/* parameter */
	if (y != Nil && GetIndex_integer(y, &limit))
		fmte("Too large dispatch parameter ~S.", y, NULL);

	/* read list */
	local = ptr->local;
	push_local(local, &stack);
	getreadtable(ptr, &table);
	root = Nil;
	size = 0;
	for (;;) {
		if (read_char_stream(stream, &c)) {
			fmte("Don't allow end-of-file in the parensis.", NULL);
		}
		if (readtable_typetable(table, c) == ReadTable_Type_whitespace) {
			continue;
		}
		if (c == ')') {
			break;
		}
		unread_char_stream(stream, c);
		Return(read_recursive(ptr, stream, &check, &pos));
		if (check)
			fmte("read error", NULL);
		cons_local(local, &root, pos, root);
		size++;

		/* size check */
		if (y != Nil && limit < size)
			fmte("Too many vector parameter.", NULL);
	}

	/* make vector */
	if (y == Nil) {
		parensis_open_normal_dispatch(root, size, &root);
	}
	else {
		if (root == Nil) {
			if (limit == 0)
				vector_heap(&root, 0);
			else
				fmte("The vector don't initialize because body is empty #n().", NULL);
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
	fmte("unmatch close parenthiesis ).", NULL);
	return 0;
}


/*
 *  dispatch #*
 */
static void asterisk_bitcons_dispatch(Execute ptr, addr stream, addr code, addr *ret)
{
	unicode c;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		if (read_char_stream(stream, &c))
			break;
		if (c == '0') {
			push_bitcons(local, cons, 0);
		}
		else if (c == '1') {
			push_bitcons(local, cons, 1);
		}
		else {
			unread_char_stream(stream, c);
			break;
		}
	}
	bitmemory_cons_heap(&pos, cons);
	rollback_local(local, stack);
	*ret = pos;
}

static void asterisk_size_dispatch(Execute ptr, addr stream, size_t size, addr *ret)
{
	int last;
	unicode c;
	addr pos;
	size_t i;

	/* read bit-vector */
	bitmemory_heap(&pos, size);
	last = 0;
	for (i = 0; ; i++) {
		if (size <= i)
			fmte("Too large bit-vector.", NULL);
		if (read_char_stream(stream, &c))
			break;
		if (c == '0') {
			bitmemory_setint(pos, i, 0);
			last = 0;
		}
		else if (c == '1') {
			bitmemory_setint(pos, i, 1);
			last = 1;
		}
		else {
			unread_char_stream(stream, c);
			break;
		}
	}

	/* fill last value */
	for (; i < size; i++)
		bitmemory_setint(pos, i, last);

	/* result */
	*ret = pos;
}

_g void asterisk_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret)
{
	size_t size;

	if (y == Nil) {
		asterisk_bitcons_dispatch(ptr, stream, x, ret);
	}
	else {
		if (GetIndex_integer(y, &size))
			fmte("The index size ~S is too large.", y, NULL);
		asterisk_size_dispatch(ptr, stream, size, ret);
	}
}


/*
 *  dispatch #:
 */
static void colon_object_dispatch(Execute ptr, addr stream, addr *ret)
{
	addr table;

	getreadtable(ptr, &table);
	setstate_readinfo(ptr, ReadInfo_State_Gensym);
	switch (readtable_result(ptr, ret, stream, table)) {
		case ReadTable_Result_normal:
			break;

		case ReadTable_Result_eof:
			fmte("After character #: must be an object, but EOF.", NULL);
			break;

		case ReadTable_Result_macro:
			fmte("After character #: don't allow a macro character.", NULL);
			break;

		default:
			fmte("Invalid result.", NULL);
			break;
	}
}

_g int colon_dispatch(Execute ptr, addr stream, addr *ret)
{
	addr control, pos;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	pushreadinfo_recursive(ptr, &pos);
	colon_object_dispatch(ptr, stream, &pos);
	/* free */
	Return(free_control_(ptr, control));
	/* result */
	return Result(ret, pos);
}


/*
 *  dispatch #<
 */
_g int less_dispatch(void)
{
	fmte("Cannot read #< dispatch character.", NULL);
	return 0;
}


/*
 *  dispatch #\
 */
_g int backslash_dispatch(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr table, pos;

	getreadtable(ptr, &table);
	unread_char_stream(stream, '\\');
	Return(read_recursive(ptr, stream, &check, &pos));
	if (check)
		fmte("Cannot read character name.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return 0;
	}
	if (! symbolp(pos))
		fmte("Invalid character type ~S.", pos, NULL);
	if (! find_name_char(&pos, pos))
		fmte("The character name ~S is not found.", pos, NULL);

	return Result(ret, pos);
}


/*
 *  dispatch #|
 */
_g void or_dispatch(addr stream)
{
	unicode u;
	size_t count;

	count = 0;
	for (;;) {
		if (read_char_stream(stream, &u))
			break;
		if (u == '#') {
			if (read_char_stream(stream, &u))
				break;
			if (u == '|')
				count++;
		}
		else if (u == '|') {
			if (read_char_stream(stream, &u))
				break;
			if (u == '#') {
				if (count == 0)
					break;
				count--;
			}
		}
	}
}


/*
 *  dispatch #+
 */
static int feature_read_dispatch(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, symbol, keyword;
	LocalHold hold;

	/* (let ((*package* (find-package "KEYWORD")))
	 *   (read))
	 */
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_KEYWORD, &keyword);
	pushspecial_control(ptr, symbol, keyword);
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int feature_eq_dispatch(addr pos, constindex index1, constindex index2)
{
	addr check;

	GetConstant(index1, &check);
	if (check == pos) return 1;
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

static int feature_check_dispatch(addr list, addr pos);
static int feature_cons_dispatch(addr list, addr cons)
{
	addr car, cdr;

	getcons(cons, &car, &cdr);
	/* not */
	if (feature_not_dispatch(car)) {
		if (! singlep(cdr))
			fmte("The feature ~S must be a (not x) form.", cons, NULL);
		GetCar(cdr, &car);
		return ! feature_check_dispatch(list, car);
	}
	/* and */
	if (feature_and_dispatch(car)) {
		while (cdr != Nil) {
			getcons(cdr, &car, &cdr);
			if (! feature_check_dispatch(list, car)) return 0;
		}
		return 1;
	}
	/* or */
	if (feature_or_dispatch(car)) {
		while (cdr != Nil) {
			getcons(cdr, &car, &cdr);
			if (feature_check_dispatch(list, car)) return 1;
		}
		return 0;
	}
	/* error */
	fmte("Invalid feature operator ~S.", car, NULL);
	return 0;
}

static int feature_check_dispatch(addr list, addr pos)
{
	if (symbolp(pos)) {
		return find_list_eq_safe(pos, list);
	}
	else if (consp(pos)) {
		return feature_cons_dispatch(list, pos);
	}
	else {
		fmte("Invalid feature ~S.", pos, NULL);
	}

	return 0;
}

static int feature_ignore_dispatch(Execute ptr, addr stream, int *result)
{
	/* (let ((*read-suppress* t)) ...) */
	addr control, symbol;

	push_close_control(ptr, &control);
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	pushspecial_control(ptr, symbol, T);
	Return(read_recursive(ptr, stream, result, &stream));
	return free_control_(ptr, control);
}

_g int plus_dispatch(Execute ptr, addr stream)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	Return(feature_read_dispatch(ptr, stream, &check, &feature));
	if (check)
		fmte("After dispatch #+ must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	getspecialcheck_local(ptr, list, &list);
	hold = LocalHold_local_push(ptr, feature);
	if (feature_check_dispatch(list, feature)) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			fmte("After dispatch #+feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			fmte("After dispatch #+feature must be a object.", NULL);
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
		fmte("After dispatch #- must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	getspecialcheck_local(ptr, list, &list);
	hold = LocalHold_local_push(ptr, feature);
	if (! feature_check_dispatch(list, feature)) {
		Return(read_recursive(ptr, stream, &check, &form));
		localhold_end(hold);
		if (check)
			fmte("After dispatch #-feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		Return(feature_ignore_dispatch(ptr, stream, &check));
		localhold_end(hold);
		if (check)
			fmte("After dispatch #-feature must be a object.", NULL);
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
	getspecialcheck_local(ptr, eval, &eval);
	if (eval == Nil)
		fmte("The dispatch #. don't read when *read-eval* is nil.", NULL);
	Return(read_recursive(ptr, stream, &check, &eval));
	if (check)
		fmte("After dispatch #. must be a object.", NULL);
	if (read_suppress_p(ptr))
		return Result(ret, Nil);

	hold = LocalHold_local_push(ptr, eval);
	Return(eval_object(ptr, eval, &eval));
	localhold_end(hold);

	return Result(ret, eval);
}


/*
 *  dispatch #R
 */
static int radix_execute_dispatch(Execute ptr, addr stream, fixnum base,
		int *result, addr *ret)
{
	addr control, symbol, value;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, base);
	pushspecial_control(ptr, symbol, value);
	Return(read_recursive(ptr, stream, result, ret));
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int radix_read_dispatch(Execute ptr, addr stream, fixnum base, addr *ret)
{
	int check;
	addr pos;

	Return(radix_execute_dispatch(ptr, stream, base, &check, &pos));
	if (check)
		fmte("After radix dispatch #<n>r must be an integer.", NULL);
	if (read_suppress_p(ptr))
		return Result(ret, Nil);
	if (! rationalp(pos))
		fmte("The radix value ~S must be an integer.", pos, NULL);

	return Result(ret, pos);
}

_g int radix_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	fixnum value;

	GetFixnum_signed(y, &value);
	if (! isBaseChar(value)) {
		if (! read_suppress_p(ptr))
			fmte("The radix ~S must be a number between 2 and 36.", y, NULL);
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
		fmte("After complex dispatch must be a (real imag) form.", NULL);
	if (read_suppress_p(ptr))
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
	complex_heap(&pos, real, imag);
	return Result(ret, pos);

error:
	fmte("The complex dispatch ~S must be a (real imag) form.", form, NULL);
	return Result(ret, Nil);
}


/*
 *  dispatch #A
 */
_g int array_dispatch(Execute ptr, addr stream, addr y, addr *ret)
{
	int check, ignore;
	addr form;

	ignore = read_suppress_p(ptr);
	if (y == Nil && (! ignore))
		fmte("There is no rank parameter at the #<n>a dispatch.", NULL);
	Return(read_recursive(ptr, stream, &check, &form));
	if (ignore)
		return Result(ret, Nil);
	if (check)
		fmte("After array dispatch must be an initial-contents form.", NULL);
	array_contents_heap(&form, y, form);
	array_readlabel(ptr, form);

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
		fmte("After #P must be a pathname-designer.", NULL);
	if (read_suppress_p(ptr))
		return Result(ret, Nil);
	pathname_designer_heap(ptr, pos, &pos);

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
	if (read_suppress_p(ptr))
		return Result(ret, Nil);
	if (! consp(pos))
		goto error;
	GetCons(pos, &pos, &rest);

	hold = LocalHold_local_push(ptr, pos);
	Return(structure_constructor_common(ptr, pos, rest, &pos));
	localhold_end(hold);
	return Result(ret, pos);

error:
	fmte("After #S must be (name key value ...) form.", NULL);
	return Result(ret, Nil);
}


/*****************************************************************************
 *  initialize
 *****************************************************************************/
_g void init_reader_function(void)
{
	SetPointerType(empty, equal_finalize_dispatch);
}

