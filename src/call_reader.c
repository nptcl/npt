#include "build.h"
#include "character.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "condition.h"
#include "constant.h"
#include "integer.h"
#include "print_dispatch.h"
#include "reader.h"
#include "reader_function.h"
#include "reader_table.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"
#include "strtype.h"

/*
 *  copy-readtable
 */
_g int copy_readtable_common(Execute ptr, addr from, addr to, addr *ret)
{
	int check1, check2;

	/* argument */
	if (from == Unbound) {
		GetConst(SPECIAL_READTABLE, &from);
		getspecialcheck_local(ptr, from, &from);
	}
	if (to == Unbound) {
		to = Nil;
	}

	/* make, copy */
	check1 = (from == Nil);
	check2 = (to == Nil);
	if (check1 && check2) {
		readtable_heap(&from);
		return Result(ret, from);
	}
	else if (check1) {
		copy_default_readtable(to);
		return Result(ret, to);
	}
	else if (check2) {
		copy_readtable_heap(from, &from);
		return Result(ret, from);
	}
	else {
		copy_readtable(from, to);
		return Result(ret, to);
	}
}


/*
 *  make-dispatch-macro-character
 */
_g int make_dispatch_macro_character_common(Execute ptr,
		addr code, addr nonterm, addr readtable)
{
	if (nonterm == Unbound) {
		nonterm = Nil;
	}
	if (readtable == Nil) {
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}
	make_dispatch_macro_character(readtable, code, nonterm != Nil);

	return 0;
}


/*
 *  read
 */
_g int read_common(Execute ptr, addr args, addr *ret)
{
	int check;
	addr stream, error, eof, recp;

	/* stream */
	if (consp(args)) {
		GetCons(args, &stream, &args);
	}
	else {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	/* errorp */
	if (consp(args)) {
		GetCons(args, &error, &args);
	}
	else
		error = Nil;
	/* eof */
	if (consp(args)) {
		GetCons(args, &eof, &args);
	}
	else
		eof = Nil;
	/* recp */
	if (consp(args)) {
		GetCons(args, &recp, &args);
	}
	else
		recp = Nil;

	/* read */
	if (recp == Nil) {
		Return(read_stream(ptr, stream, &check, &args));
		if (check) {
			if (error != Nil)
				return call_end_of_file_(ptr, stream);
			args = eof;
		}
	}
	else {
		Return(read_recursive(ptr, stream, &check, &args));
		if (check) {
			if (error != Nil)
				return fmte_("End-of-file occured by recursive-p read.", NULL);
			args = eof;
		}
	}
	*ret = args;

	return 0;
}


/*
 *  read-preserving-whitespace
 */
_g int read_preserving_whitespace_common(Execute ptr, addr args, addr *ret)
{
	int check;
	addr stream, error, eof, recp;

	/* stream */
	if (consp(args)) {
		GetCons(args, &stream, &args);
	}
	else {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	/* errorp */
	if (consp(args)) {
		GetCons(args, &error, &args);
	}
	else
		error = Nil;
	/* eof */
	if (consp(args)) {
		GetCons(args, &eof, &args);
	}
	else
		eof = Nil;
	/* recp */
	if (consp(args)) {
		GetCons(args, &recp, &args);
	}
	else
		recp = Nil;

	/* read */
	if (recp == Nil) {
		Return(read_preserving(ptr, stream, &check, &args));
		if (check) {
			if (error != Nil)
				return call_end_of_file_(ptr, stream);
			args = eof;
		}
	}
	else {
		Return(read_recursive(ptr, stream, &check, &args));
		if (check) {
			if (error != Nil)
				return fmte_("End-of-file occured by recursive-p read.", NULL);
			args = eof;
		}
	}
	*ret = args;

	return 0;
}


/*
 *  read-delimited-list
 */
_g int read_delimited_list_common(Execute ptr, addr code, addr stream, addr recp)
{
	unicode c;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	if (recp == Unbound) {
		recp = Nil;
	}

	GetCharacter(code, &c);
	return read_delimited_list(ptr, stream, c, recp != Nil);
}


/*
 *  read-from-string
 */
static int read_from_string_execute_common(Execute ptr, addr string,
		int eofp, addr eof, size_t start, size_t end, int preserve,
		addr *ret, addr *sec)
{
	int check;
	addr stream, pos;
	size_t size;

	open_input_string_stream2(&stream, string, start, end);
	if (preserve) {
		Return(read_preserving(ptr, stream, &check, &pos));
	}
	else {
		Return(read_stream(ptr, stream, &check, &pos));
	}
	if (check) {
		if (eofp)
			end_of_file(stream);
		else
			pos = eof;
	}
	getindex_input_stream(stream, &size);
	close_stream(stream);

	/* result */
	*ret = pos;
	make_index_integer_heap(sec, size);

	return 0;
}

_g int read_from_string_common(Execute ptr, addr args, addr *ret, addr *sec)
{
	int eofp, preserve;
	addr str, eof, pos, key;
	size_t start, end;

	/* string */
	if (! consp(args))
		goto error;
	GetCons(args, &str, &args);
	if (! stringp(str))
		return fmte_("The read-from-string argument ~S must be a string.", str, NULL);
	if (args == Nil)
		goto default_string;
	if (! consp(args))
		goto error;
	/* eof-error-p */
	GetCons(args, &pos, &args);
	eofp = (pos != Nil);
	if (args == Nil)
		goto default_eofp;
	if (! consp(args))
		goto error;
	/* eof-value */
	GetCons(args, &eof, &args);
	if (args == Nil)
		goto default_eof;
	if (! consp(args))
		goto error;
	/* key start */
	GetConst(KEYWORD_START, &key);
	if (getplist(args, key, &pos)) {
		start = 0;
	}
	else {
		if (GetIndex_integer(pos, &start))
			return fmte_("Too large start value ~S.", pos, NULL);
	}
	/* key end */
	GetConst(KEYWORD_END, &key);
	if (getplist(args, key, &pos)) {
		string_length(str, &end);
	}
	else {
		if (GetIndex_integer(pos, &end))
			return fmte_("Too large end value ~S.", pos, NULL);
	}
	/* key preserving-whitespace */
	GetConst(KEYWORD_PRESERVING_WHITESPACE, &key);
	if (getplist(args, key, &pos))
		preserve = 0;
	else
		preserve = (pos != Nil);
	/* execute */
	goto execute;

default_string:
	eofp = 1;
default_eofp:
	eof = Nil;
default_eof:
	start = 0;
	string_length(str, &end);
	preserve = 0;
execute:
	return read_from_string_execute_common(ptr,
			str, eofp, eof, start, end, preserve, ret, sec);

error:
	return fmte_("Invalid read-from-string argument.", NULL);
}


/*
 *  readtable-case
 */
_g int readtable_case_common(addr var, addr *ret)
{
	constindex index;

	switch (getcase_readtable(var)) {
		case ReadTable_upcase:
			index = CONSTANT_KEYWORD_UPCASE;
			break;

		case ReadTable_downcase:
			index = CONSTANT_KEYWORD_DOWNCASE;
			break;

		case ReadTable_preserve:
			index = CONSTANT_KEYWORD_PRESERVE;
			break;

		case ReadTable_invert:
			index = CONSTANT_KEYWORD_INVERT;
			break;

		default:
			return fmte_("Invalid case type.", NULL);
	}
	GetConstant(index, ret);

	return 0;
}


/*
 *  (setf readtable-case)
 */
_g int setf_readtable_case_common(addr value, addr var)
{
	addr key;

	GetConst(KEYWORD_UPCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_upcase);
		return 0;
	}

	GetConst(KEYWORD_DOWNCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_downcase);
		return 0;
	}

	GetConst(KEYWORD_PRESERVE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_preserve);
		return 0;
	}

	GetConst(KEYWORD_INVERT, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_invert);
		return 0;
	}

	/* error */
	return fmte_("Invalid case sensitivity mode ~S.", value, NULL);
}


/*
 *  get-dispatch-macro-character
 */
_g int get_dispatch_macro_character_common(Execute ptr,
		addr x, addr y, addr readtable, addr *ret)
{
	unicode a, b;

	if (readtable == Nil) {
		/* standard readtable */
		get_default_dispatch_macro(x, y, ret);
		return 0;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}
	GetCharacter(x, &a);
	GetCharacter(y, &b);
	get_dispatch_macro_character(readtable, a, b, ret);
	return 0;
}


/*
 *  set-dispatch-macro-character
 */
_g int set_dispatch_macro_character_common(Execute ptr,
		addr x, addr y, addr call, addr readtable)
{
	unicode a, b;

	if (readtable == Nil) {
		/* standard readtable */
		return fmte_("set-dispatch-macro-character "
				"don't update a standard readtable.", NULL);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}
	GetCharacter(x, &a);
	GetCharacter(y, &b);
	if (call == Nil)
		rem_dispatch_macro_character(readtable, a, b);
	else {
		if (symbolp(call))
			getspecialcheck_local(ptr, call, &call);
		set_dispatch_macro_character(readtable, a, b, call);
	}

	return 0;
}


/*
 *  get-macro-character
 */
_g int get_macro_character_common(Execute ptr,
		addr code, addr readtable, addr *ret, addr *sec)
{
	int check;
	unicode c;

	GetCharacter(code, &c);
	if (readtable == Nil) {
		/* standard readtable */
		get_default_macro_character(c, ret, &check);
		*sec = check? T: Nil;
		return 0;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}

	get_macro_character(readtable, c, ret, &check);
	*sec = check? T: Nil;
	return 0;
}


/*
 *  set-macro-character
 */
_g int set_macro_character_common(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable)
{
	unicode c;

	if (nonterm == Unbound) {
		nonterm = Nil;
	}
	if (readtable == Nil) {
		/* standard readtable */
		return fmte_("set-macro-character don't update a standard readtable.", NULL);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}

	GetCharacter(code, &c);
	if (symbolp(call))
		getspecialcheck_local(ptr, call, &call);
	set_macro_character(readtable, c, nonterm != Nil, call);

	return 0;
}


/*
 *  set-syntax-from-char
 */
_g int set_syntax_from_char_common(Execute ptr, addr x, addr y, addr z, addr w)
{
	unicode a, b;

	if (z == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &z);
		getspecialcheck_local(ptr, z, &z);
	}
	if (w == Unbound) {
		w = Nil;
	}

	GetCharacter(x, &a);
	GetCharacter(y, &b);
	if (w == Nil)
		set_syntax_from_default(a, b, z);
	else
		set_syntax_from_char(a, b, z, w);

	return 0;
}


/*
 *  with-standard-io-syntax
 */
_g int with_standard_io_syntax_common(addr form, addr env, addr *ret)
{
	addr args, symbol, value;

	args = Nil;
	/* (*package* [common-lisp-user]) */
	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_COMMON_LISP_USER, &value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-array* t) */
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-base* 10) */
	GetConst(SPECIAL_PRINT_BASE, &symbol);
	fixnum_heap(&value, 10);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-case* :upcase) */
	GetConst(SPECIAL_PRINT_CASE, &symbol);
	GetConst(KEYWORD_UPCASE, &value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-circle* nil) */
	GetConst(SPECIAL_PRINT_CIRCLE, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-escape* t) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-gensym* t) */
	GetConst(SPECIAL_PRINT_GENSYM, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*print-length* nil) */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-level* nil) */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-lines* nil) */
	GetConst(SPECIAL_PRINT_LINES, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-miser-width* nil) */
	GetConst(SPECIAL_PRINT_MISER_WIDTH, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-pprint-dispatch* [standard-pprint]) */
	GetConst(SPECIAL_PRINT_MISER_WIDTH, &symbol);
	pprint_dispatch_heap(&value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*print-pretty* nil) */
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-radix* nil) */
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-readably* nil) */
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*print-right-margin* nil) */
	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*read-base* 10) */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, 10);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*read-default-float-format* 'single-float) */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_SINGLE_FLOAT, &value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* (*read-eval* t) */
	GetConst(SPECIAL_READ_EVAL, &symbol);
	list_heap(&value, symbol, T, NULL);
	cons_heap(&args, value, args);

	/* (*read-suppress* nil) */
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	list_heap(&value, symbol, Nil, NULL);
	cons_heap(&args, value, args);

	/* (*readtable* [standard-readtable]) */
	GetConst(SPECIAL_READTABLE, &symbol);
	readtable_heap(&value);
	list_heap(&value, symbol, value, NULL);
	cons_heap(&args, value, args);

	/* `(let ,args ,@form) */
	nreverse_list_unsafe(&args, args);
	GetConst(COMMON_LET, &symbol);
	Return_getcdr(form, &form);
	lista_heap(ret, symbol, args, form, NULL);

	return 0;
}
