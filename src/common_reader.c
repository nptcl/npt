/*
 *  ANSI COMMON LISP: 23. Reader
 */
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "integer.h"
#include "print.h"
#include "print_dispatch.h"
#include "readtable.h"
#include "type_parse.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"

/* (defun copy-readtable (&optional from to) ...) -> readtable
 *   from  (or readtable null)  ;; readtable-designer
 *   to    (or readtable null)
 */
static void function_copy_readtable(Execute ptr, addr from, addr to)
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
		setresult_control(ptr, from);
	}
	else if (check1) {
		copy_default_readtable(to);
		setresult_control(ptr, to);
	}
	else if (check2) {
		copy_readtable_heap(from, &from);
		setresult_control(ptr, from);
	}
	else {
		copy_readtable(from, to);
		setresult_control(ptr, to);
	}
}

static void type_copy_readtable(addr *ret)
{
	addr arg, values;

	/* (function (readtable-designer readtable-designer)
	 *   (values readtable &rest nil))
	 */
	GetTypeTable(&arg, ReadtableDesigner);
	typeargs_opt2(&arg, arg, arg);
	GetTypeTable(&values, Readtable);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_copy_readtable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_READTABLE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt2(pos, p_defun_copy_readtable);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_readtable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-dispatch-macro-character
 *   (char &optional non-terminating-p readtable) ...) -> (eql t)
 *   char               character
 *   non-terminating-p  t  ;; boolean, default nil
 *   readtable          readtable   ;; default *readtable*
 */
static void function_make_dispatch_macro_character(Execute ptr,
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
	setresult_control(ptr, T);
}

static void type_make_dispatch_macro_character(addr *ret)
{
	addr arg, values, type1, type2;

	/* (function (character &optional t readtable) ...)
	 *    -> (values (eql t) &rest nil)
	 */
	GetTypeTable(&arg, Character);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Readtable);
	typeargs_var1opt2(&arg, arg, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_make_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read (&optional stream errorp eof recp) ...) -> object
 *   stream  stream
 *   errorp  t  (boolean)
 *   eof     t
 *   recp    t  (boolean)
 */
static void function_read(Execute ptr, addr args)
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
		if (read_stream(ptr, stream, &check, &args))
			return;
		if (check) {
			if (error != Nil)
				end_of_file(stream);
			args = eof;
		}
	}
	else {
		if (read_recursive(ptr, stream, &check, &args))
			return;
		if (check) {
			if (error != Nil)
				fmte("End-of-file occured by recursive-p read.", NULL);
			args = eof;
		}
	}
	setresult_control(ptr, args);
}

static void defun_read(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_read);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Read);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-preserving-whitespace (&optional stream errorp eof recp) ...) -> object
 *   stream  stream
 *   errorp  t  (boolean)
 *   eof     t
 *   recp    t  (boolean)
 */
static void function_read_preserving_whitespace(Execute ptr, addr args)
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
		if (read_preserving(ptr, stream, &check, &args))
			return;
		if (check) {
			if (error != Nil)
				end_of_file(stream);
			args = eof;
		}
	}
	else {
		if (read_recursive(ptr, stream, &check, &args))
			return;
		if (check) {
			if (error != Nil)
				fmte("End-of-file occured by recursive-p read.", NULL);
			args = eof;
		}
	}
	setresult_control(ptr, args);
}

static void defun_read_preserving_whitespace(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_PRESERVING_WHITESPACE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_read_preserving_whitespace);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Read);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defn read-delimited-list (character &optional stream recursive-p) ...) -> list */
static void function_read_delimited_list(Execute ptr,
		addr code, addr stream, addr recp)
{
	unicode u;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_INPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	if (recp == Unbound) {
		recp = Nil;
	}

	GetCharacter(code, &u);
	(void)read_delimited_list(ptr, stream, u, recp != Nil);
}

static void type_read_delimited_list(addr *ret)
{
	addr arg, values, type1, type2;

	/* (function (character &optional stream t) ...)
	 *    -> (values list &rest nil)
	 */
	GetTypeTable(&arg, Character);
	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, T);
	typeargs_var1opt2(&arg, arg, type1, type2);
	GetTypeTable(&values, List);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_read_delimited_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_DELIMITED_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_read_delimited_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_delimited_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-from-string
 *    (string
 *     &optional eof-error-p eof-value
 *     &key start end preserving-whitespace) ...)
 *    -> (values object position)
 *   string                 string
 *   eof-error-p            t  ;; boolean, default t
 *   eof-value              t  ;; default nil
 *   start                  (integer 0 *)  ;; default 0
 *   end                    (or (integer 0 *) null)  ;; default nil
 *   preserving-whitespace  t  ;; boolean, default nil
 *   object                 t
 *   position               (integer 0 *)
 */
static void read_from_string_execute(Execute ptr, addr string,
		int eofp, addr eof, size_t start, size_t end, int preserve)
{
	int result, check;
	addr stream, pos, index;
	size_t size;

	open_input_string_stream2(&stream, string, start, end);
	if (preserve)
		result = read_preserving(ptr, stream, &check, &pos);
	else
		result = read_stream(ptr, stream, &check, &pos);
	if (result)
		return;
	if (check) {
		if (eofp)
			end_of_file(stream);
		else
			pos = eof;
	}
	getindex_input_stream(stream, &size);
	close_stream(stream);

	make_index_integer_alloc(NULL, &index, size);
	setvalues_control(ptr, pos, index, NULL);
}

static void function_read_from_string(Execute ptr, addr args)
{
	int eofp, preserve;
	addr str, eof, pos, key;
	size_t start, end;

	/* string */
	if (! consp(args))
		goto error;
	GetCons(args, &str, &args);
	if (! stringp(str))
		fmte("The read-from-string argument ~S must be a string.", str, NULL);
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
			fmte("Too large start value ~S.", pos, NULL);
	}
	/* key end */
	GetConst(KEYWORD_END, &key);
	if (getplist(args, key, &pos)) {
		string_length(str, &end);
	}
	else {
		if (GetIndex_integer(pos, &end))
			fmte("Too large end value ~S.", pos, NULL);
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
	read_from_string_execute(ptr, str, eofp, eof, start, end, preserve);
	return;

error:
	fmte("Invalid read-from-string argument.", NULL);
}

static void type_read_from_string(addr *ret)
{
	/* (function (string &optional t t
	 *    &key (start keyword-start)
	 *         (end keyword-end)
	 *         (preserving-whitespace t))
	 *    (values t index &rest nil))
	 */
	addr str, type, type1, type2, size, var, opt, key, start, end, pre;
	addr arg, values;

	GetTypeTable(&str, String);
	GetTypeTable(&type, T);
	GetTypeTable(&size, Index);
	GetTypeTable(&type1, KeywordStart);
	GetTypeTable(&type2, KeywordEnd);
	conscar_heap(&var, str);
	list_heap(&opt, type, type, NULL);
	GetConst(KEYWORD_START, &start);
	GetConst(KEYWORD_END, &end);
	GetConst(KEYWORD_PRESERVING_WHITESPACE, &pre);
	cons_heap(&start, start, type1);
	cons_heap(&end, end, type2);
	cons_heap(&pre, pre, type);
	list_heap(&key, start, end, pre, NULL);
	typeargs_full(&arg, var, opt, Nil, key);
	typevalues_values2(&values, type, size);
	type_compiled_heap(arg, values, ret);
}

static void defun_read_from_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_FROM_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_read_from_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_from_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun readtable-case (readtable) ...) -> mode */
static void function_readtable_case(Execute ptr, addr var)
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
			fmte("Invalid case type.", NULL);
			return;
	}
	GetConstant(index, &var);
	setresult_control(ptr, var);
}

static void type_readtable_case(addr *ret)
{
	/* (function (readtable)
	 *   (values (member :upcase :downcase :preserve :invert) &rest nil))
	 */
	addr arg, values;

	GetTypeTable(&arg, Readtable);
	GetTypeTable(&values, CaseSensitivity);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_readtable_case(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLE_CASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_readtable_case);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_readtable_case(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf readtable-case) (mode readtable) ...) -> mode */
static void function_setf_readtable_case(Execute ptr, addr value, addr var)
{
	addr key;

	GetConst(KEYWORD_UPCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_upcase);
		goto final;
	}

	GetConst(KEYWORD_DOWNCASE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_downcase);
		goto final;
	}

	GetConst(KEYWORD_PRESERVE, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_preserve);
		goto final;
	}

	GetConst(KEYWORD_INVERT, &key);
	if (value == key) {
		setcase_readtable(var, ReadTable_invert);
		goto final;
	}

	fmte("Invalid case sensitivity mode ~S.", value, NULL);
	return;

final:
	setresult_control(ptr, value);
}

static void type_setf_readtable_case(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&values, CaseSensitivity);
	GetTypeTable(&type, Readtable);
	typeargs_var2(&arg, values, type);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_readtable_case(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLE_CASE, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_readtable_case);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_readtable_case(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun readtablep (object) ...) -> boolean */
static void function_readtablep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_READTABLE);
}

static void defun_readtablep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READTABLEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_readtablep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-dispatch-macro-character
 *   (disp-char sub-char &optional readtable) ...) -> function
 *   disp-char  character
 *   sub-char   character
 *   readtable  (or readtable null)  ;; readtable designer
 *   function   (or function null)
 */
static void function_get_dispatch_macro_character(Execute ptr,
		addr code1, addr code2, addr readtable)
{
	addr pos;
	unicode u1, u2;

	if (readtable == Nil) {
		/* standard readtable */
		get_default_dispatch_macro(code1, code2, &pos);
		setresult_control(ptr, pos);
		return;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}
	GetCharacter(code1, &u1);
	GetCharacter(code2, &u2);
	get_dispatch_macro_character(readtable, u1, u2, &pos);
	setresult_control(ptr, pos);
}

static void type_get_dispatch_macro_character(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Character);
	GetTypeTable(&type, ReadtableDesigner);
	typeargs_var2opt1(&arg, arg, arg, type);
	GetTypeTable(&values, FunctionNull);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_get_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_get_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-dispatch-macro-character
 *   (disp-char sub-char function &optional readtable) ...) -> (eql t)
 *   disp-char  character
 *   sub-char   character
 *   function   (or symbol function)  ;; function-designer
 *   readtable  (or readtable null)  ;; readtable designer
 */
static void function_set_dispatch_macro_character(Execute ptr,
		addr code1, addr code2, addr call, addr readtable)
{
	unicode u1, u2;

	if (readtable == Nil) {
		/* standard readtable */
		fmte("set-dispatch-macro-character don't update a standard readtable.", NULL);
		return;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}
	GetCharacter(code1, &u1);
	GetCharacter(code2, &u2);
	if (call == Nil)
		rem_dispatch_macro_character(readtable, u1, u2);
	else {
		if (symbolp(call))
			getspecialcheck_local(ptr, call, &call);
		set_dispatch_macro_character(readtable, u1, u2, call);
	}
	setresult_control(ptr, T);
}

static void type_set_dispatch_macro_character(addr *ret)
{
	addr arg, values, type1, type2;

	GetTypeTable(&arg, Character);
	GetTypeTable(&type1, FunctionNull);
	GetTypeTable(&type2, ReadtableDesigner);
	typeargs_var3opt1(&arg, arg, arg, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_set_dispatch_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_DISPATCH_MACRO_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_set_dispatch_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_dispatch_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-macro-character (char &optional readtable) ...)
 *     (values function nonterm &rest nil))
 *   char       character
 *   readtable  (or readtable null)  ;; readtable-designer
 *   function   (or function nil)
 *   nonterm    boolean
 */
static void function_get_macro_character(Execute ptr, addr code, addr readtable)
{
	int nonterm;
	addr pos;
	unicode u;

	GetCharacter(code, &u);
	if (readtable == Nil) {
		/* standard readtable */
		get_default_macro_character(u, &pos, &nonterm);
		setvalues_control(ptr, pos, nonterm? T: Nil, NULL);
		return;
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}

	get_macro_character(readtable, u, &pos, &nonterm);
	setvalues_control(ptr, pos, nonterm? T: Nil, NULL);
}

static void type_get_macro_character(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Character);
	GetTypeTable(&type, ReadtableDesigner);
	typeargs_var1opt1(&arg, arg, type);
	GetTypeTable(&values, FunctionNull);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_get_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_MACRO_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_get_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defn set-macro-character
 *     (char function &optional nonterm readtable) ...)
 *     -> (eql t)
 *   char       character
 *   function   (or symbol function)  ;; function-designer
 *   nonterm    t  ;; boolean, default nil
 *   readtable  (or readtable null)  ;; readtable-designer
 */
static void function_set_macro_character(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable)
{
	unicode u;

	if (nonterm == Unbound) {
		nonterm = Nil;
	}
	if (readtable == Nil) {
		/* standard readtable */
		fmte("set-macro-character don't update a standard readtable.", NULL);
	}
	if (readtable == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &readtable);
		getspecialcheck_local(ptr, readtable, &readtable);
	}

	GetCharacter(code, &u);
	if (symbolp(call))
		getspecialcheck_local(ptr, call, &call);
	set_macro_character(readtable, u, nonterm != Nil, call);
	setresult_control(ptr, T);
}

static void type_set_macro_character(addr *ret)
{
	addr arg, values, type1, type2, type3;

	GetTypeTable(&arg, Character);
	GetTypeTable(&type1, FunctionDesigner);
	GetTypeTable(&type2, T);
	GetTypeTable(&type3, ReadtableDesigner);
	typeargs_var2opt2(&arg, arg, type1, type2, type3);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_set_macro_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_MACRO_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_macro_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_macro_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-syntax-from-char
 *     (to-char from-char &optional to-readtable from-readtable) ...)
 *     -> (eql t)
 *   to-char         character
 *   from-char       character
 *   to-readtable    readtable  ;; default *readtable*
 *   from-readtable  (or readtable null)  ;; readtable-designer, default nil
 */
static void function_set_syntax_from_char(Execute ptr,
		addr code1, addr code2, addr table1, addr table2)
{
	unicode u1, u2;

	if (table1 == Unbound) {
		/* *readtable* */
		GetConst(SPECIAL_READTABLE, &table1);
		getspecialcheck_local(ptr, table1, &table1);
	}
	if (table2 == Unbound) {
		table2 = Nil;
	}

	GetCharacter(code1, &u1);
	GetCharacter(code2, &u2);
	if (table2 == Nil)
		set_syntax_from_default(u1, u2, table1);
	else
		set_syntax_from_char(u1, u2, table1, table2);
	setresult_control(ptr, T);
}

static void type_set_syntax_from_char(addr *ret)
{
	addr arg, values, type1, type2;

	GetTypeTable(&arg, Character);
	GetTypeTable(&type1, Readtable);
	GetTypeTable(&type2, ReadtableDesigner);
	typeargs_var2opt2(&arg, arg, arg, type1, type2);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_set_syntax_from_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_SYNTAX_FROM_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_syntax_from_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_syntax_from_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-standard-io-syntax (&body form) ...) */
static void function_with_standard_io_syntax(Execute ptr, addr form, addr env)
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
	getcdr(form, &form);
	lista_heap(&args, symbol, args, form, NULL);
	setresult_control(ptr, args);
}

static void defmacro_with_standard_io_syntax(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_STANDARD_IO_SYNTAX, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_standard_io_syntax);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defvar *read-base* 10) */
static void defvar_read_base(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, RadixInteger);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-default-float-format* 'single-float) */
static void defvar_read_default_float_format(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_SINGLE_FLOAT, &value);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* (member short-float single-float double-float long-float) */
	GetTypeTable(&type, FloatSymbol);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-eval* t) */
static void defvar_read_eval(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READ_EVAL, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *read-suppress* t) */
static void defvar_read_suppress(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *readtable*) */
static void defvar_readtable(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_READTABLE, &symbol);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Readtable);
	settype_value_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_reader(void)
{
	SetPointerCall(defun, opt2, copy_readtable);
	SetPointerCall(defun, var1opt2, make_dispatch_macro_character);
	SetPointerCall(defun, dynamic, read);
	SetPointerCall(defun, dynamic, read_preserving_whitespace);
	SetPointerCall(defun, var1opt2, read_delimited_list);
	SetPointerCall(defun, dynamic, read_from_string);
	SetPointerCall(defun, var1, readtable_case);
	SetPointerCall(defun, var2, setf_readtable_case);
	SetPointerCall(defun, var1, readtablep);
	SetPointerCall(defun, var2opt1, get_dispatch_macro_character);
	SetPointerCall(defun, var3opt1, set_dispatch_macro_character);
	SetPointerCall(defun, var1opt1, get_macro_character);
	SetPointerCall(defun, var2opt2, set_macro_character);
	SetPointerCall(defun, var2opt2, set_syntax_from_char);
	SetPointerCall(defmacro, macro, with_standard_io_syntax);
}

_g void build_common_reader(void)
{
	defun_copy_readtable();
	defun_make_dispatch_macro_character();
	defun_read();
	defun_read_preserving_whitespace();
	defun_read_delimited_list();
	defun_read_from_string();
	defun_readtable_case();
	defun_setf_readtable_case();
	defun_readtablep();
	defun_get_dispatch_macro_character();
	defun_set_dispatch_macro_character();
	defun_get_macro_character();
	defun_set_macro_character();
	defun_set_syntax_from_char();
	defmacro_with_standard_io_syntax();
	defvar_read_base();
	defvar_read_default_float_format();
	defvar_read_eval();
	defvar_read_suppress();
	defvar_readtable();
}

