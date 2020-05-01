/*
 *  ANSI COMMON LISP: 23. Reader
 */
#include "call_reader.h"
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"

/* (defun copy-readtable (&optional from to) ...) -> readtable
 *   from  (or readtable null)  ;; readtable-designer
 *   to    (or readtable null)
 */
static int function_copy_readtable(Execute ptr, addr from, addr to)
{
	Return(copy_readtable_common(ptr, from, to, &from));
	setresult_control(ptr, from);
	return 0;
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
static int function_make_dispatch_macro_character(Execute ptr,
		addr code, addr nonterm, addr readtable)
{
	Return(make_dispatch_macro_character_common(ptr, code, nonterm, readtable));
	setresult_control(ptr, T);
	return 0;
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
static int function_read(Execute ptr, addr args)
{
	Return(read_common(ptr, args, &args));
	setresult_control(ptr, args);
	return 0;
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
static int function_read_preserving_whitespace(Execute ptr, addr args)
{
	Return(read_preserving_whitespace_common(ptr, args, &args));
	setresult_control(ptr, args);
	return 0;
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
static int function_read_delimited_list(Execute ptr,
		addr code, addr stream, addr recp)
{
	return read_delimited_list_common(ptr, code, stream, recp);
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
static int function_read_from_string(Execute ptr, addr args)
{
	addr second;
	Return(read_from_string_common(ptr, args, &args, &second));
	setvalues_control(ptr, args, second, NULL);
	return 0;
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
static int function_readtable_case(Execute ptr, addr var)
{
	Return(readtable_case_common(var, &var));
	setresult_control(ptr, var);
	return 0;
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
static int function_setf_readtable_case(Execute ptr, addr value, addr var)
{
	Return(setf_readtable_case_common(value, var));
	setresult_control(ptr, value);
	return 0;
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
static int function_readtablep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_READTABLE);
	return 0;
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
static int function_get_dispatch_macro_character(Execute ptr,
		addr x, addr y, addr readtable)
{
	Return(get_dispatch_macro_character_common(ptr, x, y, readtable, &x));
	setresult_control(ptr, x);
	return 0;
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
static int function_set_dispatch_macro_character(Execute ptr,
		addr x, addr y, addr call, addr readtable)
{
	Return(set_dispatch_macro_character_common(ptr, x, y, call, readtable));
	setresult_control(ptr, T);
	return 0;
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
static int function_get_macro_character(Execute ptr, addr code, addr table)
{
	Return(get_macro_character_common(ptr, code, table, &code, &table));
	setvalues_control(ptr, code, table, NULL);
	return 0;
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
static int function_set_macro_character(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable)
{
	Return(set_macro_character_common(ptr, code, call, nonterm, readtable));
	setresult_control(ptr, T);
	return 0;
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
static int function_set_syntax_from_char(Execute ptr, addr x, addr y, addr z, addr w)
{
	Return(set_syntax_from_char_common(ptr, x, y, z, w));
	setresult_control(ptr, T);
	return 0;
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
static int function_with_standard_io_syntax(Execute ptr, addr form, addr env)
{
	Return(with_standard_io_syntax_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
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

