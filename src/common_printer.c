/*
 *  ANSI COMMON LISP: 22. Printer
 */
#include "common_header.h"
#include "format.h"
#include "print.h"
#include "stream.h"
#include "stream_string.h"
#include "type_parse.h"

/* (defun prin1 (object &optional stream) ...) -> object
 *   stream  stream-designer  ;; default standard-output
 */
static void function_prin1(Execute ptr, addr var, addr stream)
{
	struct PrintFormat format;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	format_print(ptr, &format);
	format.escape = 1;
	format.ptr = ptr;
	if (write_print(&format, stream, var)) return;
	setresult_control(ptr, var);
}

static void defun_prin1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRIN1, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_prin1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun princ (object &optional stream) ...) -> object
 *   stream  stream-designer  ;; default standard-output
 */
static void function_princ(Execute ptr, addr var, addr stream)
{
	struct PrintFormat format;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	format_print(ptr, &format);
	format.escape = 0;
	format.readably = 0;
	format.ptr = ptr;
	if (write_print(&format, stream, var)) return;
	setresult_control(ptr, var);
}

static void defun_princ(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_princ);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun print (object &optional stream) ...) -> object
 *   stream  stream-designer  ;; default standard-output
 */
static void function_print(Execute ptr, addr var, addr stream)
{
	struct PrintFormat format;

	if (stream == Unbound) {
		GetConst(SPECIAL_STANDARD_OUTPUT, &stream);
		getspecialcheck_local(ptr, stream, &stream);
	}
	format_print(ptr, &format);
	format.escape = 1;
	format.ptr = ptr;
	terpri_stream(stream);
	if (write_print(&format, stream, var)) return;
	write_char_stream(stream, ' ');
	setresult_control(ptr, var);
}

static void defun_print(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_print);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prin1-to-string (object) ...) -> string */
static void function_prin1_to_string(Execute ptr, addr var)
{
	addr stream;
	struct PrintFormat format;

	format_print(ptr, &format);
	format.escape = 1;
	format.ptr = ptr;
	open_output_string_stream(&stream, 0);
	if (write_print(&format, stream, var)) return;
	string_stream_heap(stream, &var);
	close_stream(stream);
	setresult_control(ptr, var);
}

static void defun_prin1_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRIN1_TO_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_prin1_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1ToString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun princ-to-string (object) ...) -> string */
static void function_princ_to_string(Execute ptr, addr var)
{
	addr stream;
	struct PrintFormat format;

	format_print(ptr, &format);
	format.escape = 0;
	format.readably = 0;
	format.ptr = ptr;
	open_output_string_stream(&stream, 0);
	if (write_print(&format, stream, var)) return;
	string_stream_heap(stream, &var);
	close_stream(stream);
	setresult_control(ptr, var);
}

static void defun_princ_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINC_TO_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_princ_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Prin1ToString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *print-array* boolean) */
static void defvar_print_array(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-base* (integer 2 36)) */
static void defvar_print_base(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_BASE, &symbol);
	fixnum_heap(&value, 10);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, RadixInteger);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-radix* boolean) */
static void defvar_print_radix(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-case* (member :upcase :downcase :capitalize)) */
static void type_print_case(addr *ret)
{
	addr key1, key2, key3;

	GetConst(KEYWORD_UPCASE, &key1);
	GetConst(KEYWORD_DOWNCASE, &key2);
	GetConst(KEYWORD_CAPITALIZE, &key3);
	type_member_heap(ret, key1, key2, key3, NULL);
}

static void defvar_print_case(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CASE, &symbol);
	GetConst(KEYWORD_UPCASE, &value);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);
	/* type */
	type_print_case(&type);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-circle* boolean) */
static void defvar_print_circle(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CIRCLE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-escape* boolean) */
static void defvar_print_escape(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-gensym* boolean) */
static void defvar_print_gensym(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_GENSYM, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-level* index-null) */
static void defvar_print_level(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-length* index-null) */
static void defvar_print_length(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-lines* index-null) */
static void defvar_print_lines(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_LINES, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-miser-width* index-null) */
static void defvar_print_miser_width(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_MISER_WIDTH, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-pprint-dispatch* [pprint-dispatch-type]) */
static void defvar_print_pprint_dispatch(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);  /* TODO: pprint-dispatch-table */
	settype_value_symbol(symbol, type);
}


/* (defvar *print-pretty* boolean) */
static void defvar_print_pretty(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-readably* boolean) */
static void defvar_print_readably(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-right-margin* index-null) */
static void defvar_print_right_margin(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_RIGHT_MARGIN, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, IndexNull);
	settype_value_symbol(symbol, type);
}


/*
 *  print function
 */
/* (defun format (destination control-string &rest args) -> restul
 *   destination     (or null (eql t) stream string)
 *   control-string  string
 *   args            (&rest t)
 *   result          (or null string)
 */
static void function_format(Execute ptr, addr var, addr format, addr args)
{
	format_lisp(ptr, var, format, args, &args);
	setresult_control(ptr, var == Nil? args: Nil);
}

static void type_format(addr *ret)
{
	addr arg, values, null, eqlt, stream, string, orv, any;

	/* (or null (eql t) stream string) */
	GetTypeTable(&null, Null);
	GetTypeTable(&eqlt, EqlT);
	GetTypeTable(&stream, Stream);
	GetTypeTable(&string, String);
	type4or_heap(null, eqlt, stream, string, &orv);
	/* ((or ...) string &rest t) */
	GetTypeTable(&any, T);
	typeargs_var2rest(&arg, orv, string, any);
	/* (values (or null string) &rest nil) */
	GetTypeValues(&values, StringNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_format(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FORMAT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_format);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_format(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_printer(void)
{
	SetPointerCall(defun, var1opt1, prin1);
	SetPointerCall(defun, var1opt1, princ);
	SetPointerCall(defun, var1opt1, print);
	SetPointerCall(defun, var1, prin1_to_string);
	SetPointerCall(defun, var1, princ_to_string);
	SetPointerCall(defun, var2dynamic, format);
}

_g void build_common_printer(void)
{
	defun_prin1();
	defun_princ();
	defun_print();
	defun_prin1_to_string();
	defun_princ_to_string();
	defvar_print_array();
	defvar_print_base();
	defvar_print_radix();
	defvar_print_case();
	defvar_print_circle();
	defvar_print_escape();
	defvar_print_gensym();
	defvar_print_level();
	defvar_print_length();
	defvar_print_lines();
	defvar_print_miser_width();
	defvar_print_pprint_dispatch();
	defvar_print_pretty();
	defvar_print_readably();
	defvar_print_right_margin();
	defun_format();
}

