/*
 *  ANSI COMMON LISP: 22. Printer
 */
#include "bignum.h"
#include "common_header.h"
#include "cons.h"
#include "cons_plist.h"
#include "eval_declare.h"
#include "format.h"
#include "integer.h"
#include "print.h"
#include "print_dispatch.h"
#include "print_pretty.h"
#include "print_write.h"
#include "stream.h"
#include "stream_string.h"
#include "type_parse.h"
#include "type_table.h"

/* (defun copy-pprint-dispatch (&optional table) ...) -> new-table */
static void function_copy_pprint_dispatch(Execute ptr, addr var)
{
	if (var == Unbound)
		pprint_dispatch_print(ptr, &var);
	copy_pprint_dispatch_common(var, &var);
	setresult_control(ptr, var);
}

static void type_copy_pprint_dispatch(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrintDispatchNull);
	typeargs_opt1(&args, args);
	GetTypeTable(&values, PrintDispatch);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_PPRINT_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_copy_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro formatter (string) ...) -> function */
static void function_formatter(Execute ptr, addr var, addr env)
{
	/* `(lambda (*standard-output* &rest args)
	 *    (apply (function lisp-system::format-formatter) t string args))
	 */
	addr lambda, output, rest, args, apply, fsym, formatter;

	getcdr(var, &var);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(SPECIAL_STANDARD_OUTPUT, &output);
	GetConst(AMPERSAND_REST, &rest);
	make_symbolchar(&args, "ARGS");
	GetConst(COMMON_APPLY, &apply);
	GetConst(COMMON_FUNCTION, &fsym);
	GetConst(SYSTEM_FORMAT_FORMATTER, &formatter);
	list_heap(&fsym, fsym, formatter, NULL);
	list_heap(&apply, apply, fsym, T, var, args, NULL);
	list_heap(&output, output, rest, args, NULL);
	list_heap(&lambda, lambda, output, apply, NULL);
	setresult_control(ptr, lambda);
}

static void defmacro_formatter(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_FORMATTER, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_formatter);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun pprint-dispatch (object &optional table) ...) -> function, boolean */
static void function_pprint_dispatch(Execute ptr, addr var, addr table)
{
	if (table == Unbound)
		pprint_dispatch_print(ptr, &table);
	if (pprint_dispatch_common(ptr, var, table, &var, &table))
		return;
	setvalues_control(ptr, var, table, NULL);
}

static void type_pprint_dispatch(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, PrintDispatchNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeTable(&values, FunctionDesigner);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-fill (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designer
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static void function_pprint_fill(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign)
{
	output_stream_designer(ptr, stream, &stream);
	pprint_fill_common(stream, pos, colon != Nil);
	setresult_control(ptr, Nil);
}

static void defun_pprint_fill(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_FILL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_pprint_fill);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PprintFill);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-linear (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designer
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static void function_pprint_linear(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign)
{
	output_stream_designer(ptr, stream, &stream);
	pprint_linear_common(stream, pos, colon != Nil);
	setresult_control(ptr, Nil);
}

static void defun_pprint_linear(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_LINEAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_pprint_linear);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PprintFill);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-tabular (stream object &optional colon-p at-sign-p) ...) -> null
 *   stream     (or boolean stream)  ;; output-stream-designer
 *   object     t
 *   colon-p    t  ;; boolean
 *   at-sign-p  t  ;; boolean
 */
static void function_pprint_tabular(Execute ptr,
		addr stream, addr pos, addr colon, addr atsign, addr tabsize)
{
	fixnum size;

	output_stream_designer(ptr, stream, &stream);
	getfixnum_unsigned(tabsize, &size);
	pprint_tabular_common(stream, pos, colon != Nil, size);
	setresult_control(ptr, Nil);
}

static void type_pprint_tabular(addr *ret)
{
	/* (function (output-stream-designer t &optional t t (integer 0 *))
	 *           (values null &rest nil))
	 */
	addr args, values, type;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	GetTypeTable(&type, Intplus);
	typeargs_var2opt3(&args, args, values, values, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_tabular(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_TABULAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt3(pos, p_defun_pprint_tabular);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_tabular(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-indent (relative-to n &optional stream) ...) -> null
 *   relative-to  (member :block :current)
 *   n            real
 *   stream       output-stream-designer
 */
static void function_pprint_indent(Execute ptr, addr rel, addr n, addr stream)
{
	int block_p;
	addr block, current;
	fixnum value;

	output_stream_designer(ptr, stream, &stream);
	GetConst(KEYWORD_BLOCK, &block);
	GetConst(KEYWORD_CURRENT, &current);
	if (rel == block)
		block_p = 1;
	else if (rel == current)
		block_p = 0;
	else {
		fmte("The first argument ~S must be a (MEMBER :BLOCK :CURRENT).", rel, NULL);
		return;
	}
	if (! fixnump(n))
		fmte("Too large indent value ~S.", n, NULL);
	GetFixnum(n, &value);
	pprint_indent_common(ptr, block_p, value, stream);
	setresult_control(ptr, Nil);
}

static void type_pprint_indent(addr *ret)
{
	addr args, values, type, key1, key2;

	GetConst(KEYWORD_BLOCK, &key1);
	GetConst(KEYWORD_CURRENT, &key2);
	type_member_heap(&args, key1, key2, NULL);
	GetTypeTable(&values, Real);
	GetTypeTable(&type, StreamDesigner);
	typeargs_var2opt1(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_indent(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_INDENT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_pprint_indent);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_indent(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro pprint-logical-block
 *      (stream-symbol object &key prefix per-line-prefix suffix)
 *      declaration* form*) -> null
 */
static void function_pprint_logical_block(Execute ptr, addr form, addr env)
{
	addr args, list, stream, pos, decl;
	addr key, key1, key2, key3, value, value1, value2, value3;

	getcdr(form, &form);
	if (! consp_getcons(form, &list, &args))
		goto error;
	/* first argument */
	if (! consp_getcons(list, &stream, &list))
		goto error;
	if (! consp_getcons(list, &pos, &list))
		goto error;
	GetConst(KEYWORD_PREFIX, &key1);
	GetConst(KEYWORD_PER_LINE_PREFIX, &key2);
	GetConst(KEYWORD_SUFFIX, &key3);
	value1 = value2 = value3 = Unbound;
	while (list != Nil) {
		getcons(list, &key, &list);
		getcons(list, &value, &list);
		if (key == key1) {
			if (value1 == Unbound)
				value1 = value;
		}
		else if (key == key2) {
			if (value2 == Unbound)
				value2 = value;
		}
		else if (key == key3) {
			if (value3 == Unbound)
				value3 = value;
		}
		else {
			goto error;
		}
	}
	/* result */
	declare_body_form(args, &decl, &args);
	if (value1 == Unbound) value1 = Nil;
	if (value2 == Unbound) value2 = Nil;
	if (value3 == Unbound) value3 = Nil;
	expand_pprint_logical_block_common(ptr, &pos,
			stream, pos, value1, value2, value3, decl, args);
	setresult_control(ptr, pos);
	return;

error:
	fmte("PPRINT-LOGICAL-BLOCK form ~S must be "
			"((stream object &key prefix per-line-prefix suffix) "
			"declaration* form*)", form, NULL);
}

static void defmacro_pprint_logical_block(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PPRINT_LOGICAL_BLOCK, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pprint_logical_block);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun pprint-newline (kind &optional stream) ...) -> null
 *   kind    (member :linear :fill :miser :mandatory)
 *   stream  output-steram-designer
 */
static enum pprint_newline pprint_newline_symbol(addr kind)
{
	addr check;

	GetConst(KEYWORD_LINEAR, &check);
	if (check == kind)
		return pprint_newline_linear;
	GetConst(KEYWORD_FILL, &check);
	if (check == kind)
		return pprint_newline_fill;
	GetConst(KEYWORD_MISER, &check);
	if (check == kind)
		return pprint_newline_miser;
	GetConst(KEYWORD_MANDATORY, &check);
	if (check == kind)
		return pprint_newline_mandatory;
	fmte("PPRINT-NEWLINE first argument ~S must be "
			"(member :linear :fill :miser :mandatory)", kind, NULL);
	return pprint_newline_linear;
}

static void function_pprint_newline(Execute ptr, addr kind, addr stream)
{
	enum pprint_newline value;

	value = pprint_newline_symbol(kind);
	output_stream_designer(ptr, stream, &stream);
	pprint_newline_common(ptr, value, stream);
	setresult_control(ptr, Nil);
}

static void type_pprint_newline(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PprintNewline);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_newline(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_NEWLINE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint_newline);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_newline(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-tab (kind column colink &optional stream) ...) -> null
 *   kind    (member :line :section :line-relative :section-relative)
 *   column  (integer 0 *)
 *   colinc  (integer 0 *)
 *   stream  output-steram-designer
 */
static enum pprint_tabular pprint_tabular_symbol(addr kind)
{
	addr check;

	GetConst(KEYWORD_LINE, &check);
	if (check == kind)
		return pprint_tabular_line;
	GetConst(KEYWORD_SECTION, &check);
	if (check == kind)
		return pprint_tabular_section;
	GetConst(KEYWORD_LINE_RELATIVE, &check);
	if (check == kind)
		return pprint_tabular_line_relative;
	GetConst(KEYWORD_SECTION_RELATIVE, &check);
	if (check == kind)
		return pprint_tabular_section_relative;
	fmte("PPRINT-TAB first argument ~S must be "
			"(member :line :section :line-relative :section-relative)", kind, NULL);
	return pprint_tabular_line;
}

static void function_pprint_tab(Execute ptr,
		addr kind, addr column, addr colinc, addr stream)
{
	enum pprint_tabular value;
	fixnum a, b;

	value = pprint_tabular_symbol(kind);
	getfixnum_unsigned(column, &a);
	getfixnum_unsigned(colinc, &b);
	output_stream_designer(ptr, stream, &stream);
	pprint_tab_common(ptr, stream, value, a, b);
	setresult_control(ptr, Nil);
}

static void type_pprint_tab(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, PprintTabular);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, StreamDesigner);
	typeargs_var3opt1(&args, args, values, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_tab(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT_TAB, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_pprint_tab);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint_tab(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro print-unreadable-object
 *     ((object stream &key type identity) &body form) ...) -> null
 *   object    t  ;; evaluated
 *   stream    output-stream-designer  ;; evaluated
 *   type      t  ;; boolean, evaluated
 *   identity  t  ;; boolean, evaluated
 */
static void expand_print_unreadable_object(addr *ret,
		addr pos, addr stream, addr type, addr identity, addr body)
{
	/* `(list-system::print-unreadable-call
	*     ,stream ,object ,type ,identity ,(when body `(lambda () ,@body)))
	 */
	addr call, lambda;

	if (body != Nil) {
		GetConst(COMMON_LAMBDA, &lambda);
		lista_heap(&body, lambda, Nil, body, NULL);
	}
	GetConst(SYSTEM_PRINT_UNREADABLE_CALL, &call);
	list_heap(ret, call, stream, pos, type, identity, body, NULL);
}

static void function_print_unreadable_object(Execute ptr, addr form, addr env)
{
	addr args, list, stream, pos;
	addr key, key1, key2, value, value1, value2;

	getcdr(form, &form);
	if (! consp_getcons(form, &list, &args))
		goto error;
	/* first argument */
	if (! consp_getcons(list, &pos, &list))
		goto error;
	if (! consp_getcons(list, &stream, &list))
		goto error;
	GetConst(KEYWORD_TYPE, &key1);
	GetConst(KEYWORD_IDENTITY, &key2);
	value1 = value2 = Unbound;
	while (list != Nil) {
		getcons(list, &key, &list);
		getcons(list, &value, &list);
		if (key == key1) {
			if (value1 != Unbound)
				value1 = value;
		}
		else if (key == key2) {
			if (value2 != Unbound)
				value2 = value;
		}
		else {
			goto error;
		}
	}
	/* result */
	expand_print_unreadable_object(&pos, pos, stream, value1, value2, args);
	setresult_control(ptr, pos);
	return;

error:
	fmte("PRINT-UNREADABLE-OBJECT form ~S must be "
			"((object stream &key type identity) &body form)", form, NULL);
}

static void defmacro_print_unreadable_object(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PRINT_UNREADABLE_OBJECT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_print_unreadable_object);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun set-pprint-dispatch (type function &optional priority table) ...) -> null
 *   type      type-specifier
 *   function  function-designer
 *   priority  real  ;; default 0
 *   table     print-dispatch  ;; default *print-pprint-dispatch*
 */
static void function_set_pprint_dispatch(Execute ptr,
		addr spec, addr call, addr priority, addr table)
{
	addr type;

	if (parse_type(ptr, &type, spec, Nil))
		return;
	if (call != Nil && (! functionp(call)))
		getfunctioncheck_local(ptr, call, &call);
	if (table == Unbound)
		pprint_dispatch_print(ptr, &table);
	set_pprint_dispatch_common(ptr->local, spec, type, call, priority, table);
	setresult_control(ptr, Nil);
}

static void type_set_pprint_dispatch(addr *ret)
{
	addr args, values, type, dispatch;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, FunctionDesigner);
	GetTypeTable(&type, Real);
	GetTypeTable(&dispatch, PrintDispatch);
	typeargs_var2opt2(&args, args, values, type, dispatch);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_set_pprint_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_PPRINT_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_set_pprint_dispatch);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set_pprint_dispatch(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write (object &key
 *     array base case circle escape gensym length level lines miser-width
 *     pprint-dispatch pretty radix readably right-margin stream) ...) -> object
 *   array            t  ;; boolean
 *   base             (integer 2 36)  ;; radix-integer
 *   case             (member :upcase :downcase :capitalize)
 *   circle           t  ;; boolean
 *   escape           t  ;; boolean
 *   gensym           t  ;; boolean
 *   length           (or null (integer 0 *))
 *   level            (or null (integer 0 *))
 *   lines            (or null (integer 0 *))
 *   miser-width      (or null (integer 0 *))
 *   pprint-dispatch  dispatch-table
 *   pretty           t  ;; boolean
 *   radix            t  ;; boolean
 *   readably         t  ;; boolean
 *   right-margin     (or null (integer 0 *))
 *   stream           output-stream-designer
 */
static void pushprint_control(Execute ptr, constindex index, addr pos)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, pos);
}
#define PushPrintControl(p, a, b) \
	pushprint_control((p), CONSTANT_SPECIAL_PRINT_##a, (b))

static void format_keyword_print(Execute ptr, addr args)
{
	addr pos;

	/* print-array */
	if (! getkeyargs(args, KEYWORD_ARRAY, &pos)) {
		PushPrintControl(ptr, ARRAY, pos);
	}
	/* print-base */
	if (! getkeyargs(args, KEYWORD_BASE, &pos)) {
		PushPrintControl(ptr, BASE, pos);
	}
	/* print-radix */
	if (! getkeyargs(args, KEYWORD_RADIX, &pos)) {
		PushPrintControl(ptr, RADIX, pos);
	}
	/* print-case */
	if (! getkeyargs(args, KEYWORD_CASE, &pos)) {
		PushPrintControl(ptr, CASE, pos);
	}
	/* print-circle */
	if (! getkeyargs(args, KEYWORD_CIRCLE, &pos)) {
		PushPrintControl(ptr, CIRCLE, pos);
	}
	/* print-escape */
	if (! getkeyargs(args, KEYWORD_ESCAPE, &pos)) {
		PushPrintControl(ptr, ESCAPE, pos);
	}
	/* print-gensym */
	if (! getkeyargs(args, KEYWORD_GENSYM, &pos)) {
		PushPrintControl(ptr, GENSYM, pos);
	}
	/* print-readably */
	if (! getkeyargs(args, KEYWORD_READABLY, &pos)) {
		PushPrintControl(ptr, READABLY, pos);
	}
	/* print-pretty */
	if (! getkeyargs(args, KEYWORD_PRETTY, &pos)) {
		PushPrintControl(ptr, PRETTY, pos);
	}
	/* print-level */
	if (! getkeyargs(args, KEYWORD_LEVEL, &pos)) {
		PushPrintControl(ptr, LEVEL, pos);
	}
	/* print-length */
	if (! getkeyargs(args, KEYWORD_LENGTH, &pos)) {
		PushPrintControl(ptr, LENGTH, pos);
	}
	/* print-lines */
	if (! getkeyargs(args, KEYWORD_LINES, &pos)) {
		PushPrintControl(ptr, LINES, pos);
	}
	/* print-miser-width */
	if (! getkeyargs(args, KEYWORD_MISER_WIDTH, &pos)) {
		PushPrintControl(ptr, MISER_WIDTH, pos);
	}
	/* print-right-margin */
	if (! getkeyargs(args, KEYWORD_RIGHT_MARGIN, &pos)) {
		PushPrintControl(ptr, RIGHT_MARGIN, pos);
	}
	/* print-pprint-dispatch */
	if (! getkeyargs(args, KEYWORD_PPRINT_DISPATCH, &pos)) {
		PushPrintControl(ptr, PPRINT_DISPATCH, pos);
	}
}

static void function_write(Execute ptr, addr var, addr args)
{
	addr stream, control;

	if (getkeyargs(args, KEYWORD_STREAM, &stream))
		stream = Unbound;
	output_stream_designer(ptr, stream, &stream);
	push_return_control(ptr, &control);
	format_keyword_print(ptr, args);
	if (write_print(ptr, stream, var)) return;
	exitpoint_stream(stream);
	setresult_control(ptr, var);
}

static void type_write(addr *ret, int stream_p)
{
	addr args, values, key;
	addr key1, key2, key3, key4, key5, key6, key7, key8;
	addr key9, key10, key11, key12, key13, key14, key15, key16;

	/* key */
	KeyTypeTable(&key1, ARRAY, T);
	KeyTypeTable(&key2, BASE, RadixInteger);
	KeyTypeTable(&key3, CASE, PrintCase);
	KeyTypeTable(&key4, CIRCLE, T);
	KeyTypeTable(&key5, ESCAPE, T);
	KeyTypeTable(&key6, GENSYM, T);
	KeyTypeTable(&key7, LENGTH, IntplusNull);
	KeyTypeTable(&key8, LEVEL, IntplusNull);
	KeyTypeTable(&key9, LINES, IntplusNull);
	KeyTypeTable(&key10, MISER_WIDTH, IntplusNull);
	KeyTypeTable(&key11, PPRINT_DISPATCH, PrintDispatch);
	KeyTypeTable(&key12, PRETTY, T);
	KeyTypeTable(&key13, RADIX, T);
	KeyTypeTable(&key14, READABLY, T);
	KeyTypeTable(&key15, RIGHT_MARGIN, IntplusNull);
	if (stream_p) {
		KeyTypeTable(&key16, STREAM, StreamDesigner);
		list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8,
				key9, key10, key11, key12, key13, key14, key15, key16, NULL);
	}
	else {
		list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8,
				key9, key10, key11, key12, key13, key14, key15, NULL);
	}
	/* type */
	GetTypeTable(&args, T);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_write(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write(&type, 1);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prin1 (object &optional stream) ...) -> object
 *   stream  stream-designer  ;; default standard-output
 */
static void function_prin1(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	if (prin1_print(ptr, stream, var)) return;
	exitpoint_stream(stream);
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
	output_stream_designer(ptr, stream, &stream);
	if (princ_print(ptr, stream, var)) return;
	exitpoint_stream(stream);
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
	output_stream_designer(ptr, stream, &stream);
	if (print_print(ptr, stream, var)) return;
	exitpoint_stream(stream);
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


/* (defun pprint (object &optional stream) ...) -> (values)
 *   stream  stream-designer  ;; default standard-output
 */
static void function_pprint(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	if (pprint_print(ptr, stream, var)) return;
	exitpoint_stream(stream);
	setvalues_nil_control(ptr);
}

static void type_pprint(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PPRINT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_pprint);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pprint(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-to-string (object &key
 *     array base case circle escape gensym length level lines miser-width
 *     pprint-dispatch pretty radix readably right-margin) ...) -> string
 *   array            t  ;; boolean
 *   base             (integer 2 36)  ;; radix-integer
 *   case             (member :upcase :downcase :capitalize)
 *   circle           t  ;; boolean
 *   escape           t  ;; boolean
 *   gensym           t  ;; boolean
 *   length           (or null (integer 0 *))
 *   level            (or null (integer 0 *))
 *   lines            (or null (integer 0 *))
 *   miser-width      (or null (integer 0 *))
 *   pprint-dispatch  dispatch-table
 *   pretty           t  ;; boolean
 *   radix            t  ;; boolean
 *   readably         t  ;; boolean
 *   right-margin     (or null (integer 0 *))
 */
static void function_write_to_string(Execute ptr, addr var, addr args)
{
	addr stream, control;

	open_output_string_stream(&stream, 0);
	push_return_control(ptr, &control);
	format_keyword_print(ptr, args);
	if (write_print(ptr, stream, var)) return;
	string_stream_heap(stream, &var);
	close_stream(stream);
	setresult_control(ptr, var);
}

static void defun_write_to_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_TO_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_to_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write(&type, 0);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prin1-to-string (object) ...) -> string */
static void function_prin1_to_string(Execute ptr, addr var)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (prin1_print(ptr, stream, var)) return;
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

	open_output_string_stream(&stream, 0);
	if (princ_print(ptr, stream, var)) return;
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


/* (defvar *print-array* t) ;; boolean */
static void defvar_print_array(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ARRAY, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
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


/* (defvar *print-radix* t) ;; boolean */
static void defvar_print_radix(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-case* (member :upcase :downcase :capitalize)) */
static void defvar_print_case(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CASE, &symbol);
	GetConst(KEYWORD_UPCASE, &value);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PrintCase);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-circle* t) ;; boolean */
static void defvar_print_circle(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_CIRCLE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-escape* t) ;; boolean */
static void defvar_print_escape(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-gensym* t) ;; boolean */
static void defvar_print_gensym(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_GENSYM, &symbol);
	SetValueSymbol(symbol, T);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
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
	addr symbol, type, pos;

	/* symbol */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	pprint_dispatch_heap(&pos);
	SetValueSymbol(symbol, pos);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PrintDispatch);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-pretty* t) ;; boolean */
static void defvar_print_pretty(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *print-readably* t) ;; boolean */
static void defvar_print_readably(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
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


/* (defun print-not-readable-object (print-not-readable) ...) -> t */
static void function_print_not_readable_object(Execute ptr, addr var)
{
	print_not_readable_object(var, &var);
	setresult_control(ptr, var);
}

static void type_print_not_readable_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrintNotReadable);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_print_not_readable_object(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PRINT_NOT_READABLE_OBJECT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_print_not_readable_object);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_print_not_readable_object(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


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
	addr args, values, null, eqlt, stream, string, orv, any;

	/* (or null (eql t) stream string) */
	GetTypeTable(&null, Null);
	GetTypeTable(&eqlt, EqlT);
	GetTypeTable(&stream, Stream);
	GetTypeTable(&string, String);
	type4or_heap(null, eqlt, stream, string, &orv);
	/* ((or ...) string &rest t) */
	GetTypeTable(&any, T);
	typeargs_var2rest(&args, orv, string, any);
	/* (values (or null string) &rest nil) */
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
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
	SetPointerCall(defun, opt1, copy_pprint_dispatch);
	SetPointerCall(defmacro, macro, formatter);
	SetPointerCall(defun, var1opt1, pprint_dispatch);
	SetPointerCall(defun, var2opt2, pprint_fill);
	SetPointerCall(defun, var2opt2, pprint_linear);
	SetPointerCall(defun, var2opt3, pprint_tabular);
	SetPointerCall(defun, var2opt1, pprint_indent);
	SetPointerCall(defmacro, macro, pprint_logical_block);
	SetPointerCall(defun, var1opt1, pprint_newline);
	SetPointerCall(defun, var3opt1, pprint_tab);
	SetPointerCall(defmacro, macro, print_unreadable_object);
	SetPointerCall(defun, var2opt2, set_pprint_dispatch);
	SetPointerCall(defun, var1dynamic, write);
	SetPointerCall(defun, var1opt1, prin1);
	SetPointerCall(defun, var1opt1, princ);
	SetPointerCall(defun, var1opt1, print);
	SetPointerCall(defun, var1opt1, pprint);
	SetPointerCall(defun, var1dynamic, write_to_string);
	SetPointerCall(defun, var1, prin1_to_string);
	SetPointerCall(defun, var1, princ_to_string);
	SetPointerCall(defun, var1, print_not_readable_object);
	SetPointerCall(defun, var2dynamic, format);
}

_g void build_common_printer(void)
{
	defun_copy_pprint_dispatch();
	defmacro_formatter();
	defun_pprint_dispatch();
	defun_pprint_fill();
	defun_pprint_linear();
	defun_pprint_tabular();
	defun_pprint_indent();
	defmacro_pprint_logical_block();
	defun_pprint_newline();
	defun_pprint_tab();
	defmacro_print_unreadable_object();
	defun_set_pprint_dispatch();
	defun_write();
	defun_prin1();
	defun_princ();
	defun_print();
	defun_pprint();
	defun_write_to_string();
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
	defun_print_not_readable_object();
	defun_format();
}

