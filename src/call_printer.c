#include "bignum.h"
#include "call_printer.h"
#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "function.h"
#include "format_function.h"
#include "format_parse.h"
#include "print.h"
#include "print_dispatch.h"
#include "print_function.h"
#include "print_pretty.h"
#include "print_write.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"
#include "type_parse.h"

/*
 *  formatter
 */
static int formatter_call_common(Execute ptr, addr stream, addr args)
{
	addr format;

	getdata_control(ptr, &format);
	Return(format_execute(ptr, stream, format, args, &args));
	setresult_control(ptr, args);

	return 0;
}

_g int formatter_common(LocalRoot local, addr var, addr env, addr *ret)
{
	addr pos, type;

	/* macro */
	Return_getcdr(var, &var);
	if (! singlep(var))
		goto error;
	GetCar(var, &var);
	if (! stringp(var))
		goto error;
	/* function */
	compiled_heap(&pos, Nil);
	setcompiled_var1dynamic(pos, p_formatter_call_common);
	GetTypeCompiled(&type, FormatterFunction);
	settype_function(pos, type);
	/* format */
	format_parse_heap(local, &var, var);
	SetDataFunction(pos, var);
	/* result */
	return Result(ret, pos);;

error:
	return fmte_("FORMATTER argument must be a (FORMATTER string) form.", NULL);
}


/*
 *  pprint-fill
 */
_g int pprint_fill_common(Execute ptr, addr stream, addr list, addr colon)
{
	if (colon == Unbound)
		colon = T;
	output_stream_designer(ptr, stream, &stream);
	return pprint_fill_print(ptr, stream, list, colon != Nil);
}


/*
 *  pprint-linear
 */
_g int pprint_linear_common(Execute ptr, addr stream, addr list, addr colon)
{
	if (colon == Unbound)
		colon = T;
	output_stream_designer(ptr, stream, &stream);
	return pprint_linear_print(ptr, stream, list, colon != Nil);
}


/*
 *  pprint-tabular
 */
_g int pprint_tabular_common(Execute ptr,
		addr stream, addr list, addr colon, addr tabsize)
{
	fixnum size;

	if (colon == Unbound)
		colon = T;
	if (tabsize == Unbound)
		size = 16;
	else
		getfixnum_unsigned(tabsize, &size);
	output_stream_designer(ptr, stream, &stream);

	return pprint_tabular_print(ptr, stream, list, colon != Nil, size);
}


/*
 *  pprint-indent
 */
_g int pprint_indent_common(Execute ptr, addr rel, addr n, addr stream)
{
	int block_p;
	addr block, current;
	fixnum value;

	output_stream_designer(ptr, stream, &stream);
	GetConst(KEYWORD_BLOCK, &block);
	GetConst(KEYWORD_CURRENT, &current);
	if (rel == block) {
		block_p = 1;
	}
	else if (rel == current) {
		block_p = 0;
	}
	else {
		return fmte_("The first argument ~S "
				"must be a (MEMBER :BLOCK :CURRENT).", rel, NULL);
	}
	if (! fixnump(n))
		return fmte_("Too large indent value ~S.", n, NULL);
	GetFixnum(n, &value);
	pprint_indent_print(ptr, block_p, value, stream);

	return 0;
}


/*
 *  pprint-logical-block
 */
_g int pprint_logical_block_common(addr form, addr env, addr *ret)
{
	addr args, list, stream, pos, decl;
	addr key, key1, key2, key3, value, value1, value2, value3;

	Return_getcdr(form, &form);
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
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
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
	expand_pprint_logical_block_common(ret,
			stream, pos, value1, value2, value3, decl, args);
	return 0;

error:
	return fmte_("PPRINT-LOGICAL-BLOCK form ~S must be "
			"((stream object &key prefix per-line-prefix suffix) "
			"declaration* form*)", form, NULL);
}


/*
 *  pprint-newline
 */
static int pprint_newline_symbol_common(addr kind, enum pprint_newline *ret)
{
	addr check;

	GetConst(KEYWORD_LINEAR, &check);
	if (check == kind)
		return Result(ret, pprint_newline_linear);
	GetConst(KEYWORD_FILL, &check);
	if (check == kind)
		return Result(ret, pprint_newline_fill);
	GetConst(KEYWORD_MISER, &check);
	if (check == kind)
		return Result(ret, pprint_newline_miser);
	GetConst(KEYWORD_MANDATORY, &check);
	if (check == kind)
		return Result(ret, pprint_newline_mandatory);

	/* error */
	*ret = pprint_newline_linear;
	return fmte_("PPRINT-NEWLINE first argument ~S must be "
			"(member :linear :fill :miser :mandatory)", kind, NULL);
}

_g int pprint_newline_common(Execute ptr, addr kind, addr stream)
{
	enum pprint_newline value;

	Return(pprint_newline_symbol_common(kind, &value));
	output_stream_designer(ptr, stream, &stream);
	pprint_newline_print(ptr, value, stream);

	return 0;
}


/*
 *  pprint-tab
 */
static int pprint_tab_symbol_common(addr kind, enum pprint_tabular *ret)
{
	addr check;

	GetConst(KEYWORD_LINE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_line);
	GetConst(KEYWORD_SECTION, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_section);
	GetConst(KEYWORD_LINE_RELATIVE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_line_relative);
	GetConst(KEYWORD_SECTION_RELATIVE, &check);
	if (check == kind)
		return Result(ret, pprint_tabular_section_relative);

	/* error */
	*ret = pprint_tabular_line;
	return fmte_("PPRINT-TAB first argument ~S must be "
			"(member :line :section :line-relative :section-relative)", kind, NULL);
}

_g int pprint_tab_common(Execute ptr, addr kind, addr column, addr colinc, addr stream)
{
	enum pprint_tabular value;
	fixnum a, b;

	Return(pprint_tab_symbol_common(kind, &value));
	getfixnum_unsigned(column, &a);
	getfixnum_unsigned(colinc, &b);
	output_stream_designer(ptr, stream, &stream);
	pprint_tab_print(ptr, stream, value, a, b);

	return 0;
}


/*
 *  print-unreadable-object
 */
static void print_unreadable_object_expand_common(addr *ret,
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

_g int print_unreadable_object_common(addr form, addr env, addr *ret)
{
	addr args, list, stream, pos;
	addr key, key1, key2, value, value1, value2;

	Return_getcdr(form, &form);
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
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == key1) {
			if (value1 == Unbound)
				value1 = value;
		}
		else if (key == key2) {
			if (value2 == Unbound)
				value2 = value;
		}
		else {
			goto error;
		}
	}
	/* result */
	if (value1 == Unbound) value1 = Nil;
	if (value2 == Unbound) value2 = Nil;
	print_unreadable_object_expand_common(ret, pos, stream, value1, value2, args);
	return 0;

error:
	return fmte_("PRINT-UNREADABLE-OBJECT form ~S must be "
			"((object stream &key type identity) &body form)", form, NULL);
}


/*
 *  set-pprint-dispatch
 */
_g int set_pprint_dispatch_common(Execute ptr,
		addr spec, addr call, addr priority, addr table)
{
	addr type;

	Return(parse_type(ptr, &type, spec, Nil));
	if (call != Nil && (! functionp(call)))
		getfunction_global(call, &call);
	if (table == Unbound)
		pprint_dispatch_print(ptr, &table);
	set_pprint_dispatch_print(ptr->local, spec, type, call, priority, table);

	return 0;
}


/*
 *  write
 */
static void write_special_common(Execute ptr, constindex index, addr pos)
{
	addr symbol;
	GetConstant(index, &symbol);
	pushspecial_control(ptr, symbol, pos);
}
#define WriteSpecialCommon(p, a, b) \
	write_special_common((p), CONSTANT_SPECIAL_PRINT_##a, (b))

static void write_keyword_common(Execute ptr, addr args)
{
	addr pos;

	/* print-array */
	if (! getkeyargs(args, KEYWORD_ARRAY, &pos)) {
		WriteSpecialCommon(ptr, ARRAY, pos);
	}
	/* print-base */
	if (! getkeyargs(args, KEYWORD_BASE, &pos)) {
		WriteSpecialCommon(ptr, BASE, pos);
	}
	/* print-radix */
	if (! getkeyargs(args, KEYWORD_RADIX, &pos)) {
		WriteSpecialCommon(ptr, RADIX, pos);
	}
	/* print-case */
	if (! getkeyargs(args, KEYWORD_CASE, &pos)) {
		WriteSpecialCommon(ptr, CASE, pos);
	}
	/* print-circle */
	if (! getkeyargs(args, KEYWORD_CIRCLE, &pos)) {
		WriteSpecialCommon(ptr, CIRCLE, pos);
	}
	/* print-escape */
	if (! getkeyargs(args, KEYWORD_ESCAPE, &pos)) {
		WriteSpecialCommon(ptr, ESCAPE, pos);
	}
	/* print-gensym */
	if (! getkeyargs(args, KEYWORD_GENSYM, &pos)) {
		WriteSpecialCommon(ptr, GENSYM, pos);
	}
	/* print-readably */
	if (! getkeyargs(args, KEYWORD_READABLY, &pos)) {
		WriteSpecialCommon(ptr, READABLY, pos);
	}
	/* print-pretty */
	if (! getkeyargs(args, KEYWORD_PRETTY, &pos)) {
		WriteSpecialCommon(ptr, PRETTY, pos);
	}
	/* print-level */
	if (! getkeyargs(args, KEYWORD_LEVEL, &pos)) {
		WriteSpecialCommon(ptr, LEVEL, pos);
	}
	/* print-length */
	if (! getkeyargs(args, KEYWORD_LENGTH, &pos)) {
		WriteSpecialCommon(ptr, LENGTH, pos);
	}
	/* print-lines */
	if (! getkeyargs(args, KEYWORD_LINES, &pos)) {
		WriteSpecialCommon(ptr, LINES, pos);
	}
	/* print-miser-width */
	if (! getkeyargs(args, KEYWORD_MISER_WIDTH, &pos)) {
		WriteSpecialCommon(ptr, MISER_WIDTH, pos);
	}
	/* print-right-margin */
	if (! getkeyargs(args, KEYWORD_RIGHT_MARGIN, &pos)) {
		WriteSpecialCommon(ptr, RIGHT_MARGIN, pos);
	}
	/* print-pprint-dispatch */
	if (! getkeyargs(args, KEYWORD_PPRINT_DISPATCH, &pos)) {
		WriteSpecialCommon(ptr, PPRINT_DISPATCH, pos);
	}
}

_g int write_common(Execute ptr, addr var, addr args)
{
	addr stream, control;

	if (getkeyargs(args, KEYWORD_STREAM, &stream))
		stream = Unbound;
	output_stream_designer(ptr, stream, &stream);

	push_new_control(ptr, &control);
	write_keyword_common(ptr, args);
	Return(write_print(ptr, stream, var));
	exitpoint_stream(stream);

	return free_control_(ptr, control);
}


/*
 *  prin1
 */
_g int prin1_common(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	Return(prin1_print(ptr, stream, var));
	exitpoint_stream(stream);

	return 0;
}


/*
 *  princ
 */
_g int princ_common(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	Return(princ_print(ptr, stream, var));
	exitpoint_stream(stream);

	return 0;
}


/*
 *  print
 */
_g int print_common(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	Return(print_print(ptr, stream, var));
	exitpoint_stream(stream);

	return 0;
}


/*
 *  pprint
 */
_g int pprint_common(Execute ptr, addr var, addr stream)
{
	output_stream_designer(ptr, stream, &stream);
	Return(pprint_print(ptr, stream, var));
	exitpoint_stream(stream);

	return 0;
}


/*
 *  write-to-string
 */
_g int write_to_string_common(Execute ptr, addr var, addr args, addr *ret)
{
	addr stream, control;

	open_output_string_stream(&stream, 0);
	push_new_control(ptr, &control);
	write_keyword_common(ptr, args);
	Return(write_print(ptr, stream, var));
	string_stream_heap(stream, ret);
	close_stream(stream);

	return free_control_(ptr, control);
}


/*
 *  prin1-to-string
 */
_g int prin1_to_string_common(Execute ptr, addr var, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print(ptr, stream, var));
	string_stream_heap(stream, ret);
	close_stream(stream);

	return 0;
}


/*
 *  princ-to-string
 */
_g int princ_to_string_common(Execute ptr, addr var, addr *ret)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print(ptr, stream, var));
	string_stream_heap(stream, ret);
	close_stream(stream);

	return 0;
}


/*
 *  initialize
 */
_g void init_call_printer(void)
{
	SetPointerType(var1dynamic, formatter_call_common);
}
