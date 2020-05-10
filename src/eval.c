#include "code_object.h"
#include "code_make.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval.h"
#include "eval_copy.h"
#include "eval_stack.h"
#include "fasl.h"
#include "file.h"
#include "files.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "object.h"
#include "optimize_parse.h"
#include "parse.h"
#include "pathname.h"
#include "prompt.h"
#include "reader.h"
#include "reader_table.h"
#include "scope.h"
#include "sequence.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  eval-object
 */
_g addr eval_allocr(LocalRoot local, enum EVAL_TYPE type, byte array, byte body)
{
	addr pos;
	alloc_smallsize(local, &pos, LISPTYPE_EVAL, array, body);
	SetEvalType(pos, type);
	return pos;
}
_g addr eval_heapr(enum EVAL_TYPE type, byte array, byte body)
{
	return eval_allocr(NULL, type, array, body);
}
_g addr eval_localr(LocalRoot local, enum EVAL_TYPE type, byte array, byte body)
{
	Check(local == NULL, "local error");
	return eval_allocr(local, type, array, body);
}
_g void eval_alloc(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	alloc_smallsize(local, ret, LISPTYPE_EVAL, array, body);
	SetEvalType(*ret, type);
}
_g void eval_heap(addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	eval_alloc(NULL, ret, type, array, body);
}
_g void eval_local(LocalRoot local, addr *ret, enum EVAL_TYPE type, byte array, byte body)
{
	Check(local == NULL, "local error");
	eval_alloc(local, ret, type, array, body);
}

_g addr refeval(addr pos, size_t index)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEval_Low(pos, index);
}
_g void geteval(addr pos, size_t index, addr *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEval_Low(pos, index, ret);
}
_g void seteval(addr pos, size_t index, addr value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEval_Low(pos, index, value);
}
_g enum EVAL_TYPE refevaltype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	return RefEvalType_Low(pos);
}
_g void getevaltype(addr pos, enum EVAL_TYPE *ret)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	GetEvalType_Low(pos, ret);
}
_g void setevaltype(addr pos, enum EVAL_TYPE value)
{
	Check(GetType(pos) != LISPTYPE_EVAL, "type error");
	SetEvalType_Low(pos, value);
}

_g int eval_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL;
}
_g int eval_declare_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_DECLARE;
}
_g int eval_declare_nil_p(addr pos)
{
	return pos == Nil || eval_declare_p(pos);
}
_g int eval_parse_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_PARSE;
}
_g int eval_scope_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_SCOPE;
}
_g int eval_stack_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_STACK;
}
_g int eval_tablevalue_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEVALUE;
}
_g int eval_tablefunction_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEFUNCTION;
}
_g int eval_tablecall_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLECALL;
}
_g int eval_tabletagbody_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLETAGBODY;
}
_g int eval_tableblock_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_TABLEBLOCK;
}
_g int eval_code_p(addr pos)
{
	return GetType(pos) == LISPTYPE_EVAL && RefEvalType(pos) == EVAL_TYPE_CODE;
}


/*
 *  symbol
 */
_g void symbol_evalwhen_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE_EVAL_WHEN, ret);
}

_g void symbol_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE_TOPLEVEL, ret);
}

_g void getevalwhen_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_evalwhen_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}

_g void setevalwhen_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_evalwhen_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void gettoplevel_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}

_g void settoplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;

	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

static void push_evalwhen_index(Execute ptr, constindex index)
{
	addr symbol, value;

	symbol_evalwhen_eval(&symbol);
	GetConstant(index, &value);
	pushspecial_control(ptr, symbol, value);
}

_g void push_evalwhen_eval(Execute ptr)
{
	push_evalwhen_index(ptr, CONSTANT_COMMON_EVAL);
}

_g void push_evalwhen_load(Execute ptr)
{
	push_evalwhen_index(ptr, CONSTANT_COMMON_LOAD);
}

_g void push_evalwhen_compile(Execute ptr)
{
	push_evalwhen_index(ptr, CONSTANT_COMMON_COMPILE);
}

_g int toplevelp_eval(Execute ptr)
{
	addr pos;
	symbol_toplevel_eval(&pos);
	getspecial_local(ptr, pos, &pos);
	return pos != Nil;
}


/*
 *  macro
 */
_g int eval_constantp_stable(addr var)
{
	addr check;

	switch (GetType(var)) {
		case LISPTYPE_CONS:
			GetCar(var, &var);
			GetConst(COMMON_QUOTE, &check);
			return check == var;

		case LISPTYPE_SYMBOL:
			if (keywordp(var))
				return 1;
			return GetStatusReadOnly(var);

		default:
			return 1;
	}
}

_g int eval_constantp(Execute ptr, addr var, addr env, int *ret)
{
	int check;
	addr pos;

	Return(macroexpand(ptr, &pos, var, env, &check));
	if (check)
		var = pos;
	*ret = eval_constantp_stable(var);

	return 0;
}


/*
 *  eval
 */
static void eval_compile(Execute ptr, addr pos)
{
	addr symbol, list;

	GetConst(SYSTEM_COMPILE_CODE, &symbol);
	getspecial_local(ptr, symbol, &list);
	if (list == Unbound)
		return;

	cons_heap(&list, pos, list);
	setspecial_local(ptr, symbol, list);
}

_g int eval_execute_parse(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	/* optimize parse */
	localhold_set(hold, 0, pos);
	optimize_parse(ptr->local, &pos, pos);
	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope(ptr, &pos, pos));
	/* code generator */
	localhold_set(hold, 0, pos);
	eval_code(ptr->local, &pos, pos);
	/* compile */
	localhold_set(hold, 0, pos);
	eval_compile(ptr, pos);
	/* execute */
	localhold_set(hold, 0, pos);
	Return(runcode_control(ptr, pos));
	/* end */
	localhold_end(hold);
	return 0;
}

_g int eval_execute(Execute ptr, addr pos)
{
	/* parse */
	Return(eval_parse(ptr, &pos, pos));
	/* execute */
	return eval_execute_parse(ptr, pos);
}

_g int eval_stream(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(eval_execute(ptr, pos));
	}

	return 0;
}

_g int eval_object(Execute ptr, addr eval, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	push_toplevel_eval(ptr, Nil);
	push_evalwhen_eval(ptr);
	gchold_push_local(ptr->local, eval);
	Return(eval_execute(ptr, eval));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  eval-load
 */
static int eval_load_fasl_p(addr file)
{
	if (streamp(file))
		return 0;
	GetPathname(file, PATHNAME_INDEX_TYPE, &file);
	return stringp(file) &&
		(string_equalp_char(file, "fasl") || string_equalp_char(file, "fas"));
}

static int eval_load_fasl(Execute ptr, int *ret, addr file, int exist)
{
	addr stream, control;

	/* stream */
	if (open_input_binary_stream(ptr, &stream, file)) {
		if (exist)
			simple_file_error_stdarg(file, "Cannot open file ~S.", file, NULL);
		return Result(ret, 0);
	}

	/* fasl */
	push_new_control(ptr, &control);
	setprotect_close_stream(ptr, stream);
	Return(faslread_stream(ptr, stream));
	Return(free_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_lisp(Execute ptr, int *ret, addr file, int exist)
{
	addr stream, control;

	/* stream */
	if (streamp(file)) {
		stream = file;
	}
	else if (open_input_stream(ptr, &stream, file)) {
		if (exist)
			simple_file_error_stdarg(file, "Cannot open file ~S.", file, NULL);
		return Result(ret, 0);
	}

	/* eval */
	push_new_control(ptr, &control);
	setprotect_close_stream(ptr, stream);
	Return(eval_stream(ptr, stream));
	return Result(ret, 1);
}

static void eval_load_check(
		Execute ptr, addr file, addr verbose, addr print, addr external,
		constindex file_pathname,
		constindex file_truename,
		constindex file_verbose,
		constindex file_print,
		addr *ret)
{
	addr symbol, pos, truename, value;

	/* wild-pathname-p */
	if (! streamp(file)) {
		pathname_designer_heap(ptr, file, &file);
		if (wild_pathname_boolean(file, Nil))
			file_error(file);
	}
	/* load-pathname */
	GetConstant(file_pathname, &symbol);
	if (streamp(file)) {
		physical_pathname_heap(ptr, file, &value);
		pushspecial_control(ptr, symbol, value);
	}
	else {
		physical_pathname_heap(ptr, file, &file);
		pushspecial_control(ptr, symbol, file);
		value = file;
	}
	/* load-truename */
	GetConstant(file_truename, &symbol);
	truename_files(ptr, value, &truename, 0);
	pushspecial_control(ptr, symbol, truename);
	/* package */
	GetConst(SPECIAL_PACKAGE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* readtable */
	GetConst(SPECIAL_READTABLE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	copy_readtable_heap(pos, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* verbose */
	if (verbose != Unbound) {
		GetConstant(file_verbose, &symbol);
		pushspecial_control(ptr, symbol, verbose);
	}
	/* print */
	if (print != Unbound) {
		GetConstant(file_print, &symbol);
		pushspecial_control(ptr, symbol, print);
	}
	/* external-format */
	if (external != Unbound) {
		GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
		pushspecial_control(ptr, symbol, external);
	}
	/* result */
	push_toplevel_eval(ptr, T);
	*ret = file;
}

static int eval_load_file(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_TRUENAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			CONSTANT_SPECIAL_LOAD_PRINT,
			&file);
	push_evalwhen_load(ptr);
	if (eval_load_fasl_p(file))
		return eval_load_fasl(ptr, ret, file, exist);
	else
		return eval_load_lisp(ptr, ret, file, exist);
}

_g int eval_load(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_new_control(ptr, &control);
	push_prompt_info(ptr);
	Return(eval_load_file(ptr, ret, file, verbose, print, exist, external));
	return free_control_(ptr, control);
}


/*
 *  compile-file
 */
static int compile_load_file(
		Execute ptr, addr file, addr verbose, addr print, addr external)
{
	int check;

	eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			CONSTANT_SPECIAL_COMPILE_PRINT,
			&file);
	push_evalwhen_compile(ptr);
	return eval_load_lisp(ptr, &check, file, 1);
}

_g int compile_load(Execute ptr, addr file, addr verbose, addr print, addr external)
{
	addr control;

	push_new_control(ptr, &control);
	push_prompt_info(ptr);
	Return(compile_load_file(ptr, file, verbose, print, external));
	return free_control_(ptr, control);
}


/*
 *  initialize
 */
_g void init_eval(void)
{
	init_eval_code();
	init_eval_copy();
	init_eval_main();
	init_eval_stack();
}

