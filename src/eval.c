#include "code.h"
#include "condition.h"
#include "control.h"
#include "eval.h"
#include "eval_code.h"
#include "eval_optparse.h"
#include "eval_parse.h"
#include "eval_scope.h"
#include "fasl.h"
#include "file.h"
#include "files.h"
#include "format.h"
#include "function.h"
#include "object.h"
#include "pathname.h"
#include "prompt.h"
#include "readtable.h"
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

_g void push_evalwhen_eval(Execute ptr)
{
	addr symbol, value;

	symbol_evalwhen_eval(&symbol);
	GetConst(COMMON_EVAL, &value);
	pushspecial_control(ptr, symbol, value);
}

_g void push_evalwhen_load(Execute ptr)
{
	addr symbol, value;

	symbol_evalwhen_eval(&symbol);
	GetConst(COMMON_LOAD, &value);
	pushspecial_control(ptr, symbol, value);
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

_g int eval_constantp(addr var, addr env, int *result)
{
	int check;
	addr pos;

	if (macroexpand(&pos, var, env, &check))
		return 1;
	if (check)
		var = pos;
	*result = eval_constantp_stable(var);

	return 0;
}


/*
 *  eval
 */
_g int eval_execute(Execute ptr, addr pos)
{
	if (eval_parse(&pos, pos)) return 1;
	/*eval_optparse(ptr->local, &pos, pos);*/
	if (eval_scope(ptr, &pos, pos)) return 1;
	eval_code(ptr->local, &pos, pos);
	return runcode_control(ptr, pos);
}

_g int eval_stream(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		if (read_stream(ptr, stream, &check, &pos))
			return 1;
		if (check)
			break;
		if (eval_execute(ptr, pos))
			return 1;
	}

	return 0;
}

_g int eval_object(Execute ptr, addr eval, addr *ret)
{
	addr control;

	push_close_control(ptr, &control);
	push_toplevel_eval(ptr, Nil);
	push_evalwhen_eval(ptr);
	if (eval_execute(ptr, eval)) {
		return runcode_free_control(ptr, control);
	}
	else {
		getresult_control(ptr, ret);
		return free_control(ptr, control);
	}
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

static void eval_load_finalize(Execute ptr)
{
	addr stream;
	getdata_control(ptr, &stream);
	CheckType(stream, LISPTYPE_STREAM);
	close_stream(stream);
}

static void eval_load_close(Execute ptr, addr stream)
{
	addr control, code;

	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_eval_load_finalize, stream);
	setfinalize_control(ptr, control, code);
}

static int eval_load_fasl(Execute ptr, int *result, addr file, int exist)
{
	addr stream;

	/* stream */
	if (open_input_binary_stream(ptr, &stream, file)) {
		if (exist)
			file_error(file);
		*result = 0;
		return 0;
	}
	eval_load_close(ptr, stream);

	/* fasl */
	if (fasl_stream(ptr, stream)) {
		close_stream(stream);
		return 1;
	}
	else {
		close_stream(stream);
		*result = 1;
		return 0;
	}
}

static int eval_load_lisp(Execute ptr, int *result, addr file, int exist)
{
	addr stream;

	/* stream */
	if (streamp(file)) {
		stream = file;
	}
	else if (open_input_stream(ptr, &stream, file)) {
		if (exist)
			file_error(file);
		*result = 0;
		return 0;
	}
	eval_load_close(ptr, stream);

	/* eval */
	if (eval_stream(ptr, stream)) {
		close_stream(stream);
		return 1;
	}
	else {
		close_stream(stream);
		*result = 1;
		return 0;
	}
}

static int eval_load_file(Execute ptr, int *result,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	addr symbol, pos, truename;

	/* wild-pathname-p */
	if (! streamp(file)) {
		pathname_designer_heap(ptr, file, &file);
		if (wild_pathname_boolean(file, Nil))
			file_error(file);
	}
	/* load-pathname */
	GetConst(SPECIAL_LOAD_PATHNAME, &symbol);
	pushspecial_control(ptr, symbol, file);
	/* load-truename */
	GetConst(SPECIAL_LOAD_TRUENAME, &symbol);
	physical_pathname_heap(ptr, file, &file);
	truename_files(ptr, file, &truename, 0);
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
		GetConst(SPECIAL_LOAD_VERBOSE, &symbol);
		pushspecial_control(ptr, symbol, verbose);
	}
	/* print */
	if (print != Unbound) {
		GetConst(SPECIAL_LOAD_PRINT, &symbol);
		pushspecial_control(ptr, symbol, print);
	}
	/* external-format */
	if (external != Unbound) {
		GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
		pushspecial_control(ptr, symbol, external);
	}
	/* evalwhen */
	push_evalwhen_load(ptr);
	push_toplevel_eval(ptr, T);
	/* type check */
	if (eval_load_fasl_p(file))
		return eval_load_fasl(ptr, result, file, exist);
	else
		return eval_load_lisp(ptr, result, file, exist);
}

_g int eval_load(Execute ptr, int *result,
		addr file, addr verbose, addr print, int exist, addr external)
{
	int check;
	addr control;

	push_close_control(ptr, &control);
	push_prompt_info(ptr);
	check = eval_load_file(ptr, result, file, verbose, print, exist, external);
	return free_check_control(ptr, control, check);
}


/*
 *  initialize
 */
_g void init_eval(void)
{
	SetPointerType(empty, eval_load_finalize);
	init_eval_main();
}

