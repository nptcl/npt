#include "code.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "env_code.h"
#include "format.h"
#include "function.h"
#include "integer.h"
#include "stream.h"
#include "symbol.h"

#define Fmt1(x) Return(format_stream(ptr, stream, x, NULL))
#define Fmt2(x,y) Return(format_stream(ptr, stream, x, y, NULL))
#define Fmt3(x,y,z) Return(format_stream(ptr, stream, x, y, z, NULL))

/*
 *  disassemble
 */
static int disassemble_code(Execute ptr, addr stream, addr code);

static int disassemble_symbol_execute(Execute ptr, addr stream, addr car, addr cdr)
{
	Fmt2("~2T~A ~20T#<CODE>~%", car);
	return disassemble_code(ptr, stream, cdr);
}

static int disassemble_symbol_lambda(Execute ptr, addr stream, addr car, addr cdr)
{
	addr name, code, type, doc, form, defun;

	List_bind(cdr, &name, &code, &type, &doc, &form, &defun, NULL);
	list_heap(&cdr, name, type, doc, form, defun, NULL);
	Fmt3("~2T~A ~20T~S~%", car, cdr);
	return disassemble_code(ptr, stream, code);
}

static int disassemble_code_operator(Execute ptr, addr stream, addr car, addr cdr)
{
	addr symbol;

	/* execute */
	GetConst(CODE_EXECUTE, &symbol);
	if (car == symbol)
		return disassemble_symbol_execute(ptr, stream, car, cdr);

	/* lambda */
	GetConst(CODE_LAMBDA, &symbol);
	if (car == symbol)
		return disassemble_symbol_lambda(ptr, stream, car, cdr);

	/* lambda-self */
	GetConst(CODE_LAMBDA_SELF, &symbol);
	if (car == symbol)
		return disassemble_symbol_lambda(ptr, stream, car, cdr);

	/* otherwise */
	Fmt3("~2T~A ~20T~S~%", car, cdr);

	return 0;
}

static int disassemble_code_type(Execute ptr, addr stream, addr pos)
{
	struct code_struct *str;

	Fmt1("CODE-BEGIN ~20T");
	str = StructCode(pos);
	if (str->p_control)
		Fmt1(" control");
	if (str->p_return)
		Fmt1(" return");
	if (str->p_push)
		Fmt1(" push");
	if (str->p_argument)
		Fmt1(" argument");
	terpri_stream(stream);

	return 0;
}

static int disassemble_code_body(Execute ptr, addr stream, addr pos)
{
	addr args, car, cdr;
	size_t size, i;

	getargs_code(pos, &args);
	getarray_code(pos, &pos);
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &car);
		getarray(args, i, &cdr);
		GetCar(car, &car);
		Return(disassemble_code_operator(ptr, stream, car, cdr));
	}

	return 0;
}

static int disassemble_code(Execute ptr, addr stream, addr code)
{
	if (code == Nil)
		return 0;
	/* code */
	CheckType(code, LISPTYPE_CODE);
	Return(disassemble_code_type(ptr, stream, code));
	Return(disassemble_code_body(ptr, stream, code));
	Fmt1("CODE-END~%");

	return 0;
}

static int disassemble_interpreted(Execute ptr, addr stream, addr pos)
{
	addr code;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetFunction(pos, &code);
	return disassemble_code(ptr, stream, code);
}

static int disassemble_function(Execute ptr, addr stream, addr pos)
{
	CheckType(stream, LISPTYPE_STREAM);
	CheckType(pos, LISPTYPE_FUNCTION);
	if (compiled_function_p(pos)) {
		Fmt2("Cannot disassemble COMPILED-FUNCTION ~S.~%", pos);
	}
	else {
		Fmt2("INTERPRETED-FUNCTION ~S.~%", pos);
		Return(disassemble_interpreted(ptr, stream, pos));
	}

	return 0;
}

_g int disassemble_common(Execute ptr, addr var)
{
	addr stream, check;

	standard_output_stream(ptr, &stream);
	if (symbolp(var)) {
		getfunction_local(ptr, var, &check);
		if (check == Unbound) {
			getmacro_symbol(var, &check);
			if (check == Unbound) {
				fmte("Invalid argument ~S.", var);
				return 0;
			}
		}
		var = check;
	}

	return disassemble_function(ptr, stream, var);
}


/*
 *  trace
 */
_g void trace_common(addr form, addr env, addr *ret)
{
	addr pos, value, list, add, quote;

	getcdr(form, &form);

	/* all trace name */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_LIST, ret);
		return;
	}

	/* add trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			fmtw("TRACE arguemnt ~S don't set a dotted list.", form, NULL);
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			fmtw("TRACE argument ~S should be a function-name.", pos, NULL);
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse_list_unsafe(&list, list);
	/* (lisp-system::trace-add 'list) */
	GetConst(SYSTEM_TRACE_ADD, &add);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, add, list, NULL);
}


/*
 *  untrace
 */
_g void untrace_common(addr form, addr env, addr *ret)
{
	addr pos, value, list, del, quote;

	getcdr(form, &form);

	/* all delete */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_DEL, &pos);
		list_heap(ret, pos, T, NULL);
		return;
	}

	/* del trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			fmtw("TRACE arguemnt ~S don't set a dotted list.", form, NULL);
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			fmtw("TRACE argument ~S should be a function-name.", pos, NULL);
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse_list_unsafe(&list, list);
	/* (lisp-system::trace-del 'list) */
	GetConst(SYSTEM_TRACE_DEL, &del);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, del, list, NULL);
}

static void defun_trace_function_index(Execute ptr, addr *ret)
{
	addr symbol, pos;

	GetConst(SYSTEM_TRACE_DEPTH, &symbol);
	getspecial_local(ptr, symbol, &pos);
	if (pos == Unbound)
		fixnum_heap(&pos, 1);
	else
		oneplus_integer_common(ptr->local, pos, &pos);
	pushspecial_control(ptr, symbol, pos);
	*ret = pos;
}

static int defun_trace_function(Execute ptr, addr rest)
{
	addr list, name, pos, index, stream;

	/* index */
	getdata_control(ptr, &list);
	GetCons(list, &name, &pos);
	defun_trace_function_index(ptr, &index);
	/* begin */
	trace_output_stream(ptr, &stream);
	cons_heap(&list, name, rest);
	Return(format_stream(ptr, stream, "~&~A: ~S~%", index, list, NULL));
	/* call */
	apply_control(ptr, pos, rest);
	/* end */
	getvalues_list_control_heap(ptr, &list);
	Return(format_stream(ptr, stream, "~&~A: Result => ~S~%", index, list, NULL));

	return 0;
}

static void trace_add_make(Execute ptr, addr name, addr call, addr pos, addr *ret)
{
	addr type, trace;

	compiled_heap(&trace, call);
	setcompiled_rest(trace, p_defun_trace_function);
	gettype_function(pos, &type);
	settype_function(trace, type);
	settrace_function(trace);
	cons_heap(&name, name, pos);
	SetDataFunction(trace, name);
	*ret = trace;
}

static int trace_add_function(Execute ptr, addr name, addr call)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		fmtw("The function ~S is constant.", call, NULL);
		return 1; /* error */
	}

	/* function */
	getfunction_callname_global(call, &pos);
	if (pos == Unbound) {
		fmtw("The function ~S is unbound.", call, NULL);
		return 1; /* error */
	}
	if (tracep_function(pos)) {
		fmtw("The function ~S is already traced.", pos, NULL);
		return 0; /* normal */
	}

	/* trade-add */
	trace_add_make(ptr, name, call, pos, &pos);
	setfunction_callname_global(call, pos);

	return 0; /* normal */
}

static void trace_add_push(Execute ptr, addr name)
{
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	getspecialcheck_local(ptr, symbol, &list);
	pushnew_equal_heap(list, name, &list);
	setspecial_local(ptr, symbol, list);
}

_g void trace_add_common(Execute ptr, addr list, addr *ret)
{
	addr root, name, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		parse_callname_error(&pos, name);
		if (trace_add_function(ptr, name, pos))
			continue;
		cons_heap(&root, name, root);
		trace_add_push(ptr, name);
	}
	nreverse_list_unsafe(ret, root);
}

static void trace_del_object(Execute ptr, addr trace, addr *ret)
{
	GetDataFunction(trace, &trace);
	GetCdr(trace, ret);
}

static int trace_del_function(Execute ptr, addr name, addr call)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		fmtw("The function ~S is constant.", call, NULL);
		return 1; /* error */
	}

	/* function */
	getfunction_callname_global(call, &pos);
	if (pos == Unbound) {
		fmtw("The function ~S is unbound.", call, NULL);
		return 1; /* error */
	}
	if (! tracep_function(pos)) {
		fmtw("The function ~S is not traced.", pos, NULL);
		return 0; /* normal */
	}

	/* trade-del */
	trace_del_object(ptr, pos, &pos);
	setfunction_callname_global(call, pos);

	return 0; /* normal */
}

static void trace_del_remove(Execute ptr, addr name)
{
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	getspecialcheck_local(ptr, symbol, &list);
	if (! delete_list_equal_unsafe(name, list, &list)) {
		fmtw("There is no function ~S in *trace-list*", name, NULL);
		return;
	}
	setspecial_local(ptr, symbol, list);
}

_g void trace_del_common(Execute ptr, addr list, addr *ret)
{
	addr root, name, pos;

	/* all */
	if (list == T) {
		GetConst(SYSTEM_TRACE_LIST, &list);
		getspecialcheck_local(ptr, list, &list);
	}

	/* list */
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		parse_callname_error(&pos, name);
		if (trace_del_function(ptr, name, pos))
			continue;
		cons_heap(&root, name, root);
		trace_del_remove(ptr, name);
	}
	nreverse_list_unsafe(ret, root);
}


/*
 *  step
 */
_g void step_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr eval;

	getcdr(form, &form);
	if (consp_getcons(form, &eval, &form))
		goto error;
	if (form != Nil)
		goto error;

	fmte("TODO", NULL);
	*ret = eval;
	return;

error:
	fmte("STEP argument ~S must be a (eval) form.", form, NULL);
}


/*
 *  initialize
 */
_g void init_environment_code(void)
{
	SetPointerType(rest, defun_trace_function);
}

_g void build_environment_code(void)
{
	addr symbol;
	GetConst(SYSTEM_TRACE_LIST, &symbol);
	SetValueSymbol(symbol, Nil);
}

