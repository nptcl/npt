#include "callname.h"
#include "code_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "env_code.h"
#include "execute_object.h"
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

static int disassemble_code_operator(Execute ptr, addr stream, addr car, addr cdr)
{
	addr symbol;

	/* execute */
	GetConst(CODE_EXECUTE_CONTROL_SET, &symbol);
	if (car == symbol)
		return disassemble_symbol_execute(ptr, stream, car, cdr);

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
	if (str->p_args)
		Fmt1(" argument");
	Return(terpri_stream_(stream));

	return 0;
}

static int disassemble_code_body(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;
	size_t size, i;

	getarray_code(pos, &pos);
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &car);
		GetCons(car, &car, &cdr);
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
	GetCodeFunction(pos, &code);
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

	Return(standard_output_stream_(ptr, &stream));
	if (symbolp(var)) {
		GetFunctionSymbol(var, &check);
		if (check == Unbound) {
			getmacro_symbol(var, &check);
			if (check == Unbound)
				return fmte_("Invalid argument ~S.", var, NULL);
		}
		var = check;
	}

	return disassemble_function(ptr, stream, var);
}


/*
 *  trace
 */
_g int trace_common_(addr form, addr env, addr *ret)
{
	addr pos, value, list, add, quote;

	Return_getcdr(form, &form);

	/* all trace name */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_LIST, ret);
		return 0;
	}

	/* add trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			Return(fmtw_("TRACE arguemnt ~S don't set a dotted list.", form, NULL));
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			Return(fmtw_("TRACE argument ~S should be a function-name.", pos, NULL));
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse(&list, list);
	/* (lisp-system::trace-add 'list) */
	GetConst(SYSTEM_TRACE_ADD, &add);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, add, list, NULL);

	return 0;
}


/*
 *  untrace
 */
_g int untrace_common_(addr form, addr env, addr *ret)
{
	addr pos, value, list, del, quote;

	Return_getcdr(form, &form);

	/* all delete */
	if (form == Nil) {
		GetConst(SYSTEM_TRACE_DEL, &pos);
		list_heap(ret, pos, T, NULL);
		return 0;
	}

	/* del trace */
	for (list = Nil; form != Nil; ) {
		if (! consp(form)) {
			Return(fmtw_("TRACE arguemnt ~S don't set a dotted list.", form, NULL));
			break;
		}
		GetCons(form, &pos, &form);
		if (parse_callname_heap(&value, pos)) {
			Return(fmtw_("TRACE argument ~S should be a function-name.", pos, NULL));
			continue;
		}
		cons_heap(&list, pos, list);
	}
	nreverse(&list, list);
	/* (lisp-system::trace-del 'list) */
	GetConst(SYSTEM_TRACE_DEL, &del);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&list, quote, list, NULL);
	list_heap(ret, del, list, NULL);

	return 0;
}


/*
 *  trace-add
 */
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
	addr list, name, pos, index, stream, values;
	size_t size;

	/* index */
	getdata_control(ptr, &list);
	GetCons(list, &name, &pos);
	defun_trace_function_index(ptr, &index);
	/* begin */
	Return(trace_output_stream_(ptr, &stream));
	cons_heap(&list, name, rest);
	Return(format_stream(ptr, stream, "~&~A: ~S~%", index, list, NULL));
	/* call */
	apply_control(ptr, pos, rest);
	save_values_control(ptr, &values, &size);
	/* end */
	getvalues_list_control_heap(ptr, &list);
	Return(format_stream(ptr, stream, "~&~A: Result => ~S~%", index, list, NULL));
	restore_values_control(ptr, values, size);

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

static int trace_add_function_(Execute ptr, addr name, addr call, int *ret)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		Return(fmtw_("The function ~S is constant.", call, NULL));
		return Result(ret, 1); /* error */
	}

	/* function */
	getglobal_callname(call, &pos);
	if (pos == Unbound) {
		Return(fmtw_("The function ~S is unbound.", call, NULL));
		return Result(ret, 1); /* error */
	}
	if (tracep_function(pos)) {
		Return(fmtw_("The function ~S is already traced.", pos, NULL));
		return Result(ret, 0); /* normal */
	}

	/* trade-add */
	trace_add_make(ptr, name, call, pos, &pos);
	Return(setglobal_callname_(call, pos));

	return Result(ret, 0); /* normal */
}

static int trace_add_push_(Execute ptr, addr name)
{
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(pushnew_equal_heap_(list, name, &list));
	setspecial_local(ptr, symbol, list);

	return 0;
}

_g int trace_add_common_(Execute ptr, addr list, addr *ret)
{
	int check;
	addr root, name, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(parse_callname_error_(&pos, name));
		Return(trace_add_function_(ptr, name, pos, &check));
		if (check)
			continue;
		cons_heap(&root, name, root);
		Return(trace_add_push_(ptr, name));
	}
	nreverse(ret, root);

	return 0;
}


/*
 *  trace-del
 */
static void trace_del_object(Execute ptr, addr trace, addr *ret)
{
	GetDataFunction(trace, &trace);
	GetCdr(trace, ret);
}

static int trace_del_function_(Execute ptr, addr name, addr call, int *ret)
{
	addr pos;

	/* callname */
	if (GetStatusReadOnly(call)) {
		Return(fmtw_("The function ~S is constant.", call, NULL));
		return Result(ret, 1); /* error */
	}

	/* function */
	getglobal_callname(call, &pos);
	if (pos == Unbound) {
		Return(fmtw_("The function ~S is unbound.", call, NULL));
		return Result(ret, 1); /* error */
	}
	if (! tracep_function(pos)) {
		Return(fmtw_("The function ~S is not traced.", pos, NULL));
		return Result(ret, 0); /* normal */
	}

	/* trade-del */
	trace_del_object(ptr, pos, &pos);
	Return(setglobal_callname_(call, pos));

	return Result(ret, 0); /* normal */
}

static int trace_del_remove_(Execute ptr, addr name)
{
	int check;
	addr symbol, list;

	GetConst(SYSTEM_TRACE_LIST, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	Return(delete_list_equal_unsafe_(name, list, &list, &check));
	if (! check)
		return fmtw_("There is no function ~S in *trace-list*", name, NULL);
	setspecial_local(ptr, symbol, list);

	return 0;
}

_g int trace_del_common_(Execute ptr, addr list, addr *ret)
{
	int check;
	addr root, name, pos;

	/* all */
	if (list == T) {
		GetConst(SYSTEM_TRACE_LIST, &list);
		Return(getspecialcheck_local_(ptr, list, &list));
	}

	/* list */
	for (root = Nil; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(parse_callname_error_(&pos, name));
		Return(trace_del_function_(ptr, name, pos, &check));
		if (check)
			continue;
		cons_heap(&root, name, root);
		Return(trace_del_remove_(ptr, name));
	}
	nreverse(ret, root);

	return 0;
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

