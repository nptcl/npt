#undef LISP_DEBUG_TRACE

#include "callname.h"
#include "clos_generic.h"
#include "code_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_callbind.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute.h"
#include "function.h"
#include "gc.h"
#include "lambda.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

/*
 *  runcode
 */
#ifdef LISP_DEBUG_TRACE
#include <sys/time.h>
struct timeval prevtime = {0, 0};
_g void output_timestamp(void)
{
	struct timeval now, diff;
	gettimeofday(&now, NULL);
	timersub(&now, &prevtime, &diff);
	printf("[TRACE] %6ld: ", diff.tv_usec);
	prevtime = now;
}

static void output_trace(addr control, size_t point)
{
	addr code;
	struct control_struct *str;

	output_timestamp();
	str = StructControl(control);
	code = str->trace;
	getarray_code(code, &code);
	GetArrayA4(code, point, &code);
	infoprint(code);
}
#define OutputTrace(a,b) output_trace(a,b)
#else
#define OutputTrace(a,b)
#endif


/*
 *  runcode-control
 */
#ifdef LISP_DEBUG
static int runcode_control_check(Execute ptr)
{
	addr control, check;

	control = ptr->control;
	check = ptr->throw_control;
	while (control != Nil) {
		if (control == check)
			return 0;
		GetControl(control, Control_Next, &control);
	}

	return 1;
}
#endif

#ifdef LISP_DEBUG_FORCE_GC
#define LispForceGc(x)			gcsync(x)
#else
#define LispForceGc(x)			;
#endif

#ifdef LISP_DEBUG_TRACE
#define LispGcTrace()			printf("[TRACE] GC-SYNC\n")
#else
#define LispGcTrace()			;
#endif

#ifdef LISP_DEBUG_TRACE
#define LispBeginControl(x,y)	printf("[TRACE] BEGIN-%s: %p\n", (x), (void *)(y))
#define LispEndControl(x,y)		printf("[TRACE] END-%s: %p\n", (x), (void *)(y))
#else
#define LispBeginControl(x,y)	;
#define LispEndControl(x,y)		;
#endif

#ifdef LISP_GC_SYNC
#define LispGcCheck(x)			{ \
	if (lisp_gcsync || ((LISP_GC_SYNC % ControlCounter) == 0)) { \
		LispGcTrace(); \
		gcsync(x); \
	} \
}
#else
#define LispGcCheck(x)			{ \
	if (lisp_gcsync) { \
		LispGcTrace(); \
		gcsync(x); \
	} \
}
#endif

#ifdef LISP_DEBUG_TRACE
#define TraceControl(str, x) ((str)->trace = x)
#else
#define TraceControl(str, x)
#endif

static int runcode_execute(Execute ptr, addr code)
{
	addr control, throw;
	addr *args;
	callbind_code *sys, bind;
	struct control_struct *str;
	struct code_struct *body;
	size_t point, *index;

	/* code */
	CheckType(code, LISPTYPE_CODE);
	control = ptr->control;
	body = StructCode(code);
	args = body->args;
	sys = body->sys;

	/* control */
	str = StructControl(control);
	str->point = 0;
	index = &(str->point);

	LispBeginControl("EXECUTE", control);
loop:
	/* counter */
	ControlCounter++;
	LispForceGc(ptr);
	TraceControl(str, code);

	/* execute */
	point = (*index)++;
	bind = sys[point];
	if (bind == NULL) {
		LispGcCheck(ptr);
		LispEndControl("EXECUTE", control);
		return 0;
	}
	OutputTrace(control, point);
	if ((*bind)(ptr, args[point]) == 0)
		goto loop;

	/* throw */
	throw = ptr->throw_control;
	if (throw && (control != throw)) {
		Check(runcode_control_check(ptr), "throw_control error");
		return 1;
	}
	ptr->throw_control = NULL;

	/* block */
	if (ptr->throw_point_p == 0)
		return 0;
	ptr->throw_point_p = 0;

	/* tagbody / goto */
	str->point = ptr->throw_point;
	ptr->throw_point = 0;
	goto loop;
}

static int runcode_free(Execute ptr, addr control)
{
	int throw_point_p;
	addr throw_control;
	size_t throw_point;

	throw_control = ptr->throw_control;
	throw_point = ptr->throw_point;
	throw_point_p = ptr->throw_point_p;
	ptr->throw_control = NULL;
	ptr->throw_point = 0;
	ptr->throw_point_p = 0;
	Return(free_control_(ptr, control));
	ptr->throw_control = throw_control;
	ptr->throw_point = throw_point;
	ptr->throw_point_p = throw_point_p;

	return 1;
}

_g int runcode_control(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str;

	CheckType(code, LISPTYPE_CODE);
	str = StructCode(code);
	/* no-control */
	if (str->p_control == 0)
		return runcode_execute(ptr, code);

	/* push-control */
	if (str->p_args)
		push_args_control(ptr, &control);
	else
		push_new_control(ptr, &control);

	/* execute */
	if (runcode_execute(ptr, code))
		return runcode_free(ptr, control);
	else
		return free_control_(ptr, control);
}


/*
 *  runcode-simple
 */
_g int runcode_simple(Execute ptr, addr code)
{
	addr *args;
	callbind_code *sys, bind;
	struct code_struct *body;
	size_t point;
#ifdef LISP_DEBUG_TRACE
	addr control;
	struct control_struct *str;
#endif

	/* code */
	CheckType(code, LISPTYPE_CODE);
	body = StructCode(code);
	args = body->args;
	sys = body->sys;

	/* control */
#ifdef LISP_DEBUG_TRACE
	control = ptr->control;
	str = StructControl(control);
#endif
	point = 0;

	LispBeginControl("SIMPLE", control);
loop:
	/* counter */
	ControlCounter++;
	LispForceGc(ptr);
	TraceControl(str, code);

	/* execute */
	bind = sys[point];
	if (bind == 0) {
		LispGcCheck(ptr);
		LispEndControl("SIMPLE", control);
		return 0;
	}
	OutputTrace(control, point);
	Return((*bind)(ptr, args[point]));
	point++;
	goto loop;
}


/*
 *  runcode-normal
 */
_g int runcode_normal(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str;

	CheckType(code, LISPTYPE_CODE);
	str = StructCode(code);
	Check(str->p_control == 0, "p_control error.");

	/* push-control */
	if (str->p_args)
		push_args_control(ptr, &control);
	else
		push_new_control(ptr, &control);

	/* execute */
	if (runcode_simple(ptr, code))
		return runcode_free(ptr, control);
	else
		return free_control_(ptr, control);
}


/*
 *  execute-switch  (handler-case, restart-case)
 */
static int runcode_switch_execute(Execute ptr, addr code)
{
	int check;
	addr control;
	codejump jump;

	/* execute */
	check = 0;
	control = ptr->control;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		check = runcode_execute(ptr, code);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* handler */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control))
			return 1;
		return rollback_control_(ptr, control);
	}

	/* normal */
	throw_switch(&jump);
	return 0;
}

_g int runcode_switch(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str;

	CheckType(code, LISPTYPE_CODE);
	str = StructCode(code);
	/* no-control */
	if (str->p_control == 0)
		return runcode_switch_execute(ptr, code);

	/* push-control */
	if (str->p_args)
		push_args_control(ptr, &control);
	else
		push_new_control(ptr, &control);

	/* execute */
	if (runcode_switch_execute(ptr, code))
		return runcode_free(ptr, control);
	else
		return free_control_(ptr, control);
}


/*
 *  interface
 */
static int execute_function_control(Execute ptr, addr pos)
{
	addr value;

	/* control data */
	GetDataFunction(pos, &value);
	if (value != Unbound)
		setdata_control(ptr, value);
	/* execute */
	if (StructFunction(pos)->compiled) {
		return call_compiled_function(ptr, pos);
	}
	else {
		GetCodeFunction(pos, &value);
		return runcode_control(ptr, value);
	}
}

_g int execute_control(Execute ptr, addr call)
{
	addr args;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");
	switch (GetType(call)) {
		case LISPTYPE_FUNCTION:
			return execute_function_control(ptr, call);

		case LISPTYPE_CLOS:
			GetControl(ptr->control, Control_Cons, &args);
			return closrun_execute(ptr, call, args);

		default:
			Abort("type error");
			return 1;
	}
}

static int checkargs_var(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 0, &array); /* var */
	while (array != Nil) {
		if (*args == Nil)
			fmte("Too few argument.", NULL);
		getcons(*args, &value, args);
		GetCons(array, &type, &array);
		if (typep_asterisk_error(ptr, value, type))
			return 1;
	}

	return 0;
}

static int checkargs_opt(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 1, &array); /* opt */
	while (*args != Nil && array != Nil) {
		getcons(*args, &value, args);
		GetCons(array, &type, &array);
		if (typep_asterisk_error(ptr, value, type))
			return 1;
	}

	return 0;
}

static void contargs_keyvalue(LocalRoot local, int keyvalue, addr cons, addr *ret)
{
	if (keyvalue == 0) {
		/* name */
		GetCar(cons, &cons);
		type_eql_local(local, cons, ret);
	}
	else {
		/* type */
		GetCdr(cons, ret);
	}
}

static void contargs_key(Execute ptr, int keyvalue, addr cons, addr *ret)
{
	LocalRoot local;
	addr pos, array;
	size_t size, i;

	/* &allow-other-keys */
	if (cons == T) {
		if (keyvalue)
			GetTypeTable(ret, T);
		else
			GetTypeTable(ret, Symbol);
		return;
	}

	/* &key */
	local = ptr->local;
	size = length_list_unsafe(cons);
	if (size == 1) {
		GetCar(cons, &pos);
		contargs_keyvalue(local, keyvalue, pos, ret);
		return;
	}

	/* or */
	vector4_local(local, &array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		contargs_keyvalue(local, keyvalue, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static int checkargs_restkey(Execute ptr, addr array, addr args)
{
	int keyvalue;
	addr rest, key, value, type;

	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);
	if (find_keyword_allow_other_keys(args))
		key = T;
	if (rest == Nil && key == Nil) {
		if (args != Nil)
			fmte("Too many argument.", NULL);
	}
	for (keyvalue = 0; args != Nil; keyvalue = (! keyvalue)) {
		getcons(args, &value, &args);
		/* &rest */
		if (rest != Nil) {
			if (typep_asterisk_error(ptr, value, rest))
				return 1;
		}
		/* &key */
		if (key != Nil) {
			contargs_key(ptr, keyvalue, key, &type);
			if (typep_asterisk_error(ptr, value, type))
				return 1;
		}
	}

	/* error check */
	if (key != Nil && keyvalue)
		fmte("Invalid keyword argument.", NULL);

	return 0;
}

static int checkargs_execute(Execute ptr, addr array, addr args)
{
	LocalRoot local;
	LocalStack stack;

	/* var */
	if (checkargs_var(ptr, array, &args))
		return 1;
	if (args == Nil)
		return 0;
	/* opt */
	if (checkargs_opt(ptr, array, &args))
		return 1;
	if (args == Nil)
		return 0;
	/* rest, key */
	local = ptr->local;
	push_local(local, &stack);
	if (checkargs_restkey(ptr, array, args))
		return 1;
	rollback_local(local, stack);

	return 0;
}

static int checkargs_control(Execute ptr, addr call, addr args)
{
	addr type;

	/* asterisk check */
	gettype_function(call, &type);
	if (type == Nil || type_asterisk_p(type))
		return 0;
	Check(! type_function_p(type), "type error");
	GetArrayType(type, 0, &type); /* args */
	if (type_asterisk_p(type))
		return 0;

	/* type check */
	return checkargs_execute(ptr, type, args);
}

static int apply_function_control(Execute ptr, addr call, addr args)
{
	if (checkargs_control(ptr, call, args))
		return 1;
	SetControl(ptr->control, Control_Cons, args);
	SetControl(ptr->control, Control_ConsTail, Nil);
	return execute_function_control(ptr, call);
}

_g int apply_control(Execute ptr, addr call, addr args)
{
	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");
	switch (GetType(call)) {
		case LISPTYPE_FUNCTION:
			return apply_function_control(ptr, call, args);

		case LISPTYPE_CLOS:
			return closrun_execute(ptr, call, args);

		default:
			Abort("type error");
			return 1;
	}
}

_g int applya_control(Execute ptr, addr call, ...)
{
	addr list;
	va_list args;

	va_start(args, call);
	lista_stdarg_alloc(ptr->local, &list, args);
	va_end(args);
	return apply_control(ptr, call, list);
}

_g int funcall_control(Execute ptr, addr call, ...)
{
	addr list;
	va_list args;

	va_start(args, call);
	list_alloc_stdarg(ptr->local, &list, args);
	va_end(args);
	return apply_control(ptr, call, list);
}

_g int call_control(Execute ptr, addr args)
{
	addr call;

	Check(args == Nil, "argument error");
	GetCons(args, &call, &args);
	return apply_control(ptr, call, args);
}


/*
 *  C language
 */
static void callclang_function(Execute ptr, addr *ret, addr call)
{
	Check(call == Unbound, "type error");
	switch (GetType(call)) {
		case LISPTYPE_SYMBOL:
			getfunction_global(call, ret);
			break;

		case LISPTYPE_CALLNAME:
			getglobalcheck_callname(call, ret);
			break;

		case LISPTYPE_CLOS:
		case LISPTYPE_FUNCTION:
			*ret = call;
			break;

		default:
			fmte("The object ~S cannot execute.", call, NULL);
			break;
	}
}

_g int callclang_apply(Execute ptr, addr *ret, addr call, addr cons)
{
	addr control;
	LocalHold hold;

	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	/* code */
	Return(apply_control(ptr, call, cons));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int callclang_applya(Execute ptr, addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr list;
	va_list args;

	local = ptr->local;
	push_local(local, &stack);

	va_start(args, call);
	lista_stdarg_alloc(ptr->local, &list, args);
	va_end(args);

	Return(callclang_apply(ptr, ret, call, list));
	rollback_local(local, stack);

	return 0;
}

_g int callclang_funcall(Execute ptr, addr *ret, addr call, ...)
{
	addr list;
	va_list args;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);

	va_start(args, call);
	list_alloc_stdarg(ptr->local, &list, args);
	va_end(args);

	Return(callclang_apply(ptr, ret, call, list));
	rollback_local(local, stack);

	return 0;
}

