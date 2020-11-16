#undef LISP_DEBUG_TRACE

#include "callname.h"
#include "clos_generic.h"
#include "code_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_callbind.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute.h"
#include "function.h"
#include "gc.h"
#include "hold.h"
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
void output_timestamp(void)
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
#define LispForceGc(x)			gcsync((x), GcMode_Full)
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
	if ((lisp_gcsync != GcMode_Off) || ((LISP_GC_SYNC % ControlCounter) == 0)) { \
		LispGcTrace(); \
		gcsync((x), GcMode_Default); \
	} \
}
#else
#define LispGcCheck(x)			{ \
	if (lisp_gcsync != GcMode_Off) { \
		LispGcTrace(); \
		gcsync((x), GcMode_Default); \
	} \
}
#endif

#ifdef LISP_DEBUG_TRACE
#define TraceControl(str, x) ((str)->trace = x)
#else
#define TraceControl(str, x)
#endif

static int runcode_execute_(Execute ptr, addr code)
{
	addr control, throw_control;
	struct control_struct *str;
	struct code_struct *body;
	struct code_value *sys, *bind;
	size_t point, *index;

	/* code */
	CheckType(code, LISPTYPE_CODE);
	control = ptr->control;
	body = StructCode(code);
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
	bind = sys + point;
	if (bind->call == NULL) {
		LispGcCheck(ptr);
		LispEndControl("EXECUTE", control);
		return 0;
	}
	OutputTrace(control, point);
	if ((bind->call)(ptr, bind->value) == 0)
		goto loop;

	/* throw */
	throw_control = ptr->throw_control;
	if (throw_control && (control != throw_control)) {
		Check(runcode_control_check(ptr), "throw_control error");
		return 1;
	}
	ptr->throw_value = throw_normal;
	ptr->throw_handler = NULL;
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

int runcode_control_(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str;

	CheckType(code, LISPTYPE_CODE);
	str = StructCode(code);
	/* no-control */
	if (str->p_control == 0)
		return runcode_execute_(ptr, code);

	/* push-control */
	if (str->p_args)
		push_args_control(ptr, &control);
	else
		push_control(ptr, &control);

	/* execute */
	(void)runcode_execute_(ptr, code);
	return pop_control_(ptr, control);
}


/*
 *  (call ...) execute
 */
static int execute_no_control_(Execute ptr, addr call)
{
	addr value;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");

	/* generic function */
	if (GetType(call) != LISPTYPE_FUNCTION) {
		GetControl(ptr->control, Control_Cons, &value);
		return closrun_execute_(ptr, call, value);
	}

	/* closure */
	GetDataFunction(call, &value);
	if (value != Unbound)
		setdata_control(ptr, value);

	/* compiled function */
	if (StructFunction(call)->compiled)
		return call_compiled_function(ptr, call);

	/* interpreted function */
	GetCodeFunction(call, &value);
	return runcode_control_(ptr, value);
}

int execute_control(Execute ptr, addr call)
{
	addr control;

	push_args_control(ptr, &control);
	execute_no_control_(ptr, call);
	return pop_control_(ptr, control);
}


/*
 *  checkargs
 */
static int checkargs_var_(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 0, &array); /* var */
	while (array != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(*args, &value, args);
		GetCons(array, &type, &array);
		Return(call_typep_asterisk_error_(ptr, value, type));
	}

	return 0;
}

static int checkargs_opt_(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 1, &array); /* opt */
	while (*args != Nil && array != Nil) {
		Return_getcons(*args, &value, args);
		GetCons(array, &type, &array);
		Return(call_typep_asterisk_error_(ptr, value, type));
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

static int checkargs_restkey_(Execute ptr, addr array, addr args)
{
	int keyvalue;
	addr rest, key, value, type;

	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);
	if (find_keyword_allow_other_keys(args))
		key = T;
	if (rest == Nil && key == Nil) {
		if (args != Nil)
			return fmte_("Too many argument.", NULL);
	}
	for (keyvalue = 0; args != Nil; keyvalue = (! keyvalue)) {
		Return_getcons(args, &value, &args);
		/* &rest */
		if (rest != Nil) {
			Return(call_typep_asterisk_error_(ptr, value, rest));
		}
		/* &key */
		if (key != Nil) {
			contargs_key(ptr, keyvalue, key, &type);
			Return(call_typep_asterisk_error_(ptr, value, type));
		}
	}

	/* error check */
	if (key != Nil && keyvalue)
		return fmte_("Invalid keyword argument.", NULL);

	return 0;
}

static int checkargs_execute_(Execute ptr, addr array, addr args)
{
	LocalRoot local;
	LocalStack stack;

	/* var */
	Return(checkargs_var_(ptr, array, &args));
	if (args == Nil)
		return 0;
	/* opt */
	Return(checkargs_opt_(ptr, array, &args));
	if (args == Nil)
		return 0;
	/* rest, key */
	local = ptr->local;
	push_local(local, &stack);
	Return(checkargs_restkey_(ptr, array, args));
	rollback_local(local, stack);

	return 0;
}

static int checkargs_control_(Execute ptr, addr call, addr args)
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
	return checkargs_execute_(ptr, type, args);
}


/*
 *  apply
 */
static int apply_no_control_(Execute ptr, addr call, addr list)
{
	addr value;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");

	/* generic function */
	if (GetType(call) != LISPTYPE_FUNCTION)
		return closrun_execute_(ptr, call, list);

	/* args */
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	/* closure */
	GetDataFunction(call, &value);
	if (value != Unbound)
		setdata_control(ptr, value);

	/* compiled function */
	if (StructFunction(call)->compiled) {
		Return(checkargs_control_(ptr, call, list));
		return call_compiled_function(ptr, call);
	}

	/* interpreted function */
	GetCodeFunction(call, &value);
	return runcode_control_(ptr, value);
}

int apply_control(Execute ptr, addr call, addr list)
{
	addr control;

	push_control(ptr, &control);
	(void)apply_no_control_(ptr, call, list);
	return pop_control_(ptr, control);
}

int applya_control(Execute ptr, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &list, va);
	va_end(va);
	(void)apply_control(ptr, call, list);
	return pop_control_(ptr, control);
}

int funcall_control(Execute ptr, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	list_stdarg_alloc(ptr->local, &list, va);
	va_end(va);
	(void)apply_no_control_(ptr, call, list);
	return pop_control_(ptr, control);
}

int call_control(Execute ptr, addr args)
{
	addr call;

	Check(args == Nil, "argument error");
	GetCons(args, &call, &args);
	return apply_control(ptr, call, args);
}


/*
 *  C language
 */
static int callclang_function_(Execute ptr, addr *ret, addr call)
{
	Check(call == Unbound, "type error");
	switch (GetType(call)) {
		case LISPTYPE_SYMBOL:
			return getfunction_global_(call, ret);

		case LISPTYPE_CALLNAME:
			return getglobalcheck_callname_(call, ret);

		case LISPTYPE_CLOS:
		case LISPTYPE_FUNCTION:
			return Result(ret, call);

		default:
			*ret = Nil;
			return fmte_("The object ~S cannot execute.", call, NULL);
	}
}

static int callclang_apply_no_control_(Execute ptr, addr call, addr list)
{
	Return(callclang_function_(ptr, &call, call));
	return apply_no_control_(ptr, call, list);
}

int callclang_apply(Execute ptr, addr *ret, addr call, addr list)
{
	addr control;

	push_control(ptr, &control);
	(void)callclang_apply_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}

int callclang_applya(Execute ptr, addr *ret, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &list, va);
	va_end(va);

	(void)callclang_apply_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}

int callclang_funcall(Execute ptr, addr *ret, addr call, ...)
{
	addr control, list;
	va_list va;

	push_control(ptr, &control);
	va_start(va, call);
	list_stdarg_alloc(ptr->local, &list, va);
	va_end(va);

	(void)callclang_apply_no_control_(ptr, call, list);
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);

	return 0;
}

