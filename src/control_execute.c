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

#undef LISP_DEBUG_TRACE

/*
 *  runcode
 */
#ifdef LISP_DEBUG_TRACE
#include <sys/time.h>
static unsigned prevtime = 0;
_g void output_timestamp(void)
{
	struct timeval now;
	gettimeofday(&now, NULL);
	printf("[TIME] %ld.%ld-%ld:  ",
			(long)now.tv_sec,
			(long)now.tv_usec,
			(long)now.tv_usec - prevtime);
	prevtime = now.tv_usec;
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

#ifdef LISP_GC_SYNC
#define LispGcCheck(x)			{ \
	if (lisp_gcsync || ((LISP_GC_SYNC % ControlCounter) == 0)) { \
		gcsync(x); \
	} \
}
#else
#define LispGcCheck(x)			{ \
	if (lisp_gcsync) { \
		gcsync(x); \
	} \
}
#endif

static int runcode_execute(Execute ptr, addr code)
{
	addr control, pos, throw;
	addr *args;
	pointer id;
	callbind_code bind;
	struct control_struct *str;
	const pointer *call;
	size_t point, *index;

	CheckType(code, LISPTYPE_CODE);
	control = ptr->control;
	call = getcalltype_code(code);
	str = StructControl(control);
	str->point = 0;
	str->call = call;
#ifdef LISP_DEBUG_TRACE
	str->trace = code;
#endif
	SetControl(control, Control_Args, code);
	getargs_code(code, &pos);
	args = (addr *)PtrArrayA4(pos);
	index = &(str->point);

loop:
	/* counter */
	ControlCounter++;
	LispForceGc(ptr);

	/* execute */
	point = (*index)++;
	id = call[point];
	if (id == 0) {
		LispGcCheck(ptr);
		return 0;
	}
	OutputTrace(control, point);
	GetPointer_code(id, &bind);
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
	int check, throw_point_p;
	addr throw_control;
	size_t throw_point;

	throw_control = ptr->throw_control;
	throw_point = ptr->throw_point;
	throw_point_p = ptr->throw_point_p;
	ptr->throw_control = NULL;
	ptr->throw_point = 0;
	ptr->throw_point_p = 0;
	/* disable copy */
	ptr->disable_copy_p = 1;
	check = free_control_(ptr, control);
	ptr->disable_copy_p = 0;
	/* new event */
	if (check)
		return 1;
	/* success */
	ptr->throw_control = throw_control;
	ptr->throw_point = throw_point;
	ptr->throw_point_p = throw_point_p;
	return 1;
}

_g int runcode_control(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str1;
	struct control_struct *str2;

	CheckType(code, LISPTYPE_CODE);
	str1 = StructCode(code);
	/* no-control */
	if (str1->p_control == 0)
		return runcode_execute(ptr, code);

	/* push-control */
	str2 = str1->p_argument?
		copy_argument_control(ptr):
		push_control(ptr);
	control = ptr->control;
	str2->p_return = str1->p_return;
	str2->p_push = str1->p_push;

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
	addr control, pos;
	addr *args;
	pointer id;
	callbind_code bind;
	const pointer *call;
	size_t point;
#ifdef LISP_DEBUG_TRACE
	struct control_struct *str;
#endif

	CheckType(code, LISPTYPE_CODE);
	control = ptr->control;
	call = getcalltype_code(code);
#ifdef LISP_DEBUG_TRACE
	str = StructControl(control);
	str->trace = code;
#endif
	SetControl(control, Control_Args, code);
	getargs_code(code, &pos);
	args = (addr *)PtrArrayA4(pos);
	point = 0;

loop:
	/* counter */
	ControlCounter++;
	LispForceGc(ptr);

	/* execute */
	id = call[point];
	if (id == 0) {
		LispGcCheck(ptr);
		return 0;
	}
	OutputTrace(control, point);
	GetPointer_code(id, &bind);
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
	struct code_struct *str1;
	struct control_struct *str2;

	CheckType(code, LISPTYPE_CODE);
	str1 = StructCode(code);
	Check(str1->p_control == 0, "p_control error.");

	/* push-control */
	str2 = str1->p_argument?
		copy_argument_control(ptr):
		push_control(ptr);
	control = ptr->control;
	str2->p_return = str1->p_return;
	str2->p_push = str1->p_push;

	/* execute */
	if (runcode_simple(ptr, code))
		return runcode_free(ptr, control);
	else
		return free_control_(ptr, control);
}


/*
 *  runcode-tagbody
 */
_g int runcode_tagbody(Execute ptr, addr code);


/*
 *  runcode-throw
 */
_g int runcode_throw(Execute ptr, addr code);


/*
 *  execute-switch
 */
static int runcode_rollback(Execute ptr, addr control)
{
	int check;

	ptr->disable_copy_p = 1;
	check = rollback_control_(ptr, control);
	ptr->disable_copy_p = 0;

	return check;
}

static int runcode_begin(Execute ptr, addr code)
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
		return runcode_rollback(ptr, control);
	}

	/* normal */
	throw_switch(&jump);
	return 0;
}

_g int runcode_switch(Execute ptr, addr code)
{
	addr control;
	struct code_struct *str1;
	struct control_struct *str2;

	CheckType(code, LISPTYPE_CODE);
	str1 = StructCode(code);
	/* no-control */
	if (str1->p_control == 0)
		return runcode_begin(ptr, code);

	/* push-control */
	str2 = str1->p_argument?
		copy_argument_control(ptr):
		push_control(ptr);
	control = ptr->control;
	str2->p_return = str1->p_return;
	str2->p_push = str1->p_push;

	/* execute */
	if (runcode_begin(ptr, code))
		return runcode_free(ptr, control);
	else
		return free_control_(ptr, control);
}


/*
 *  interface
 */
static int execute_function_object(Execute ptr, addr pos)
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
		GetFunction(pos, &value);
		return runcode_control(ptr, value);
	}
}

static void function_recursive_control(Execute ptr, addr pos)
{
	addr name;
	GetNameFunction(pos, &name);
	pushcallname_control(ptr, name, pos);
}

static void pushlexical_closure_control(Execute ptr, addr pos, addr value)
{
	addr control, list, snapshot;

	control = ptr->control;
	GetControl(control, Control_Lexical, &list);
	if (getplist(list, pos, &snapshot)) {
		Check(stack_check_control(ptr), "stack error");
		snapshot_lexical_local(ptr, pos, &snapshot);
		if (setplist_local(ptr->local, list, pos, snapshot, &list))
			SetControl(control, Control_Lexical, list);
	}
	pushlexical_closure_unsafe(ptr, pos, value);
}

static void function_closure_control(Execute ptr, addr pos)
{
	addr key, value, list;

	/* value */
	GetClosureValueFunction(pos, &list);
	while (list != Nil) {
		GetCons(list, &key, &list);
		GetCons(key, &key, &value);
		pushlexical_closure_control(ptr, key, value);
	}

	/* function / setf */
	GetClosureFunctionFunction(pos, &list);
	while (list != Nil) {
		GetCons(list, &key, &list);
		GetCons(key, &key, &value);
		pushcallname_control(ptr, key, value);
	}

	/* tagbody */
	GetClosureTagbodyFunction(pos, &list);
	while (list != Nil) {
		GetCons(list, &key, &list);
		GetCons(key, &key, &value);
		pushtagbody_control(ptr, key, value);
	}

	/* block */
	GetClosureBlockFunction(pos, &list);
	while (list != Nil) {
		GetCons(list, &value, &list);
		pushblock_control(ptr, value);
	}
}

static int execute_function_control(Execute ptr, addr pos)
{
	int check1, check2;
	addr control;
	struct function_struct *str;

	str = StructFunction(pos);
	check1 = str->recursive;
	check2 = str->closure;
	if (check1 == 0 && check2 == 0)
		return execute_function_object(ptr, pos);

	/* closure or recursive */
	push_argument_control(ptr, &control);
	if (check1)
		function_recursive_control(ptr, pos);
	if (check2)
		function_closure_control(ptr, pos);
	if (execute_function_object(ptr, pos))
		return 1;

	return free_control_(ptr, control);
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
			getfunctioncheck_local(ptr, call, ret);
			break;

		case LISPTYPE_CALLNAME:
			getfunctioncheck_callname_local(ptr, call, ret);
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
	push_close_control(ptr, &control);
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

