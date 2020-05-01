#include <stdarg.h>
#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "equal.h"
#include "execute.h"
#include "function.h"
#include "gc.h"
#include "restart.h"
#include "symbol.h"

/*
 *  arguments
 */
_g void setargs_nil_control(Execute ptr)
{
	addr root;

	root = ptr->control;
	Check(root == Nil, "control error");
	SetControl(root, Control_Cons, Nil);
	SetControl(root, Control_ConsTail, Nil);
}

_g void setargs_va_control(Execute ptr, ...)
{
	addr pos;
	va_list args;

	setargs_nil_control(ptr);
	va_start(args, ptr);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		pushargs_control(ptr, pos);
	}
	va_end(args);
}

_g void pushargs_list_control(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushargs_control(ptr, pos);
	}
}

_g void setargs_list_control(Execute ptr, addr list)
{
	setargs_nil_control(ptr);
	pushargs_list_control(ptr, list);
}

_g void pushargs_control(Execute ptr, addr value)
{
	addr root, cons, next;

	root = ptr->control;
	Check(root == Nil, "control error");
	GetControl(root, Control_ConsTail, &cons);
	if (cons == Nil) {
		conscar_local(ptr->local, &cons, value);
		SetControl(root, Control_Cons, cons);
		SetControl(root, Control_ConsTail, cons);
	}
	else {
		conscar_local(ptr->local, &next, value);
		SetCdr(cons, next);
		SetControl(root, Control_ConsTail, next);
	}
}

_g void getargs_control(Execute ptr, size_t index, addr *ret)
{
	addr root;

	root = ptr->control;
	GetControl(root, Control_Cons, &root);
	getnth_unbound_unsafe(root, index, ret);
}

_g void getargs_tail_control(Execute ptr, addr *ret)
{
	addr cons;

	GetControl(ptr->control, Control_ConsTail, &cons);
	GetCar(cons, ret);
}

_g void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret)
{
	addr root;

	root = ptr->control;
	GetControl(root, Control_Cons, &root);
	getnthcdr_unsafe(root, index, ret);
}

_g void getargs_list_control_heap(Execute ptr, size_t index, addr *ret)
{
	addr cons;

	getargs_list_control_unsafe(ptr, index, &cons);
	copy_list_heap_unsafe(ret, cons);
}


/*
 *  values
 */
static void pushvalues_dynamic(addr root, struct control_struct *str, addr value)
{
	addr cons;

	GetControl(root, Control_Result, &cons);
	if (GetStatusDynamic(value)) {
		str->p_dynamic = 1;
		conscdr_heap(&cons, cons);
		SetCar_force(cons, value);
	}
	else {
		cons_heap(&cons, value, cons);
	}
	SetControl(root, Control_Result, cons);
}

static void pushvalues_unsafe(Execute ptr, addr value)
{
	addr root;
	struct control_struct *str;
	size_t size;

	root = ptr->control;
	Check(root == Nil, "control error");
	str = StructControl(root);
	size = str->sizer;
	if (size < ControlSize_Result)
		SetResultControl(root, size, value);
	else
		pushvalues_dynamic(root, str, value);
	str->sizer++;
}

static void nreverse_values_unsafe(Execute ptr)
{
	addr list;

	GetControl(ptr->control, Control_Result, &list);
	nreverse_list_unsafe(&list, list);
	SetControl(ptr->control, Control_Result, list);
}

_g void setresult_control(Execute ptr, addr value)
{
	setvalues_nil_control(ptr);
	pushvalues_unsafe(ptr, value);
}

_g void setbool_control(Execute ptr, int value)
{
	setresult_control(ptr, value? T: Nil);
}

_g void setvalues_control(Execute ptr, ...)
{
	addr pos;
	va_list args;

	setvalues_nil_control(ptr);
	va_start(args, ptr);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL) break;
		pushvalues_unsafe(ptr, pos);
	}
	va_end(args);
	nreverse_values_unsafe(ptr);
}

_g void setvalues_nil_control(Execute ptr)
{
	addr root;
	struct control_struct *str;

	root = ptr->control;
	Check(root == Nil, "control error");
	str = StructControl(root);
	str->sizer = 0;
	close_result_control(root, str);
}

_g void setvalues_list_control(Execute ptr, addr list)
{
	addr pos;

	setvalues_nil_control(ptr);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushvalues_unsafe(ptr, pos);
	}
	nreverse_values_unsafe(ptr);
}

_g void getresult_control(Execute ptr, addr *ret)
{
	addr root;

	root = ptr->control;
	if (StructControl(root)->sizer)
		GetResultControl(root, 0, ret);
	else
		*ret = Nil;
}

_g void getvalues_control(Execute ptr, size_t index, addr *ret)
{
	addr root;
	struct control_struct *str;

	root = ptr->control;
	str = StructControl(root);
	if (str->sizer <= index) {
		*ret = Unbound;
		return;
	}
	if (index < ControlSize_Result) {
		GetResultControl(root, index, ret);
	}
	else {
		index -= ControlSize_Result;
		GetControl(root, Control_Result, &root);
		getnth_unsafe(root, index, ret);
	}
}

static void list_from_vector_control(LocalRoot local,
		addr root, size_t size, addr cons, addr *ret)
{
	addr pos;
	size_t i;

	Check(size == 0, "size error");
	Check(ControlSize_Result < size, "size error");
	for (i = Control_Size + size - 1; ; i--) {
		GetControl(root, i, &pos);
		cons_alloc(local, &cons, pos, cons);
		if (i <= Control_Size)
			break;
	}
	*ret = cons;
}

static void getvalues_list_control(Execute ptr, LocalRoot local, addr *ret)
{
	addr root, cons;
	struct control_struct *str;
	size_t size;

	root = ptr->control;
	str = StructControl(root);
	size = str->sizer;
	if (size == 0) {
		*ret = Nil;
		return;
	}
	if (size < ControlSize_Result) {
		list_from_vector_control(local, root, size, Nil, ret);
	}
	else {
		GetControl(root, Control_Result, &cons);
		copy_list_alloc_unsafe(local, &cons, cons);
		list_from_vector_control(local, root, ControlSize_Result, cons, ret);
	}
}

_g void getvalues_list_control_local(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, ptr->local, ret);
}

_g void getvalues_list_control_heap(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, NULL, ret);
}

_g size_t lengthvalues_control(Execute ptr)
{
	return StructControl(ptr->control)->sizer;
}

_g void pushargs_allvalues(Execute ptr)
{
	int check;
	addr root, pos, cons;
	struct control_struct *str;
	size_t size, sizer, i;

	root = ptr->control;
	str = StructControl(root);
	sizer = str->sizer;
	check = ControlSize_Result < sizer;
	size = check? ControlSize_Result: sizer;

	for (i = 0; i < size; i++) {
		GetResultControl(root, i, &pos);
		pushargs_control(ptr, pos);
	}
	if (check) {
		GetControl(root, Control_Result, &cons);
		while (cons != Nil) {
			GetCons(cons, &pos, &cons);
			pushargs_control(ptr, pos);
		}
	}
}


/*
 *  flow control
 */
_g int goto_control_(Execute ptr, size_t point)
{
	struct control_struct *str;

	str = StructControl(ptr->control);
	str->point = point;

	return 0;
}

static int go_find_control(Execute ptr, addr *ret, addr tag)
{
	addr control, cons, pos, check;

	control = ptr->control;
	while (control != Nil) {
		gettagbody_control(control, &cons);
		while (cons != Nil) {
			GetCons(cons, &pos, &cons);
			GetNameTagInfo(pos, &check);
			if (eql_function(check, tag)) {
				*ret = pos;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}

	*ret = Nil;
	return 0;
}

_g int go_control_(Execute ptr, addr tag)
{
	addr pos;
	struct taginfo_struct *str;

	/* find tag */
	if (! go_find_control(ptr, &pos, tag))
		fmte("Cannot find tag ~S.", tag, NULL);
	str = StructTagInfo(pos);
	if (str->open == 0)
		fmte("Tag ~S already closed.", tag, NULL);

	/* rollback */
	ptr->throw_control = str->control;
	ptr->throw_point = str->point;
	ptr->throw_point_p = 1;
	return 1;
}

static int return_from_find_control(Execute ptr, addr *ret, addr name)
{
	addr control, list, pos, check;

	control = ptr->control;
	while (control != Nil) {
		getblock_control(control, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			GetNameTagInfo(pos, &check);
			if (eql_function(check, name)) {
				*ret = pos;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = 0;
	return 0;
}

_g int return_from_control_(Execute ptr, addr name)
{
	addr pos, control;
	struct taginfo_struct *str;

	/* find name */
	if (! return_from_find_control(ptr, &pos, name))
		fmte("Cannot find block name ~S.", name, NULL);
	str = StructTagInfo(pos);
	if (str->open == 0)
		fmte("Block ~S already closed.", name, NULL);

	/* copy values */
	control = str->control;
	copy_values_control(ptr, ptr->control, control);
	/* rollback */
	ptr->throw_control = control;
	ptr->throw_point_p = 0;
	return 1;
}

_g void catch_control(Execute ptr, addr name)
{
	setcatch_control(ptr->local, ptr->control, name);
}

static int throw_find_control(Execute ptr, addr *ret, addr name)
{
	addr control, check;

	control = ptr->control;
	while (control != Nil) {
		if (getcatch_control(control, &check)) {
			if (check == name) {
				*ret = control;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = 0;
	return 0;
}

_g int throw_control_(Execute ptr, addr name)
{
	addr next;

	/* find name */
	if (! throw_find_control(ptr, &next, name))
		fmte("Cannot find catch name ~S.", name, NULL);
	/* copy values */
	copy_values_control(ptr, ptr->control, next);
	/* rollback */
	ptr->throw_control = next;
	ptr->throw_point_p = 0;
	return 1;
}

_g void gettagbody_execute(Execute ptr, addr *ret, addr name)
{
	if (! go_find_control(ptr, ret, name))
		fmte("Cannot find tag name ~S", name, NULL);
}

_g void getblock_execute(Execute ptr, addr *ret, addr name)
{
	if (! return_from_find_control(ptr, ret, name))
		fmte("Cannot find block name ~S", name, NULL);
}

static int eval_control_p(addr control)
{
	gettable_control(control, CONSTANT_SYSTEM_EVAL_LEXICAL, &control);
	return control != Nil;
}

_g void hide_lexical_control(Execute ptr)
{
	addr control, pos, list, symbol;

	control = pos = ptr->control;
	while (pos != Nil && ! eval_control_p(pos)) {
		GetControl(pos, Control_Lexical, &list);
		while (list != Nil) {
			GetCons(list, &symbol, &list);
			GetCdr(list, &list);
			pushlexical_control(ptr, symbol, Unbound);
		}
		GetControl(pos, Control_Next, &pos);
	}
	seteval_control(ptr->local, control);
}


/*
 *  handler
 */
_g void pushhandler_control(Execute ptr, addr name, addr call, int escape)
{
	addr pos;

	/* condition */
	if (symbolp(name))
		clos_find_class(name, &name);
	if (! conditionp(name))
		fmte("The value ~S must be a condition instance.", name, NULL);

	/* push handler */
	handler_local(ptr->local, &pos, name, call, escape);
	pushtable_control(ptr, CONSTANT_SYSTEM_HANDLER, pos);
}

_g void reverse_handler_control(Execute ptr)
{
	addr control, cons;

	control = ptr->control;
	gettable_control(control, CONSTANT_SYSTEM_HANDLER, &cons);
	nreverse_list_unsafe(&cons, cons);
	settable_control(ptr->local, control, CONSTANT_SYSTEM_HANDLER, cons);
}

_g void push_restart_control(Execute ptr, addr object)
{
	pushtable_control(ptr, CONSTANT_SYSTEM_RESTART, object);
}

_g void pushbind_restart_control(Execute ptr, addr list, int escape)
{
	addr name, form, inter, report, test, pos;

	/* A2 restart */
	List_bind(list, &name, &form, &inter, &report, &test, NULL);
	restart_heap(&pos, name);
	setfunction_restart(pos, form);
	setinteractive_restart(pos, inter);
	setreport_restart(pos, report);
	settest_restart(pos, test);
	setescape_restart(pos, escape);

	/* push handler */
	push_restart_control(ptr, pos);
}

_g void reverse_restart_control(Execute ptr)
{
	addr control, cons;

	control = ptr->control;
	gettable_control(control, CONSTANT_SYSTEM_RESTART, &cons);
	nreverse_list_unsafe(&cons, cons);
	settable_control(ptr->local, control, CONSTANT_SYSTEM_RESTART, cons);
}


/*
 *  find-condition
 */
_g int find_condition_control(Execute ptr, addr instance)
{
	addr control, list, pos;

	for (control = ptr->control; control != Nil; ) {
		gethandler_control(control, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			if (checkhandler_control(pos, instance))
				return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	return 0;
}


/*
 *  invoke-handler
 */
static int wake_handler_(Execute ptr, addr next, addr instance, addr pos)
{
	int escape;
	addr call;

	/* call */
	GetCallHandler(pos, &call);
	Return(funcall_control(ptr, call, instance, NULL));

	/* escape */
	GetEscapeHandler(pos, &escape);
	if (escape) {
		copy_values_control(ptr, ptr->control, next);
		ptr->throw_control = next;
		ptr->throw_point_p = 0;
		exit_control(ptr);
		return 1;
	}

	return 0;
}

_g int invoke_handler_control_(Execute ptr, addr instance)
{
	addr next, list, pos;

	next = ptr->control;
	while (next != Nil) {
		gethandler_control(next, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			if (checkhandler_control(pos, instance)) {
				Return(wake_handler_(ptr, next, instance, pos));
			}
		}
		GetControl(next, Control_Next, &next);
	}

	return 0;
}


/*
 *  invoke-restart
 */
static void redirect_restart(addr restart, addr *ret)
{
	for (;;) {
		if (! getenable_restart(restart))
			fmte("The restart ~S is already closed.", restart, NULL);
		if (! getredirect_restart(restart))
			break;
		getreference_restart(restart, &restart);
	}
	*ret = restart;
}

static void rollback_restart_control(addr control, addr restart, addr *ret)
{
	addr list, check;

	while (control != Nil) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &check, &list);
			if (check == restart) {
				*ret = control;
				return;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = 0;
	fmte("The restart ~S is invalid.", restart, NULL);
}

_g int invoke_restart_control_(Execute ptr, addr restart, addr args)
{
	int escape;
	addr call, next;

	if (symbolp(restart)) {
		Return(find_restart_control_error_(ptr, restart, Nil, &restart));
	}

	/* call */
	redirect_restart(restart, &restart);
	getfunction_restart(restart, &call);
	Return(apply_control(ptr, call, args));

	/* escape */
	escape = getescape_restart(restart);
	if (escape) {
		rollback_restart_control(ptr->control, restart, &next);
		copy_values_control(ptr, ptr->control, next);
		ptr->throw_control = next;
		ptr->throw_point_p = 0;
		exit_control(ptr);
		return 1;
	}

	return 0;
}

_g int invoke_restart_interactively_control_(Execute ptr, addr restart)
{
	addr args;

	if (symbolp(restart)) {
		Return(find_restart_control_error_(ptr, restart, Nil, &restart));
	}

	redirect_restart(restart, &restart);
	getinteractive_restart(restart, &args);
	if (args != Nil) {
		Return(callclang_apply(ptr, &args, args, Nil));
	}

	return invoke_restart_control_(ptr, restart, args);
}


/*
 *  find-restart
 */
static int test_restart_control_(Execute ptr, addr restart, addr condition, int *ret)
{
	addr test;

	if (condition == Nil)
		return Result(ret, 1);
	gettest_restart(restart, &test);
	if (test == Nil)
		return Result(ret, 1);
	Return(callclang_funcall(ptr, &test, test, condition, NULL));

	return Result(ret, test != Nil);
}

static int equal_restart_control_(Execute ptr,
		addr restart, addr symbol, addr condition, addr *value, int *ret)
{
	int check;
	addr name;

	getname_restart(restart, &name);
	if (name == symbol) {
		Return(test_restart_control_(ptr, restart, condition, &check));
		if (check) {
			*value = restart;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int find_restart_stack_(Execute ptr,
		addr symbol, addr condition, addr *value, int *ret)
{
	int check;
	addr control, list, restart;

	control = ptr->control;
	while (control != Nil) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			redirect_restart(restart, &restart);
			Return(equal_restart_control_(ptr,
						restart, symbol, condition, value, &check));
			if (check)
				return Result(ret, 1);
		}
		GetControl(control, Control_Next, &control);
	}

	/* not found */
	return Result(ret, 0);
}

_g int find_restart_control_(Execute ptr,
		addr name, addr condition, addr *value, int *ret)
{
	/* name = restart */
	if (GetType(name) == LISPTYPE_RESTART) {
		if (getenable_restart(name)) {
			*value = name;
			return Result(ret, 1);
		}
		else {
			/* disable */
			return Result(ret, 0);
		}
	}

	/* name = symbol */
	if (name == Nil)
		return fmte_("The restart name ~S must not be a NIL.", name, NULL);
	if (! symbolp(name))
		return fmte_("The argument ~S must be a symbol.", name, NULL);
	if (condition != Nil && (! condition_instance_p(condition)))
		return fmte_("The argument ~S must be a NIL or condition.", condition, NULL);

	return find_restart_stack_(ptr, name, condition, value, ret);
}

_g int find_restart_control_error_(Execute ptr, addr name, addr condition, addr *ret)
{
	int check;

	Return(find_restart_control_(ptr, name, condition, ret, &check));
	if (check == 0)
		return fmte_("The restart name ~S is not found.", name, NULL);

	return 0;
}

_g int compute_restarts_control_(Execute ptr, addr condition, addr *ret)
{
	int check;
	addr control, root, list, restart;
	LocalHold hold;

	if (condition != Nil && (! condition_instance_p(condition)))
		return fmte_("The argument ~S must be a NIL or condition.", condition, NULL);
	hold = LocalHold_array(ptr, 1);
	control = ptr->control;
	for (root = Nil; control != Nil; ) {
		getrestart_control(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			redirect_restart(restart, &restart);
			Return(test_restart_control_(ptr, restart, condition, &check));
			if (check) {
				pushnew_heap(root, restart, &root);
				localhold_set(hold, 0, root);
			}
		}
		GetControl(control, Control_Next, &control);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	return 0;
}


/*
 *  restart interface
 */
struct restart_call {
	union {
		int (*call_0)(Execute, void *);
		int (*call_1)(Execute, addr);
		int (*call_1r)(Execute, addr, addr *);
		int (*call_2)(Execute, addr, addr);
	} u;
	Execute ptr;
	addr restart;
	addr args[8];
	addr *ret;
	void *voidp;
};

static int restart_call_control(struct restart_call *str,
		int (*call)(struct restart_call *))
{
	int check;
	Execute ptr;
	addr control;
	codejump jump;

	/* execute */
	ptr = str->ptr;
	control = ptr->control;
	check = 0;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		if (str->restart)
			push_restart_control(ptr, str->restart);
		check = (*call)(str);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control)) {
			throw_switch(&jump);
			return 1;
		}
		return 0;
	}

	/* free control */
	throw_switch(&jump);
	setresult_control(ptr, Nil);
	return 0;
}

/* restart */
_g int restart_control(Execute ptr, int (*call)(Execute, void *), void *voidp)
{
	return restart0_control(ptr, NULL, call, voidp);
}

/* restart0 */
static int restart0_control_adaptor(struct restart_call *str)
{
	return (str->u.call_0)(str->ptr, str->voidp);
}

_g int restart0_control(Execute ptr, addr restart,
		int (*call)(Execute, void *), void *voidp)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_0 = call;
	str.voidp = voidp;
	return restart_call_control(&str, restart0_control_adaptor);
}

/* restart1 */
static int restart1_control_adaptor(struct restart_call *str)
{
	return (str->u.call_1)(str->ptr, str->args[0]);
}

_g int restart1_control(Execute ptr, addr restart,
		int (*call)(Execute, addr), addr v1)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_1 = call;
	str.args[0] = v1;
	return restart_call_control(&str, restart1_control_adaptor);
}

/* restart1r */
static int restart1r_control_adaptor(struct restart_call *str)
{
	return (str->u.call_1r)(str->ptr, str->args[0], str->ret);
}

_g int restart1r_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr *), addr v1, addr *ret)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_1r = call;
	str.args[0] = v1;
	str.ret = ret;
	return restart_call_control(&str, restart1r_control_adaptor);
}

/* restart2 */
static int restart2_control_adaptor(struct restart_call *str)
{
	return (str->u.call_2)(str->ptr, str->args[0], str->args[1]);
}

_g int restart2_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr), addr v1, addr v2)
{
	struct restart_call str;

	str.ptr = ptr;
	str.restart = restart;
	str.u.call_2 = call;
	str.args[0] = v1;
	str.args[1] = v2;
	return restart_call_control(&str, restart2_control_adaptor);
}


/*
 *  code
 */
_g void set_taginfo_control(Execute ptr, addr list)
{
	addr control, root, key, value;
	LocalRoot local;

	local = ptr->local;
	control = ptr->control;
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		taginfo_heap(&value, control, key, RefIndex(value));
		cons_local(local, &root, value, root);
	}
	settagbody_control(local, control, root);
}

_g void set_blockinfo_control(Execute ptr, addr pos)
{
	addr control;
	LocalRoot local;

	local = ptr->local;
	control = ptr->control;
	taginfo_heap(&pos, control, pos, 0);
	conscar_local(local, &pos, pos);
	setblock_control(local, control, pos);
}

_g void set_protect_control(Execute ptr, addr pos)
{
	setprotect_plist_control(ptr->local, ptr->control, pos);
}


/*
 *  catch / throw
 */
_g int catch_clang(Execute ptr, pointer call, addr tag, addr value)
{
	addr pos, control;

	/* function */
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, call);
	SetDataFunction(pos, value);
	/* execute */
	push_close_control(ptr, &control);
	catch_control(ptr, tag);
	if (callclang_apply(ptr, &pos, pos, Nil)) {
		if (! equal_control_restart(ptr, control))
			return 1;
	}
	return free_control_(ptr, control);
}

