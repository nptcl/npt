#include "clos_class.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute.h"
#include "function.h"
#include "heap.h"
#include "restart.h"
#include "symbol.h"

/*
 *  control
 */
_g void *ptrbodycontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return PtrBodyControl_Low(pos);
}

_g struct control_struct *structcontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return StructControl_Low(pos);
}

_g void getcontrol_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_CONTROL);
	GetControl_Low(pos, index, ret);
}

_g void setcontrol_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_CONTROL);
	SetControl_Low(pos, index, value);
}


/*
 *  taginfo
 */
_g void taginfo_heap(addr *ret, addr control, addr tag, size_t point)
{
	addr pos;
	struct taginfo_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TAGINFO,
			TagInfo_Size, sizeoft(struct taginfo_struct));
	str = StructTagInfo(pos);
	Check(GetStatusDynamic(tag), "dynamic error");
	SetNameTagInfo(pos, tag);
	str->open = 1;
	str->control = control;
	str->point = point;
	*ret = pos;
}

static void close_taginfo(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	StructTagInfo(pos)->open = 0;
}

_g void *ptrtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return PtrTagInfo_Low(pos);
}

_g struct taginfo_struct *structtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return StructTagInfo_Low(pos);
}

_g void getnametaginfo_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	GetNameTagInfo_Low(pos, ret);
}

_g void setnametaginfo_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	SetNameTagInfo_Low(pos, value);
}


/*
 *  handler
 */
_g void handler_local(LocalRoot local, addr *ret, addr name, addr call, int esc)
{
	addr pos;

	local_array2(local, &pos, LISPSYSTEM_HANDLER, Handler_Size);
	SetNameHandler_Low(pos, name);
	SetCallHandler_Low(pos, call);
	SetEscapeHandler_Low(pos, esc);
	*ret = pos;
}

_g void getnamehandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetNameHandler_Low(pos, ret);
}

_g void setnamehandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetNameHandler_Low(pos, value);
}

_g void getcallhandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetCallHandler_Low(pos, ret);
}

_g void setcallhandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetCallHandler_Low(pos, value);
}

_g void getescapehandler_debug(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetEscapeHandler_Low(pos, ret);
}

_g void setescapehandler_debug(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetEscapeHandler_Low(pos, value);
}

_g int checkhandler_control(addr pos, addr instance)
{
	addr clos;

	CheckType(pos, LISPSYSTEM_HANDLER);
	GetNameHandler(pos, &clos);
	return clos != Nil && clos_subtype_p(instance, clos);
}


/*
 *  push control
 */
_g struct control_struct *push_control(Execute ptr)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct control_struct *str;

	/* local */
	local = ptr->local;
	push_local(local, &stack);

	/* object */
	local_smallsize(local, &pos,
			LISPTYPE_CONTROL,
			Control_Size,
			sizeoft(struct control_struct));
	str = StructControl(pos);
	clearpoint(str);
	str->stack = stack;

	/* push */
	SetControl(pos, Control_Next, ptr->control);
	ptr->control = pos;

	return str;
}

_g void push_new_control(Execute ptr, addr *ret)
{
	push_control(ptr);
	*ret = ptr->control;
}

_g void push_args_control(Execute ptr, addr *ret)
{
	addr prev, next, pos;

	prev = ptr->control;
	push_new_control(ptr, &next);
	GetControl(prev, Control_Cons, &pos);
	SetControl(next, Control_Cons, pos);
	*ret = next;
}


/*
 *  free_control
 */
static void close_function_control(Execute ptr, addr list)
{
	addr symbol, snapshot;

	while (list != Nil) {
		GetCons(list, &symbol, &list);
		GetCons(list, &snapshot, &list);
		rollback_function_local(ptr, symbol, snapshot);
	}
}

static void close_setf_control(Execute ptr, addr list)
{
	addr symbol, snapshot;

	while (list != Nil) {
		GetCons(list, &symbol, &list);
		GetCons(list, &snapshot, &list);
		rollback_setf_local(ptr, symbol, snapshot);
	}
}

static void close_tagbody_control(Execute ptr, addr control, addr list)
{
	addr value;

	while (list != Nil) {
		GetCons(list, &value, &list);
		close_taginfo(value);
	}
}

static void close_restart_control(Execute ptr, addr control, addr list)
{
	addr value;

	while (list != Nil) {
		GetCons(list, &value, &list);
		setenable_restart(value, 0);
	}
}

static int close_protect_control_(Execute ptr, addr call)
{
	addr control, values;
	size_t size;

	if (call == Unbound)
		return 0;
	push_new_control(ptr, &control);
	save_values_control(ptr, &values, &size);
	if (functionp(call)) {
		Return(apply_control(ptr, call, Nil));
	}
	else {
		Return(runcode_control(ptr, call));
	}
	restore_values_control(ptr, values, size);
	return free_control_(ptr, control);
}

static int close_plist_control_(Execute ptr, addr control)
{
	addr list, key, value;
	addr key1, key2, key3, key4, key5, key6;

	GetConst(COMMON_FUNCTION, &key1);
	GetConst(COMMON_SETF, &key2);
	GetConst(COMMON_TAGBODY, &key3);
	GetConst(COMMON_BLOCK, &key4);
	GetConst(SYSTEM_RESTART, &key5);
	GetConst(COMMON_UNWIND_PROTECT, &key6);

	GetControl(control, Control_Table, &list);
	while (list != Nil) {
		GetCons(list, &key, &list);
		GetCar(list, &value);
		SetCar(list, Unbound);
		GetCdr(list, &list);
		if (value == Unbound)
			continue;

		/* function */
		if (key == key1) {
			close_function_control(ptr, value);
			continue;
		}
		/* setf */
		if (key == key2) {
			close_setf_control(ptr, value);
			continue;
		}
		/* tagbody */
		if (key == key3) {
			close_tagbody_control(ptr, control, value);
			continue;
		}
		/* block */
		if (key == key4) {
			close_tagbody_control(ptr, control, value);
			continue;
		}
		/* restart */
		if (key == key5) {
			close_restart_control(ptr, control, value);
			continue;
		}
		/* unwind-protect */
		if (key == key6) {
			Return(close_protect_control_(ptr, value));
			continue;
		}
	}

	return 0;
}

static int close_table_control_(Execute ptr, addr control, struct control_struct *str)
{
	addr list, key, value, key1;

	if (str->p_protect) {
		GetConst(COMMON_UNWIND_PROTECT, &key1);
		GetControl(control, Control_Table, &list);
		while (list != Nil) {
			GetCons(list, &key, &list);
			if (key == key1) {
				GetCar(list, &value);
				SetCar(list, Unbound);
				Return(close_protect_control_(ptr, value));
			}
			GetCdr(list, &list);
		}
	}

	return close_plist_control_(ptr, control);
}

static void close_lexical_control(Execute ptr, addr control)
{
	addr list, symbol, snapshot;

	GetControl(control, Control_Lexical, &list);
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		GetCons(list, &snapshot, &list);
		rollback_lexical_local(ptr, symbol, snapshot);
	}
}

static void close_special_control(Execute ptr, addr control)
{
	addr list, symbol, snapshot;

	GetControl(control, Control_Special, &list);
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		GetCons(list, &snapshot, &list);
		rollback_special_local(ptr, symbol, snapshot);
	}
}

static int pop_control_(Execute ptr)
{
	addr control;
	LocalStack stack;
	struct control_struct *str;

	control = ptr->control;
	Check(control == Nil, "Execute error");
	str = StructControl(control);
	stack = str->stack;

	/* close */
	Return(close_table_control_(ptr, control, str));
	close_lexical_control(ptr, control);
	close_special_control(ptr, control);

	/* pop */
	GetControl(control, Control_Next, &control);
	ptr->control = control;
	rollback_local(ptr->local, stack);

	return 0;
}

_g int free_control_(Execute ptr, addr control)
{
	addr root;

	/* check */
#ifdef LISP_DEBUG
	for (root = ptr->control; root == control; ) {
		Check(root == Nil, "free_control check error");
		GetControl(root, Control_Next, &root);
	}
#endif

	/* rollback */
	do {
		root = ptr->control;
		Check(root == Nil, "free_control error");
		Return(pop_control_(ptr));
	} while (root != control);

	return 0;
}

_g int rollback_control_(Execute ptr, addr control)
{
	addr root;

	/* check */
#ifdef LISP_DEBUG
	for (root = ptr->control; root == control; ) {
		Check(root == Nil, "free_control check error");
		GetControl(root, Control_Next, &root);
	}
#endif

	/* rollback */
	for (;;) {
		root = ptr->control;
		Check(root == Nil, "free_control error");
		if (root == control)
			break;
		Return(pop_control_(ptr));
	}

	return 0;
}


/*
 *  data
 */
_g int stack_check_control(Execute ptr)
{
	LocalStack stack1, stack2;
	stack1 = StructControl(ptr->control)->stack;
	stack2 = ptr->local->stack;
	return stack1 != stack2;
}

_g void pushlexical_control(Execute ptr, addr pos, addr value)
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
	pushlexical_unsafe(ptr, pos, value);
}

_g void pushspecial_control(Execute ptr, addr pos, addr value)
{
	addr control, list, snapshot;

	control = ptr->control;
	GetControl(control, Control_Special, &list);
	if (getplist(list, pos, &snapshot)) {
		Check(stack_check_control(ptr), "stack error");
		snapshot_special_local(ptr, pos, &snapshot);
		if (setplist_local(ptr->local, list, pos, snapshot, &list))
			SetControl(control, Control_Special, list);
	}
	pushspecial_unsafe(ptr, pos, value);
}

_g void pushcallname_control(Execute ptr, addr pos, addr value)
{
	addr symbol;

	GetCallName(pos, &symbol);
	if (RefCallNameType(pos) == CALLNAME_SYMBOL)
		pushfunction_control(ptr, symbol, value);
	else
		pushsetf_control(ptr, symbol, value);
}

_g void pushfunction_control(Execute ptr, addr pos, addr value)
{
	addr control, table, key, snapshot;

	control = ptr->control;
	GetConst(COMMON_FUNCTION, &key);
	GetControl(control, Control_Table, &table);
	if (getplistplist(table, key, pos, &snapshot)) {
		Check(stack_check_control(ptr), "stack error");
		snapshot_function_local(ptr, pos, &snapshot);
		if (setplistplist_local(ptr->local, table, key, pos, snapshot, &table))
			SetControl(control, Control_Table, table);
	}
	pushfunction_unsafe(ptr, pos, value);
}

_g void pushsetf_control(Execute ptr, addr pos, addr value)
{
	addr control, table, key, snapshot;

	control = ptr->control;
	GetConst(COMMON_SETF, &key);
	GetControl(control, Control_Table, &table);
	if (getplistplist(table, key, pos, &snapshot)) {
		Check(stack_check_control(ptr), "stack error");
		snapshot_setf_local(ptr, pos, &snapshot);
		if (setplistplist_local(ptr->local, table, key, pos, snapshot, &table))
			SetControl(control, Control_Table, table);
	}
	pushsetf_unsafe(ptr, pos, value);
}

_g void pushtable_control(Execute ptr, constindex index, addr pos)
{
	addr control, key, table, cons;
	LocalRoot local;

	local = ptr->local;
	control = ptr->control;
	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	getplist(table, key, &cons);
	cons_local(local, &cons, pos, cons);
	if (setplist_local(local, table, key, cons, &table))
		SetControl(control, Control_Table, table);
}

_g void pushtagbody_control(Execute ptr, addr pos, addr value)
{
	pushtable_control(ptr, CONSTANT_COMMON_TAGBODY, value);
}

_g void pushblock_control(Execute ptr, addr pos)
{
	pushtable_control(ptr, CONSTANT_COMMON_BLOCK, pos);
}

_g int existspecial_control(Execute ptr, addr pos)
{
	addr list;
	GetControl(ptr->control, Control_Special, &list);
	return getplist(list, pos, &list) == 0;
}


/*
 *  access
 */
_g void getdata_control(Execute ptr, addr *ret)
{
	GetControl(ptr->control, Control_Data, ret);
}

_g void setdata_control(Execute ptr, addr value)
{
	SetControl(ptr->control, Control_Data, value);
}

_g int gettable_control(addr pos, constindex index, addr *ret)
{
	addr key;

	GetConstant(index, &key);
	GetControl(pos, Control_Table, &pos);

	return getplist(pos, key, ret) == 0;
}

_g void settable_control(LocalRoot local, addr control, constindex index, addr value)
{
	addr key, table;

	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	if (setplist_local(local, table, key, value, &table))
		SetControl(control, Control_Table, table);
}

_g int gettagbody_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_TAGBODY, ret);
}

_g int getblock_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_BLOCK, ret);
}

_g int getcatch_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CATCH, ret);
}

_g int getcondition_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CONDITION, ret);
}

_g int gethandler_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_HANDLER, ret);
}

_g int getrestart_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_RESTART, ret);
}

_g void seteval_control(LocalRoot local, addr pos)
{
	settable_control(local, pos, CONSTANT_SYSTEM_EVAL_LEXICAL, T);
}

_g void settagbody_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_TAGBODY, value);
}

_g void setblock_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_BLOCK, value);
}

_g void setcatch_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CATCH, value);
}

_g void setprotect_plist_control(LocalRoot local, addr pos, addr value)
{
	StructControl(pos)->p_protect = 1;
	settable_control(local, pos, CONSTANT_COMMON_UNWIND_PROTECT, value);
}

_g void setprotect_control(Execute ptr, pointer id, addr value)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, id);
	SetDataFunction(pos, value);
	setprotect_plist_control(ptr->local, ptr->control, pos);
}

_g void setprotect_control_local(Execute ptr, pointer id, addr value)
{
	addr pos;
	LocalRoot local;

	local = ptr->local;
	compiled_local(local, &pos, Nil);
	setcompiled_empty(pos, id);
	SetDataFunction(pos, value);
	setprotect_plist_control(local, ptr->control, pos);
}

