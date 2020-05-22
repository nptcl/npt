#include "callname.h"
#include "clos_class.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute.h"
#include "execute_object.h"
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
 *  special
 */
static void special_local(Execute ptr, addr *ret, addr symbol)
{
	addr pos, value;

	Check(! symbolp(symbol), "type error");
	snapshot_special_local(ptr, symbol, &value);
	local_array2(ptr->local, &pos, LISPSYSTEM_SPECIAL, Special_Size);
	SetArrayA2(pos, Special_Symbol, symbol);
	SetArrayA2(pos, Special_Snapshot, value);
	*ret = pos;
}

static void getsymbol_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Symbol, ret);
}

static void getsnapshot_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Snapshot, ret);
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
	str->lexical = ptr->lexical;
#ifdef LISP_DEBUG
	str->lexical_vector = ptr->lexical_vector;
#endif
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
static int close_protect_control_(Execute ptr, addr control)
{
	addr call, values;
	size_t size;

	GetControl(control, Control_Protect, &call);
	if (call == Nil)
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

static void close_special_control(Execute ptr, addr pos)
{
	addr symbol, snapshot;

	getsymbol_special(pos, &symbol);
	getsnapshot_special(pos, &snapshot);
	rollback_special_local(ptr, symbol, snapshot);
}

static void close_close_control(Execute ptr, addr control)
{
	addr list, pos;

	GetControl(control, Control_Close, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		switch (GetType(pos)) {
			case LISPSYSTEM_SPECIAL:
				close_special_control(ptr, pos);
				break;

			case LISPSYSTEM_TAGINFO:
				close_taginfo(pos);
				break;

			case LISPTYPE_RESTART:
				setenable_restart(pos, 0);
				break;

			default:
				Abort("Invalid control-close type.");
				break;
		}
	}
}

static int pop_control_(Execute ptr)
{
	addr control, *lexical;
	LocalStack stack;
	struct control_struct *str;
#ifdef LISP_DEBUG
	addr lexical_vector;
#endif

	control = ptr->control;
	Check(control == Nil, "Execute error");
	str = StructControl(control);
	stack = str->stack;
	lexical = str->lexical;
#ifdef LISP_DEBUG
	lexical_vector = str->lexical_vector;
#endif

	/* close */
	Return(close_protect_control_(ptr, control));
	close_close_control(ptr, control);

	/* pop */
	GetControl(control, Control_Next, &control);
	ptr->control = control;
	ptr->lexical = lexical;
#ifdef LISP_DEBUG
	ptr->lexical_vector = lexical_vector;
#endif
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

static void pushclose_control(Execute ptr, addr pos)
{
	addr list;

	GetControl(ptr->control, Control_Close, &list);
	cons_local(ptr->local, &list, pos, list);
	SetControl(ptr->control, Control_Close, list);
}

_g void pushspecial_control(Execute ptr, addr pos, addr value)
{
	addr x;

	Check(! symbolp(pos), "type error");
	Check(stack_check_control(ptr), "stack error");
	/* push close */
	special_local(ptr, &x, pos);
	pushclose_control(ptr, x);
	/* push symbol */
	pushspecial_unsafe(ptr, pos, value);
}

_g void pushtaginfo_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	pushclose_control(ptr, pos);
}

static void pushtable_control(Execute ptr, constindex index, addr pos)
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

_g void pushhandler_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	pushtable_control(ptr, CONSTANT_SYSTEM_HANDLER, pos);
}

_g void pushrestart_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_RESTART);
	pushclose_control(ptr, pos);
	pushtable_control(ptr, CONSTANT_SYSTEM_RESTART, pos);
}

_g int existspecial_control(Execute ptr, addr pos)
{
	addr list, check;

	GetControl(ptr->control, Control_Close, &list);
	while (list != Nil) {
		GetCons(list, &check, &list);
		if (GetType(check) != LISPSYSTEM_SPECIAL)
			continue;
		getsymbol_special(check, &check);
		if (check == pos)
			return 1;
	}

	return 0;
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

static int gettable_control(addr pos, constindex index, addr *ret)
{
	addr key;

	GetConstant(index, &key);
	GetControl(pos, Control_Table, &pos);

	return getplist(pos, key, ret) == 0;
}

static void settable_control(LocalRoot local, addr control, constindex index, addr value)
{
	addr key, table;

	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	if (setplist_local(local, table, key, value, &table))
		SetControl(control, Control_Table, table);
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

_g void setcatch_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CATCH, value);
}

_g void sethandler_control(LocalRoot local, addr pos, addr value)
{
	return settable_control(local, pos, CONSTANT_SYSTEM_HANDLER, value);
}

_g void setrestart_control(LocalRoot local, addr pos, addr value)
{
	return settable_control(local, pos, CONSTANT_SYSTEM_RESTART, value);
}

_g void setprotect_value_control(addr pos, addr value)
{
	SetControl(pos, Control_Protect, value);
}

_g void setprotect_control(Execute ptr, pointer id, addr value)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, id);
	SetDataFunction(pos, value);
	setprotect_value_control(ptr->control, pos);
}

_g void setprotect_control_local(Execute ptr, pointer id, addr value)
{
	addr pos;

	compiled_local(ptr->local, &pos, Nil);
	setcompiled_empty(pos, id);
	SetDataFunction(pos, value);
	setprotect_value_control(ptr->control, pos);
}

