#include "callname.h"
#include "clos_class.h"
#include "condition.h"
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
#include "type_typep.h"

/*
 *  control
 */
void *ptrbodycontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return PtrBodyControl_Low(pos);
}

struct control_struct *structcontrol_debug(addr pos)
{
	CheckType(pos, LISPTYPE_CONTROL);
	return StructControl_Low(pos);
}

void getcontrol_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_CONTROL);
	GetControl_Low(pos, index, ret);
}

void setcontrol_debug(addr pos, size_t index, addr value)
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
	getspecial_unsafe(ptr, symbol, &value);
	local_array2(ptr->local, &pos, LISPSYSTEM_SPECIAL, Special_Size);
	SetArrayA2(pos, Special_Symbol, symbol);
	SetArrayA2(pos, Special_Value, value);
	*ret = pos;
}

static void getsymbol_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Symbol, ret);
}

static void getvalue_special(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SPECIAL);
	GetArrayA2(pos, Special_Value, ret);
}


/*
 *  taginfo
 */
void taginfo_heap(addr *ret, addr control, addr tag, size_t point)
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

void *ptrtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return PtrTagInfo_Low(pos);
}

struct taginfo_struct *structtaginfo_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	return StructTagInfo_Low(pos);
}

void getnametaginfo_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	GetNameTagInfo_Low(pos, ret);
}

void setnametaginfo_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_TAGINFO);
	SetNameTagInfo_Low(pos, value);
}


/*
 *  handler
 */
void handler_local(LocalRoot local, addr *ret, addr name, addr call, int esc)
{
	addr pos;

	local_array2(local, &pos, LISPSYSTEM_HANDLER, Handler_Size);
	SetNameHandler_Low(pos, name);
	SetCallHandler_Low(pos, call);
	setescape_handler(pos, esc);
	setdisable_handler(pos, 0);
	*ret = pos;
}

void getnamehandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetNameHandler_Low(pos, ret);
}

void setnamehandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetNameHandler_Low(pos, value);
}

void getcallhandler_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	GetCallHandler_Low(pos, ret);
}

void setcallhandler_debug(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	SetCallHandler_Low(pos, value);
}

int getescape_handler(addr pos)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	return (int)GetBitByte(c, 0);
}

void setescape_handler(addr pos, int value)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	SetBitByte(c, 0, value);
	SetUser(pos, c);
}

int getdisable_handler(addr pos)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	return (int)GetBitByte(c, 1);
}

void setdisable_handler(addr pos, int value)
{
	byte c;

	CheckType(pos, LISPSYSTEM_HANDLER);
	c = (byte)GetUser(pos);
	SetBitByte(c, 1, value);
	SetUser(pos, c);
}

int checkhandler_control_(Execute ptr, addr pos, addr instance, int *ret)
{
	addr clos;

	CheckType(pos, LISPSYSTEM_HANDLER);

	/* ignore */
	if (getdisable_handler(pos))
		return Result(ret, 0);

	/* check */
	GetNameHandler(pos, &clos);
	if (clos == Nil)
		return Result(ret, 0);

	return typep_clang_(ptr, instance, clos, ret);
}


/*
 *  push control
 */
void push_control(Execute ptr, addr *ret)
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
	str->lexical_reader = ptr->lexical_reader;
	str->lexical_vector = ptr->lexical_vector;
	str->stack = stack;

	/* push */
	SetControl(pos, Control_Next, ptr->control);
	*ret = ptr->control = pos;
}

void push_args_control(Execute ptr, addr *ret)
{
	addr prev, next, pos;

	prev = ptr->control;
	push_control(ptr, &next);
	GetControl(prev, Control_Cons, &pos);
	SetControl(next, Control_Cons, pos);
	*ret = next;
}


/*
 *  pop_control
 */
static void close_special_control(Execute ptr, addr pos)
{
	addr symbol, value;

	getsymbol_special(pos, &symbol);
	getvalue_special(pos, &value);
	setspecial_unsafe(ptr, symbol, value);
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

int pop_control_(Execute ptr, addr control)
{
	addr *lexical_reader, lexical_vector;
	LocalStack stack;
	struct control_struct *str;

	Check(ptr->control != control, "control error");
	str = StructControl(control);
	stack = str->stack;
	lexical_reader = str->lexical_reader;
	lexical_vector = str->lexical_vector;

	/* close */
	close_close_control(ptr, control);

	/* pop */
	GetControl(control, Control_Next, &control);
	ptr->control = control;
	ptr->lexical_reader = lexical_reader;
	ptr->lexical_vector = lexical_vector;
	rollback_local(ptr->local, stack);

	return ptr->throw_value != throw_normal;
}

int free_control_degrade_(Execute ptr, addr control)
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
		Return(pop_control_(ptr, root));
	} while (root != control);

	return 0;
}


/*
 *  data
 */
int stack_check_control(Execute ptr)
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

void pushspecial_control(Execute ptr, addr pos, addr value)
{
	addr x;

	Check(! symbolp(pos), "type error");
	Check(stack_check_control(ptr), "stack error");
	special_local(ptr, &x, pos);
	pushclose_control(ptr, x);
	setspecial_unsafe(ptr, pos, value);
}

void pushtaginfo_control(Execute ptr, addr pos)
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

void pushhandler_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_HANDLER);
	pushtable_control(ptr, CONSTANT_SYSTEM_HANDLER, pos);
}

void pushrestart_control(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_RESTART);
	pushclose_control(ptr, pos);
	pushtable_control(ptr, CONSTANT_SYSTEM_RESTART, pos);
}

int existspecial_control(Execute ptr, addr pos)
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
void getdata_control(Execute ptr, addr *ret)
{
	GetControl(ptr->control, Control_Data, ret);
}

void getdata_code_control(Execute ptr, addr *ret)
{
	addr control;
	struct control_struct *str;

	control = ptr->control;
	for (;;) {
		str = StructControl(control);
		if (str->run_code)
			break;
		GetControl(control, Control_Next, &control);
		Check(control == Unbound, "revert-goto error.");
	}
	GetControl(control, Control_Data, ret);
}

void setdata_control(Execute ptr, addr value)
{
	SetControl(ptr->control, Control_Data, value);
}

int getcall_control(Execute ptr, addr *ret)
{
	addr control, pos;

	control = ptr->control;
	while (control != Nil) {
		GetControl(control, Control_Call, &pos);
		if (pos != Nil) {
			*ret = pos;
			return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	*ret = Nil;
	return 0;
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

int getcatch_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CATCH, ret);
}

int getcondition_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CONDITION, ret);
}

int gethandler_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_HANDLER, ret);
}

int getrestart_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_RESTART, ret);
}

void setcatch_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CATCH, value);
}

void sethandler_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_SYSTEM_HANDLER, value);
}

void setrestart_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_SYSTEM_RESTART, value);
}

void pushdebug_control(Execute ptr, addr pos)
{
	Check(! (pos == Nil || fixnump(pos)), "type error");
	settable_control(ptr->local, ptr->control, CONSTANT_CODE_BEGIN, pos);
}

int getdebug_control(Execute ptr, addr *ret)
{
	return gettable_control(ptr->control, CONSTANT_CODE_BEGIN, ret);
}

void save_control(Execute ptr)
{
	addr pos;
	save_execute_control(ptr, &pos);
	settable_control(ptr->local, ptr->control, CONSTANT_CODE_SAVE, pos);
}

int restore_control_(Execute ptr)
{
	addr pos;

	if (! gettable_control(ptr->control, CONSTANT_CODE_SAVE, &pos))
		return fmte_("Invalid code, RESTORE.", NULL);
	CheckType(pos, LISPSYSTEM_EXECUTE);
	restore_execute_control(ptr, pos);

	return 0;
}

