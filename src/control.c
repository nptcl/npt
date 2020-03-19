#include "build.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "code.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "control_callbind.h"
#include "copy.h"
#include "equal.h"
#include "execute.h"
#include "function.h"
#include "format.h"
#include "gc.h"
#include "heap.h"
#include "lambda.h"
#include "object.h"
#include "package.h"
#include "restart.h"
#include "sequence.h"
#include "symbol.h"
#include "syscall.h"
#include "type_parse.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_value.h"

#undef LISP_DEBUG_TRACE
//#define LISP_DEBUG_TRACE

#ifdef LISP_DEBUG_FORCE_GC
_g size_t GcCounterForce = 0;
#endif
_g size_t ControlCounter = 0;


/*
 *  control
 */
#ifdef LISP_DEBUG
_g void output_result_execute(void)
{
	addr pos, value;
	Execute ptr;

	info("output-result-execute[.]: ***start***");
	ptr = Execute_Thread;
	pos = ptr->control;
	while (pos != Nil) {
		switch (StructControl(pos)->type) {
			case ControlType_Return:
				info_noeol("output-result-execute[r]: "); break;
			case ControlType_Push:
				info_noeol("output-result-execute[p]: "); break;
			case ControlType_Close:
				info_noeol("output-result-execute[c]: "); break;
			case ControlType_Local:
				info_noeol("output-result-execute[l]: "); break;
			case ControlType_Protect:
				info_noeol("output-result-execute[u]: "); break;
			case ControlType_TagBody:
				info_noeol("output-result-execute[t]: "); break;
			case ControlType_Block:
				info_noeol("output-result-execute[b]: "); break;
			case ControlType_Restart:
				info_noeol("output-result-execute[R]: "); break;
			default:
				info_noeol("output-result-execute[.]: "); break;
		}
		GetResultControl(pos, 0, &value);
		infoprint(value);
		GetControl(pos, Control_Next, &pos);
	}
	info("output-result-execute[.]: ***end***");
}
#endif


/*
 *  taginfo
 */
static int gettable_control(addr pos, constindex index, addr *ret)
{
	addr key;

	GetConstant(index, &key);
	GetControl(pos, Control_Table, &pos);

	return getplist(pos, key, ret) == 0;
}
static void settable_control(LocalRoot local,
		addr control, constindex index, addr value)
{
	addr key, table;

	GetConstant(index, &key);
	GetControl(control, Control_Table, &table);
	if (setplist_local(local, table, key, value, &table))
		SetControl(control, Control_Table, table);
}
static void setcode_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_SYSTEM_CODE, value);
}
static void gettagbody_control(addr pos, addr *ret)
{
	gettable_control(pos, CONSTANT_COMMON_TAGBODY, ret);
}
static void settagbody_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_TAGBODY, value);
}
static void getblock_control(addr pos, addr *ret)
{
	gettable_control(pos, CONSTANT_COMMON_BLOCK, ret);
}
static void setblock_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_BLOCK, value);
}
static int getcatch_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_COMMON_CATCH, ret);
}
static void setcatch_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CATCH, value);
}
static void getcondition_control(addr pos, addr *ret)
{
	gettable_control(pos, CONSTANT_COMMON_CONDITION, ret);
}
static void setcondition_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_CONDITION, value);
}
static void getrestart_control(addr pos, addr *ret)
{
	gettable_control(pos, CONSTANT_COMMON_RESTART, ret);
}
static void setrestart_control(LocalRoot local, addr pos, addr value)
{
	settable_control(local, pos, CONSTANT_COMMON_RESTART, value);
}
static void getrestart_object(addr pos, addr *ret)
{
	gettable_control(pos, CONSTANT_SYSTEM_RESTART, ret);
}
static void setprotect_control(LocalRoot local, addr control, addr code)
{
	getinfo_code(code, &code);
	settable_control(local, control, CONSTANT_COMMON_UNWIND_PROTECT, code);
}

enum TagInfo_Index {
	TagInfo_Name,
	TagInfo_Size
};
#define PtrTagInfo(p)		PtrBodySSa((p), TagInfo_Size)
#define StructTagInfo(p)	((struct taginfo_struct *)PtrTagInfo(p))
#define GetNameTagInfo(p,v)	GetArraySS(p, TagInfo_Name, v)
#define SetNameTagInfo(p,v)	SetArraySS(p, TagInfo_Name, v)

static void taginfo_heap(addr *ret, addr control, addr tag, size_t point, int thr)
{
	addr pos;
	struct taginfo_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TAGINFO,
			TagInfo_Size, sizeoft(struct taginfo_struct));
	str = StructTagInfo(pos);
	if (tag != Unbound)
		copylocal_object(NULL, &tag, tag);
	SetNameTagInfo(pos, tag);
	str->open = 1;
	str->wake = 0;
	str->thr = thr;
	str->point = point;
	str->control = control;
	*ret = pos;
}

static void push_taginfo(LocalRoot local, addr control, addr code)
{
	addr root, key, value;

	getinfo_code(code, &code);
	for (root = Nil; code != Nil; ) {
		GetCons(code, &key, &code);
		GetCons(key, &key, &value);
		taginfo_heap(&value, control, key, RefIndex(value), 0);
		cons_local(local, &root, value, root);
	}
	settagbody_control(local, control, root);
}

static void close_taginfo(addr value)
{
	StructTagInfo(value)->open = 0;
}

static void push_blockinfo(LocalRoot local, addr control, addr code)
{
	addr pos;

	getinfo_code(code, &pos);
	taginfo_heap(&pos, control, pos, 0, 1);
	conscar_local(local, &pos, pos);
	setblock_control(local, control, pos);
}

static void close_blockinfo(addr value)
{
	StructTagInfo(value)->open = 0;
}

static void push_catchinfo(LocalRoot local, addr control, addr code)
{
	addr pos;

	taginfo_heap(&pos, control, Unbound, 0, 1);
	setcatch_control(local, control, pos);
}


/*
 *  symstack
 */
#ifdef LISP_DEBUG
static int stack_check_control(Execute ptr)
{
	LocalStack stack1, stack2;
	stack1 = StructControl(ptr->control)->stack;
	stack2 = ptr->local->stack;
	return stack1 != stack2;
}
#endif

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

static void pushtagbody_control(Execute ptr, addr pos, addr value)
{
	pushtable_control(ptr, CONSTANT_COMMON_TAGBODY, value);
}

static void pushblock_control(Execute ptr, addr pos)
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
 *  stack
 */
static void push_control(Execute ptr, enum ControlType_Index type)
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
			ControlSize_Arary,
			sizeoft(struct control_struct));
	str = StructControl(pos);
	str->type = type;
	str->stack = stack;
	str->call = NULL;
	str->sizer = 0;
	str->point = 0;
	str->dynamic_result = 0;

	/* push */
	SetControl(pos, Control_Next, ptr->control);
	ptr->control = pos;
}

static void push_stack_control(Execute ptr, addr *ret, enum ControlType_Index type)
{
	push_control(ptr, type);
	*ret = ptr->control;
}
_g void push_return_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Return);
}
_g void push_push_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Push);
}
_g void push_close_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Close);
}
_g void push_local_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Local);
}
_g void push_protect_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Protect);
}
_g void push_finalize_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Finalize);
	/* table allocate */
	settable_control(ptr->local, *ret, CONSTANT_COMMON_UNWIND_PROTECT, Nil);
}
_g void setfinalize_control(Execute ptr, addr control, addr code)
{
	settable_control(ptr->local, control, CONSTANT_COMMON_UNWIND_PROTECT, code);
}

static void push_tagbody_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_TagBody);
}
static void push_block_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Block);
}
_g void push_restart_control(Execute ptr, addr *ret)
{
	push_stack_control(ptr, ret, ControlType_Restart);
}
static void push_argument_control(Execute ptr, addr *ret)
{
	addr prev, next, pos;

	prev = ptr->control;
	push_stack_control(ptr, &next, ControlType_Return);
	GetControl(prev, Control_Cons, &pos);
	SetControl(next, Control_Cons, pos);
	*ret = next;
}

/* free_control */
_g void runcode_push(Execute ptr, struct runcode_value *value)
{
	value->signal = ptr->signal;
	value->taginfo = ptr->taginfo;
}

_g void runcode_rollback(Execute ptr, const struct runcode_value *value)
{
	ptr->signal = value->signal;
	ptr->taginfo = value->taginfo;
}

_g int runcode_free_control(Execute ptr, addr control)
{
	struct runcode_value value;

	runcode_push(ptr, &value);
	if (! free_control(ptr, control))
		runcode_rollback(ptr, &value);

	return 1;
}

_g int free_check_control(Execute ptr, addr control, int check)
{
	if (check)
		return runcode_free_control(ptr, control);
	else
		return free_control(ptr, control);
}

static void close_result_control(addr pos)
{
	addr cons, next, check;
	struct control_struct *str;

	str = StructControl(pos);
	if (str->dynamic_result) {
		GetControl(pos, Control_Result, &cons);
		for (; cons != Nil; cons = next) {
			GetCons(cons, &check, &next);
			if (GetStatusDynamic(check))
				SetCar(cons, Unbound);
		}
		str->dynamic_result = 0;
	}
	SetControl(pos, Control_Result, Nil);
}

static void copy_values_control(addr src, addr dst)
{
	int check;
	addr pos;
	size_t size, i, last;
	struct control_struct *str1, *str2;

	str1 = StructControl(src);
	size = str1->sizer;
	if (size <= ControlSize_Result) {
		check = 1;
	}
	else {
		size = ControlSize_Result;
		check = 0;
	}

	/* copy array */
	last = Control_Size + size;
	for (i = Control_Size; i < last; i++) {
		GetControl(src, i, &pos);
		SetControl(dst, i, pos);
	}

	/* copy cons */
	str2 = StructControl(dst);
	if (check) {
		SetControl(dst, Control_Result, Nil);
		close_result_control(dst);
	}
	else {
		GetControl(src, Control_Result, &pos);
		close_result_control(dst);
		SetControl(dst, Control_Result, pos);
		str2->dynamic_result = str1->dynamic_result;
	}

	/* size */
	str2->sizer = str1->sizer;
}

_g void return_values_control(Execute ptr, addr control)
{
	addr next;

	GetControl(control, Control_Next, &next);
	Check(next == Nil, "return_values_control error");
	copy_values_control(control, next);
}

static int close_return(Execute ptr, addr root)
{
	addr next;

	if (ptr->signal == ExecuteControl_Throw)
		return 1;
	GetControl(root, Control_Next, &next);
	Check(next == Nil, "close_return error");
	copy_values_control(root, next);

	return 0;
}

static int close_protect(Execute ptr, addr pos)
{
	int check;
	addr code;
	struct runcode_value value;

	/* no protect */
	gettable_control(pos, CONSTANT_COMMON_UNWIND_PROTECT, &code);
	if (code == Nil)
		return close_return(ptr, pos);

	/* execute */
	runcode_push(ptr, &value);
	ptr->signal = ExecuteControl_Run;
	ptr->taginfo = NULL;
	check = runcode_control(ptr, code);
	if (! check) {
		runcode_rollback(ptr, &value);
		return close_return(ptr, pos);
	}
	return check;
}

static int close_finalize(Execute ptr, addr pos)
{
	addr code, control;
	struct runcode_value value;

	/* no protect */
	gettable_control(pos, CONSTANT_COMMON_UNWIND_PROTECT, &code);
	if (code == Nil)
		return 0;

	/* runcode */
	push_close_control(ptr, &control);
	runcode_push(ptr, &value);
	if (runcode_control(ptr, code)) {
		return runcode_free_control(ptr, control);
	}
	else {
		runcode_rollback(ptr, &value);
		return free_control(ptr, control);
	}
}

static int close_tagbody(Execute ptr, addr pos)
{
	addr cons, value;

	/* close tag */
	gettagbody_control(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		close_taginfo(value);
	}

	/* result nil */
	if (ptr->signal == ExecuteControl_Throw)
		return 1;
	GetControl(pos, Control_Next, &pos);
	Check(pos == Nil, "control error");
	StructControl(pos)->sizer = 1;
	close_result_control(pos);
	SetResultControl(pos, 0, Nil);

	return 0;
}

static int close_block(Execute ptr, addr pos)
{
	addr cons, value;

	getblock_control(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		close_blockinfo(value);
	}
	return close_return(ptr, pos);
}

static int close_restart(Execute ptr, addr pos)
{
	addr cons, value;

	getrestart_object(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		setenable_restart(value, 0);
	}
	return close_return(ptr, pos);
}

static int close_control_empty(Execute ptr, addr pos)
{
	/* do nothing */
	return 0;
}

typedef int (*close_control_calltype)(Execute ptr, addr pos);
static close_control_calltype CloseControlTable[ControlType_Size];

static int close_type_control(Execute ptr, addr pos, struct control_struct *str)
{
	close_control_calltype call = CloseControlTable[str->type];
	Check(call == NULL, "control type error");
	return (*call)(ptr, pos);
}

static void init_close_control(void)
{
	CloseControlTable[ControlType_Return] = close_return;
	CloseControlTable[ControlType_Push] = close_control_empty;
	CloseControlTable[ControlType_Close] = close_control_empty;
	CloseControlTable[ControlType_Local] = NULL;
	CloseControlTable[ControlType_Protect] = close_protect;
	CloseControlTable[ControlType_Finalize] = close_finalize;
	CloseControlTable[ControlType_TagBody] = close_tagbody;
	CloseControlTable[ControlType_Block] = close_block;
	CloseControlTable[ControlType_Restart] = close_restart;
}

static void close_lexical_control(Execute ptr, addr root)
{
	addr symbol, snapshot;

	GetControl(root, Control_Lexical, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_lexical_local(ptr, symbol, snapshot);
	}
}

static void close_special_control(Execute ptr, addr root)
{
	addr symbol, snapshot;

	GetControl(root, Control_Special, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_special_local(ptr, symbol, snapshot);
	}
}

static void close_function_control(Execute ptr, addr root)
{
	addr symbol, snapshot;

	gettable_control(root, CONSTANT_COMMON_FUNCTION, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_function_local(ptr, symbol, snapshot);
	}
}

static void close_setf_control(Execute ptr, addr root)
{
	addr symbol, snapshot;

	gettable_control(root, CONSTANT_COMMON_SETF, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_setf_local(ptr, symbol, snapshot);
	}
}

static void pop_control_common(Execute ptr)
{
	addr pos;
	LocalStack stack;
	struct control_struct *str;

	pos = ptr->control;
	Check(pos == Nil, "Execute error");
	str = StructControl(pos);
	stack = str->stack;

	/* close */
	if (str->type != ControlType_Local) {
		close_type_control(ptr, pos, str);
		close_result_control(pos);
		close_lexical_control(ptr, pos);
		close_special_control(ptr, pos);
		close_function_control(ptr, pos);
		close_setf_control(ptr, pos);
	}

	/* pop */
	GetControl(pos, Control_Next, &pos);
	ptr->control = pos;
	rollback_local(ptr->local, stack);
}

static void pop_control_unsafe(Execute ptr)
{
	int check;
	addr push;
	struct control_struct *str;

	/* push */
	str = StructControl(ptr->control);
	check = (str->type == ControlType_Push);
	if (check) {
		getresult_control(ptr, &push);
	}
	/* pop */
	pop_control_common(ptr);
	/* push */
	if (check) {
		pushargs_control(ptr, push);
	}
}

_g int free_control(Execute ptr, addr control)
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
		pop_control_unsafe(ptr);
	} while (root != control);

	return 0;
}


/*
 *  getcontrol
 */
_g void getdata_control(Execute ptr, addr *ret)
{
	GetControl(ptr->control, Control_Data, ret);
}

_g void setdata_control(Execute ptr, addr value)
{
	SetControl(ptr->control, Control_Data, value);
}

_g void array_data_control(Execute ptr, size_t size)
{
	addr pos;
	vector4_local(ptr->local, &pos, size);
	SetControl(ptr->control, Control_Data, pos);
}

_g void getdata_array_control(Execute ptr, size_t index, addr *ret)
{
	addr array;

	GetControl(ptr->control, Control_Data, &array);
	CheckType(array, LISPTYPE_VECTOR);
	GetArrayA4(array, index, ret);
}

_g void setdata_array_control(Execute ptr, size_t index, addr value)
{
	addr array;

	GetControl(ptr->control, Control_Data, &array);
	CheckType(array, LISPTYPE_VECTOR);
	SetArrayA4(array, index, value);
}

_g void pushhandler_control(Execute ptr, addr name, addr call, int escape)
{
	addr pos;

	/* condition */
	if (symbolp(name))
		clos_find_class(name, &name);
	if (! conditionp(name))
		_fmte("The value ~S must be a condition instance.", name, NULL);

	/* A2 [name call] escape=SetUser */
	vector2_local(ptr->local, &pos, 2);
	SetArrayA2(pos, 0, name);
	SetArrayA2(pos, 1, call);
	SetUser(pos, (byte)escape);

	/* push handler */
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

_g void pushobject_restart_control(Execute ptr, addr object)
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
	pushobject_restart_control(ptr, pos);
}

_g void reverse_restart_control(Execute ptr)
{
	addr control, cons;

	control = ptr->control;
	gettable_control(control, CONSTANT_SYSTEM_RESTART, &cons);
	nreverse_list_unsafe(&cons, cons);
	settable_control(ptr->local, control, CONSTANT_SYSTEM_RESTART, cons);
}

static int gethandler_control(addr pos, addr *ret)
{
	return gettable_control(pos, CONSTANT_SYSTEM_HANDLER, ret);
}

static int wake_call_handler(Execute ptr, addr control, addr call, addr pos, int escape)
{
	addr tag;

	if (funcall_control(ptr, call, pos, NULL))
		return 1;
	if (escape) {
		copy_values_control(ptr->control, control);
		getcondition_control(control, &tag);
		ptr->signal = ExecuteControl_Throw;
		ptr->taginfo = StructTagInfo(tag);
		exit_control(ptr);
		return 0;
	}

	return 0;
}

static int wake_handler(Execute ptr, addr control, addr instance, addr array)
{
	int escape;
	addr clos, value;

	GetArrayA2(array, 0, &clos);
	escape = GetUser(array);
	if (clos != Nil && clos_subtype_p(instance, clos)) {
		GetArrayA2(array, 1, &value);
		Return(wake_call_handler(ptr, control, value, instance, escape));
	}

	return 0;
}

_g int find_condition_control(Execute ptr, addr instance)
{
	addr control, list, array, clos;

	for (control = ptr->control; control != Nil; ) {
		gethandler_control(control, &list);
		while (list != Nil) {
			GetCons(list, &array, &list);
			GetArrayA2(array, 0, &clos);
			if (clos != Nil && clos_subtype_p(instance, clos))
				return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	return 0;
}

_g int invoke_handler_control(Execute ptr, addr instance)
{
	addr control, cons, array;

	control = ptr->control;
	while (control != Nil) {
		gethandler_control(control, &cons);
		while (cons != Nil) {
			GetCons(cons, &array, &cons);
			if (wake_handler(ptr, control, instance, array))
				return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	return 0;
}

static int wake_call_handler_(Execute ptr, addr control, addr call, addr pos, int escape)
{
	addr tag;

	Return(funcall_control(ptr, call, pos, NULL));
	if (escape) {
		copy_values_control(ptr->control, control);
		getcondition_control(control, &tag);
		ptr->signal = ExecuteControl_Throw;
		ptr->taginfo = StructTagInfo(tag);
		return 1;
	}

	return 0;
}

static int wake_handler_(Execute ptr, addr control, addr instance, addr array)
{
	int escape;
	addr clos, value;

	GetArrayA2(array, 0, &clos);
	escape = GetUser(array);
	if (clos != Nil && clos_subtype_p(instance, clos)) {
		GetArrayA2(array, 1, &value);
		Return(wake_call_handler_(ptr, control, value, instance, escape));
	}

	return 0;
}

_g int invoke_handler_control_(Execute ptr, addr instance)
{
	addr control, cons, array;

	control = ptr->control;
	while (control != Nil) {
		gethandler_control(control, &cons);
		while (cons != Nil) {
			GetCons(cons, &array, &cons);
			Return(wake_handler_(ptr, control, instance, array));
		}
		GetControl(control, Control_Next, &control);
	}

	return 0;
}

static void redirect_restart(addr restart, addr *ret)
{
	for (;;) {
		if (! getenable_restart(restart))
			_fmte("The restart ~S is already closed.", restart, NULL);
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
		getrestart_object(control, &list);
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
	_fmte("The restart ~S is invalid.", restart, NULL);
}

_g int invoke_restart_control(Execute ptr, addr restart, addr args)
{
	int escape;
	addr call, tag, control;

	if (symbolp(restart)) {
		if (! find_restart_control(ptr, restart, Nil, &call))
			_fmte("The restart name ~S is not found.", restart, NULL);
		restart = call;
	}

	escape = getescape_restart(restart);
	redirect_restart(restart, &restart);
	getfunction_restart(restart, &call);
	if (apply_control(ptr, call, args)) {
		return 1;
	}

	if (escape) {
		rollback_restart_control(ptr->control, restart, &control);
		copy_values_control(ptr->control, control);
		getrestart_control(control, &tag);
		ptr->signal = ExecuteControl_Throw;
		ptr->taginfo = StructTagInfo(tag);
		exit_control(ptr);
		return 1;
	}

	return 0;
}

_g int invoke_restart_interactively_control(Execute ptr, addr restart)
{
	addr args, value;

	if (symbolp(restart)) {
		if (! find_restart_control(ptr, restart, Nil, &value))
			_fmte("The restart name ~S is not found.", restart, NULL);
		restart = value;
	}

	redirect_restart(restart, &restart);
	getinteractive_restart(restart, &args);
	if (args != Nil) {
		if (callclang_apply(ptr, &args, args, Nil)) {
			return 1;
		}
	}

	return invoke_restart_control(ptr, restart, args);
}

static int condition_test(Execute ptr, addr restart, addr condition)
{
	addr test;

	if (condition == Nil) return 1;
	gettest_restart(restart, &test);
	if (test == Nil) return 1;
	if (callclang_funcall(ptr, &test, test, condition, NULL)) {
		exit_control(ptr);
	}

	return test != Nil;
}

static int equal_restart(Execute ptr,
		addr restart, addr symbol, addr condition, addr *ret)
{
	addr check;

	getname_restart(restart, &check);
	if (check == symbol && condition_test(ptr, restart, condition)) {
		*ret = restart;
		return 1;
	}

	return 0;
}

static int find_restart_stack(Execute ptr, addr symbol, addr condition, addr *ret)
{
	addr control, list, restart;

	control = ptr->control;
	while (control != Nil) {
		getrestart_object(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			redirect_restart(restart, &restart);
			if (equal_restart(ptr, restart, symbol, condition, ret))
				return 1;
		}
		GetControl(control, Control_Next, &control);
	}

	return 0; /* not found */
}

_g int find_restart_control(Execute ptr, addr name, addr condition, addr *ret)
{
	/* name = restart */
	if (GetType(name) == LISPTYPE_RESTART) {
		if (getenable_restart(name)) {
			*ret = name;
			return 1;
		}
		else {
			return 0; /* disable */
		}
	}

	/* name = symbol */
	if (name == Nil)
		_fmte("The restart name ~S must not be a NIL.", name, NULL);
	if (! symbolp(name))
		_fmte("The argument ~S must be a symbol.", name, NULL);
	if (condition != Nil && (! condition_instance_p(condition)))
		_fmte("The argument ~S must be a NIL or condition.", condition, NULL);
	return find_restart_stack(ptr, name, condition, ret);
}

_g void compute_restarts_control(Execute ptr, addr condition, addr *ret)
{
	addr control, root, list, restart;
	LocalHold hold;

	if (condition != Nil && (! condition_instance_p(condition)))
		_fmte("The argument ~S must be a NIL or condition.", condition, NULL);
	hold = LocalHold_array(ptr, 1);
	control = ptr->control;
	for (root = Nil; control != Nil; ) {
		getrestart_object(control, &list);
		while (list != Nil) {
			GetCons(list, &restart, &list);
			redirect_restart(restart, &restart);
			if (condition_test(ptr, restart, condition)) {
				pushnew_heap(root, restart, &root);
				localhold_set(hold, 0, root);
			}
		}
		GetControl(control, Control_Next, &control);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);
}


/*
 *  argument
 */
_g void setargs_control(Execute ptr, addr value)
{
	setargs_nil_control(ptr);
	pushargs_control(ptr, value);
}

_g void setargs_va_control(Execute ptr, ...)
{
	addr pos;
	va_list args;

	setargs_nil_control(ptr);
	va_start(args, ptr);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL) break;
		pushargs_control(ptr, pos);
	}
	va_end(args);
}

_g void setargs_nil_control(Execute ptr)
{
	addr root;

	root = ptr->control;
	Check(root == Nil, "control error");
	SetControl(root, Control_Cons, Nil);
	SetControl(root, Control_ConsTail, Nil);
}

_g void setargs_list_control(Execute ptr, addr list)
{
	setargs_nil_control(ptr);
	pushargs_list_control(ptr, list);
}

_g void setargs_list_control_unsafe(Execute ptr, addr list)
{
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);
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

_g void pushargs_list_control(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushargs_control(ptr, pos);
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
		str->dynamic_result = 1;
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

	root = ptr->control;
	Check(root == Nil, "control error");
	StructControl(root)->sizer = 0;
	close_result_control(root);
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

_g void getvalues_nil_control(Execute ptr, size_t index, addr *ret)
{
	getvalues_control(ptr, index, ret);
	if (*ret == Unbound) *ret = Nil;
}

static void list_from_vector(LocalRoot local,
		addr root, size_t size, addr cons, addr *ret)
{
	addr pos;
	size_t i;

	Check(size == 0, "size error");
	Check(ControlSize_Result < size, "size error");
	for (i = Control_Size + size - 1; ; i--) {
		GetControl(root, i, &pos);
		cons_alloc(local, &cons, pos, cons);
		if (i <= Control_Size) break;
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
		list_from_vector(local, root, size, Nil, ret);
	}
	else {
		GetControl(root, Control_Result, &cons);
		copy_list_alloc_unsafe(local, &cons, cons);
		list_from_vector(local, root, ControlSize_Result, cons, ret);
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

_g void getvalues_unsafe_control(Execute ptr, size_t index, addr *ret)
{
	Check(ControlSize_Result <= index, "size error");
	GetResultControl(ptr->control, index, ret);
}

_g void setvalues_unsafe_control(Execute ptr, size_t index, addr value)
{
	Check(ControlSize_Result <= index, "size error");
	SetResultControl(ptr->control, index, value);
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
 *  execute
 */
#ifdef LISP_DEBUG_TRACE
#include <sys/time.h>
static unsigned roottime = 0;
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

	output_timestamp();
	gettable_control(control, CONSTANT_SYSTEM_CODE, &code);
	getarray_code(code, &code);
	GetArrayA4(code, point, &code);
	infoprint(code);
}
#define OutputTrace(a,b) output_trace(a,b)
#else
#define OutputTrace(a,b)
#endif

static void runinfo(Execute ptr,
		addr *retcontrol,
		const pointer **retcall,
		addr **retargs)
{
	addr control, pos, *args;
	struct control_struct *str;
	const pointer *call;

	control = ptr->control;
	str = StructControl(control);
	call = str->call;
	GetControl(control, Control_Args, &pos);
	Check(pos == Nil, "run_interpret error");
	args = (addr *)PtrArrayA4(pos);

	*retcontrol = control;
	*retcall = call;
	*retargs = args;
}

static void runcode_update_point(Execute ptr)
{
	struct control_struct *str1;
	struct taginfo_struct *str2;

	str1 = StructControl(ptr->control);
	str2 = ptr->taginfo;
	str1->point = str2->point;
}

static int runcode_throw(Execute ptr, codejump *jump)
{
	struct taginfo_struct *str;

	str = ptr->taginfo;
	/* throw */
	if (str->control != ptr->control) {
		if (jump && str->wake)
			throw_switch(jump);
		return 0;
	}

	/* block */
	if (str->thr) {
		ptr->signal = ExecuteControl_Run;
		return 0;
	}

	/* tagbody */
	ptr->signal = ExecuteControl_Run;
	runcode_update_point(ptr);
	return 1;
}

static int runcode_signal(Execute ptr, codejump *jump)
{
	switch (ptr->signal) {
		case ExecuteControl_End:
			ptr->signal = ExecuteControl_Run;
			return 0;

		case ExecuteControl_Point:
			ptr->signal = ExecuteControl_Run;
			return 1;

		case ExecuteControl_Throw:
			return runcode_throw(ptr, jump);

		default:
			Abort("signal error");
			return 0;
	}
}

static size_t *runcode_point(Execute ptr)
{
	struct control_struct *str;
	str = StructControl(ptr->control);
	return &str->point;
}

static int runcode_execute(Execute ptr)
{
	addr control, *args;
	enum ExecuteControl *signal;
	const pointer *call;
	callbind_code code;
	size_t point, *index;

	runinfo(ptr, &control, &call, &args);
	signal = &ptr->signal;
	index = runcode_point(ptr);
point:
	while (*signal == ExecuteControl_Run) {
		point = (*index)++;
		OutputTrace(control, point);
		GetPointer_code(call[point], &code);
		(code)(ptr, args[point]);
		/* counter */
		ControlCounter++;
#ifdef LISP_DEBUG_FORCE_GC
		if (GcCounterForce && (ControlCounter % GcCounterForce) == 0)
			gcstate_execute();
#endif
	}
	if (ptr->state == ThreadState_Signal)
		gcsync(ptr);
	if (runcode_signal(ptr, NULL))
		goto point;

	return ptr->signal != ExecuteControl_Run;
}

static int runcode_normal(Execute ptr, addr code)
{
	addr control, args;
	struct control_struct *str;

	control = ptr->control;
	str = StructControl(control);
	/* set code (for garbage collect) */
	setcode_control(ptr->local, control, code);
	/* pointer */
	str->point = 0;
	str->call = getcalltype_code(code);
	/* argument */
	getargs_code(code, &args);
	SetControl(control, Control_Args, args);
	/* run */
	return runcode_execute(ptr);
}

static void rollback_control(Execute ptr, addr control)
{
	while (ptr->control != control)
		pop_control_unsafe(ptr);
}

static int runcode_switch(Execute ptr, addr code)
{
	addr control;
	codejump jump;

	control = ptr->control;
point:
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump))
		(void)runcode_normal(ptr, code);
	end_switch(&jump);
	if (codejump_control_p(&jump))
		throw_switch(&jump);
	if (ptr->state == ThreadState_Signal)
		gcsync(ptr);
	if (ptr->signal == ExecuteControl_Run)
		return 0;
	if (ptr->signal == ExecuteControl_Throw)
		rollback_control(ptr, control);
	if (runcode_signal(ptr, &jump))
		goto point;

	return ptr->signal != ExecuteControl_Run;
}

static int runcode_no_switch(Execute ptr, addr code)
{
	addr control;

	control = ptr->control;
point:
	(void)runcode_normal(ptr, code);
	if (ptr->state == ThreadState_Signal)
		gcsync(ptr);
	if (ptr->signal == ExecuteControl_Run)
		return 0;
	if (ptr->signal == ExecuteControl_Throw)
		rollback_control(ptr, control);
	if (runcode_signal(ptr, NULL))
		goto point;

	return ptr->signal != ExecuteControl_Run;
}

static int runcode_free(Execute ptr,
		addr control, addr code, int (*call)(Execute, addr))
{
	int check = (*call)(ptr, code);
	return free_check_control(ptr, control, check);
}

static int runcode_control_return(Execute ptr, addr code)
{
	addr control;
	push_return_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_argument(Execute ptr, addr code)
{
	addr control;
	push_argument_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_push(Execute ptr, addr code)
{
	addr control;
	push_push_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_close(Execute ptr, addr code)
{
	addr control;
	push_close_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_protect(Execute ptr, addr code)
{
	addr control;
	push_protect_control(ptr, &control);
	setprotect_control(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_switch);
}

static int runcode_control_tagbody(Execute ptr, addr code)
{
	addr control;

	push_tagbody_control(ptr, &control);
	push_taginfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_block(Execute ptr, addr code)
{
	addr control;

	push_block_control(ptr, &control);
	push_blockinfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_catch(Execute ptr, addr code)
{
	addr control;

	push_return_control(ptr, &control);
	push_catchinfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_no_switch);
}

static int runcode_control_condition(Execute ptr, addr code)
{
	addr control, pos;

	push_return_control(ptr, &control);
	taginfo_heap(&pos, control, Unbound, 0, 1);
	setcondition_control(ptr->local, control, pos);
	return runcode_free(ptr, control, code, runcode_switch);
}

_g void push_restart_initialize_control(Execute ptr, addr *ret)
{
	addr control, pos;

	push_restart_control(ptr, &control);
	taginfo_heap(&pos, control, Unbound, 0, 1);
	StructTagInfo(pos)->wake = 1;
	setrestart_control(ptr->local, control, pos);
	*ret = control;
}

static int runcode_control_restart(Execute ptr, addr code)
{
	addr control;
	push_restart_initialize_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_switch);
}

typedef int (*runcode_control_calltype)(Execute ptr, addr code);
static runcode_control_calltype RunCodeControlTable[CodeType_Size];

_g int runcode_control(Execute ptr, addr code)
{
	enum CodeType type;
	runcode_control_calltype call;

	CheckType(code, LISPTYPE_CODE);
	type = gettype_code(code);
	call = RunCodeControlTable[type];
	Check(call == NULL, "CodeType error");
	return (*call)(ptr, code);
}

static void init_runcode_control(void)
{
	RunCodeControlTable[CodeType_Default] = runcode_no_switch;
	RunCodeControlTable[CodeType_Return] = runcode_control_return;
	RunCodeControlTable[CodeType_Argument] = runcode_control_argument;
	RunCodeControlTable[CodeType_Push] = runcode_control_push;
	RunCodeControlTable[CodeType_Remove] = runcode_control_return;
	RunCodeControlTable[CodeType_Close] = runcode_control_close;
	RunCodeControlTable[CodeType_Protect] = runcode_control_protect;
	RunCodeControlTable[CodeType_TagBody] = runcode_control_tagbody;
	RunCodeControlTable[CodeType_Block] = runcode_control_block;
	RunCodeControlTable[CodeType_Catch] = runcode_control_catch;
	RunCodeControlTable[CodeType_Condition] = runcode_control_condition;
	RunCodeControlTable[CodeType_Restart] = runcode_control_restart;
}

_g int signal_control(Execute ptr)
{
	Check(ptr->signal == ExecuteControl_Point, "signal error");
	return ptr->signal != ExecuteControl_Run;
}

static int execute_normal(Execute ptr, addr pos)
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

static void pushlexical_closure(Execute ptr, addr pos, addr value)
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

static void execute_function_closure(Execute ptr, addr pos)
{
	addr key, value, list;

	/* value */
	GetClosureValueFunction(pos, &list);
	while (list != Nil) {
		GetCons(list, &key, &list);
		GetCons(key, &key, &value);
		pushlexical_closure(ptr, key, value);
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

static int execute_function(Execute ptr, addr pos)
{
	addr control, name;
	struct function_struct *str;

	str = StructFunction(pos);
	if (str->recursive == 0 && str->closure == 0)
		return execute_normal(ptr, pos);

	/* closure or recursive */
	push_argument_control(ptr, &control);
	if (str->recursive) {
		GetNameFunction(pos, &name);
		pushcallname_control(ptr, name, pos);
	}
	execute_function_closure(ptr, pos);
	return runcode_free(ptr, control, pos, execute_normal);
}

_g int execute_control(Execute ptr, addr call)
{
	addr args;

	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");
	switch (GetType(call)) {
		case LISPTYPE_FUNCTION:
			return execute_function(ptr, call);

		case LISPTYPE_CLOS:
			getargs_list_control_unsafe(ptr, 0, &args);
			closrun_execute(ptr, call, args);
			return signal_control(ptr);

		default:
			Abort2("type error: %d", (int)GetType(call));
			return 1;
	}
}

static int checkargs_var(Execute ptr, addr array, addr *args)
{
	addr value, type;

	GetArrayA2(array, 0, &array); /* var */
	while (array != Nil) {
		if (*args == Nil)
			_fmte("Too few argument.", NULL);
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
			_fmte("Too many argument.", NULL);
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
		_fmte("Invalid keyword argument.", NULL);

	return 0;
}

static int execute_checkargs(Execute ptr, addr array, addr args)
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

static int execute_typecheck(Execute ptr, addr call, addr args)
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
	return execute_checkargs(ptr, type, args);
}

_g int apply_control(Execute ptr, addr call, addr args)
{
	Check(ptr->control == Nil, "root error");
	Check(call == Unbound, "Function is Unbound.");
	switch (GetType(call)) {
		case LISPTYPE_FUNCTION:
			if (execute_typecheck(ptr, call, args))
				return 1;
			setargs_list_control_unsafe(ptr, args);
			return execute_function(ptr, call);

		case LISPTYPE_CLOS:
			closrun_execute(ptr, call, args);
			return signal_control(ptr);

		default:
			Abort2("type error: %d", (int)GetType(call));
			return 1;
	}
}

_g int applya_control(Execute ptr, addr call, ...)
{
	addr args;
	va_list va;

	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &args, va);
	va_end(va);
	return apply_control(ptr, call, args);
}

_g int stdarg_control(Execute ptr, addr call, va_list args)
{
	addr list;
	list_alloc_stdarg(ptr->local, &list, args);
	return apply_control(ptr, call, list);
}

_g int funcall_control(Execute ptr, addr call, ...)
{
	int result;
	va_list args;

	va_start(args, call);
	result = stdarg_control(ptr, call, args);
	va_end(args);

	return result;
}

_g int call_control(Execute ptr, addr args)
{
	addr call;

	Check(args == Nil, "argument error");
	GetCons(args, &call, &args);
	return apply_control(ptr, call, args);
}

_g int goto_control_(Execute ptr, size_t point)
{
	struct control_struct *str;

	str = StructControl(ptr->control);
	str->point = point;
	ptr->signal = ExecuteControl_Point;

	return 1;
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
		_fmte("Cannot find tag ~S.", tag, NULL);
	str = StructTagInfo(pos);
	if (str->open == 0)
		_fmte("Tag ~S already closed.", tag, NULL);

	/* rollback */
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = str;
	return 1;
}

static int return_from_find_control(Execute ptr, addr *next, addr *ret, addr name)
{
	addr control, list, info, check;

	for (control = ptr->control; control != Nil; ) {
		getblock_control(control, &list);
		while (list != Nil) {
			GetCons(list, &info, &list);
			GetNameTagInfo(info, &check);
			if (eql_function(check, name)) {
				*ret = info;
				*next = StructTagInfo(info)->control;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*next = *ret = 0;

	return 0;
}

_g int return_from_control_(Execute ptr, addr name)
{
	addr pos, next;
	struct taginfo_struct *str;

	/* find name */
	if (! return_from_find_control(ptr, &next, &pos, name))
		_fmte("Cannot find block name ~S.", name, NULL);
	str = StructTagInfo(pos);
	if (str->open == 0)
		_fmte("Block ~S already closed.", name, NULL);

	/* copy values */
	copy_values_control(ptr->control, next);
	/* rollback */
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = str;
	return 1;
}

_g void catch_control(Execute ptr, addr name)
{
	addr pos;
	getcatch_control(ptr->control, &pos);
	SetNameTagInfo(pos, name);
}

static int throw_find_control(Execute ptr, addr *next, addr *ret, addr name)
{
	addr control, pos, check;

	control = ptr->control;
	while (control != Nil) {
		if (getcatch_control(control, &pos)) {
			GetNameTagInfo(pos, &check);
			if (check == name) {
				*ret = pos;
				*next = control;
				return 1;
			}
		}
		GetControl(control, Control_Next, &control);
	}
	*next = *ret = 0;

	return 0;
}

_g int throw_control_(Execute ptr, addr name)
{
	addr pos, next;

	/* find name */
	if (! throw_find_control(ptr, &next, &pos, name))
		_fmte("Cannot find catch name ~S.", name, NULL);
	/* copy values */
	copy_values_control(ptr->control, next);
	/* rollback */
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = StructTagInfo(pos);
	return 1;
}

_g void gettagbody_execute(Execute ptr, addr *ret, addr name)
{
	if (! go_find_control(ptr, ret, name))
		_fmte("Cannot find tag name ~S", name, NULL);
}

_g void getblock_execute(Execute ptr, addr *ret, addr name)
{
	addr temp;
	if (! return_from_find_control(ptr, &temp, ret, name))
		_fmte("Cannot find block name ~S", name, NULL);
}

static int eval_control_p(addr control)
{
	gettable_control(control, CONSTANT_SYSTEM_EVAL_LEXICAL, &control);
	return control != Nil;
}

static void set_eval_control(LocalRoot local, addr control)
{
	settable_control(local, control, CONSTANT_SYSTEM_EVAL_LEXICAL, T);
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
	set_eval_control(ptr->local, control);
}


/*
 *  C language
 */
_g int callablep(addr pos)
{
	Check(pos == Unbound, "type error");
	switch (GetType(pos)) {
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CALLNAME:
		case LISPTYPE_FUNCTION:
			return 1;

		case LISPTYPE_CLOS:
			return clos_funcallable_p(pos);

		default:
			return 0;
	}
}

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
			_fmte("The object ~S cannot execute.", call, NULL);
			break;
	}
}

static int values_apply(Execute ptr, LocalRoot local,
		addr *ret, addr call, addr cons)
{
	addr control;
	LocalHold hold;

	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	if (apply_control(ptr, call, cons)) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getvalues_list_control(ptr, local, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}

_g int callclang_values_apply_local(Execute ptr, addr *ret, addr call, addr cons)
{
	return values_apply(ptr, ptr->local, ret, call, cons);
}

_g int callclang_values_apply_heap(Execute ptr, addr *ret, addr call, addr cons)
{
	return values_apply(ptr, NULL, ret, call, cons);
}

static int values_stdarg(Execute ptr, LocalRoot local,
		addr *ret, addr call, va_list args)
{
	addr control;
	LocalHold hold;

	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	if (stdarg_control(ptr, call, args)) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getvalues_list_control(ptr, local, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}

_g int callclang_values_stdarg_local(Execute ptr, addr *ret, addr call, va_list args)
{
	return values_stdarg(ptr, ptr->local, ret, call, args);
}

_g int callclang_values_stdarg_heap(Execute ptr, addr *ret, addr call, va_list args)
{
	return values_stdarg(ptr, NULL, ret, call, args);
}

_g int callclang_values_funcall_local(Execute ptr, addr *ret, addr call, ...)
{
	int result;
	va_list args;

	va_start(args, call);
	result = values_stdarg(ptr, ptr->local, ret, call, args);
	va_end(args);

	return result;
}

_g int callclang_values_funcall_heap(Execute ptr, addr *ret, addr call, ...)
{
	int result;
	va_list args;

	va_start(args, call);
	result = values_stdarg(ptr, NULL, ret, call, args);
	va_end(args);

	return result;
}

_g int callclang_values_char_local(Execute ptr, addr *ret,
		const char *package, const char *name, ...)
{
	int result;
	addr call;
	va_list args;

	internchar(package, name, &call);
	va_start(args, name);
	result = values_stdarg(ptr, ptr->local, ret, call, args);
	va_end(args);

	return result;
}

_g int callclang_values_char_heap(Execute ptr, addr *ret,
		const char *package, const char *name, ...)
{
	int result;
	addr call;
	va_list args;

	internchar(package, name, &call);
	va_start(args, name);
	result = values_stdarg(ptr, NULL, ret, call, args);
	va_end(args);

	return result;
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
	if (apply_control(ptr, call, cons)) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}

_g int callclang_applya(Execute ptr, addr *ret, addr call, ...)
{
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lista_stdarg_alloc(ptr->local, &args, va);
	va_end(va);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

_g int callclang_stdarg(Execute ptr, addr *ret, addr call, va_list args)
{
	addr control;
	LocalHold hold;

	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	if (stdarg_control(ptr, call, args)) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}

_g int callclang_funcall(Execute ptr, addr *ret, addr call, ...)
{
	int check;
	addr control;
	va_list args;
	LocalHold hold;

	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	va_start(args, call);
	check = stdarg_control(ptr, call, args);
	va_end(args);
	if (check) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}

_g int callclang_char(Execute ptr, addr *ret,
		const char *package, const char *name, ...)
{
	int check;
	addr control, call;
	va_list args;
	LocalHold hold;

	internchar(package, name, &call);
	callclang_function(ptr, &call, call);
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	va_start(args, name);
	check = stdarg_control(ptr, call, args);
	va_end(args);
	if (check) {
		Return(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return(free_control(ptr, control));
	}
	localhold_end(hold);

	return 0;
}


/*
 *  debug
 */
static void output_stack_control(addr control)
{
	addr pos;
	struct control_struct *str;

	str = StructControl(control);
	GetControl(control, Control_Special, &pos);
	info("Stack: %p", (void *)str->stack);
	info_noeol("Special: ");
	infoprint(pos);
}

static void info_stack_control(addr control, size_t index)
{
	if (index == 0) {
		info("**************************");
		info("**** STACK-FRAME BEGIN ***");
	}
	else {
		info("**** STACK-FRAME %zu ***", index);
	}
	output_stack_control(control);
	GetControl(control, Control_Next, &control);
	if (control != Nil)
		info_stack_control(control, index + 1);
}

_g void info_control(addr control)
{
	info_stack_control(control, 0);
	info("**** STACK-FRAME END   ***");
	info("**************************");
}


/*
 *  initialize
 */
_g void init_control(void)
{
	init_close_control();
	init_callbind_control();
	init_runcode_control();

#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = LISP_DEBUG_FORCE_GC;
#endif
	ControlCounter = 0;
}

