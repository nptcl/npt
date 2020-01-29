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
enum Control_Index {
	Control_Next,
	Control_Cons,
	Control_ConsTail,
	Control_Result,
	Control_Lexical,
	Control_Special,
	Control_Table,
	Control_Data,
	Control_Args,
	Control_Size
};

enum ControlType_Index {
	ControlType_Return,
	ControlType_Push,
	ControlType_Close,
	ControlType_Local,
	ControlType_Protect,
	ControlType_Finalize,
	ControlType_TagBody,
	ControlType_Block,
	ControlType_Restart,
	ControlType_Size
};

struct control_struct {
	unsigned dynamic_result : 1;
	enum ControlType_Index type;
	LocalStack stack;
	const pointer *call;
	size_t sizer, point;
};

#ifdef LISP_DEBUG
#define ControlSize_Result			2
#else
#define ControlSize_Result			8
#endif
#define ControlSize_Arary			(Control_Size + ControlSize_Result)
#define PtrBodyControl(p)			PtrBodySSa(p, ControlSize_Arary)
#define StructControl(p)			((struct control_struct *)PtrBodyControl(p))
#define GetControl					GetArraySS
#define SetControl					SetArraySS

#define GetResultControl(p,i,v)		GetControl((p),(i) + Control_Size,(v))
#define SetResultControl(p,i,v)		SetControl((p),(i) + Control_Size,(v))
#define exit_control(ptr)			exit_code(ptr, LISPCODE_CONTROL);

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

static void close_result(addr pos)
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
		close_result(dst);
	}
	else {
		GetControl(src, Control_Result, &pos);
		close_result(dst);
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
	Check(next == Nil, "close_return error");
	copy_values_control(control, next);
}

static void close_return(Execute ptr, addr root)
{
	addr next;

	if (ptr->signal == ExecuteControl_Throw) return;
	GetControl(root, Control_Next, &next);
	Check(next == Nil, "close_return error");
	copy_values_control(root, next);
}

static int close_protect(Execute ptr, addr pos)
{
	int check;
	addr code;
	struct runcode_value value;

	/* no protect */
	gettable_control(pos, CONSTANT_COMMON_UNWIND_PROTECT, &code);
	if (code == Nil) {
		close_return(ptr, pos);
		return 0;
	}

	/* execute */
	runcode_push(ptr, &value);
	ptr->signal = ExecuteControl_Run;
	ptr->taginfo = NULL;
	check = runcode_control(ptr, code);
	if (! check) {
		runcode_rollback(ptr, &value);
		close_return(ptr, pos);
	}
	return check;
}

static int close_finalize(Execute ptr, addr pos)
{
	addr code, control;
	struct runcode_value value;

	/* no protect */
	gettable_control(pos, CONSTANT_COMMON_UNWIND_PROTECT, &code);
	if (code == Nil) {
		return 0;
	}

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

static void close_tagbody(Execute ptr, addr pos)
{
	addr cons, value;

	/* close tag */
	gettagbody_control(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		close_taginfo(value);
	}

	/* result nil */
	if (ptr->signal == ExecuteControl_Throw) return;
	GetControl(pos, Control_Next, &pos);
	Check(pos == Nil, "control error");
	StructControl(pos)->sizer = 1;
	close_result(pos);
	SetResultControl(pos, 0, Nil);
}

static void close_block(Execute ptr, addr pos)
{
	addr cons, value;

	getblock_control(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		close_blockinfo(value);
	}
	close_return(ptr, pos);
}

static void close_restart(Execute ptr, addr pos)
{
	addr cons, value;

	getrestart_object(pos, &cons);
	while (cons != Nil) {
		GetCons(cons, &value, &cons);
		setenable_restart(value, 0);
	}
	close_return(ptr, pos);
}

static void close_type(Execute ptr, addr pos, struct control_struct *str)
{
	switch (str->type) {
		case ControlType_Return:
			close_return(ptr, pos);
			break;

		case ControlType_Push:
		case ControlType_Close:
			break;

		case ControlType_Protect:
			close_protect(ptr, pos);
			break;

		case ControlType_Finalize:
			close_finalize(ptr, pos);
			break;

		case ControlType_TagBody:
			close_tagbody(ptr, pos);
			break;

		case ControlType_Block:
			close_block(ptr, pos);
			break;

		case ControlType_Restart:
			close_restart(ptr, pos);
			break;

		default:
			Abort("control type error");
			break;
	}
}

static void close_lexical(Execute ptr, addr root)
{
	addr symbol, snapshot;

	GetControl(root, Control_Lexical, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_lexical_local(ptr, symbol, snapshot);
	}
}

static void close_special(Execute ptr, addr root)
{
	addr symbol, snapshot;

	GetControl(root, Control_Special, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_special_local(ptr, symbol, snapshot);
	}
}

static void close_function(Execute ptr, addr root)
{
	addr symbol, snapshot;

	gettable_control(root, CONSTANT_COMMON_FUNCTION, &root);
	while (root != Nil) {
		GetCons(root, &symbol, &root);
		GetCons(root, &snapshot, &root);
		rollback_function_local(ptr, symbol, snapshot);
	}
}

static void close_setf(Execute ptr, addr root)
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
		close_type(ptr, pos, str);
		close_result(pos);
		close_lexical(ptr, pos);
		close_special(ptr, pos);
		close_function(ptr, pos);
		close_setf(ptr, pos);
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
		fmte("The value ~S must be a condition instance.", name, NULL);

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

static int wake_handler(Execute ptr, addr control, addr instance, addr array, int *ret)
{
	addr clos, value;

	GetArrayA2(array, 0, &clos);
	if (clos != Nil && clos_subtype_p(instance, clos)) {
		GetArrayA2(array, 1, &value);
		if (wake_call_handler(ptr, control, value, instance, GetUser(array)))
			return 1;
		*ret = 1;
		return 0;
	}
	*ret = 0;
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

_g int invoke_handler_control(Execute ptr, addr instance, int *ret)
{
	int result, check;
	addr control, cons, array;

	control = ptr->control;
	for (result = 0; control != Nil; ) {
		gethandler_control(control, &cons);
		while (cons != Nil) {
			GetCons(cons, &array, &cons);
			if (wake_handler(ptr, control, instance, array, &check))
				return 1;
			result |= check;
		}
		GetControl(control, Control_Next, &control);
	}
	*ret = result;
	return 0;
}

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
	fmte("The restart ~S is invalid.", restart, NULL);
}

_g int invoke_restart_control(Execute ptr, addr restart, addr args)
{
	int escape;
	addr call, tag, control;

	if (symbolp(restart)) {
		if (! find_restart_control(ptr, restart, Nil, &call))
			fmte("The restart name ~S is not found.", restart, NULL);
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
			fmte("The restart name ~S is not found.", restart, NULL);
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
		fmte("The restart name ~S must not be a NIL.", name, NULL);
	if (! symbolp(name))
		fmte("The argument ~S must be a symbol.", name, NULL);
	if (condition != Nil && (! condition_instance_p(condition)))
		fmte("The argument ~S must be a NIL or condition.", condition, NULL);
	return find_restart_stack(ptr, name, condition, ret);
}

_g void compute_restarts_control(Execute ptr, addr condition, addr *ret)
{
	addr control, root, list, restart;
	LocalHold hold;

	if (condition != Nil && (! condition_instance_p(condition)))
		fmte("The argument ~S must be a NIL or condition.", condition, NULL);
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
	close_result(root);
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
 *  call_compiled_function
 */
typedef const struct callbind_struct *callstr;
typedef void (*callbind_control)(Execute, addr, callstr);
static callbind_control CallBindTable[CallBind_size];

static void call_callbind_code(Execute ptr, addr pos, callstr call)
{
	Abort("Cannot call callbind_code in control.");
}

static void call_callbind_macro(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
	(call->call.macro)(ptr, var1, var2);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_none(Execute ptr, addr pos, callstr call)
{
	(call->call.none)();
}

static void call_callbind_any(Execute ptr, addr pos, callstr call)
{
	(call->call.any)(ptr);
}

static void call_callbind_empty(Execute ptr, addr pos, callstr call)
{
	addr check;

	GetControl(ptr->control, Control_Cons, &check);
	if (check != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
	(call->call.any)(ptr);
}

static void call_callbind_dynamic(Execute ptr, addr pos, callstr call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	(call->call.dynamic)(ptr, cons);
}

static void call_callbind_rest(Execute ptr, addr pos, callstr call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	(call->call.rest)(ptr, cons);
}

static void call_callbind_var1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
	(call->call.var1)(ptr, var1);
}

static void call_callbind_var2(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
	(call->call.var2)(ptr, var1, var2);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_var3(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	(call->call.var3)(ptr, var1, var2, var3);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_var4(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var3, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var4, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	(call->call.var4)(ptr, var1, var2, var3, var4);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_var5(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4, var5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var3, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var4, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var5, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	(call->call.var5)(ptr, var1, var2, var3, var4, var5);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_var6(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4, var5, var6;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var1, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var2, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var3, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var4, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var5, &cons);
	if (cons == Nil) goto toofew;
	getcons(cons, &var6, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	(call->call.var6)(ptr, var1, var2, var3, var4, var5, var6);
	return;
toofew:
	GetNameFunction(pos, &check);
	fmte("Too few call argument ~S.", check, NULL);
}

static void call_callbind_opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		opt1 = Unbound;
	else
		getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
	(call->call.opt1)(ptr, opt1);
}

static void call_callbind_opt2(Execute ptr, addr pos, callstr call)
{
	addr check, cons, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.opt2)(ptr, opt1, opt2);
}

static void call_callbind_opt3(Execute ptr, addr pos, callstr call)
{
	addr check, cons, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
finish:
	(call->call.opt3)(ptr, opt1, opt2, opt3);
}

static void call_callbind_opt4(Execute ptr, addr pos, callstr call)
{
	addr check, cons, opt1, opt2, opt3, opt4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = Unbound;
		goto finish;
	}
	getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = Unbound;
		goto finish;
	}
	getcons(cons, &opt4, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
finish:
	(call->call.opt4)(ptr, opt1, opt2, opt3, opt4);
}

static void call_callbind_opt5(Execute ptr, addr pos, callstr call)
{
	addr check, cons, opt1, opt2, opt3, opt4, opt5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = opt5 = Unbound;
		goto finish;
	}
	getcons(cons, &opt4, &cons);
	if (cons == Nil) {
		opt5 = Unbound;
		goto finish;
	}
	getcons(cons, &opt5, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
finish:
	(call->call.opt5)(ptr, opt1, opt2, opt3, opt4, opt5);
}

static void call_callbind_var1opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var1opt1)(ptr, var1, opt1);
}

static void call_callbind_var2opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var2opt1)(ptr, var1, var2, opt1);
}

static void call_callbind_var3opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var3opt1)(ptr, var1, var2, var3, opt1);
}

static void call_callbind_var4opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var4, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var4opt1)(ptr, var1, var2, var3, var4, opt1);
}

static void call_callbind_var5opt1(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4, var5, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var4, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var5, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var5opt1)(ptr, var1, var2, var3, var4, var5, opt1);
}

static void call_callbind_var1opt2(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var1opt2)(ptr, var1, opt1, opt2);
}

static void call_callbind_var2opt2(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var2opt2)(ptr, var1, var2, opt1, opt2);
}

static void call_callbind_var2opt3(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		fmte("Too many call argument ~S.", check, NULL);
	}
finish:
	(call->call.var2opt3)(ptr, var1, var2, opt1, opt2, opt3);
}

static void call_callbind_var1rest(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var1);
	getargs_list_control_heap(ptr, 1, &rest);
	(call->call.var1rest)(ptr, var1, rest);
}

static void call_callbind_var2rest(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var2);
	getargs_list_control_heap(ptr, 2, &rest);
	(call->call.var2rest)(ptr, var1, var2, rest);
}

static void call_callbind_opt1rest(Execute ptr, addr pos, callstr call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		getcar(cons, &opt1);
		getargs_list_control_heap(ptr, 1, &rest);
	}
	(call->call.opt1rest)(ptr, opt1, rest);
}

static void call_callbind_var1dynamic(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var1);
	getargs_list_control_unsafe(ptr, 1, &rest);
	(call->call.var1dynamic)(ptr, var1, rest);
}

static void call_callbind_var2dynamic(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var2);
	getargs_list_control_unsafe(ptr, 2, &rest);
	(call->call.var2dynamic)(ptr, var1, var2, rest);
}

static void call_callbind_var3dynamic(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var3);
	getargs_list_control_unsafe(ptr, 3, &rest);
	(call->call.var3dynamic)(ptr, var1, var2, var3, rest);
}

static void call_callbind_var4dynamic(Execute ptr, addr pos, callstr call)
{
	addr check, cons, var1, var2, var3, var4, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		fmte("Too few call argument ~S.", check, NULL);
	}
	getcar(cons, &var4);
	getargs_list_control_unsafe(ptr, 4, &rest);
	(call->call.var4dynamic)(ptr, var1, var2, var3, var4, rest);
}

static void call_callbind_opt1dynamic(Execute ptr, addr pos, callstr call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		getcar(cons, &opt1);
		getargs_list_control_unsafe(ptr, 1, &rest);
	}
	(call->call.opt1dynamic)(ptr, opt1, rest);
}

static void call_callbind_extend_dynamic(Execute ptr, addr pos, callstr call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	(call->call.extend_dynamic)(cons);
}

static void call_callbind_extend_rest(Execute ptr, addr pos, callstr call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	(call->call.extend_rest)(cons);
}

_g void init_control(void)
{
	int i;

	for (i = 0; i < CallBind_size; i++)
		CallBindTable[i] = NULL;
	CallBindTable[CallBind_code] = call_callbind_code;
	CallBindTable[CallBind_macro] = call_callbind_macro;
	CallBindTable[CallBind_none] = call_callbind_none;
	CallBindTable[CallBind_any] = call_callbind_any;
	CallBindTable[CallBind_empty] = call_callbind_empty;
	CallBindTable[CallBind_dynamic] = call_callbind_dynamic;
	CallBindTable[CallBind_rest] = call_callbind_rest;
	CallBindTable[CallBind_var1] = call_callbind_var1;
	CallBindTable[CallBind_var2] = call_callbind_var2;
	CallBindTable[CallBind_var3] = call_callbind_var3;
	CallBindTable[CallBind_var4] = call_callbind_var4;
	CallBindTable[CallBind_var5] = call_callbind_var5;
	CallBindTable[CallBind_var6] = call_callbind_var6;
	CallBindTable[CallBind_opt1] = call_callbind_opt1;
	CallBindTable[CallBind_opt2] = call_callbind_opt2;
	CallBindTable[CallBind_opt3] = call_callbind_opt3;
	CallBindTable[CallBind_opt4] = call_callbind_opt4;
	CallBindTable[CallBind_opt5] = call_callbind_opt5;
	CallBindTable[CallBind_var1opt1] = call_callbind_var1opt1;
	CallBindTable[CallBind_var2opt1] = call_callbind_var2opt1;
	CallBindTable[CallBind_var3opt1] = call_callbind_var3opt1;
	CallBindTable[CallBind_var4opt1] = call_callbind_var4opt1;
	CallBindTable[CallBind_var5opt1] = call_callbind_var5opt1;
	CallBindTable[CallBind_var1opt2] = call_callbind_var1opt2;
	CallBindTable[CallBind_var2opt2] = call_callbind_var2opt2;
	CallBindTable[CallBind_var2opt3] = call_callbind_var2opt3;
	CallBindTable[CallBind_var1rest] = call_callbind_var1rest;
	CallBindTable[CallBind_var2rest] = call_callbind_var2rest;
	CallBindTable[CallBind_opt1rest] = call_callbind_opt1rest;
	CallBindTable[CallBind_var1dynamic] = call_callbind_var1dynamic;
	CallBindTable[CallBind_var2dynamic] = call_callbind_var2dynamic;
	CallBindTable[CallBind_var3dynamic] = call_callbind_var3dynamic;
	CallBindTable[CallBind_var4dynamic] = call_callbind_var4dynamic;
	CallBindTable[CallBind_opt1dynamic] = call_callbind_opt1dynamic;
	CallBindTable[CallBind_extend_dynamic] = call_callbind_extend_dynamic;
	CallBindTable[CallBind_extend_rest] = call_callbind_extend_rest;

#ifdef LISP_DEBUG_FORCE_GC
	GcCounterForce = LISP_DEBUG_FORCE_GC;
#endif
	ControlCounter = 0;
}

static int call_compiled_function(Execute ptr, addr compiled)
{
	struct callbind_struct *str;
	callbind_control call;
	pointer p;

	p = StructFunction(compiled)->index;
	str = &(pointer_table[p]);
	Check(CallBind_size <= str->type, "index error");
	call = CallBindTable[str->type];
	Check(call == NULL, "call error. (build_control?)");
	(*call)(ptr, compiled, str);

	return ptr->signal != ExecuteControl_Run;
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

static int runcode_free(Execute ptr,
		addr control, addr code, int (*call)(Execute, addr))
{
	int check = (*call)(ptr, code);
	return free_check_control(ptr, control, check);
}

static int runcode_tagbody(Execute ptr, addr code)
{
	addr control;

	push_tagbody_control(ptr, &control);
	push_taginfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_switch);
}

static int runcode_block(Execute ptr, addr code)
{
	addr control;

	push_block_control(ptr, &control);
	push_blockinfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_switch);
}

static int runcode_catch(Execute ptr, addr code)
{
	addr control;

	push_return_control(ptr, &control);
	push_catchinfo(ptr->local, control, code);
	return runcode_free(ptr, control, code, runcode_switch);
}

static int runcode_condition(Execute ptr, addr code)
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

static int runcode_restart(Execute ptr, addr code)
{
	addr control;
	push_restart_initialize_control(ptr, &control);
	return runcode_free(ptr, control, code, runcode_switch);
}

_g int signal_control(Execute ptr)
{
	Check(ptr->signal == ExecuteControl_Point, "signal error");
	return ptr->signal != ExecuteControl_Run;
}

_g int runcode_control(Execute ptr, addr code)
{
	addr control;

	CheckType(code, LISPTYPE_CODE);
	switch (gettype_code(code)) {
		case CodeType_Return:
		case CodeType_Remove:
			push_return_control(ptr, &control);
			break;

		case CodeType_Argument:
			push_argument_control(ptr, &control);
			break;

		case CodeType_Push:
			push_push_control(ptr, &control);
			break;

		case CodeType_Close:
			push_close_control(ptr, &control);
			break;

		case CodeType_TagBody:
			return runcode_tagbody(ptr, code);

		case CodeType_Block:
			return runcode_block(ptr, code);

		case CodeType_Catch:
			return runcode_catch(ptr, code);

		case CodeType_Condition:
			return runcode_condition(ptr, code);

		case CodeType_Restart:
			return runcode_restart(ptr, code);

		case CodeType_Protect:
			push_protect_control(ptr, &control);
			setprotect_control(ptr->local, control, code);
			break;

		case CodeType_Default:
		default:
			return runcode_switch(ptr, code);
	}

	return runcode_free(ptr, control, code, runcode_switch);
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

static void execute_recursive(Execute ptr, addr pos)
{
	addr name;

	GetNameFunction(pos, &name);
	pushcallname_control(ptr, name, pos);
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

static void execute_closure(Execute ptr,
		addr cons1, addr cons2, addr cons3, addr cons4)
{
	addr key, pos;

	/* value */
	while (cons1 != Nil) {
		GetCons(cons1, &key, &cons1);
		GetCons(key, &key, &pos);
		pushlexical_closure(ptr, key, pos);
	}

	/* function / setf */
	while (cons2 != Nil) {
		GetCons(cons2, &key, &cons2);
		GetCons(key, &key, &pos);
		pushcallname_control(ptr, key, pos);
	}

	/* tagbody */
	while (cons3 != Nil) {
		GetCons(cons3, &key, &cons3);
		GetCons(key, &key, &pos);
		pushtagbody_control(ptr, key, pos);
	}

	/* block */
	while (cons4 != Nil) {
		GetCons(cons4, &pos, &cons4);
		pushblock_control(ptr, pos);
	}
}

static int execute_function(Execute ptr, addr pos)
{
	int recp, check;
	addr cons1, cons2, cons3, cons4, control;

	recp = recursivep_function(pos);
	GetClosureValueFunction(pos, &cons1);
	GetClosureFunctionFunction(pos, &cons2);
	GetClosureTagbodyFunction(pos, &cons3);
	GetClosureBlockFunction(pos, &cons4);
	check = recp || cons1 != Nil || cons2 != Nil || cons3 != Nil || cons4 != Nil;
	if (! check)
		return execute_normal(ptr, pos);

	/* push */
	push_argument_control(ptr, &control);
	/* code */
	if (recp)
		execute_recursive(ptr, pos);
	execute_closure(ptr, cons1, cons2, cons3, cons4);
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

_g void goto_control(Execute ptr, size_t point)
{
	struct control_struct *str;

	str = StructControl(ptr->control);
	str->point = point;
	ptr->signal = ExecuteControl_Point;
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

_g void go_control(Execute ptr, addr tag)
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
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = str;
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

_g void return_from_control(Execute ptr, addr name)
{
	addr pos, next;
	struct taginfo_struct *str;

	/* find name */
	if (! return_from_find_control(ptr, &next, &pos, name))
		fmte("Cannot find block name ~S.", name, NULL);
	str = StructTagInfo(pos);
	if (str->open == 0)
		fmte("Block ~S already closed.", name, NULL);

	/* copy values */
	copy_values_control(ptr->control, next);
	/* rollback */
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = str;
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

_g void throw_control(Execute ptr, addr name)
{
	addr pos, next;

	/* find name */
	if (! throw_find_control(ptr, &next, &pos, name))
		fmte("Cannot find catch name ~S.", name, NULL);
	/* copy values */
	copy_values_control(ptr->control, next);
	/* rollback */
	ptr->signal = ExecuteControl_Throw;
	ptr->taginfo = StructTagInfo(pos);
}

_g void gettagbody_execute(Execute ptr, addr *ret, addr name)
{
	if (! go_find_control(ptr, ret, name))
		fmte("Cannot find tag name ~S", name, NULL);
}

_g void getblock_execute(Execute ptr, addr *ret, addr name)
{
	addr temp;
	if (! return_from_find_control(ptr, &temp, ret, name))
		fmte("Cannot find block name ~S", name, NULL);
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
			fmte("The object ~S cannot execute.", call, NULL);
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getvalues_list_control(ptr, local, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getvalues_list_control(ptr, local, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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
	Return1(callclang_apply(ptr, ret, call, args));
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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
		Return1(runcode_free_control(ptr, control));
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		Return1(free_control(ptr, control));
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

