#include "control.c"
#include "code.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "copy.h"
#include "degrade.h"
#include "execute.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#if 0
/*
 *  taginfo
 */
static int test_gettable_control(void)
{
	addr rollback, pos, table, check;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	rollback = ptr->control;
	local = ptr->local;
	push_local(local, &stack);
	push_new_control(ptr, &pos);

	setplist_constant_heap(Nil, CONSTANT_COMMON_FUNCTION, fixnumh(10), &table);
	SetControl(pos, Control_Table, table);
	test(gettable_control(pos, CONSTANT_COMMON_FUNCTION, &check), "gettable_control1");
	test(RefFixnum(check) == 10, "gettable_control2");
	test(! gettable_control(pos, CONSTANT_COMMON_SETF, &check), "gettable_control3");

#if 0
	getfunction_control(pos, &check);
	test(RefFixnum(check) == 10, "getfunction_control1");
	setplist_constant_heap(table, CONSTANT_COMMON_SETF, fixnumh(20), &table);
	SetControl(pos, Control_Table, table);
	getsetf_control(pos, &check);
	test(RefFixnum(check) == 20, "getsetf_control1");
#endif

	settable_control(local, pos, CONSTANT_COMMON_FUNCTION, T);
	gettable_control(pos, CONSTANT_COMMON_FUNCTION, &check);
	test(check == T, "settable_control1");

	settagbody_control(local, pos, fixnumh(111));
	setblock_control(local, pos, fixnumh(222));
	gettagbody_control(pos, &check);
	test(RefFixnum(check) == 111, "gettagbody_control1");
	getblock_control(pos, &check);
	test(RefFixnum(check) == 222, "getblock_control1");

	ptr->control = rollback;
	rollback_local(local, stack);

	RETURN;
}

static int test_taginfo_heap(void)
{
	addr pos, control, tag, check;
	struct taginfo_struct *str;

	fixnum_heap(&control, 10);
	fixnum_heap(&tag, 20);
	taginfo_heap(&pos, control, tag, 123, 0);
	test(GetType(pos) == LISPSYSTEM_TAGINFO, "taginfo_heap1");
	str = StructTagInfo(pos);
	test(str->open, "taginfo_heap2");
	test(str->point == 123, "taginfo_heap3");
	test(str->control == control, "taginfo_heap4");
	GetNameTagInfo(pos, &check);
	test(check == tag, "taginfo_heap5");
	SetNameTagInfo(pos, T);
	GetNameTagInfo(pos, &check);
	test(check == T, "taginfo_heap6");

	RETURN;
}

static int test_push_taginfo(void)
{
	addr rollback, control, pos, list, cons1, cons2;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	rollback = ptr->control;
	local = ptr->local;
	push_local(local, &stack);
	push_new_control(ptr, &control);

	vector4_heap(&pos, 0);
	code_heap(&pos, pos);
	cons_heap(&cons1, fixnumh(10), index_heapr(20));
	cons_heap(&cons2, fixnumh(30), index_heapr(40));
	list_heap(&list, cons1, cons2, NULL);
	setinfo_code(pos, list);
	push_taginfo(local, control, pos);
	gettagbody_control(control, &list);
	test(length_list_unsafe(list) == 2, "push_taginfo1");
	GetCons(list, &pos, &list);
	GetNameTagInfo(pos, &cons1);
	test(RefFixnum(cons1) == 30, "push_taginfo2");
	test(StructTagInfo(pos)->point == 40, "push_taginfo3");

	ptr->control = rollback;
	rollback_local(local, stack);

	RETURN;
}

static int test_close_taginfo(void)
{
	addr pos, control, tag;
	struct taginfo_struct *str;

	fixnum_heap(&control, 10);
	fixnum_heap(&tag, 20);
	taginfo_heap(&pos, control, tag, 123, 0);
	str = StructTagInfo(pos);
	test(str->open, "close_taginfo1");
	close_taginfo(pos);
	test(str->open == 0, "close_taginfo2");

	RETURN;
}

static int test_push_blockinfo(void)
{
	addr rollback, control, pos, list, value;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	rollback = ptr->control;
	local = ptr->local;
	push_local(local, &stack);
	push_new_control(ptr, &control);

	vector4_heap(&pos, 0);
	code_heap(&pos, pos);
	fixnum_heap(&value, 10);
	setinfo_code(pos, value);
	push_blockinfo(local, control, pos);
	getblock_control(control, &list);
	test(length_list_unsafe(list) == 1, "push_blockinfo1");
	GetCons(list, &pos, &list);
	GetNameTagInfo(pos, &value);
	test(RefFixnum(value) == 10, "push_blockinfo2");
	test(StructTagInfo(pos)->point == 00, "push_blockinfo3");

	ptr->control = rollback;
	rollback_local(local, stack);

	RETURN;
}

static int test_close_blockinfo(void)
{
	addr pos, control, tag;
	struct taginfo_struct *str;

	fixnum_heap(&control, 10);
	fixnum_heap(&tag, 20);
	taginfo_heap(&pos, control, tag, 0, 1);
	str = StructTagInfo(pos);
	test(str->open, "close_blockinfo1");
	close_blockinfo(pos);
	test(str->open == 0, "close_blockinfo2");

	RETURN;
}


/*
 *  symstack
 */
static int test_pushlexical_control(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	fixnum_heap(&check, 10);
	pushlexical_control(ptr, pos, check);
	getlexical_local(ptr, pos, &check);
	test(RefFixnum(check) == 10, "pushlexical_control1");

	free_control(ptr, control);

	RETURN;
}

static int test_pushspecial_control(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	fixnum_heap(&check, 10);
	pushspecial_control(ptr, pos, check);
	getspecial_local(ptr, pos, &check);
	test(RefFixnum(check) == 10, "pushspecial_control1");

	free_control(ptr, control);

	RETURN;
}

static int test_pushtable_control(void)
{
	addr control, pos, cons, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	pushtable_control(ptr, CONSTANT_COMMON_KEYWORD, pos);
	gettable_control(control, CONSTANT_COMMON_KEYWORD, &cons);
	test(length_list_unsafe(cons) == 1, "pushtable_control1");
	GetCar(cons, &check);
	test(pos == check, "pushtable_control2");

	internchar(LISP_PACKAGE, "AAA", &pos);
	pushtable_control(ptr, CONSTANT_COMMON_KEYWORD, pos);
	gettable_control(control, CONSTANT_COMMON_KEYWORD, &cons);
	test(length_list_unsafe(cons) == 2, "pushtable_control3");
	GetCar(cons, &check);
	test(pos == check, "pushtable_control4");

	free_control(ptr, control);

	RETURN;
}

static int test_pushfunction_control(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	fixnum_heap(&check, 10);
	pushfunction_control(ptr, pos, check);
	GetFunctionSymbol(pos, &check);
	test(RefFixnum(check) == 10, "pushfunction_control1");

	free_control(ptr, control);

	RETURN;
}

static int test_pushsetf_control(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	fixnum_heap(&check, 10);
	pushsetf_control(ptr, pos, check);
	getsetf_local(ptr, pos, &check);
	test(RefFixnum(check) == 10, "pushsetf_control1");

	free_control(ptr, control);

	RETURN;
}


/*
 *  stack
 */
static int test_push_control(void)
{
	addr pos, prev, check;
	struct control_struct *str;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	test(ControlSize_Arary <= 0xFF, "push_control1");
	test(sizeoft(struct control_struct) <= 0xFF, "push_control2");
	prev = ptr->control;
	push_control(ptr, ControlType_Close);
	pos = ptr->control;
	test(GetType(pos) == LISPTYPE_CONTROL, "push_control1");
	GetControl(pos, Control_Next, &check);
	test(prev == check, "push_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Close, "push_control3");
	test(str->stack != NULL, "push_control4");
	test(str->call == NULL, "push_control5");
	test(str->sizer == 0, "push_control6");
	test(str->point == 0, "push_control7");
	test(str->dynamic_result == 0, "push_control8");

	ptr->control = prev;
	rollback_local(local, stack);

	RETURN;
}

static int test_push_stack_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_stack_control(ptr, &pos, ControlType_Return);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_stack_control1");
	test(prev != pos, "push_stack_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Return, "push_stack_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_return_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_return_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_return_control1");
	test(prev != pos, "push_return_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Return, "push_return_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_push_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_push_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_push_control1");
	test(prev != pos, "push_push_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Push, "push_push_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_close_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_close_control1");
	test(prev != pos, "push_close_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Close, "push_close_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_protect_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_protect_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_protect_control1");
	test(prev != pos, "push_protect_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Protect, "push_protect_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_tagbody_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_tagbody_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_tagbody_control1");
	test(prev != pos, "push_tagbody_control2");
	str = StructControl(pos);
	test(str->type == ControlType_TagBody, "push_tagbody_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_push_block_control(void)
{
	addr prev, pos;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_block_control(ptr, &pos);
	test(GetType(pos) == LISPTYPE_CONTROL, "push_block_control1");
	test(prev != pos, "push_block_control2");
	str = StructControl(pos);
	test(str->type == ControlType_Block, "push_block_control3");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_close_result_control(void)
{
	addr prev, pos, list, check, cons;
	struct control_struct *str;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &pos);
	local = ptr->local;

	str = StructControl(pos);
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	SetControl(pos, Control_Result, list);
	str->dynamic_result = 0;
	close_result_control(pos);
	GetCons(list, &check, &cons);
	test(check != Unbound, "close_result_control1");
	test(consp(cons), "close_result_control2");
	GetControl(pos, Control_Result, &cons);
	test(cons == Nil, "close_result_control3");

	list_local(local, &list,
			fixnuml(10), fixnuml(20), NULL);
	SetControl(pos, Control_Result, list);
	str->dynamic_result = 1;
	close_result_control(pos);
	GetCons(list, &check, &cons);
	test(check == Unbound, "close_result_control4");
	GetCons(cons, &check, &cons);
	test(check == Unbound, "close_result_control5");
	test(cons == Nil, "close_result_control6");
	GetControl(pos, Control_Result, &cons);
	test(cons == Nil, "close_result_control7");

	ptr->control = prev;
	rollback_local(ptr->local, str->stack);

	RETURN;
}

static int test_copy_values_control(void)
{
	addr prev, next, pos, value1, value2, cons, check;
	struct control_struct *str;
	Execute ptr;
	size_t i;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &next);
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	list_heap(&cons, fixnumh(30), NULL);

	SetResultControl(pos, 0, value1);
	SetResultControl(pos, 1, value2);
	SetControl(pos, Control_Result, cons);
	str->sizer = 0;
	SetResultControl(next, 0, Unbound);
	SetResultControl(next, 1, Unbound);
	SetControl(next, Control_Result, Unbound);
	copy_values_control(pos, next);
	GetResultControl(next, 0, &check);
	test(check == Unbound, "copy_values_control1");
	GetResultControl(next, 1, &check);
	test(check == Unbound, "copy_values_control2");
	GetControl(next, Control_Result, &check);
	test(check == Nil, "copy_values_control3");

	str->sizer = 1;
	SetResultControl(next, 0, Unbound);
	SetResultControl(next, 1, Unbound);
	SetControl(next, Control_Result, Unbound);
	copy_values_control(pos, next);
	GetResultControl(next, 0, &check);
	test(check == value1, "copy_values_control4");
	GetResultControl(next, 1, &check);
	test(check == Unbound, "copy_values_control5");
	GetControl(next, Control_Result, &check);
	test(check == Nil, "copy_values_control6");

	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, fixnumh((fixnum)(i * 10)));
	list_heap(&cons, fixnumh(111), fixnumh(222), NULL);
	SetControl(pos, Control_Result, cons);

	str->sizer = ControlSize_Result;
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(next, i, Unbound);
	SetControl(next, Control_Result, Unbound);
	copy_values_control(pos, next);
	GetResultControl(next, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"copy_values_control7");
	GetControl(next, Control_Result, &check);
	test(check == Nil, "copy_values_control8");

	str->sizer = ControlSize_Result + 1;
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(next, i, Unbound);
	SetControl(next, Control_Result, Unbound);
	copy_values_control(pos, next);
	GetResultControl(next, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"copy_values_control9");
	GetControl(next, Control_Result, &check);
	test(check == cons, "copy_values_control10");

	str->sizer = ControlSize_Result + 2;
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(next, i, Unbound);
	SetControl(next, Control_Result, Unbound);
	copy_values_control(pos, next);
	GetResultControl(next, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"copy_values_control11");
	GetControl(next, Control_Result, &check);
	test(check == cons, "copy_values_control12");

	ptr->control = prev;
	rollback_local(ptr->local, StructControl(pos)->stack);

	RETURN;
}

static int test_close_return(void)
{
	addr prev, next, pos, value1, value2, cons, check;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &next);
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	list_heap(&cons, fixnumh(30), NULL);

	SetResultControl(pos, 0, value1);
	SetResultControl(pos, 1, value2);
	SetControl(pos, Control_Result, cons);
	str->sizer = 0;
	SetResultControl(next, 0, Unbound);
	SetResultControl(next, 1, Unbound);
	SetControl(next, Control_Result, Unbound);
	close_return(ptr, pos);
	GetResultControl(next, 0, &check);
	test(check == Unbound, "close_return1");
	GetResultControl(next, 1, &check);
	test(check == Unbound, "close_return2");
	GetControl(next, Control_Result, &check);
	test(check == Nil, "close_return3");

	ptr->control = prev;
	rollback_local(ptr->local, StructControl(pos)->stack);

	RETURN;
}

static int test_close_protect(void)
{
	addr control, code1, code2, list, set, end, value, array;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	push_protect_control(ptr, &value);

	internchar(LISP_CODE, "SET", &set);
	internchar(LISP_CODE, "END", &end);

	vector4_heap(&array, 2);
	cons_heap(&list, set, fixnumh(10));
	setarray(array, 0, list);
	conscar_heap(&list, end);
	setarray(array, 1, list);
	code_heap(&code1, array);

	vector4_heap(&array, 2);
	cons_heap(&list, set, fixnumh(20));
	setarray(array, 0, list);
	conscar_heap(&list, end);
	setarray(array, 1, list);
	code_heap(&code2, array);

	settype_code(code1, CodeType_Protect);
	setinfo_code(code1, code2);
	runcode_control(ptr, code1);
	getresult_control(ptr, &value);
	test(RefFixnum(value) == 20, "close_protect1");

	settype_code(code2, CodeType_Close);
	runcode_control(ptr, code1);
	getresult_control(ptr, &value);
	test(RefFixnum(value) == 10, "close_protect2");

	free_control(ptr, control);

	RETURN;
}

static int test_close_tagbody(void)
{
	addr control, pos, left, right;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	setvalues_control(ptr, T, T, T, NULL);
	push_tagbody_control(ptr, &pos);

	right = Nil;
	taginfo_heap(&left, pos, T, 10, 0);
	cons_local(local, &right, left, right);
	taginfo_heap(&left, pos, Nil, 20, 0);
	cons_local(local, &right, left, right);
	taginfo_heap(&left, pos, T, 30, 0);
	cons_local(local, &right, left, right);
	settagbody_control(local, pos, right);

	close_tagbody(ptr, pos);
	gettagbody_control(pos, &right);
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_tagbody1");
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_tagbody2");
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_tagbody3");

	test(StructControl(control)->sizer == 1, "close_tagbody4");
	GetResultControl(control, 0, &left);
	test(left == Nil, "close_tagbody5");

	free_control(ptr, control);

	RETURN;
}

static int test_close_block(void)
{
	addr control, pos, left, right;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	setvalues_control(ptr, T, T, T, NULL);
	push_block_control(ptr, &pos);
	setvalues_control(ptr, fixnumh(11), T, NULL);

	right = Nil;
	taginfo_heap(&left, pos, T, 0, 1);
	cons_local(local, &right, left, right);
	taginfo_heap(&left, pos, Nil, 0, 1);
	cons_local(local, &right, left, right);
	taginfo_heap(&left, pos, T, 0, 1);
	cons_local(local, &right, left, right);
	setblock_control(local, pos, right);

	close_block(ptr, pos);
	getblock_control(pos, &right);
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_block1");
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_block2");
	GetCons(right, &left, &right);
	test(StructTagInfo(left)->open == 0, "close_block3");

	test(StructControl(control)->sizer == 2, "close_block4");
	GetResultControl(control, 0, &left);
	test(RefFixnum(left) == 11, "close_block5");

	free_control(ptr, control);

	RETURN;
}

static int test_close_type_control(void)
{
	addr prev, next, pos, value1, value2, cons, check;
	struct control_struct *str;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_return_control(ptr, &next);
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	list_heap(&cons, fixnumh(30), NULL);

	SetResultControl(pos, 0, value1);
	SetResultControl(pos, 1, value2);
	SetControl(pos, Control_Result, cons);

	str->sizer = 1;
	SetResultControl(next, 0, Unbound);
	SetResultControl(next, 1, Unbound);
	SetControl(next, Control_Result, Unbound);
	close_type_control(ptr, pos, str);
	GetResultControl(next, 0, &check);
	test(check == Unbound, "close_type_control1");

	str->type = ControlType_Return;
	str->sizer = 1;
	SetResultControl(next, 0, Unbound);
	SetResultControl(next, 1, Unbound);
	SetControl(next, Control_Result, Unbound);
	close_type_control(ptr, pos, str);
	GetResultControl(next, 0, &check);
	test(check == value1, "close_type_control2");

	ptr->control = prev;
	rollback_local(ptr->local, StructControl(pos)->stack);

	RETURN;
}

static int test_close_lexical_control(void)
{
	addr pos, next, sym1, sym2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	symbol_heap(&sym1);
	symbol_heap(&sym2);
	pushlexical_control(ptr, sym1, fixnumh(111));
	pushlexical_control(ptr, sym2, fixnumh(222));

	push_new_control(ptr, &next);
	pushlexical_control(ptr, sym1, fixnumh(10));
	pushlexical_control(ptr, sym1, fixnumh(20));
	pushlexical_control(ptr, sym2, fixnumh(30));
	pushlexical_control(ptr, sym2, fixnumh(40));

	getlexical_local(ptr, sym1, &check);
	test(RefFixnum(check) == 20, "close_lexical_control1");
	getlexical_local(ptr, sym2, &check);
	test(RefFixnum(check) == 40, "close_lexical_control2");

	close_lexical_control(ptr, next);
	getlexical_local(ptr, sym1, &check);
	test(RefFixnum(check) == 111, "close_lexical_control3");
	getlexical_local(ptr, sym2, &check);
	test(RefFixnum(check) == 222, "close_lexical_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_close_special_control(void)
{
	addr pos, next, sym1, sym2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	symbol_heap(&sym1);
	symbol_heap(&sym2);
	pushspecial_control(ptr, sym1, fixnumh(111));
	pushspecial_control(ptr, sym2, fixnumh(222));

	push_new_control(ptr, &next);
	pushspecial_control(ptr, sym1, fixnumh(10));
	pushspecial_control(ptr, sym1, fixnumh(20));
	pushspecial_control(ptr, sym2, fixnumh(30));
	pushspecial_control(ptr, sym2, fixnumh(40));

	getspecial_local(ptr, sym1, &check);
	test(RefFixnum(check) == 20, "close_special_control1");
	getspecial_local(ptr, sym2, &check);
	test(RefFixnum(check) == 40, "close_special_control2");

	close_special_control(ptr, next);
	getspecial_local(ptr, sym1, &check);
	test(RefFixnum(check) == 111, "close_special_control3");
	getspecial_local(ptr, sym2, &check);
	test(RefFixnum(check) == 222, "close_special_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_close_function_control(void)
{
	addr pos, next, sym1, sym2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	symbol_heap(&sym1);
	symbol_heap(&sym2);
	pushfunction_control(ptr, sym1, fixnumh(111));
	pushfunction_control(ptr, sym2, fixnumh(222));

	push_new_control(ptr, &next);
	pushfunction_control(ptr, sym1, fixnumh(10));
	pushfunction_control(ptr, sym1, fixnumh(20));
	pushfunction_control(ptr, sym2, fixnumh(30));
	pushfunction_control(ptr, sym2, fixnumh(40));

	GetFunctionSymbol(sym1, &check);
	test(RefFixnum(check) == 20, "close_function_control1");
	GetFunctionSymbol(sym2, &check);
	test(RefFixnum(check) == 40, "close_function_control2");

	close_function_control(ptr, next);
	GetFunctionSymbol(sym1, &check);
	test(RefFixnum(check) == 111, "close_function_control3");
	GetFunctionSymbol(sym2, &check);
	test(RefFixnum(check) == 222, "close_function_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_close_setf_control(void)
{
	addr pos, next, sym1, sym2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	symbol_heap(&sym1);
	symbol_heap(&sym2);
	pushsetf_control(ptr, sym1, fixnumh(111));
	pushsetf_control(ptr, sym2, fixnumh(222));

	push_new_control(ptr, &next);
	pushsetf_control(ptr, sym1, fixnumh(10));
	pushsetf_control(ptr, sym1, fixnumh(20));
	pushsetf_control(ptr, sym2, fixnumh(30));
	pushsetf_control(ptr, sym2, fixnumh(40));

	getsetf_local(ptr, sym1, &check);
	test(RefFixnum(check) == 20, "close_setf_control1");
	getsetf_local(ptr, sym2, &check);
	test(RefFixnum(check) == 40, "close_setf_control2");

	close_setf_control(ptr, next);
	getsetf_local(ptr, sym1, &check);
	test(RefFixnum(check) == 111, "close_setf_control3");
	getsetf_local(ptr, sym2, &check);
	test(RefFixnum(check) == 222, "close_setf_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_pop_control_common(void)
{
	addr prev, pos;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &pos);
	pop_control_common(ptr);
	test(prev == ptr->control, "pop_control_common1");

	RETURN;
}

static int test_pop_control_unsafe(void)
{
	addr base, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &base);

	setargs_nil_control(ptr);
	push_new_control(ptr, &pos);
	setresult_control(ptr, T);
	pop_control_unsafe(ptr);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(length_list_unsafe(pos) == 0, "pop_control_unsafe1");

	setargs_nil_control(ptr);
	push_push_control(ptr, &pos);
	setresult_control(ptr, T);
	pop_control_unsafe(ptr);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(length_list_unsafe(pos) == 1, "pop_control_unsafe2");
	getargs_control(ptr, 0, &pos);
	test(pos == T, "pop_control_unsafe3");

	setargs_nil_control(ptr);
	push_push_control(ptr, &pos);
	setvalues_nil_control(ptr);
	pop_control_unsafe(ptr);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(length_list_unsafe(pos) == 1, "pop_control_unsafe4");
	getargs_control(ptr, 0, &pos);
	test(pos == Nil, "pop_control_unsafe5");

	setargs_nil_control(ptr);
	push_push_control(ptr, &pos);
	setvalues_control(ptr, T, fixnumh(10), NULL);
	pop_control_unsafe(ptr);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(length_list_unsafe(pos) == 1, "pop_control_unsafe6");
	getargs_control(ptr, 0, &pos);
	test(pos == T, "pop_control_unsafe7");

	RETURN;
}

static int test_free_control(void)
{
	addr prev, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	prev = ptr->control;
	push_new_control(ptr, &pos);
	push_new_control(ptr, &check);
	push_new_control(ptr, &check);
	push_new_control(ptr, &check);
	push_new_control(ptr, &check);
	push_return_control(ptr, &check);
	push_return_control(ptr, &check);
	push_return_control(ptr, &check);
	push_return_control(ptr, &check);
	test(ptr->control != prev, "free_control1");
	free_control(ptr, pos);
	test(ptr->control == prev, "free_control2");

	RETURN;
}


/*
 *  getcontrol
 */
static int test_getdata_control(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	SetControl(pos, Control_Data, T);
	getdata_control(ptr, &check);
	test(check == T, "getdata_control1");
	setdata_control(ptr, Nil);
	getdata_control(ptr, &check);
	test(check == Nil, "setdata_control1");
	free_control(ptr, pos);

	RETURN;
}

static int test_array_data_control(void)
{
	addr pos, array, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	array_data_control(ptr, 10);
	getdata_control(ptr, &array);
	test(GetType(array) == LISPTYPE_VECTOR, "array_data_control1");
	test(lenarrayr(array) == 10, "array_data_control2");
	setarray(array, 2, T);
	getdata_array_control(ptr, 2, &check);
	test(check == T, "getdata_array_control1");
	setdata_array_control(ptr, 2, Nil);
	getdata_array_control(ptr, 2, &check);
	test(check == Nil, "setdata_array_control1");

	free_control(ptr, pos);

	RETURN;
}

static int test_setargs_control(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_control(ptr, fixnumh(10));
	GetControl(pos, Control_Cons, &check);
	test(length_list_unsafe(check) == 1, "setargs_control1");
	GetCar(check, &check);
	test(RefFixnum(check) == 10, "setargs_control2");

	setargs_control(ptr, fixnumh(20));
	GetControl(pos, Control_Cons, &check);
	test(length_list_unsafe(check) == 1, "setargs_control3");
	GetCar(check, &check);
	test(RefFixnum(check) == 20, "setargs_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_setargs_va_control(void)
{
	addr pos, cons, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_control(ptr, fixnumh(10));
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	GetControl(pos, Control_Cons, &cons);
	test(length_list_unsafe(cons) == 2, "setargs_va_control1");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "setargs_va_control2");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "setargs_va_control3");

	free_control(ptr, pos);

	RETURN;
}

static int test_setargs_nil_control(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	SetControl(pos, Control_Cons, Unbound);
	SetControl(pos, Control_ConsTail, Unbound);

	setargs_nil_control(ptr);
	GetControl(pos, Control_Cons, &check);
	test(check == Nil, "setargs_control1");
	GetControl(pos, Control_ConsTail, &check);
	test(check == Nil, "setargs_control2");

	free_control(ptr, pos);

	RETURN;
}

static int test_setargs_list_control(void)
{
	addr pos, cons, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_control(ptr, fixnumh(10));
	list_heap(&cons, fixnumh(20), fixnumh(30), NULL);
	setargs_list_control(ptr, cons);

	GetControl(pos, Control_Cons, &cons);
	test(length_list_unsafe(cons) == 2, "setargs_list_control1");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "setargs_list_control2");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 30, "setargs_list_control3");

	free_control(ptr, pos);

	RETURN;
}

static int test_setargs_list_control_unsafe(void)
{
	addr pos, cons, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_control(ptr, fixnumh(10));
	list_heap(&check, fixnumh(20), fixnumh(30), NULL);
	setargs_list_control_unsafe(ptr, check);

	GetControl(pos, Control_Cons, &cons);
	test(check == cons, "setargs_list_control_unsafe1");
	GetControl(pos, Control_ConsTail, &check);
	test(check == Nil, "setargs_list_control_unsafe2");
	test(length_list_unsafe(cons) == 2, "setargs_list_control_unsafe3");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "setargs_list_control_unsafe4");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 30, "setargs_list_control_unsafe5");

	free_control(ptr, pos);

	RETURN;
}

static int test_pushargs_control(void)
{
	addr pos, check, cons;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_nil_control(ptr);
	pushargs_control(ptr, fixnumh(10));
	GetControl(pos, Control_Cons, &check);
	test(length_list_unsafe(check) == 1, "pushargs_control1");
	GetCar(check, &check);
	test(RefFixnum(check) == 10, "pushargs_control2");
	GetControl(pos, Control_ConsTail, &cons);
	test(cons != Nil, "pushargs_control3");

	pushargs_control(ptr, fixnumh(20));
	GetControl(pos, Control_Cons, &cons);
	test(length_list_unsafe(cons) == 2, "pushargs_control4");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "pushargs_control5");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "pushargs_control6");

	pushargs_control(ptr, fixnumh(30));
	GetControl(pos, Control_Cons, &cons);
	test(length_list_unsafe(cons) == 3, "pushargs_control7");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "pushargs_control8");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "pushargs_control9");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 30, "pushargs_control10");

	free_control(ptr, pos);

	RETURN;
}

static int test_pushargs_list_control(void)
{
	addr pos, cons, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_control(ptr, fixnumh(10));
	list_heap(&cons, fixnumh(20), fixnumh(30), NULL);
	pushargs_list_control(ptr, cons);

	GetControl(pos, Control_Cons, &cons);
	test(length_list_unsafe(cons) == 3, "pushargs_list_control1");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "pushargs_list_control2");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "pushargs_list_control3");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 30, "pushargs_list_control4");

	free_control(ptr, pos);

	RETURN;
}

static int test_getargs_control(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	getargs_control(ptr, 0, &check);
	test(check == Unbound, "getargs_control1");
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	getargs_control(ptr, 0, &check);
	test(RefFixnum(check) == 10, "getargs_control2");
	getargs_control(ptr, 1, &check);
	test(RefFixnum(check) == 20, "getargs_control3");
	getargs_control(ptr, 2, &check);
	test(check == Unbound, "getargs_control4");
	getargs_control(ptr, 3, &check);
	test(check == Unbound, "getargs_control5");

	free_control(ptr, pos);

	RETURN;
}

static int test_getargs_list_control_unsafe(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_va_control(ptr,
			fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	getargs_list_control_unsafe(ptr, 0, &check);
	test(length_list_unsafe(check) == 3, "getargs_list_control_unsafe1");
	GetCar(check, &check);
	test(RefFixnum(check) == 10, "getargs_list_control_unsafe2");
	getargs_list_control_unsafe(ptr, 1, &check);
	test(length_list_unsafe(check) == 2, "getargs_list_control_unsafe3");
	GetCar(check, &check);
	test(RefFixnum(check) == 20, "getargs_list_control_unsafe4");
	getargs_list_control_unsafe(ptr, 2, &check);
	test(length_list_unsafe(check) == 1, "getargs_list_control_unsafe5");
	GetCar(check, &check);
	test(RefFixnum(check) == 30, "getargs_list_control_unsafe6");
	getargs_list_control_unsafe(ptr, 3, &check);
	test(check == Nil, "getargs_list_control_unsafe7");
	getargs_list_control_unsafe(ptr, 4, &check);
	test(check == Nil, "getargs_list_control_unsafe8");

	free_control(ptr, pos);

	RETURN;
}

static int test_getargs_list_control_heap(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	setargs_va_control(ptr,
			fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	getargs_list_control_heap(ptr, 0, &check);
	test(length_list_unsafe(check) == 3, "getargs_list_control_heap1");
	GetCar(check, &check);
	test(RefFixnum(check) == 10, "getargs_list_control_heap2");
	getargs_list_control_heap(ptr, 1, &check);
	test(length_list_unsafe(check) == 2, "getargs_list_control_heap3");
	GetCar(check, &check);
	test(RefFixnum(check) == 20, "getargs_list_control_heap4");
	getargs_list_control_heap(ptr, 2, &check);
	test(length_list_unsafe(check) == 1, "getargs_list_control_heap5");
	GetCar(check, &check);
	test(RefFixnum(check) == 30, "getargs_list_control_heap6");
	getargs_list_control_heap(ptr, 3, &check);
	test(check == Nil, "getargs_list_control_heap7");
	getargs_list_control_heap(ptr, 4, &check);
	test(check == Nil, "getargs_list_control_heap8");

	free_control(ptr, pos);

	RETURN;
}

static int test_pushvalues_dynamic(void)
{
	addr pos, cons;
	Execute ptr;
	LocalRoot local;
	struct control_struct *str;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	SetControl(pos, Control_Result, Nil);
	pushvalues_dynamic(pos, str, fixnumh(10));
	pushvalues_dynamic(pos, str, fixnumh(20));
	pushvalues_dynamic(pos, str, fixnumh(30));
	test(str->dynamic_result == 0, "pop_control_dynamic1");
	pushvalues_dynamic(pos, str, fixnuml(40));
	pushvalues_dynamic(pos, str, fixnuml(50));
	pushvalues_dynamic(pos, str, fixnuml(60));
	test(str->dynamic_result != 0, "pop_control_dynamic2");
	GetControl(pos, Control_Result, &cons);
	test(length_list_unsafe(cons) == 6, "pop_control_dynamic3");
	close_result_control(pos);

	free_control(ptr, pos);

	RETURN;
}

static int test_pushvalues_unsafe(void)
{
	int i;
	addr pos, check, cons;
	Execute ptr;
	struct control_struct *str;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	SetControl(pos, Control_Result, Unbound);
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, Unbound);
	str->sizer = 0;
	pushvalues_unsafe(ptr, fixnumh(10));
	test(str->sizer == 1, "pushvalues_unsafe1");
	GetResultControl(pos, 0, &check);
	test(RefFixnum(check) == 10, "pushvalues_unsafe2");
	GetControl(pos, Control_Result, &check);
	test(check == Unbound, "pushvalues_unsafe3");

	pushvalues_unsafe(ptr, fixnumh(20));
	test(str->sizer == 2, "pushvalues_unsafe4");
	GetResultControl(pos, 1, &check);
	test(RefFixnum(check) == 20, "pushvalues_unsafe5");

	/* size - 1 */
	SetControl(pos, Control_Result, Unbound);
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, Unbound);
	str->sizer = 0;
	for (i = 0; i < ControlSize_Result - 1; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	GetResultControl(pos, ControlSize_Result - 2, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 2) * 10),
			"pushvalues_unsafe6");
	GetControl(pos, Control_Result, &check);
	test(check == Unbound, "pushvalues_unsafe7");

	/* size */
	SetControl(pos, Control_Result, Unbound);
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, Unbound);
	str->sizer = 0;
	for (i = 0; i < ControlSize_Result; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	GetResultControl(pos, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"pushvalues_unsafe8");
	GetControl(pos, Control_Result, &check);
	test(check == Unbound, "pushvalues_unsafe9");

	/* size + 1 */
	SetControl(pos, Control_Result, Nil);
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, Unbound);
	str->sizer = 0;
	for (i = 0; i < ControlSize_Result + 1; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	GetResultControl(pos, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"pushvalues_unsafe10");
	GetControl(pos, Control_Result, &cons);
	test(length_list_unsafe(cons) == 1, "pushvalues_unsafe11");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == ControlSize_Result * 10, "pushvalues_unsafe12");

	/* size + 2 */
	SetControl(pos, Control_Result, Nil);
	for (i = 0; i < ControlSize_Result; i++)
		SetResultControl(pos, i, Unbound);
	str->sizer = 0;
	for (i = 0; i < ControlSize_Result + 2; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	GetResultControl(pos, ControlSize_Result - 1, &check);
	test(RefFixnum(check) == (fixnum)((ControlSize_Result - 1) * 10),
			"pushvalues_unsafe13");
	GetControl(pos, Control_Result, &cons);
	test(length_list_unsafe(cons) == 2, "pushvalues_unsafe14");
	test(str->sizer == ControlSize_Result + 2, "pushvalues_unsafe15");
	nreverse_values_unsafe(ptr);
	GetControl(pos, Control_Result, &cons);
	test(length_list_unsafe(cons) == 2, "nreverse_values_unsafe1");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == ControlSize_Result * 10, "nreverse_values_unsafe2");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == (ControlSize_Result + 1) * 10, "nreverse_values_unsafe3");

	free_control(ptr, pos);

	RETURN;
}

static int test_setresult_control(void)
{
	addr pos, check;
	Execute ptr;
	struct control_struct *str;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	setresult_control(ptr, fixnumh(10));
	setresult_control(ptr, fixnumh(20));
	test(str->sizer == 1, "setresult_control1");
	GetResultControl(pos, 0, &check);
	test(RefFixnum(check) == 20, "setresult_control2");

	free_control(ptr, pos);

	RETURN;
}

static int test_setvalues_control(void)
{
	addr pos, check;
	Execute ptr;
	struct control_struct *str;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	setresult_control(ptr, fixnumh(10));
	setvalues_control(ptr, fixnumh(20), fixnumh(30), NULL);
	test(str->sizer == 2, "setvalues_control1");
	GetResultControl(pos, 0, &check);
	test(RefFixnum(check) == 20, "setvalues_control2");
	GetResultControl(pos, 1, &check);
	test(RefFixnum(check) == 30, "setvalues_control3");

	free_control(ptr, pos);

	RETURN;
}

static int test_setvalues_nil_control(void)
{
	addr pos, check;
	Execute ptr;
	struct control_struct *str;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	setresult_control(ptr, fixnumh(10));
	SetControl(pos, Control_Result, Unbound);
	setvalues_nil_control(ptr);
	test(str->sizer == 0, "setvalues_nil_control1");
	GetControl(pos, Control_Result, &check);
	test(check == Nil, "setvalues_nil_control2");

	free_control(ptr, pos);

	RETURN;
}

static int test_setvalues_list_control(void)
{
	addr pos, check;
	Execute ptr;
	struct control_struct *str;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	str = StructControl(pos);

	setresult_control(ptr, fixnumh(10));
	list_heap(&check, fixnumh(20), fixnumh(30), NULL);
	setvalues_list_control(ptr, check);
	test(str->sizer == 2, "setvalues_list_control1");
	GetResultControl(pos, 0, &check);
	test(RefFixnum(check) == 20, "setvalues_list_control2");
	GetResultControl(pos, 1, &check);
	test(RefFixnum(check) == 30, "setvalues_list_control3");

	free_control(ptr, pos);

	RETURN;
}

static int test_getresult_control(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	setvalues_nil_control(ptr);
	getresult_control(ptr, &check);
	test(check == Nil, "getresult_control1");

	setresult_control(ptr, fixnumh(10));
	getresult_control(ptr, &check);
	test(RefFixnum(check) == 10, "getresult_control2");

	setvalues_control(ptr, fixnumh(20), fixnumh(30), NULL);
	getresult_control(ptr, &check);
	test(RefFixnum(check) == 20, "getresult_control3");

	free_control(ptr, pos);

	RETURN;
}

static int test_getvalues_control(void)
{
	addr pos, check;
	Execute ptr;
	size_t i, size;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	setvalues_nil_control(ptr);
	getvalues_control(ptr, 0, &check);
	test(check == Unbound, "getvalues_control1");

	setvalues_nil_control(ptr);
	size = ControlSize_Result * 2;
	for (i = 0; i < size; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	nreverse_values_unsafe(ptr);

	i = 1;
	getvalues_control(ptr, i, &check);
	test(RefFixnum(check) == (fixnum)(i * 10), "getvalues_control2");

	i = ControlSize_Result - 1;
	getvalues_control(ptr, i, &check);
	test(RefFixnum(check) == (fixnum)(i * 10), "getvalues_control3");

	i = ControlSize_Result;
	getvalues_control(ptr, i, &check);
	test(RefFixnum(check) == (fixnum)(i * 10), "getvalues_control4");

	i = ControlSize_Result + 1;
	getvalues_control(ptr, i, &check);
	test(RefFixnum(check) == (fixnum)(i * 10), "getvalues_control5");

	i = ControlSize_Result * 2 - 1;
	getvalues_control(ptr, i, &check);
	test(RefFixnum(check) == (fixnum)(i * 10), "getvalues_control6");

	i = ControlSize_Result * 2;
	getvalues_control(ptr, i, &check);
	test(check == Unbound, "getvalues_control7");

	i = ControlSize_Result * 2 + 1;
	getvalues_control(ptr, i, &check);
	test(check == Unbound, "getvalues_control8");

	free_control(ptr, pos);

	RETURN;
}

static int test_list_from_vector(void)
{
	addr pos, cons, check;
	Execute ptr;
	size_t i;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	setvalues_nil_control(ptr);
	for (i = 0; i < ControlSize_Result; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	nreverse_values_unsafe(ptr);

	list_heap(&cons, fixnumh(99), NULL);
	list_from_vector(NULL, pos, 1, cons, &cons);
	test(length_list_unsafe(cons) == 2, "list_from_vector1");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 0, "list_from_vector2");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 99, "list_from_vector3");

	list_heap(&cons, fixnumh(20), fixnumh(30), NULL);
	list_from_vector(NULL, pos, 2, cons, &cons);
	test(length_list_unsafe(cons) == 4, "list_from_vector4");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 0, "list_from_vector5");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "list_from_vector6");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "list_from_vector7");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 30, "list_from_vector8");

	free_control(ptr, pos);

	RETURN;
}

static void init_getvalues_list_control(Execute ptr, size_t size)
{
	size_t i;

	setvalues_nil_control(ptr);
	for (i = 0; i < size; i++)
		pushvalues_unsafe(ptr, fixnumh((fixnum)(i * 10)));
	nreverse_values_unsafe(ptr);
}

static int loop_getvalues_list_control(Execute ptr, size_t size)
{
	int result;
	addr cons, check;
	size_t i;

	init_getvalues_list_control(ptr, size);
	getvalues_list_control(ptr, NULL, &cons);
	result = 1;
	for (i = 0; i < size; i++) {
		GetCons(cons, &check, &cons);
		if (RefFixnum(check) != (fixnum)(i * 10)) {
			result = 0;
			break;
		}
	}

	return result;
}

static int test_getvalues_list_control(void)
{
	int result;
	addr pos, cons;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	setvalues_nil_control(ptr);
	getvalues_list_control(ptr, NULL, &cons);
	test(cons == Nil, "getvalues_list_control1");

	result = loop_getvalues_list_control(ptr, 2);
	test(result, "getvalues_list_control2");
	result = loop_getvalues_list_control(ptr, ControlSize_Result - 1);
	test(result, "getvalues_list_control3");
	result = loop_getvalues_list_control(ptr, ControlSize_Result);
	test(result, "getvalues_list_control4");
	result = loop_getvalues_list_control(ptr, ControlSize_Result + 1);
	test(result, "getvalues_list_control5");
	result = loop_getvalues_list_control(ptr, ControlSize_Result * 2);
	test(result, "getvalues_list_control6");

	size = ControlSize_Result * 2;
	init_getvalues_list_control(ptr, size);
	getvalues_list_control_local(ptr, &cons);
	test(length_list_unsafe(cons) == size, "getvalues_list_control_local1");
	test(GetStatusDynamic(cons), "getvalues_list_control_local2");
	getvalues_list_control_heap(ptr, &cons);
	test(length_list_unsafe(cons) == size, "getvalues_list_control_heap1");
	test(! GetStatusDynamic(cons), "getvalues_list_control_heap2");

	free_control(ptr, pos);

	RETURN;
}

static int test_lengthvalues_control(void)
{
	addr pos;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);

	size = ControlSize_Result * 2;
	init_getvalues_list_control(ptr, size);
	test(lengthvalues_control(ptr) == size, "lengthvalues_control1");

	free_control(ptr, pos);

	RETURN;
}


/*
 *  execute
 */
static int test_function_length_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	fixnum_heap(&right, (int)length_list_unsafe(right));
	setresult_control(ptr, right);
	return 0;
}

static void test_make_code(addr *ret)
{
	addr code, call, pos;

	/* #((length) (end)) */
	vector4_heap(&code, 2);
	compiled_system(&call, Nil);
	SetPointer_code(p_debug1, test_function_length_code);
	setcompiled_code(call, p_debug1);
	symbol_heap(&pos);
	SetFunctionSymbol(pos, call);
	list_heap(&pos, pos, NULL);
	setarray(code, 0, pos);
	internchar(LISP_CODE, "END", &pos);
	list_heap(&pos, pos, NULL);
	setarray(code, 1, pos);
	code_heap(ret, code);
}

static int test_runcode_control(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	test_make_code(&pos);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "runcode_control1");

	free_control(ptr, control);

	RETURN;
}

static int test_function_length(Execute ptr, addr cons)
{
	fixnum_heap(&cons, (int)length_list_unsafe(cons));
	setresult_control(ptr, cons);
	return 0;
}

static int test_execute_control(void)
{
	addr control, call, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	/* compiled */
	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	execute_control(ptr, call);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "execute_control1");

	/* interpreter */
	test_make_code(&call);
	function_heap(&call, Nil, call);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);
	setargs_va_control(ptr, fixnumh(10), fixnumh(20), NULL);
	execute_control(ptr, call);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "execute_control2");

	free_control(ptr, control);

	RETURN;
}

static int test_apply_control(void)
{
	addr control, call, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	test_make_code(&call);
	function_heap(&call, Nil, call);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);

	apply_control(ptr, call, Nil);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "apply_control1");

	list_heap(&pos, fixnumh(10), fixnumh(20), NULL);
	apply_control(ptr, call, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "apply_control2");

	free_control(ptr, control);

	RETURN;
}

static int call_funcall_control(Execute ptr, fixnum size, addr call, ...)
{
	addr pos;
	va_list args;

	va_start(args, call);
	stdarg_control(ptr, call, args);
	va_end(args);
	getresult_control(ptr, &pos);
	return RefFixnum(pos) == size;
}

static int test_stdarg_control(void)
{
	addr control, call;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	test_make_code(&call);
	function_heap(&call, Nil, call);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);

	test(call_funcall_control(ptr, 0, call, NULL), "stdarg_control1");
	test(call_funcall_control(ptr, 2, call,
				fixnumh(10), fixnumh(20), NULL),
			"stdarg_control2");

	free_control(ptr, control);

	RETURN;
}

static int test_funcall_control(void)
{
	addr control, call, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	test_make_code(&call);
	function_heap(&call, Nil, call);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);

	funcall_control(ptr, call, NULL);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "funcall_control1");

	funcall_control(ptr, call, fixnumh(10), fixnumh(20), NULL);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "funcall_control2");

	free_control(ptr, control);

	RETURN;
}

static int test_call_control(void)
{
	addr control, call, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	test_make_code(&call);
	function_heap(&call, Nil, call);
	setargs_nil_control(ptr);
	setvalues_nil_control(ptr);

	list_heap(&pos, call, NULL);
	call_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "call_control1");

	list_heap(&pos, call, fixnumh(10), fixnumh(20), NULL);
	call_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 2, "call_control2");

	free_control(ptr, control);

	RETURN;
}


/*
 *  execute-operator
 */
static int test_execute_goto(void)
{
	addr control, array, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	/*
	 *    0: goto 3
	 *    1: set 11
	 *    2: end
	 *    3: set 22
	 *    4: end
	 */
	vector4_heap(&array, 5);
	internchar(LISP_CODE, "GOTO", &pos);
	cons_heap(&pos, pos, index_heapr(3));
	setarray(array, 0, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(11));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(22));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 4, pos);
	/* function */
	code_heap(&pos, array);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 22, "execute_goto1");

	free_control(ptr, control);

	RETURN;
}

static int test_execute_if(void)
{
	addr control, array, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	/*
	 *  (if nil 11 22) -> 22
	 *    0: set nil
	 *    1: if-nil 4
	 *    2: set 11
	 *    3: goto 5
	 *    4: set 22
	 *    5: end
	 */
	vector4_heap(&array, 6);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, Nil);
	setarray(array, 0, pos);
	internchar(LISP_CODE, "IF-NIL", &pos);
	cons_heap(&pos, pos, index_heapr(4));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(11));
	setarray(array, 2, pos);
	internchar(LISP_CODE, "GOTO", &pos);
	cons_heap(&pos, pos, index_heapr(5));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(22));
	setarray(array, 4, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 5, pos);
	/* function */
	code_heap(&pos, array);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 22, "execute_if1");

	/*
	 *  (if t 11 22) -> 11
	 *    0: set nil
	 *    1: if-nil 4
	 *    2: set 11
	 *    3: goto 5
	 *    4: set 22
	 *    5: end
	 */
	vector4_heap(&array, 6);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, T);
	setarray(array, 0, pos);
	internchar(LISP_CODE, "IF-NIL", &pos);
	cons_heap(&pos, pos, index_heapr(4));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(11));
	setarray(array, 2, pos);
	internchar(LISP_CODE, "GOTO", &pos);
	cons_heap(&pos, pos, index_heapr(5));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(22));
	setarray(array, 4, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 5, pos);
	/* function */
	code_heap(&pos, array);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 11, "execute_if2");

	free_control(ptr, control);

	RETURN;
}

static addr test_execute_go_value = NULL;
static int test_execute_go_set(Execute ptr, addr right)
{
	test_execute_go_value = right;
	return 0;
}

static int test_execute_go(void)
{
	addr control, array, child, pos, value, debug;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	/*
	 *  tag: (hello . 3)
	 *    0: go hello
	 *    1: set-debug 11
	 *    2: end
	 *    3: set-debug 22
	 *    4: set 33
	 *    5: end
	 */
	symbol_heap(&debug);
	compiled_system(&pos, debug);
	SetPointer_code(p_debug2, test_execute_go_set);
	setcompiled_code(pos, p_debug2);
	SetFunctionSymbol(debug, pos);

	vector4_heap(&array, 6);
	internchar(LISP_CODE, "GO", &pos);
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&pos, pos, value);
	setarray(array, 0, pos);
	cons_heap(&pos, debug, fixnumh(11));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	cons_heap(&pos, debug, fixnumh(22));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(33));
	setarray(array, 4, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 5, pos);
	/* function */
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&value, value, index_heapr(3));
	conscar_heap(&value, value);
	code_heap(&pos, array);
	settype_code(pos, CodeType_TagBody);
	setinfo_code(pos, value);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	test_execute_go_value = NULL;
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "execute_go1");
	test(RefFixnum(test_execute_go_value) == 22, "execute_go2");

	/*
	 *  tag: (hello . 3)
	 *    0: execute
	 *         0: go hello
	 *         1: set-debug 11
	 *         2: end
	 *    1: set-debug 22
	 *    2: end
	 *    3: set-debug 33
	 *    4: end
	 */
	vector4_heap(&array, 3);
	internchar(LISP_CODE, "GO", &pos);
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&pos, pos, value);
	setarray(array, 0, pos);
	cons_heap(&pos, debug, fixnumh(11));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	code_heap(&child, array);
	settype_code(child, CodeType_Return);

	vector4_heap(&array, 5);
	internchar(LISP_CODE, "EXECUTE", &pos);
	cons_heap(&pos, pos, child);
	setarray(array, 0, pos);
	cons_heap(&pos, debug, fixnumh(22));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	cons_heap(&pos, debug, fixnumh(33));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 4, pos);
	/* function */
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&value, value, index_heapr(3));
	conscar_heap(&value, value);
	code_heap(&pos, array);
	settype_code(pos, CodeType_TagBody);
	setinfo_code(pos, value);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	test_execute_go_value = NULL;
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "execute_go3");
	test(RefFixnum(test_execute_go_value) == 33, "execute_go4");

	/*
	 *  tag: (hello . 3)
	 *    0: execute
	 *       tag: (aaa . 0)
	 *         0: set-debug 11
	 *         1: execute
	 *            0: go hello
	 *            1: end
	 *         2: set-debug 22
	 *         3: end
	 *    1: set-debug 33
	 *    2: end
	 *    3: set-debug 44
	 *    4: end
	 */
	vector4_heap(&array, 2);
	internchar(LISP_CODE, "GO", &pos);
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&pos, pos, value);
	setarray(array, 0, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 1, pos);
	code_heap(&child, array);
	settype_code(child, CodeType_Return);

	vector4_heap(&array, 4);
	cons_heap(&pos, debug, fixnumh(11));
	setarray(array, 0, pos);
	internchar(LISP_CODE, "EXECUTE", &pos);
	cons_heap(&pos, pos, child);
	setarray(array, 1, pos);
	cons_heap(&pos, debug, fixnumh(22));
	setarray(array, 2, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 3, pos);
	code_heap(&child, array);
	settype_code(child, CodeType_Return);

	vector4_heap(&array, 5);
	internchar(LISP_CODE, "EXECUTE", &pos);
	cons_heap(&pos, pos, child);
	setarray(array, 0, pos);
	cons_heap(&pos, debug, fixnumh(33));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	cons_heap(&pos, debug, fixnumh(44));
	setarray(array, 3, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 4, pos);
	/* function */
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&value, value, index_heapr(3));
	conscar_heap(&value, value);
	code_heap(&pos, array);
	settype_code(pos, CodeType_TagBody);
	setinfo_code(pos, value);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	test_execute_go_value = NULL;
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "execute_go5");
	test(RefFixnum(test_execute_go_value) == 44, "execute_go6");

	free_control(ptr, control);

	RETURN;
}

static int test_execute_return_from(void)
{
	addr control, array, child, pos, value;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	/*
	 *  block hello
	 *    0: set 11
	 *    1: return-from hello
	 *    2: set 22
	 *    3: end
	 */
	vector4_heap(&array, 4);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(11));
	setarray(array, 0, pos);
	internchar(LISP_CODE, "RETURN-FROM", &pos);
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&pos, pos, value);
	setarray(array, 1, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(22));
	setarray(array, 2, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 3, pos);
	/* function */
	internchar(LISP_PACKAGE, "HELLO", &value);
	code_heap(&pos, array);
	settype_code(pos, CodeType_Block);
	setinfo_code(pos, value);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 11, "execute_return_from1");

	/*
	 *  block hello
	 *    0: execute
	 *         0: set 11
	 *         1: return-from hello
	 *         2: set 22
	 *         3: end
	 *    1: set 33
	 *    2: end
	 */
	vector4_heap(&array, 4);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(11));
	setarray(array, 0, pos);
	internchar(LISP_CODE, "RETURN-FROM", &pos);
	internchar(LISP_PACKAGE, "HELLO", &value);
	cons_heap(&pos, pos, value);
	setarray(array, 1, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(22));
	setarray(array, 2, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 3, pos);
	code_heap(&child, array);

	vector4_heap(&array, 3);
	internchar(LISP_CODE, "EXECUTE", &pos);
	cons_heap(&pos, pos, child);
	setarray(array, 0, pos);
	internchar(LISP_CODE, "SET", &pos);
	cons_heap(&pos, pos, fixnumh(33));
	setarray(array, 1, pos);
	internchar(LISP_CODE, "END", &pos);
	conscar_heap(&pos, pos);
	setarray(array, 2, pos);
	/* function */
	internchar(LISP_PACKAGE, "HELLO", &value);
	code_heap(&pos, array);
	settype_code(pos, CodeType_Block);
	setinfo_code(pos, value);
	function_heap(&pos, Nil, pos);
	setvalues_nil_control(ptr);
	test_execute_go_value = NULL;
	funcall_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 11, "execute_return_from2");

	free_control(ptr, control);

	RETURN;
}


/*
 *  C language
 */
static int test_callclang_function(void)
{
	addr call, symbol, pos;
	Execute ptr;

	ptr = Execute_Thread;
	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	symbol_heap(&symbol);
	SetFunctionSymbol(symbol, call);
	pos = Nil;
	callclang_function(ptr, &pos, symbol);
	test(functionp(pos), "callclang_function1");

	pos = Nil;
	parse_callname_heap(&pos, symbol);
	callclang_function(ptr, &pos, pos);
	test(functionp(pos), "callclang_function2");

	pos = Nil;
	callclang_function(ptr, &pos, call);
	test(functionp(pos), "callclang_function3");

	RETURN;
}

static int test_callclang_values_apply_heap(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	list_heap(&pos, T, T, T, NULL);
	callclang_values_apply_heap(Execute_Thread, &pos, call, pos);
	test(singlep(pos), "callclang_values_apply_heap1");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 3, "callclang_values_apply_heap2");

	RETURN;
}

static void call_callclang_values_stdarg_heap(Execute ptr, addr *ret, addr call, ...)
{
	va_list args;

	va_start(args, call);
	callclang_values_stdarg_heap(ptr, ret, call, args);
	va_end(args);
}

static int test_callclang_values_stdarg_heap(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	call_callclang_values_stdarg_heap(Execute_Thread, &pos, call, T, T, T, NULL);
	test(singlep(pos), "callclang_values_stdarg_heap1");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 3, "callclang_values_stdarg_heap2");

	RETURN;
}

static int test_callclang_values_funcall_heap(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	callclang_values_funcall_heap(Execute_Thread, &pos, call, T, T, T, NULL);
	test(singlep(pos), "callclang_values_funcall_heap1");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 3, "callclang_values_funcall_heap2");

	RETURN;
}

static int test_callclang_values_char_heap(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	internchar(LISP_PACKAGE, "AAA", &pos);
	SetFunctionSymbol(pos, call);
	callclang_values_char_heap(Execute_Thread, &pos, LISP_PACKAGE, "AAA", T, T, T, NULL);
	test(singlep(pos), "callclang_values_char_heap1");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 3, "callclang_values_char_heap2");

	RETURN;
}

static int test_callclang_apply(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	list_heap(&pos, T, T, T, NULL);
	callclang_apply(Execute_Thread, &pos, call, pos);
	test(RefFixnum(pos) == 3, "callclang_apply1");

	RETURN;
}

static void call_callclang_stdarg(Execute ptr, addr *ret, addr call, ...)
{
	va_list args;

	va_start(args, call);
	callclang_stdarg(ptr, ret, call, args);
	va_end(args);
}

static int test_callclang_stdarg(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	call_callclang_stdarg(Execute_Thread, &pos, call, T, T, T, NULL);
	test(RefFixnum(pos) == 3, "callclang_stdarg1");

	RETURN;
}

static int test_callclang_funcall(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	callclang_funcall(Execute_Thread, &pos, call, T, T, T, NULL);
	test(RefFixnum(pos) == 3, "callclang_funcall1");

	RETURN;
}

static int test_callclang_char(void)
{
	addr call, pos;

	compiled_system(&call, Nil);
	SetPointer(p_debug1, dynamic, test_function_length);
	setcompiled_dynamic(call, p_debug1);
	internchar(LISP_PACKAGE, "AAA", &pos);
	SetFunctionSymbol(pos, call);
	callclang_char(Execute_Thread, &pos, LISP_PACKAGE, "AAA", T, T, T, NULL);
	test(RefFixnum(pos) == 3, "callclang_char1");

	RETURN;
}
#endif



/*
 *  Main
 */
static int testbreak_control(void)
{
#if 0
	/* taginfo */
	TestBreak(test_gettable_control);
	TestBreak(test_taginfo_heap);
	TestBreak(test_push_taginfo);
	TestBreak(test_close_taginfo);
	TestBreak(test_push_blockinfo);
	TestBreak(test_close_blockinfo);
	/* symstack */
	TestBreak(test_pushlexical_control);
	TestBreak(test_pushspecial_control);
	TestBreak(test_pushtable_control);
	TestBreak(test_pushfunction_control);
	TestBreak(test_pushsetf_control);
	/* stack */
	TestBreak(test_push_control);
	TestBreak(test_push_stack_control);
	TestBreak(test_push_return_control);
	TestBreak(test_push_push_control);
	TestBreak(test_push_close_control);
	TestBreak(test_push_protect_control);
	TestBreak(test_push_tagbody_control);
	TestBreak(test_push_block_control);
	TestBreak(test_close_result_control);
	TestBreak(test_copy_values_control);
	TestBreak(test_close_return);
	TestBreak(test_close_protect);
	TestBreak(test_close_tagbody);
	TestBreak(test_close_block);
	TestBreak(test_close_type_control);
	TestBreak(test_close_lexical_control);
	TestBreak(test_close_special_control);
	TestBreak(test_close_function_control);
	TestBreak(test_close_setf_control);
	TestBreak(test_pop_control_common);
	TestBreak(test_pop_control_unsafe);
	TestBreak(test_free_control);
	/* getcontrol */
	TestBreak(test_getdata_control);
	TestBreak(test_array_data_control);
	TestBreak(test_setargs_control);
	TestBreak(test_setargs_va_control);
	TestBreak(test_setargs_nil_control);
	TestBreak(test_setargs_list_control);
	TestBreak(test_setargs_list_control_unsafe);
	TestBreak(test_pushargs_control);
	TestBreak(test_pushargs_list_control);
	TestBreak(test_getargs_control);
	TestBreak(test_getargs_list_control_unsafe);
	TestBreak(test_getargs_list_control_heap);
	TestBreak(test_pushvalues_dynamic);
	TestBreak(test_pushvalues_unsafe);
	TestBreak(test_setresult_control);
	TestBreak(test_setvalues_control);
	TestBreak(test_setvalues_nil_control);
	TestBreak(test_setvalues_list_control);
	TestBreak(test_getresult_control);
	TestBreak(test_getvalues_control);
	TestBreak(test_list_from_vector);
	TestBreak(test_getvalues_list_control);
	TestBreak(test_lengthvalues_control);
	/* execute */
	TestBreak(test_runcode_control);
	TestBreak(test_execute_control);
	TestBreak(test_apply_control);
	TestBreak(test_stdarg_control);
	TestBreak(test_funcall_control);
	TestBreak(test_call_control);
	/* execute-operator */
	TestBreak(test_execute_goto);
	TestBreak(test_execute_if);
	TestBreak(test_execute_go);
	TestBreak(test_execute_return_from);
	/* C language */
	TestBreak(test_callclang_function);
	TestBreak(test_callclang_values_apply_heap);
	TestBreak(test_callclang_values_stdarg_heap);
	TestBreak(test_callclang_values_funcall_heap);
	TestBreak(test_callclang_values_char_heap);
	TestBreak(test_callclang_apply);
	TestBreak(test_callclang_stdarg);
	TestBreak(test_callclang_funcall);
	TestBreak(test_callclang_char);
#endif

	return 0;
}

int test_control(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		build_pathname();
		build_code();
		lisp_initialize = 1;
		result = testbreak_control();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

