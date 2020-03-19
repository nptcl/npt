#include "readtable.c"
#include "clos.h"
#include "character.h"
#include "code.h"
#include "condition.h"
#include "common.h"
#include "control.h"
#include "degrade.h"
#include "eval_declare.h"
#include "function.h"
#include "object.h"
#include "print.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  chartable
 */
static int test_init_readtable(void)
{
	memset(CharTable, 0, sizeoft(CharTable));
	init_readtable();
	test(CharTable[(size_t)'a'].chartype, "init_readtable1");
	test(CharTable[(size_t)'f'].exponent, "init_readtable2");

	RETURN;
}


/*
 *  readlabel
 */
static int test_readlabel_heap(void)
{
	addr pos, value, check;
	Execute ptr;

	ptr = Execute_Thread;
	fixnum_heap(&value, 10);
	readlabel_heap(ptr, &pos, value);
	test(GetType(pos) == LISPSYSTEM_READLABEL, "readlabel_heap1");
	test(gensymp_readlabel(pos), "readlabel_heap2");
	normal_readlabel(pos);
	test(! gensymp_readlabel(pos), "readlabel_heap3");
	gensym_readlabel(pos);
	test(gensymp_readlabel(pos), "readlabel_heap4");
	GetReadLabel(pos, ReadLabel_Label, &check);
	test(check == value, "readlabel_heap5");
	GetReadLabel(pos, ReadLabel_Value, &check);
	test(symbolp(check), "readlabel_heap6");
	GetPackageSymbol(check, &check);
	test(check == Nil, "readlabel_heap7");

	RETURN;
}


/*
 *  readinfo
 */
static int test_readinfo_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct readinfo_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	readinfo_local(local, &pos);
	test(GetType(pos) == LISPSYSTEM_READINFO, "readinfo_local1");
	GetReadInfo(pos, ReadInfo_Queue, &check);
	test(GetType(check) == LISPSYSTEM_CHARQUEUE, "readinfo_local2");
	GetReadInfo(pos, ReadInfo_Label, &check);
	test(consp(check), "readinfo_local3");
	str = ReadInfoStruct(pos);
	test(str->state == ReadInfo_State_First, "readinfo_local4");
	rollback_local(local, stack);

	RETURN;
}

static int test_readinfo_symbol(void)
{
	addr pos;

	readinfo_symbol(&pos);
	test(symbolp(pos), "readinfo_symbol1");

	RETURN;
}

static int test_getreadinfo(void)
{
	addr symbol, value, pos, control;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	readinfo_symbol(&symbol);
	readinfo_local(local, &value);
	pushspecial_control(ptr, symbol, value);
	getreadinfo(ptr, &pos);
	test(pos == value, "getreadinfo1");
	free_control(ptr, control);

	RETURN;
}

static int test_pushreadinfo(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);
	test(GetType(pos) == LISPSYSTEM_READINFO, "pushreadinfo1");
	getreadinfo(ptr, &check);
	test(check == pos, "pushreadinfo2");
	free_control(ptr, control);

	RETURN;
}

static int test_pushreadinfo_recursive(void)
{
	addr control, pos, value, check;
	Execute ptr;
	struct readinfo_struct *str;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);
	str = ReadInfoStruct(pos);
	str->preserving = 1;
	fixnum_heap(&value, 10);
	SetReadInfo(pos, ReadInfo_Label, value);

	pushreadinfo_recursive(ptr, &pos);
	test(GetType(pos) == LISPSYSTEM_READINFO, "pushreadinfo_recursive1");
	getreadinfo(ptr, &check);
	test(check == pos, "pushreadinfo_recursive2");
	str = ReadInfoStruct(pos);
	test(str->preserving, "pushreadinfo_recursive3");
	test(str->recursive, "pushreadinfo_recursive4");
	GetReadInfo(pos, ReadInfo_Label, &check);
	test(check == value, "pushreadinfo_recursive5");

	RETURN;
}

static int test_getpackage_readinfo(void)
{
	addr control, pos, value, check;
	Execute ptr;
	struct readinfo_struct *str;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);
	fixnum_heap(&value, 10);
	SetReadInfo(pos, ReadInfo_Package, value);
	getpackage_readinfo(ptr, &check);
	test(check == value, "getpackage_readinfo1");
	setpackage_readinfo(ptr, Nil);
	getpackage_readinfo(ptr, &check);
	test(check == Nil, "setpackage_readinfo1");
	getqueue_readinfo(ptr, &check);
	test(GetType(check) == LISPSYSTEM_CHARQUEUE, "getqueue_readinfo1");

	str = ReadInfoStruct(pos);
	test(getpreserving_readinfo(ptr) == 0, "getpreserving_readinfo1");
	str->preserving = 1;
	test(getpreserving_readinfo(ptr) == 1, "getpreserving_readinfo2");
	test(getescape_readinfo(ptr) == 0, "getescape_readinfo1");
	setescape_readinfo(ptr, 1);
	test(getescape_readinfo(ptr) == 1, "getescape_readinfo2");
	test(getdot_readinfo(ptr) == 0, "getdot_readinfo1");
	str->dot = 1;
	test(getdot_readinfo(ptr) == 1, "getdot_readinfo2");
	test(getreplace_readinfo(ptr, &check) == 0, "getreplace_readinfo1");
	str->replace = 1;
	fixnum_heap(&value, 10);
	conscar_heap(&check, value);
	SetReadInfo(pos, ReadInfo_Label, check);
	test(getreplace_readinfo(ptr, &check) == 1, "getreplace_readinfo2");
	test(check == value, "getreplace_readinfo3");
	test(getstate_readinfo(ptr) == ReadInfo_State_First, "getstate_readinfo1");
	setstate_readinfo(ptr, ReadInfo_State_Colon1);
	test(getstate_readinfo(ptr) == ReadInfo_State_Colon1, "getstate_readinfo2");

	RETURN;
}

static int test_clear_readinfo(void)
{
	addr control, pos, check;
	Execute ptr;
	struct readinfo_struct *str;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);
	setpackage_readinfo(ptr, T);
	getqueue_readinfo(ptr, &check);
	push_charqueue_heap(check, 'a');
	push_charqueue_heap(check, 'a');
	push_charqueue_heap(check, 'a');
	str = ReadInfoStruct(pos);
	str->escape = 1;
	str->state = ReadInfo_State_Colon1;

	clear_readinfo(ptr);
	getreadinfo(ptr, &check);
	test(check == pos, "clear_readinfo1");
	getpackage_readinfo(ptr, &check);
	test(check == Nil, "clear_readinfo2");
	getqueue_readinfo(ptr, &check);
	test(RefCharQueueSize(check) == 0, "clear_readinfo3");
	str = ReadInfoStruct(pos);
	test(str->escape == 0, "clear_readinfo4");
	test(str->state == ReadInfo_State_First, "clear_readinfo5");

	RETURN;
}


/*
 *  readtype
 */
static int test_make_readtype(void)
{
	addr pos;
	struct readtype_struct *str;

	make_readtype(&pos, ReadTable_Type_constituent, 'a', 0);
	test(GetType(pos) == LISPSYSTEM_READTYPE, "make_readtype1");
	str = ReadTypeStruct(pos);
	test(str->type == ReadTable_Type_constituent, "make_readtype2");
	test(str->code == 'a', "make_readtype3");
	test(str->dispatch == 0, "make_readtype4");
	test(! dispatch_readtype(pos), "make_readtype5");

	make_readtype(&pos, ReadTable_Type_whitespace, '#', 1);
	str = ReadTypeStruct(pos);
	test(str->dispatch, "make_readtype6");
	test(dispatch_readtype(pos), "make_readtype7");

	RETURN;
}

static int test_copy_readtype(void)
{
	addr pos, value;
	struct readtype_struct *str;

	make_readtype(&value, ReadTable_Type_constituent, 'a', 1);
	SetReadType(value, T);
	copy_readtype(&pos, value);
	test(pos != value, "copy_readtype1");
	test(GetType(pos) == LISPSYSTEM_READTYPE, "copy_readtype2");
	GetReadType(pos, &value);
	test(value == T, "copy_readtype3");
	str = ReadTypeStruct(pos);
	test(str->type == ReadTable_Type_constituent, "copy_readtype4");
	test(str->code == 'a', "copy_readtype5");
	test(str->dispatch == 1, "copy_readtype6");

	RETURN;
}

static int test_array_readtype(void)
{
	addr pos;
	struct readtype_struct *str;

	vector2_heap(&pos, 0x80);
	array_readtype(pos, ReadTable_Type_constituent, 'a');
	GetArrayA2(pos, (size_t)'a', &pos);
	test(GetType(pos) == LISPSYSTEM_READTYPE, "array_readtype1");
	str = ReadTypeStruct(pos);
	test(str->type == ReadTable_Type_constituent, "array_readtype2");
	test(str->code == 'a', "array_readtype3");
	test(str->dispatch == 0, "array_readtype4");

	RETURN;
}

static int test_macro_readtype(void)
{
	addr pos, check;
	struct readtype_struct *str;

	vector2_heap(&pos, 0x80);
	TermReadType(pos, '"', DOUBLE_QUOTE);
	GetArrayA2(pos, (size_t)'"', &pos);
	test(GetType(pos) == LISPSYSTEM_READTYPE, "macro_readtype1");
	str = ReadTypeStruct(pos);
	test(str->type == ReadTable_Type_macro_term, "macro_readtype2");
	test(str->code == '"', "macro_readtype3");
	test(str->dispatch == 0, "macro_readtype4");
	GetReadType(pos, &check);
	GetConst(SYSTEM_DOUBLE_QUOTE_READER, &pos);
	GetFunctionSymbol(pos, &pos);
	test(functionp(pos), "macro_readtype5");
	test(pos == check, "macro_readtype6");

	vector2_heap(&pos, 0x80);
	DispatchReadType(pos, '#', SHARP);
	GetArrayA2(pos, (size_t)'#', &pos);
	test(GetType(pos) == LISPSYSTEM_READTYPE, "macro_readtype7");
	str = ReadTypeStruct(pos);
	test(str->type == ReadTable_Type_macro_nonterm, "macro_readtype8");
	test(str->code == '#', "macro_readtype9");
	test(str->dispatch, "macro_readtype10");
	GetReadType(pos, &check);
	GetConst(SYSTEM_SHARP_READER, &pos);
	GetFunctionSymbol(pos, &pos);
	test(functionp(pos), "macro_readtype11");
	test(pos == check, "macro_readtype12");

	RETURN;
}

static int test_default_array_readtype(void)
{
	addr pos, check;
	struct readtype_struct *str;

	vector2_heap(&pos, 0x80);
	default_array_readtype(pos);
	GetArrayA2(pos, (size_t)' ', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_whitespace, "default_array_readtype1");
	GetArrayA2(pos, (size_t)'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_constituent, "default_array_readtype2");
	GetArrayA2(pos, (size_t)'\\', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_escape_single, "default_array_readtype3");
	GetArrayA2(pos, (size_t)'|', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_escape_multiple, "default_array_readtype4");
	GetArrayA2(pos, (size_t)'"', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_term, "default_array_readtype5");
	GetArrayA2(pos, (size_t)'#', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "default_array_readtype6");

	RETURN;
}

static int test_dispatch_character(void)
{
	addr pos, check;

	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	dispatch_character(pos, '#', 'A', CONSTANT_SYSTEM_ARRAY_DISPATCH);
	findvalue_character2_hashtable(pos, '#', 'A', &check);
	test(check != Nil, "dispatch_character1");
	GetConst(SYSTEM_ARRAY_DISPATCH, &pos);
	GetFunctionSymbol(pos, &pos);
	test(pos == check, "dispatch_character2");

	RETURN;
}

static int test_default_dispatch_readtype(void)
{
	addr pos, check, call;

	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	default_dispatch_readtype(pos, '#');

	findvalue_character2_hashtable(pos, '#', 'A', &check);
	test(check != Nil, "default_dispatch_readtype1");
	GetConst(SYSTEM_ARRAY_DISPATCH, &call);
	GetFunctionSymbol(call, &call);
	test(call == check, "default_dispatch_readtype2");

	findvalue_character2_hashtable(pos, '#', '<', &check);
	test(check != Nil, "default_dispatch_readtype3");
	GetConst(SYSTEM_LESS_DISPATCH, &call);
	GetFunctionSymbol(call, &call);
	test(call == check, "default_dispatch_readtype4");

	RETURN;
}

static int test_make_array_readtype(void)
{
	addr pos, check;
	size_t size;
	struct readtype_struct *str;

	make_array_readtype(&pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "make_array_readtype1");
	LenArrayA2(pos, &size);
	test(size == 0x80, "make_array_readtype1");
	GetArrayA2(pos, (size_t)'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_constituent, "make_array_readtype3");

	RETURN;
}

static int test_make_table_readtype(void)
{
	addr pos;
	enum HASHTABLE_TEST value;

	make_table_readtype(&pos);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "make_table_readtype1");
	gettest_hashtable(pos, &value);
	test(value == HASHTABLE_TEST_EQL, "make_table_readtype2");

	RETURN;
}

static int test_make_dispatch_readtype(void)
{
	addr pos, check, call;

	make_dispatch_readtype(&pos);
	findvalue_character2_hashtable(pos, '#', 'X', &check);
	test(check != Nil, "make_dispatch_readtype1");
	GetConst(SYSTEM_HEXADECIMAL_DISPATCH, &call);
	GetFunctionSymbol(call, &call);
	test(call == check, "make_dispatch_readtype2");

	RETURN;
}


/*
 *  readtable
 */
static int test_copy_array_readtable(void)
{
	addr pos1, pos2, check, check2;
	struct readtype_struct *str;

	vector2_heap(&pos1, 0x80);
	vector2_heap(&pos2, 0x80);
	default_array_readtype(pos1);
	SetArrayA2(pos2, 1, T);
	copy_array_readtable(pos1, pos2);

	GetArrayA2(pos2, (size_t)' ', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_whitespace, "copy_array_readtable1");
	GetArrayA2(pos2, (size_t)'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_constituent, "copy_array_readtable2");
	GetArrayA2(pos1, (size_t)'a', &check2);
	test(check != check2, "copy_array_readtable3");
	GetArrayA2(pos2, 1, &check);
	test(check == Nil, "copy_array_readtable4");

	RETURN;
}

static int test_copy_table_readtable(void)
{
	addr pos1, pos2, key, value;
	struct readtype_struct *str;
	size_t size;

	hashtable_heap(&pos1);
	settest_hashtable(pos1, HASHTABLE_TEST_EQL);
	/* hiragana-letter-a */
	make_readtype(&value, ReadTable_Type_constituent, 0x3042, 0);
	character_heap(&key, 0x3042);
	intern_hashheap(pos1, key, &key);
	SetCdr(key, value);
	/* katakana-letter-n */
	make_readtype(&value, ReadTable_Type_constituent, 0x30F3, 0);
	character_heap(&key, 0x30F3);
	intern_hashheap(pos1, key, &key);
	SetCdr(key, value);
	/* dummy -> pos2 */
	hashtable_heap(&pos2);
	settest_hashtable(pos2, HASHTABLE_TEST_EQL);
	make_readtype(&value, ReadTable_Type_constituent, 1, 0);
	character_heap(&key, 1);
	intern_hashheap(pos2, key, &key);
	SetCdr(key, value);

	/* copy */
	copy_table_readtable(pos1, pos2);
	getcount_hashtable(pos2, &size);
	test(size == 3, "copy_table_readtable1");
	findvalue_unicode_hashtable(pos2, 0x3042, &value);
	test(GetType(value) == LISPSYSTEM_READTYPE, "copy_table_readtable2");
	str = ReadTypeStruct(value);
	test(str->code == 0x3042, "copy_table_readtable3");
	test(str->type == ReadTable_Type_constituent, "copy_table_readtable4");
	findvalue_unicode_hashtable(pos2, 0x30F3, &value);
	test(GetType(value) == LISPSYSTEM_READTYPE, "copy_table_readtable5");
	str = ReadTypeStruct(value);
	test(str->code == 0x30F3, "copy_table_readtable6");
	findvalue_unicode_hashtable(pos2, 1, &value);
	test(value != Nil, "copy_table_readtable7");
	findvalue_unicode_hashtable(pos2, 0x10, &value);
	test(value == Nil, "copy_table_readtable8");

	RETURN;
}

static int test_copy_dispatch_readtable(void)
{
	addr pos1, pos2, key, value;
	size_t size;

	hashtable_heap(&pos1);
	settest_hashtable(pos1, HASHTABLE_TEST_EQUAL);
	/* hiragana-letter-a */
	character2_heap(&key, '#', 0x3042);
	intern_hashheap(pos1, key, &key);
	fixnum_heap(&value, 10);
	SetCdr(key, value);
	/* katakana-letter-n */
	character2_heap(&key, '#', 0x30F3);
	intern_hashheap(pos1, key, &key);
	fixnum_heap(&value, 20);
	SetCdr(key, value);
	/* hiragana-letter-a */
	character2_heap(&key, '$', 0x3042);
	intern_hashheap(pos1, key, &key);
	fixnum_heap(&value, 30);
	SetCdr(key, value);
	/* dummy -> pos2 */
	hashtable_heap(&pos2);
	settest_hashtable(pos2, HASHTABLE_TEST_EQUAL);
	character2_heap(&key, '#', 1);
	intern_hashheap(pos2, key, &key);
	fixnum_heap(&value, 40);
	SetCdr(key, value);

	/* copy */
	copy_dispatch_readtable(pos1, pos2);
	getcount_hashtable(pos2, &size);
	test(size == 4, "copy_dispatch_readtable1");
	findvalue_character2_hashtable(pos2, '#', 0x3042, &value);
	test(RefFixnum(value) == 10, "copy_dispatch_readtable2");
	findvalue_character2_hashtable(pos2, '#', 0x30F3, &value);
	test(RefFixnum(value) == 20, "copy_dispatch_readtable3");
	findvalue_character2_hashtable(pos2, '$', 0x3042, &value);
	test(RefFixnum(value) == 30, "copy_dispatch_readtable4");
	findvalue_character2_hashtable(pos2, '$', 0x30F3, &value);
	test(value == Nil, "copy_dispatch_readtable5");
	findvalue_character2_hashtable(pos2, '#', 1, &value);
	test(value != Nil, "copy_dispatch_readtable6");

	RETURN;
}

static int test_copy_readtable(void)
{
	addr pos1, pos2, a, b, key, value;
	struct readtype_struct *str;

	heap_smallsize(&pos1, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	heap_smallsize(&pos2, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	*PtrCaseReadtable(pos1) = ReadTable_invert;

	vector2_heap(&a, 0x80);
	SetArrayReadtable(pos1, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQL);
	SetTableReadtable(pos1, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQUAL);
	SetDispatchReadtable(pos1, a);

	vector2_heap(&a, 0x80);
	SetArrayReadtable(pos2, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQL);
	SetTableReadtable(pos2, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQUAL);
	SetDispatchReadtable(pos2, a);

	/* array */
	GetArrayReadtable(pos1, &a);
	GetArrayReadtable(pos2, &b);
	make_readtype(&value, ReadTable_Type_constituent, 'a', 0);
	SetArrayA2(a, (size_t)'a', value);
	make_readtype(&value, ReadTable_Type_constituent, 'b', 0);
	SetArrayA2(a, (size_t)'b', value);
	make_readtype(&value, ReadTable_Type_whitespace, 'b', 0);
	SetArrayA2(b, (size_t)'b', value);
	make_readtype(&value, ReadTable_Type_whitespace, 'c', 0);
	SetArrayA2(b, (size_t)'c', value);
	/* table */
	GetTableReadtable(pos1, &a);
	GetTableReadtable(pos2, &b);
	make_readtype(&value, ReadTable_Type_constituent, 'd', 0);
	character_heap(&key, 'd');
	intern_hashheap(a, key, &key);
	SetCdr(key, value);
	make_readtype(&value, ReadTable_Type_constituent, 'e', 0);
	character_heap(&key, 'e');
	intern_hashheap(a, key, &key);
	SetCdr(key, value);
	make_readtype(&value, ReadTable_Type_whitespace, 'e', 0);
	character_heap(&key, 'e');
	intern_hashheap(b, key, &key);
	SetCdr(key, value);
	make_readtype(&value, ReadTable_Type_whitespace, 'f', 0);
	character_heap(&key, 'f');
	intern_hashheap(b, key, &key);
	SetCdr(key, value);
	/* dispatch */
	GetDispatchReadtable(pos1, &a);
	GetDispatchReadtable(pos2, &b);
	character2_heap(&key, '$', 'a');
	intern_hashheap(a, key, &key);
	fixnum_heap(&value, 10);
	SetCdr(key, value);
	character2_heap(&key, '$', 'b');
	intern_hashheap(a, key, &key);
	fixnum_heap(&value, 20);
	SetCdr(key, value);
	character2_heap(&key, '$', 'b');
	intern_hashheap(b, key, &key);
	fixnum_heap(&value, 30);
	SetCdr(key, value);
	character2_heap(&key, '$', 'c');
	intern_hashheap(b, key, &key);
	fixnum_heap(&value, 40);
	SetCdr(key, value);
	/* copy */
	copy_readtable(pos1, pos2);
	GetArrayReadtable(pos2, &a);
	GetArrayA2(a, (size_t)'a', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable1");
	GetArrayA2(a, (size_t)'b', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable2");
	GetArrayA2(a, (size_t)'c', &b);
	test(b == Nil, "copy_readtable3");

	GetTableReadtable(pos2, &a);
	findvalue_unicode_hashtable(a, 'd', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable4");
	findvalue_unicode_hashtable(a, 'e', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable5");
	findvalue_unicode_hashtable(a, 'f', &b);
	test(b == Nil, "copy_readtable6");

	GetDispatchReadtable(pos2, &a);
	findvalue_character2_hashtable(a, '$', 'a', &b);
	test(RefFixnum(b) == 10, "copy_readtable7");
	findvalue_character2_hashtable(a, '$', 'b', &b);
	test(RefFixnum(b) == 20, "copy_readtable8");
	findvalue_character2_hashtable(a, '$', 'c', &b);
	test(b == Nil, "copy_readtable9");

	test(*PtrCaseReadtable(pos2) == ReadTable_invert, "copy_readtable10");

	RETURN;
}

static int test_copy_readtable_heap(void)
{
	addr pos1, pos2, a, b, key, value;
	struct readtype_struct *str;

	heap_smallsize(&pos1, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	*PtrCaseReadtable(pos1) = ReadTable_invert;

	vector2_heap(&a, 0x80);
	SetArrayReadtable(pos1, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQL);
	SetTableReadtable(pos1, a);
	hashtable_heap(&a);
	settest_hashtable(a, HASHTABLE_TEST_EQUAL);
	SetDispatchReadtable(pos1, a);

	/* array */
	GetArrayReadtable(pos1, &a);
	make_readtype(&value, ReadTable_Type_constituent, 'a', 0);
	SetArrayA2(a, (size_t)'a', value);
	make_readtype(&value, ReadTable_Type_constituent, 'b', 0);
	SetArrayA2(a, (size_t)'b', value);
	/* table */
	GetTableReadtable(pos1, &a);
	make_readtype(&value, ReadTable_Type_constituent, 'd', 0);
	character_heap(&key, 'd');
	intern_hashheap(a, key, &key);
	SetCdr(key, value);
	make_readtype(&value, ReadTable_Type_constituent, 'e', 0);
	character_heap(&key, 'e');
	intern_hashheap(a, key, &key);
	SetCdr(key, value);
	/* dispatch */
	GetDispatchReadtable(pos1, &a);
	character2_heap(&key, '$', 'a');
	intern_hashheap(a, key, &key);
	fixnum_heap(&value, 10);
	SetCdr(key, value);
	character2_heap(&key, '$', 'b');
	intern_hashheap(a, key, &key);
	fixnum_heap(&value, 20);
	SetCdr(key, value);
	/* copy */
	copy_readtable_heap(pos1, &pos2);
	GetArrayReadtable(pos2, &a);
	GetArrayA2(a, (size_t)'a', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable_heap1");
	GetArrayA2(a, (size_t)'b', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable_heap2");
	GetArrayA2(a, (size_t)'c', &b);
	test(b == Nil, "copy_readtable_heap3");

	GetTableReadtable(pos2, &a);
	findvalue_unicode_hashtable(a, 'd', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable_heap4");
	findvalue_unicode_hashtable(a, 'e', &b);
	str = ReadTypeStruct(b);
	test(str->type == ReadTable_Type_constituent, "copy_readtable_heap5");
	findvalue_unicode_hashtable(a, 'f', &b);
	test(b == Nil, "copy_readtable_heap6");

	GetDispatchReadtable(pos2, &a);
	findvalue_character2_hashtable(a, '$', 'a', &b);
	test(RefFixnum(b) == 10, "copy_readtable_heap7");
	findvalue_character2_hashtable(a, '$', 'b', &b);
	test(RefFixnum(b) == 20, "copy_readtable_heap8");
	findvalue_character2_hashtable(a, '$', 'c', &b);
	test(b == Nil, "copy_readtable_heap9");

	test(*PtrCaseReadtable(pos2) == ReadTable_invert, "copy_readtable_heap10");

	RETURN;
}

static int test_readtable_heap(void)
{
	addr pos, check, a, key, value;
	struct readtype_struct *str;
	size_t size;

	readtable_heap(&pos);
	test(*PtrCaseReadtable(pos) == ReadTable_upcase, "readtable_heap1");

	GetArrayReadtable(pos, &a);
	GetArrayA2(a, (size_t)'a', &value);
	str = ReadTypeStruct(value);
	test(str->type == ReadTable_Type_constituent, "readtable_heap2");

	GetTableReadtable(pos, &a);
	getcount_hashtable(a, &size);
	test(size == 0, "readtable_heap3");

	GetDispatchReadtable(pos, &a);
	findvalue_character2_hashtable(a, '#', 'A', &value);
	test(value != Nil, "readtable_heap4");

	GetTableReadtable(pos, &a);
	character2_heap(&key, '#', 0x3042);
	intern_hashheap(a, key, &key);
	make_readtype(&value, ReadTable_Type_constituent, 0x3042, 0);
	SetCdr(key, value);

	/* copy-hashtable */
	readtable_heap(&check);
	copy_readtable(pos, check);

	GetArrayReadtable(check, &a);
	GetArrayA2(a, (size_t)'a', &value);
	str = ReadTypeStruct(value);
	test(str->type == ReadTable_Type_constituent, "readtable_heap5");

	GetTableReadtable(check, &a);
	getcount_hashtable(a, &size);
	test(size == 1, "readtable_heap6");

	GetDispatchReadtable(check, &a);
	findvalue_character2_hashtable(a, '#', 'A', &value);
	test(value != Nil, "readtable_heap7");

	/* copy-hashtable */
	copy_readtable_heap(pos, &check);

	GetArrayReadtable(check, &a);
	GetArrayA2(a, (size_t)'a', &value);
	str = ReadTypeStruct(value);
	test(str->type == ReadTable_Type_constituent, "readtable_heap8");

	GetTableReadtable(check, &a);
	getcount_hashtable(a, &size);
	test(size == 1, "readtable_heap9");

	GetDispatchReadtable(check, &a);
	findvalue_character2_hashtable(a, '#', 'A', &value);
	test(value != Nil, "readtable_heap10");

	RETURN;
}

static int test_setreadtype_readtable(void)
{
	addr pos, check;

	readtable_heap(&pos);
	setreadtype_readtable(pos, 'z', T);
	GetArrayReadtable(pos, &check);
	GetArrayA2(check, (size_t)'z', &check);
	test(check == T, "setreadtype_readtable1");

	setreadtype_readtable(pos, 10000, T);
	GetTableReadtable(pos, &check);
	findvalue_unicode_hashtable(check, 10000, &check);
	test(check == T, "setreadtype_readtable2");

	RETURN;
}

static int test_make_dispatch_macro_character(void)
{
	addr pos, check;
	struct readtype_struct *str;

	readtable_heap(&pos);
	character_heap(&check, '$');
	make_dispatch_macro_character(pos, check, 0);
	character_heap(&check, '%');
	make_dispatch_macro_character(pos, check, 1);
	GetArrayReadtable(pos, &pos);
	GetArrayA2(pos, (size_t)'$', &check);
	test(GetType(check) == LISPSYSTEM_READTYPE, "make_dispatch_macro_character1");
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_term, "make_dispatch_macro_character2");
	GetReadType(check, &check);
	test(check != Nil, "make_dispatch_macro_character3");

	GetArrayA2(pos, (size_t)'%', &check);
	test(GetType(check) == LISPSYSTEM_READTYPE, "make_dispatch_macro_character4");
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "make_dispatch_macro_character5");
	GetReadType(check, &check);
	test(check != Nil, "make_dispatch_macro_character6");

	GetConst(SYSTEM_DISPATCH_FUNCTION, &pos);
	GetFunctionSymbol(pos, &pos);
	test(functionp(pos), "make_dispatch_macro_character7");
	test(check == pos, "make_dispatch_macro_character8");

	RETURN;
}

static int test_get_default_dispatch_sharp(void)
{
	addr pos, check;

	character_heap(&pos, '+');
	get_default_dispatch_sharp(pos, &pos);
	GetConst(SYSTEM_PLUS_DISPATCH, &check);
	GetFunctionSymbol(check, &check);
	test(functionp(pos), "get_default_dispatch_sharp1");
	test(pos == check, "get_default_dispatch_sharp2");

	character_heap(&pos, 'E');
	get_default_dispatch_sharp(pos, &pos);
	test(pos == Nil, "get_default_dispatch_sharp3");

	RETURN;
}

static int test_get_default_dispatch_macro(void)
{
	addr sharp, pos, check;

	character_heap(&sharp, '#');
	character_heap(&pos, '+');
	get_default_dispatch_macro(sharp, pos, &pos);
	GetConst(SYSTEM_PLUS_DISPATCH, &check);
	GetFunctionSymbol(check, &check);
	test(functionp(pos), "get_default_dispatch_macro1");
	test(pos == check, "get_default_dispatch_macro2");

	character_heap(&pos, 'E');
	get_default_dispatch_macro(sharp, pos, &pos);
	test(pos == Nil, "get_default_dispatch_macro3");

	RETURN;
}


/*
 *  macro_character_execute
 */
static int test_readtype_readtable(void)
{
	addr pos, check;
	struct readtype_struct *str;

	readtable_heap(&pos);
	readtype_readtable(pos, '#', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "readtype_readtable1");

	setreadtype_readtable(pos, 10000, T);
	readtype_readtable(pos, 10000, &check);
	test(check == T, "readtype_readtable2");

	RETURN;
}

static int test_macro_character_call1(Execute ptr, addr stream, addr code)
{
	if (code == T)
		setresult_control(ptr, code);
	else
		setvalues_nil_control(ptr);
	
	return 0;
}

static int test_macro_character_call(void)
{
	int result, check;
	addr call, pos;
	Execute ptr;

	ptr = Execute_Thread;
	compiled_heap(&call, Nil);
	SetPointer(p_debug1, var2, test_macro_character_call1);
	setcompiled_var2(call, p_debug1);
	check = 999;
	result = macro_character_call(ptr, &check, &pos, call, Nil, T);
	test(result == 0, "macro_character_call1");
	test(check == 1, "macro_character_call2");
	test(pos == T, "macro_character_call3");

	result = macro_character_call(ptr, &check, &pos, call, T, Nil);
	test(result == 0, "macro_character_call4");
	test(check == 0, "macro_character_call5");

	RETURN;
}

static int test_macro_character_execute_value = 0;
static int test_macro_character_execute1(Execute ptr, addr stream, addr code)
{
	if (test_macro_character_execute_value)
		setresult_control(ptr, T);
	else
		setvalues_nil_control(ptr);
	
	return 0;
}

static int test_macro_character_execute(void)
{
	int result, check;
	addr pos, call, read, value;
	Execute ptr;

	ptr = Execute_Thread;
	readtable_heap(&pos);

	compiled_heap(&call, Nil);
	SetPointer(p_debug1, var2, test_macro_character_execute1);
	setcompiled_var2(call, p_debug1);
	make_readtype(&read, ReadTable_Type_macro_nonterm, 10000, 0);
	SetReadType(read, call);
	setreadtype_readtable(pos, 10000, read);

	test_macro_character_execute_value = 1;
	check = 0;
	result = macro_character_execute(ptr, &check, &value, 10000, Nil, pos);
	test(result == 0, "macro_character_execute1");
	test(check == 1, "macro_character_execute2");
	test(value == T, "macro_character_execute3");

	test_macro_character_execute_value = 0;
	result = macro_character_execute(ptr, &check, &value, 10000, Nil, pos);
	test(result == 0, "macro_character_execute4");
	test(check == 0, "macro_character_execute5");

	RETURN;
}

static int test_get_dispatch_macro_character(void)
{
	addr pos, check;

	readtable_heap(&pos);
	get_dispatch_macro_character(pos, '#', 'A', &check);
	test(check != Nil, "get_dispatch_macro_character1");
	get_dispatch_macro_character(pos, '#', 'z', &check);
	test(check == Nil, "get_dispatch_macro_character2");

	RETURN;
}

static int test_rem_dispatch_macro_character(void)
{
	addr pos, check;

	readtable_heap(&pos);
	rem_dispatch_macro_character(pos, '#', 'A');
	rem_dispatch_macro_character(pos, '#', 'z');
	get_dispatch_macro_character(pos, '#', 'A', &check);
	test(check == Nil, "rem_dispatch_macro_character1");

	RETURN;
}

static int test_set_dispatch_macro_character(void)
{
	addr pos, check;

	readtable_heap(&pos);
	set_dispatch_macro_character(pos, '#', 'a', T);
	set_dispatch_macro_character(pos, '#', 'B', T);
	set_dispatch_macro_character(pos, '#', 'z', T);

	get_dispatch_macro_character(pos, '#', 'A', &check);
	test(check == T, "set_dispatch_macro_character1");
	get_dispatch_macro_character(pos, '#', 'B', &check);
	test(check == T, "set_dispatch_macro_character2");
	get_dispatch_macro_character(pos, '#', 'Z', &check);
	test(check == T, "set_dispatch_macro_character3");

	RETURN;
}

static int test_get_default_macro_character(void)
{
	int nonterm;
	addr pos;

	get_default_macro_character('"', &pos, &nonterm);
	test(functionp(pos), "get_default_macro_character1");
	test(nonterm == 0, "get_default_macro_character2");
	get_default_macro_character('#', &pos, &nonterm);
	test(functionp(pos), "get_default_macro_character3");
	test(nonterm, "get_default_macro_character4");
	get_default_macro_character('Z', &pos, &nonterm);
	test(pos == Nil, "get_default_macro_character5");
	test(nonterm == 0, "get_default_macro_character6");

	RETURN;
}

static int test_get_macro_character(void)
{
	int nonterm;
	addr pos, check;

	readtable_heap(&pos);
	get_macro_character(pos, 'z', &check, &nonterm);
	test(check == Nil, "get_macro_character1");
	test(nonterm == 0, "get_macro_character2");
	get_macro_character(pos, '`', &check, &nonterm);
	test(functionp(check), "get_macro_character3");
	test(nonterm == 0, "get_macro_character4");
	get_macro_character(pos, '#', &check, &nonterm);
	test(functionp(check), "get_macro_character5");
	test(nonterm, "get_macro_character6");
	get_macro_character(pos, 'A', &check, &nonterm);
	test(check == Nil, "get_macro_character7");
	test(nonterm == 0, "get_macro_character8");

	RETURN;
}

static int test_set_macro_character(void)
{
	int nonterm;
	addr pos;

	readtable_heap(&pos);
	set_macro_character(pos, 'Z', 1, T);
	get_macro_character(pos, 'Z', &pos, &nonterm);
	test(pos == T, "set_macro_character1");
	test(nonterm == 1, "set_macro_character2");

	RETURN;
}

static int test_readtype_whitespace(void)
{
	test(readtype_whitespace(' '), "readtype_whitespace1");
	test(readtype_whitespace(0x0C), "readtype_whitespace2");
	test(! readtype_whitespace('a'), "readtype_whitespace3");
	test(! readtype_whitespace(10000), "readtype_whitespace4");

	RETURN;
}

static int test_readtype_constituent(void)
{
	test(readtype_constituent('a'), "readtype_constituent1");
	test(readtype_constituent('8'), "readtype_constituent2");
	test(! readtype_constituent(' '), "readtype_constituent3");
	test(! readtype_constituent(10000), "readtype_constituent4");

	RETURN;
}

static int test_readtype_termmacro(void)
{
	addr pos;

	test(readtype_termmacro('(', &pos), "readtype_termmacro1");
	test(functionp(pos), "readtype_termmacro2");
	test(readtype_termmacro('a', &pos) == 0, "readtype_termmacro3");

	RETURN;
}

static int test_readtype_sharpmacro(void)
{
	addr pos;

	test(readtype_sharpmacro('#', &pos), "readtype_sharpmacro1");
	test(functionp(pos), "readtype_sharpmacro2");
	test(readtype_sharpmacro('a', &pos) == 0, "readtype_sharpmacro3");

	RETURN;
}

static int test_delete_dispatch_macro(void)
{
	addr pos, check;
	size_t size;

	dispatch_readtype_heap(&pos);
	character2_heap(&check, '#', 'a');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '#', 'b');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'b');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'c');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);

	delete_dispatch_macro(pos, '$');
	getcount_hashtable(pos, &size);
	test(size == 2, "delete_dispatch_macro1");
	findvalue_character2_hashtable(pos, '#', 'a', &check);
	test(check == T, "delete_dispatch_macro2");
	findvalue_character2_hashtable(pos, '#', 'b', &check);
	test(check == T, "delete_dispatch_macro3");
	findvalue_character2_hashtable(pos, '$', 'a', &check);
	test(check == Nil, "delete_dispatch_macro4");
	findvalue_character2_hashtable(pos, '$', 'b', &check);
	test(check == Nil, "delete_dispatch_macro5");

	/* size=1, no-resize */
	hashtable_full_heap(&pos, HASHTABLE_TEST_EQUAL, 1, 1.0, 1.0);
	default_dispatch_readtype(pos, '#');
	getcount_hashtable(pos, &size);
	test(size != 0, "delete_dispatch_macro7");
	delete_dispatch_macro(pos, '$');
	getcount_hashtable(pos, &size);
	test(size != 0, "delete_dispatch_macro8");
	delete_dispatch_macro(pos, '#');
	getcount_hashtable(pos, &size);
	test(size == 0, "delete_dispatch_macro9");

	hashtable_full_heap(&pos, HASHTABLE_TEST_EQUAL, 1, 1.0, 1.0);
	character2_heap(&check, '#', 'a');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '#', 'b');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'b');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'c');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'd');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'e');
	intern_hashheap(pos, check, &check);
	SetCdr(check, T);
	delete_dispatch_macro(pos, '$');
	getcount_hashtable(pos, &size);
	test(size == 2, "delete_dispatch_macro10");

	RETURN;
}

static int test_delete_readtype(void)
{
	addr pos, check;
	size_t size;

	readtable_heap(&pos);
	delete_readtype(pos, 10000);
	test(1, "delete_readtype1");
	readtype_readtable(pos, 10000, &check);
	test(check == Nil, "delete_readtype2");

	delete_readtype(pos, '#');
	readtype_readtable(pos, '#', &check);
	test(check == Nil, "delete_readtype3");
	GetDispatchReadtable(pos, &check);
	getcount_hashtable(check, &size);
	test(size == 0, "delete_readtype4");

	delete_readtype(pos, 'A');
	readtype_readtable(pos, 'A', &check);
	test(check == Nil, "delete_readtype5");

	make_readtype(&check, ReadTable_Type_whitespace, 10000, 0);
	setreadtype_readtable(pos, 10000, check);
	readtype_readtable(pos, 10000, &check);
	test(check != Nil, "delete_readtype6");
	delete_readtype(pos, 10000);
	readtype_readtable(pos, 10000, &check);
	test(check == Nil, "delete_readtype7");

	RETURN;
}

static int test_setreadtype_default(void)
{
	addr pos, check;
	struct readtype_struct *str;

	readtable_heap(&pos);
	setreadtype_default(pos, 'z', ReadTable_Type_whitespace, T);
	readtype_readtable(pos, 'z', &check);
	test(GetType(check) == LISPSYSTEM_READTYPE, "setreadtype_default1");
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_whitespace, "setreadtype_default2");
	test(str->code == 'z', "setreadtype_default3");
	GetReadType(check, &check);
	test(check == T, "setreadtype_default4");

	RETURN;
}

static int test_setdispatch_default(void)
{
	addr pos, check;
	struct readtype_struct *str;

	readtable_heap(&pos);
	setdispatch_default(pos, '$', T);
	readtype_readtable(pos, '$', &check);
	test(GetType(check) == LISPSYSTEM_READTYPE, "setdispatch_default1");
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "setdispatch_default2");
	test(str->code == '$', "setdispatch_default3");
	GetReadType(check, &check);
	test(check == T, "setdispatch_default4");

	GetDispatchReadtable(pos, &pos);
	findvalue_character2_hashtable(pos, '#', 'A', &check);
	test(functionp(check), "setdispatch_default5");
	findvalue_character2_hashtable(pos, '$', 'A', &check);
	test(functionp(check), "setdispatch_default6");

	RETURN;
}

static int test_set_syntax_from_default(void)
{
	addr pos, check;
	struct readtype_struct *str;

	readtable_heap(&pos);

	set_syntax_from_default('a', ' ', pos);
	readtype_readtable(pos, 'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_whitespace, "set_syntax_from_default1");

	set_syntax_from_default('a', 'z', pos);
	readtype_readtable(pos, 'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_constituent, "set_syntax_from_default2");

	set_syntax_from_default('a', '\\', pos);
	readtype_readtable(pos, 'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_escape_single, "set_syntax_from_default3");

	set_syntax_from_default('a', '|', pos);
	readtype_readtable(pos, 'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_escape_multiple, "set_syntax_from_default4");

	set_syntax_from_default('a', '"', pos);
	readtype_readtable(pos, 'a', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_term, "set_syntax_from_default5");
	GetReadType(pos, &check);
	test(check != Nil, "set_syntax_from_default6");

	set_syntax_from_default('$', '#', pos);
	readtype_readtable(pos, '$', &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "set_syntax_from_default7");
	GetReadType(pos, &check);
	test(check != Nil, "set_syntax_from_default8");
	GetDispatchReadtable(pos, &check);
	findvalue_character2_hashtable(check, '$', 'A', &check);
	test(functionp(check), "set_syntax_from_default9");

	set_syntax_from_default('a', 10000, pos);
	readtype_readtable(pos, 'a', &check);
	test(check == Nil, "set_syntax_from_default10");

	RETURN;
}

static int test_copy_dispatch_macro(void)
{
	addr pos1, pos2, check;

	dispatch_readtype_heap(&pos1);
	character2_heap(&check, '#', 'a');
	intern_hashheap(pos1, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '#', 'b');
	intern_hashheap(pos1, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'b');
	intern_hashheap(pos1, check, &check);
	SetCdr(check, T);
	character2_heap(&check, '$', 'c');
	intern_hashheap(pos1, check, &check);
	SetCdr(check, T);

	dispatch_readtype_heap(&pos2);
	copy_dispatch_macro('!', '$', pos2, pos1);

	findvalue_character2_hashtable(pos2, '!', 'a', &check);
	test(check == Nil, "copy_dispatch_macro1");
	findvalue_character2_hashtable(pos2, '!', 'b', &check);
	test(check == T, "copy_dispatch_macro2");
	findvalue_character2_hashtable(pos2, '!', 'c', &check);
	test(check == T, "copy_dispatch_macro3");

	RETURN;
}

static int test_set_syntax_from_char(void)
{
	addr pos1, pos2, check;
	struct readtype_struct *str;

	readtable_heap(&pos1);
	readtable_heap(&pos2);
	set_syntax_from_char(10000, 20000, pos1, pos2);
	readtype_readtable(pos1, 10000, &check);
	test(check == Nil, "set_syntax_from_char1");

	set_syntax_from_char(10000, 'a', pos1, pos2);
	readtype_readtable(pos1, 10000, &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_constituent, "set_syntax_from_char1");

	set_syntax_from_char(10000, ' ', pos1, pos2);
	readtype_readtable(pos1, 10000, &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_whitespace, "set_syntax_from_char2");

	set_syntax_from_char(10000, '#', pos1, pos2);
	readtype_readtable(pos1, 10000, &check);
	str = ReadTypeStruct(check);
	test(str->type == ReadTable_Type_macro_nonterm, "set_syntax_from_char3");
	GetReadType(check, &check);
	test(functionp(check), "set_syntax_from_char4");

	GetDispatchReadtable(pos1, &check);
	findvalue_character2_hashtable(check, 10000, 'A', &check);
	test(functionp(check), "set_syntax_from_char5");

	RETURN;
}


/*
 *  tokentype
 */
static int test_checktable_char(void)
{
	test(checktable_char(0) == 0, "checktable_char1");
	test(checktable_char('a') != 0, "checktable_char2");
	test(checktable_char('E') != 0, "checktable_char3");
	test(checktable_char('<') != 0, "checktable_char4");
	test(checktable_char(0x7F) == 0, "checktable_char5");
	test(checktable_char(0x80) == 0, "checktable_char6");
	test(checktable_char(10000) == 0, "checktable_char7");

	RETURN;
}

static int test_checktable_base(void)
{
	test(checktable_base(10, 0) == 0, "checktable_base1");
	test(checktable_base(8, '0') == 1, "checktable_base2");
	test(checktable_base(8, '7') == 1, "checktable_base3");
	test(checktable_base(8, '8') == 0, "checktable_base4");
	test(checktable_base(8, 'a') == 0, "checktable_base5");
	test(checktable_base(8, 'A') == 0, "checktable_base6");
	test(checktable_base(10, '9') == 1, "checktable_base7");
	test(checktable_base(10, 'a') == 0, "checktable_base8");
	test(checktable_base(10, 'A') == 0, "checktable_base9");
	test(checktable_base(16, '0') == 1, "checktable_base10");
	test(checktable_base(16, '0'-1) == 0, "checktable_base11");
	test(checktable_base(16, '9') == 1, "checktable_base12");
	test(checktable_base(16, 'a') == 1, "checktable_base13");
	test(checktable_base(16, 'A') == 1, "checktable_base14");
	test(checktable_base(16, 'F') == 1, "checktable_base15");
	test(checktable_base(16, 'G') == 0, "checktable_base16");

	RETURN;
}

static int test_checktable_sign(void)
{
	test(checktable_sign('-') == 1, "checktable_sign1");
	test(checktable_sign('+') == 1, "checktable_sign2");
	test(checktable_sign('a') == 0, "checktable_sign3");

	RETURN;
}

static int test_checktable_exponent(void)
{
	int check;
	const char *str = "defslDEFSL";

	test(checktable_exponent('e') == 1, "checktable_exponent1");
	test(checktable_exponent('L') == 1, "checktable_exponent2");
	test(checktable_exponent('a') == 0, "checktable_exponent3");
	test(checktable_exponent(10000) == 0, "checktable_exponent4");
	check = 1;
	for (; *str; str++) {
		if (! checktable_exponent(*str)) {
			check = 0;
			break;
		}
	}
	test(check, "checktable_exponent5");

	RETURN;
}

static int test_checktable_potential(void)
{
	test(checktable_potential(10, '1') == 1, "checktable_potential1");
	test(checktable_potential(10, 'e') == 1, "checktable_potential2");
	test(checktable_potential(10, 'a') == 0, "checktable_potential3");
	test(checktable_potential(16, 'a') == 1, "checktable_potential4");
	test(checktable_potential(16, 'A') == 1, "checktable_potential5");
	test(checktable_potential(16, 'G') == 0, "checktable_potential6");
	test(checktable_potential(16, 'l') == 1, "checktable_potential7");
	test(checktable_potential(16, 'L') == 1, "checktable_potential8");

	test(checktable_potential(16, '+') == 1, "checktable_potential9");
	test(checktable_potential(16, '-') == 1, "checktable_potential10");
	test(checktable_potential(16, '/') == 1, "checktable_potential11");
	test(checktable_potential(16, '.') == 1, "checktable_potential12");
	test(checktable_potential(16, '^') == 1, "checktable_potential13");
	test(checktable_potential(16, '_') == 1, "checktable_potential14");

	RETURN;
}

static int test_checktable_firstpotential(void)
{
	test(checktable_firstpotential(10, '0') == 1, "checktable_firstpotential1");
	test(checktable_firstpotential(10, 'a') == 0, "checktable_firstpotential2");
	test(checktable_firstpotential(16, 'a') == 1, "checktable_firstpotential3");
	test(checktable_firstpotential(16, '+') == 1, "checktable_firstpotential4");
	test(checktable_firstpotential(16, '-') == 1, "checktable_firstpotential5");
	test(checktable_firstpotential(16, '.') == 1, "checktable_firstpotential6");
	test(checktable_firstpotential(16, '^') == 1, "checktable_firstpotential7");
	test(checktable_firstpotential(16, '_') == 1, "checktable_firstpotential8");
	test(checktable_firstpotential(16, '/') == 0, "checktable_firstpotential9");
	test(checktable_firstpotential(16, 'l') == 0, "checktable_firstpotential10");

	RETURN;
}

static int test_checktable_isdigit(void)
{
	test(checktable_isdigit('0') == 1, "checktable_isdigit1");
	test(checktable_isdigit('2') == 1, "checktable_isdigit2");
	test(checktable_isdigit('9') == 1, "checktable_isdigit3");
	test(checktable_isdigit('a') == 0, "checktable_isdigit4");
	test(checktable_isdigit('E') == 0, "checktable_isdigit5");

	RETURN;
}

static int test_checktable_isalpha(void)
{
	test(checktable_isalpha('a') == 1, "checktable_isalpha1");
	test(checktable_isalpha('b') == 1, "checktable_isalpha2");
	test(checktable_isalpha('z') == 1, "checktable_isalpha3");
	test(checktable_isalpha('A') == 1, "checktable_isalpha4");
	test(checktable_isalpha('Y') == 1, "checktable_isalpha5");
	test(checktable_isalpha('Z') == 1, "checktable_isalpha6");
	test(checktable_isalpha('0') == 0, "checktable_isalpha7");
	test(checktable_isalpha('+') == 0, "checktable_isalpha8");

	RETURN;
}

static enum TokenType tokentypechar(unsigned base, addr pos, const char *str)
{
	clear_charqueue(pos);
	while (*str)
		push_charqueue_heap(pos, *(str++));
	return tokentype(base, pos);
}

static int test_tokentype(void)
{
	addr pos;

	charqueue_heap(&pos, 0);
	test(tokentype(10, pos) == TokenType_error, "tokentype1");
	test(tokentypechar(10, pos, "1") == TokenType_integer, "tokentype2");
	test(tokentypechar(10, pos, "123") == TokenType_integer, "tokentype3");
	test(tokentypechar(10, pos, "12a") == TokenType_potential, "tokentype4");
	test(tokentypechar(16, pos, "12a") == TokenType_integer, "tokentype5");
	test(tokentypechar(16, pos, "12F") == TokenType_integer, "tokentype6");
	test(tokentypechar(10, pos, "10.") == TokenType_integer, "tokentype7");
	test(tokentypechar(10, pos, "10.4e3") == TokenType_float, "tokentype8");
	test(tokentypechar(10, pos, "Hello") == TokenType_symbol, "tokentype9");
	test(tokentypechar(10, pos, "aaa-bbb") == TokenType_symbol, "tokentype10");
	test(tokentypechar(10, pos, ".") == TokenType_dot, "tokentype11");
	test(tokentypechar(10, pos, "10/20") == TokenType_ratio, "tokentype12");
	test(tokentypechar(16, pos, "1a.3") == TokenType_potential, "tokentype13");

	RETURN;
}

static enum TokenType tokentype_char(unsigned base, const char *str)
{
	addr queue;
	enum TokenType type;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	pushchar_charqueue_local(local, queue, str);
	type = tokentype(base, queue);
	rollback_local(local, stack);

	return type;
}
static int checktoken_symbol(unsigned base, const char *str)
{
	enum TokenType type = tokentype_char(base, str);
	return type == TokenType_symbol || type == TokenType_potential;
}
static int checktoken_symbolonly(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_symbol;
}
static int checktoken_integer(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_integer;
}
static int checktoken_ratio(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_ratio;
}
static int checktoken_float(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_float;
}
static int checktoken_potential(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_potential;
}
static int checktoken_dot(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_dot;
}
static int checktoken_error(unsigned base, const char *str)
{
	return tokentype_char(base, str) == TokenType_error;
}

#define CheckToken_symbol(x,y) test(checktoken_symbol(x,y), "tokentype: " y)
#define CheckToken_symbolonly(x,y) test(checktoken_symbolonly(x,y), "tokentype: " y)
#define CheckToken_integer(x,y) test(checktoken_integer(x,y), "tokentype: " y)
#define CheckToken_ratio(x,y) test(checktoken_ratio(x,y), "tokentype: " y)
#define CheckToken_float(x,y) test(checktoken_float(x,y), "tokentype: " y)
#define CheckToken_potential(x,y) test(checktoken_potential(x,y), "tokentype: " y)
#define CheckToken_dot(x,y) test(checktoken_dot(x,y), "tokentype: " y)
#define CheckToken_error(x,y) test(checktoken_error(x,y), "tokentype: " y)

static int test_checktoken(void)
{
	/* base integer */
	CheckToken_symbol(10, "a");
	CheckToken_symbol(10, "A");
	CheckToken_integer(11, "a");
	CheckToken_integer(11, "A");
	CheckToken_symbol(11, "b");
	CheckToken_symbol(11, "B");
	CheckToken_integer(12, "b");
	CheckToken_integer(12, "B");
	CheckToken_integer(11, "+a");
	CheckToken_integer(11, "-A");
	CheckToken_integer(11, "+a.");
	CheckToken_integer(11, "-A.");
	CheckToken_integer(11, "01234a1234a");
	CheckToken_integer(11, "-AaAa9999AaAaA");
	CheckToken_integer(10, "1");
	CheckToken_integer(10, "9");
	CheckToken_integer(10, "0");
	CheckToken_integer(10, "009");
	CheckToken_integer(10, "100");
	CheckToken_integer(10, "1234567890123456789001");
	CheckToken_integer(10, "+0");
	CheckToken_integer(10, "-0");
	CheckToken_integer(10, "-1");
	CheckToken_integer(10, "-0099");
	CheckToken_integer(10, "+12345678901234567890111");

	/* integer */
	CheckToken_integer(10, "987");
	CheckToken_integer(10, "+987");
	CheckToken_integer(10, "-987");
	CheckToken_integer(10, "9");
	CheckToken_integer(10, "+8");
	CheckToken_integer(10, "-7");
	CheckToken_integer(10, "987.");
	CheckToken_integer(10, "+987.");
	CheckToken_integer(10, "-987.");
	CheckToken_integer(10, "9.");
	CheckToken_integer(10, "+8.");
	CheckToken_integer(10, "-7.");

	/* digit -> symbol */
	CheckToken_symbol(5, "5");
	CheckToken_symbol(5, "46");
	CheckToken_symbol(5, "+47");
	CheckToken_symbol(5, "-74");
	CheckToken_symbol(5, "5.");
	CheckToken_symbol(5, "46.");
	CheckToken_symbol(5, "+47.");
	CheckToken_symbol(5, "-74.");

	/* digit -> float */
	CheckToken_float(5, "10.1");
	CheckToken_float(5, "+10.1");
	CheckToken_float(5, "-103.1");
	CheckToken_float(5, "19.1");
	CheckToken_float(5, "+90.1");
	CheckToken_float(5, "-109.1");

	/* digit -> float, exponent */
	CheckToken_symbol(5, "-109.");
	CheckToken_float(5, "-109.e1");
	CheckToken_float(5, "+109.8e1");
	CheckToken_float(5, "+109.88");

	/* exponent */
	CheckToken_float(10, "33.3e1");
	CheckToken_float(10, "33.3E+2");
	CheckToken_float(10, "33.3s-9");
	CheckToken_float(10, "+33.3S12345");
	CheckToken_float(10, "+33.3f+2999");
	CheckToken_float(10, "+33.3F-998765");
	CheckToken_float(5, "39.3d1");
	CheckToken_float(5, "39.3D+2");
	CheckToken_float(5, "39.3l-9");
	CheckToken_float(5, "+93.3L12345");
	CheckToken_float(5, "+93.3f+2999");
	CheckToken_float(5, "+93.3F-998765");

	/* float */
	CheckToken_float(5, "9e1");
	CheckToken_float(10, "+9e1");
	CheckToken_float(10, "9e1");
	CheckToken_float(10, "+98e1");
	CheckToken_float(10, "94e1");
	CheckToken_float(10, "9.1");
	CheckToken_float(10, "+9.1");
	CheckToken_float(10, "97.1");
	CheckToken_float(10, "-97.1");
	CheckToken_float(10, "97.e1");
	CheckToken_float(10, "-97.e2");

	/* float no front */
	CheckToken_float(10, ".12e2");
	CheckToken_float(10, "+.90e9");
	CheckToken_float(10, "-.7e+123");
	CheckToken_float(10, ".12");
	CheckToken_float(10, "+.90");
	CheckToken_float(10, "-.7");
	CheckToken_symbol(10, ".e2");
	CheckToken_symbol(10, "+.");
	CheckToken_symbol(10, "-.e+123");

	/* float have front */
	CheckToken_float(10, "10e10");
	CheckToken_float(10, "+22.e33");
	CheckToken_float(10, "-33.567e+44");
	CheckToken_integer(10, "10");
	CheckToken_integer(10, "44.");
	CheckToken_symbol(5, "88");
	CheckToken_symbol(5, "88.");
	CheckToken_symbol(5, "+88.");
	CheckToken_float(10, "+22.33");
	CheckToken_float(10, "-33.567");

	/* ratio */
	CheckToken_ratio(10, "1/1");
	CheckToken_ratio(10, "3/1");
	CheckToken_ratio(10, "44/1");
	CheckToken_ratio(10, "123/1");
	CheckToken_ratio(10, "1/34");
	CheckToken_ratio(10, "1/234");
	CheckToken_ratio(10, "98767/1234455");

	CheckToken_ratio(10, "+1/1");
	CheckToken_ratio(10, "+3/1");
	CheckToken_ratio(10, "-44/1");
	CheckToken_ratio(10, "-123/1");
	CheckToken_ratio(16, "+8F/34Ab");
	CheckToken_symbol(5, "+8/34");
	CheckToken_symbol(5, "-1/238");
	CheckToken_symbol(5, "-1/833");
	CheckToken_symbol(5, "112288/12344");
	CheckToken_symbol(5, "+881122/12344");
	CheckToken_symbol(5, "11233/882344");
	CheckToken_symbol(5, "-1122/1234499");

	/* float marker */
	CheckToken_float(10, "123e456");
	CheckToken_integer(16, "123e456");

	/* float ansi-common-lisp */
	CheckToken_float(10, "0.0");
	CheckToken_float(10, "0E0");
	CheckToken_float(10, "0e0");
	CheckToken_float(10, "-.0");
	CheckToken_integer(10, "0.");
	CheckToken_float(10, "0.0s0");
	CheckToken_float(10, "0s0");
	CheckToken_float(10, "6.02E+23");
	CheckToken_float(10, "602E21");

	/* dot */
	CheckToken_integer(10, "1.");
	CheckToken_symbol(10, "+.");
	CheckToken_dot(10, ".");
	CheckToken_float(10, ".2");
	CheckToken_symbol(10, "..2");
	CheckToken_symbol(10, "..z");
	CheckToken_symbol(10, "...z");
	CheckToken_error(10, "..");
	CheckToken_error(10, "...");

	/* potential */
	CheckToken_potential(10, "1b5000");
	CheckToken_potential(10, "777777q");
	CheckToken_potential(10, "1.7J");
	CheckToken_potential(10, "-3/4+6.7J");
	CheckToken_potential(10, "12/25/83");
	CheckToken_potential(10, "27^19");
	CheckToken_potential(10, "3^4/5");
	CheckToken_potential(10, "6//7");
	CheckToken_potential(10, "3.1.2.6");
	CheckToken_potential(10, "^-43^");
	CheckToken_potential(10, "3.141_592_653_589_793_238_4");
	CheckToken_potential(10, "-3.7+2.6i-6.17j+19.6k");
	CheckToken_symbolonly(10, "/");
	CheckToken_symbolonly(10, "/5");
	CheckToken_symbolonly(10, "+");
	CheckToken_symbolonly(10, "1+");
	CheckToken_symbolonly(10, "1-");
	CheckToken_symbolonly(10, "foo+");
	CheckToken_symbolonly(10, "ab.cd");
	CheckToken_symbolonly(10, "_");
	CheckToken_symbolonly(10, "^");
	CheckToken_symbolonly(10, "^/-");

	CheckToken_potential(16, "bad-face");
	CheckToken_potential(16, "25-dec-83");
	CheckToken_ratio(16, "a/b");
	CheckToken_potential(16, "fad_cafe");
	CheckToken_potential(16, "f^");
	CheckToken_symbol(10, "bad-face");
	CheckToken_symbol(10, "25-dec-83");
	CheckToken_symbol(10, "a/b");
	CheckToken_symbol(10, "fad_cafe");
	CheckToken_symbol(10, "f^");

	CheckToken_symbolonly(10, "+");
	CheckToken_symbolonly(10, "10+");
	CheckToken_symbolonly(10, "0+");
	CheckToken_symbolonly(10, "10.3+");
	CheckToken_symbolonly(10, "10.3+++");
	CheckToken_potential(10, "10.3+++3");
	CheckToken_potential(10, "10e");
	CheckToken_symbolonly(10, "10ea");
	CheckToken_symbolonly(10, "10ee");
	CheckToken_potential(10, "10e10e20e30e");
	CheckToken_symbolonly(10, "10e10e20e30ee");
	CheckToken_symbolonly(10, "a10.2");
	CheckToken_symbolonly(10, "z10.2");
	CheckToken_potential(10, "10a3.2");

	CheckToken_symbolonly(5, "99.9e9e");
	CheckToken_potential(10, "99.9e9e");

	CheckToken_float(16, "10.23");
	CheckToken_symbol(16, "1a.23");

	RETURN;
}


/*
 *  maketoken
 */
static unsigned getreadbasecheck(fixnum value)
{
	unsigned result;
	codejump jump;
	addr control, symbol, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	result = 999;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		GetConst(SPECIAL_READ_BASE, &symbol);
		fixnum_heap(&pos, value);
		pushspecial_control(ptr, symbol, pos);
		result = getreadbase(ptr);
	}
	end_switch(&jump);
	free_control(ptr, control);
	throw_switch(&jump);

	return result;
}

static int test_getreadbase(void)
{
	test(getreadbasecheck(10) == 10, "getreadbase1");
	test(getreadbasecheck(2) == 2, "getreadbase2");
	test(getreadbasecheck(36) == 36, "getreadbase3");
	test(getreadbasecheck(16) == 16, "getreadbase4");

	RETURN;
}

static int check_symbol(addr pos, const char *pname, const char *sname)
{
	addr check;

	if (GetType(pos) != LISPTYPE_SYMBOL) return 0;
	GetPackageSymbol(pos, &check);
	if (check == Nil)
		return pname == NULL;
	getname_package(check, &check);
	if (string_equal_char(check, pname) == 0) return 0;
	GetNameSymbol(pos, &check);
	if (string_equal_char(check, sname) == 0) return 0;

	return 1;
}

static int test_maketoken_package(void)
{
	addr pos, queue, package;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	pushreadinfo(ptr, &pos);

	charqueue_local(local, &queue, 0);
	pushchar_charqueue_local(local, queue, "HELLO");
	maketoken_package(ptr, &pos, queue, T);
	test(check_symbol(pos, LISP_KEYWORD, "HELLO"), "maketoken_package1");

	setescape_readinfo(ptr, 1);
	clear_charqueue(queue);
	getreadinfo_struct(ptr)->unexport = 1;
	pushchar_charqueue_local(local, queue, "0011");
	GetConst(PACKAGE_COMMON_LISP, &package);
	maketoken_package(ptr, &pos, queue, package);
	test(check_symbol(pos, LISP_COMMON, "0011"), "maketoken_package2");

	setescape_readinfo(ptr, 0);
	clear_charqueue(queue);
	pushchar_charqueue_local(local, queue, "Hello");
	maketoken_package(ptr, &pos, queue, T);
	test(check_symbol(pos, LISP_KEYWORD, "Hello"), "maketoken_package3");

	rollback_local(local, stack);

	RETURN;
}

static void testreadinfo(Execute ptr, const char *pack, const char *str)
{
	addr package, pos, queue;

	if (pack)
		strvect_char_heap(&package, pack);
	else
		package = Nil;
	getreadinfo(ptr, &pos);
	setpackage_readinfo(ptr, package);
	getqueue_readinfo(ptr, &queue);
	clear_charqueue(queue);
	pushchar_charqueue_local(Local_Thread, queue, str);
}

static int test_maketoken_normal(void)
{
	addr pos, left, right;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	pushreadinfo(ptr, &pos);

	/* package - symbol */
	getreadinfo_struct(ptr)->unexport = 1;
	testreadinfo(ptr, LISP_COMMON_USER, "HELLO");
	maketoken_normal(ptr, &pos);
	test(symbolp(pos), "maketoken_normal1");
	GetPackageSymbol(pos, &left);
	GetConst(PACKAGE_COMMON_LISP_USER, &right);
	test(left == right, "maketoken_normal2");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "HELLO"), "maketoken_normal3");

	/* escape symbol */
	setescape_readinfo(ptr, 1);
	testreadinfo(ptr, LISP_COMMON_USER, "100");
	maketoken_normal(ptr, &pos);
	setescape_readinfo(ptr, 0);
	test(symbolp(pos), "maketoken_normal3");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "100"), "maketoken_normal4");

	/* symbol */
	testreadinfo(ptr, NULL, "AAA");
	maketoken_normal(ptr, &pos);
	test(symbolp(pos), "maketoken_normal5");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "AAA"), "maketoken_normal6");

	/* potential */
	testreadinfo(ptr, NULL, "BAD-FACE");
	maketoken_normal(ptr, &pos);
	test(symbolp(pos), "maketoken_normal7");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "BAD-FACE"), "maketoken_normal8");

	/* integer */
	testreadinfo(ptr, NULL, "123");
	maketoken_normal(ptr, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "maketoken_normal9");
	test(RefFixnum(pos) == 123, "maketoken_normal10");

	testreadinfo(ptr, NULL, "99999999988888888888887777777777777777776666666");
	maketoken_normal(ptr, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "maketoken_normal11");

	/* float */
	testreadinfo(ptr, NULL, "0.25");
	maketoken_normal(ptr, &pos);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "maketoken_normal12");
	test(RefSingleFloat(pos) == 0.25f, "maketoken_normal13");

	/* ratio */
	testreadinfo(ptr, NULL, "11/76");
	maketoken_normal(ptr, &pos);
	test(GetType(pos) == LISPTYPE_RATIO, "maketoken_normal14");

	/* dot */
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->dot = 1;
	testreadinfo(ptr, NULL, ".");
	maketoken_normal(ptr, &pos);
	test(symbolp(pos), "maketoken_normal15");
	GetConst(SYSTEM_READTABLE_DOT, &right);
	test(pos == right, "maketoken_normal16");

	rollback_local(local, stack);

	RETURN;
}

static int test_maketoken_gensym(void)
{
	addr pos, left;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	pushreadinfo(ptr, &pos);

	/* escape symbol */
	setescape_readinfo(ptr, 1);
	testreadinfo(ptr, LISP_COMMON_USER, "100");
	maketoken_gensym(ptr, &pos);
	setescape_readinfo(ptr, 0);
	test(symbolp(pos), "maketoken_gensym1");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "100"), "maketoken_gensym2");
	GetPackageSymbol(pos, &left);
	test(left == Nil, "maketoken_gensym3");

	/* symbol */
	testreadinfo(ptr, NULL, "AAA");
	maketoken_gensym(ptr, &pos);
	test(symbolp(pos), "maketoken_gensym4");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "AAA"), "maketoken_gensym5");
	GetPackageSymbol(pos, &left);
	test(left == Nil, "maketoken_gensym6");

	/* potential */
	testreadinfo(ptr, NULL, "BAD-FACE");
	maketoken_gensym(ptr, &pos);
	test(symbolp(pos), "maketoken_gensym7");
	GetNameSymbol(pos, &left);
	test(string_equal_char(left, "BAD-FACE"), "maketoken_gensym8");
	GetPackageSymbol(pos, &left);
	test(left == Nil, "maketoken_gensym9");

	rollback_local(local, stack);

	RETURN;
}

static int test_maketoken(void)
{
	addr control, pos, left;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);

	/* symbol */
	testreadinfo(ptr, NULL, "100");
	maketoken(ptr, &pos);
	test(integerp(pos), "maketoken1");

	/* potential */
	setstate_readinfo(ptr, ReadInfo_State_Gensym);
	testreadinfo(ptr, NULL, "BAD-FACE");
	maketoken(ptr, &pos);
	test(symbolp(pos), "maketoken2");
	GetNameSymbol(pos, &left);
	GetPackageSymbol(pos, &left);
	test(left == Nil, "maketoken3");

	free_control(ptr, control);

	RETURN;
}


/*
 *  pushchar_readtable
 */
static int test_charmode_readtable(void)
{
	addr pos;

	readtable_heap(&pos);

	*PtrCaseReadtable(pos) = ReadTable_upcase;
	test(charmode_readtable(pos, 'a') == 'A', "charmode_readtable1");
	test(charmode_readtable(pos, 'A') == 'A', "charmode_readtable2");
	test(charmode_readtable(pos, '0') == '0', "charmode_readtable3");

	*PtrCaseReadtable(pos) = ReadTable_downcase;
	test(charmode_readtable(pos, 'a') == 'a', "charmode_readtable4");
	test(charmode_readtable(pos, 'A') == 'a', "charmode_readtable5");
	test(charmode_readtable(pos, '0') == '0', "charmode_readtable6");

	*PtrCaseReadtable(pos) = ReadTable_preserve;
	test(charmode_readtable(pos, 'a') == 'a', "charmode_readtable7");
	test(charmode_readtable(pos, 'A') == 'A', "charmode_readtable8");
	test(charmode_readtable(pos, '0') == '0', "charmode_readtable9");

	*PtrCaseReadtable(pos) = ReadTable_invert;
	test(charmode_readtable(pos, 'a') == 'A', "charmode_readtable10");
	test(charmode_readtable(pos, 'A') == 'a', "charmode_readtable11");
	test(charmode_readtable(pos, '0') == '0', "charmode_readtable12");

	RETURN;
}

static void testcharqueue(const char *name)
{
	addr pos;

	getqueue_readinfo(Execute_Thread, &pos);
	clear_charqueue(pos);
	pushchar_charqueue_local(Local_Thread, pos, name);
}

static int test_tokenmode_readtable(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);

	setescape_readinfo(ptr, 1);
	testcharqueue("");
	test(tokenmode_readtable(ptr) == TokenType_symbol, "tokenmode_readtable1");

	setescape_readinfo(ptr, 0);
	testcharqueue("");
	test(tokenmode_readtable(ptr) == TokenType_empty, "tokenmode_readtable2");

	testcharqueue("HELLO");
	test(tokenmode_readtable(ptr) == TokenType_symbol, "tokenmode_readtable3");

	free_control(ptr, control);

	RETURN;
}

static int test_setpackage_readtable(void)
{
	addr pos, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &pos);

	setescape_readinfo(ptr, 1);
	testcharqueue("");
	setpackage_readinfo(ptr, Nil);
	setpackage_readtable(ptr);
	getpackage_readinfo(ptr, &pos);
	test(string_equal_char(pos, ""), "setpackage_readtable1");

	setescape_readinfo(ptr, 0);
	testcharqueue("");
	setpackage_readinfo(ptr, Nil);
	setpackage_readtable(ptr);
	getpackage_readinfo(ptr, &pos);
	test(pos == T, "setpackage_readtable2");

	setescape_readinfo(ptr, 0);
	testcharqueue("HELLO");
	setpackage_readtable(ptr);
	getpackage_readinfo(ptr, &pos);
	test(string_equal_char(pos, "HELLO"), "setpackage_readtable3");

	testcharqueue("AAA");
	setpackage_readtable(ptr);
	getpackage_readinfo(ptr, &pos);
	test(string_equal_char(pos, "AAA"), "setpackage_readtable4");

	free_control(ptr, control);

	RETURN;
}


/*
 *  read
 */
static int test_getcase_readtable(void)
{
	addr pos;

	readtable_heap(&pos);
	test(getcase_readtable(pos) == ReadTable_upcase, "getcase_readtable1");
	setcase_readtable(pos, ReadTable_invert);
	test(getcase_readtable(pos) == ReadTable_invert, "setcase_readtable1");

	RETURN;
}

static int test_getreadtable(void)
{
	addr left, right;
	Execute ptr;

	ptr = Execute_Thread;
	getreadtable(ptr, &left);
	test(GetType(left) == LISPTYPE_READTABLE, "getreadtable1");
	GetConst(SPECIAL_READTABLE, &right);
	getspecialcheck_local(ptr, right, &right);
	test(left == right, "getreadtable2");

	RETURN;
}

static int test_read_stream(void)
{
	int result, check;
	addr stream, pos;
	Execute ptr;
	fixnum value;
	unicode u;

	ptr = Execute_Thread;
	open_input_char_stream(&stream, "");
	result = read_stream(ptr, stream, &check, &pos);
	test(result == 0, "read_stream1");
	test(check, "read_stream2");

	open_input_char_stream(&stream, "100");
	result = read_stream(ptr, stream, &check, &pos);
	test(result == 0, "read_stream3");
	test(check == 0, "read_stream4");
	GetFixnum(pos, &value);
	test(value == 100, "read_stream5");

	open_input_char_stream(&stream, "  100 -200");
	result = read_stream(ptr, stream, &check, &pos);
	test(result == 0, "read_stream6");
	test(check == 0, "read_stream7");
	GetFixnum(pos, &value);
	test(value == 100, "read_stream8");

	read_char_stream(stream, &u);
	test(u == '-', "read_stream9");
	unread_char_stream(stream, u);

	result = read_stream(ptr, stream, &check, &pos);
	test(result == 0, "read_stream10");
	test(check == 0, "read_stream11");
	GetFixnum(pos, &value);
	test(value == -200, "read_stream12");

	result = read_stream(ptr, stream, &check, &pos);
	test(result == 0, "read_stream13");
	test(check, "read_stream14");

	RETURN;
}

static int test_read_preserving(void)
{
	int result, check;
	addr stream, pos;
	Execute ptr;
	fixnum value;
	unicode u;

	ptr = Execute_Thread;
	open_input_char_stream(&stream, "");
	result = read_preserving(ptr, stream, &check, &pos);
	test(result == 0, "read_preserving1");
	test(check, "read_preserving2");

	open_input_char_stream(&stream, "100");
	result = read_preserving(ptr, stream, &check, &pos);
	test(result == 0, "read_preserving3");
	test(check == 0, "read_preserving4");
	GetFixnum(pos, &value);
	test(value == 100, "read_preserving5");

	open_input_char_stream(&stream, "  100 -200");
	result = read_preserving(ptr, stream, &check, &pos);
	test(result == 0, "read_preserving6");
	test(check == 0, "read_preserving7");
	GetFixnum(pos, &value);
	test(value == 100, "read_preserving8");

	read_char_stream(stream, &u);
	test(u == ' ', "read_preserving9");
	unread_char_stream(stream, u);

	result = read_preserving(ptr, stream, &check, &pos);
	test(result == 0, "read_preserving10");
	test(check == 0, "read_preserving11");
	GetFixnum(pos, &value);
	test(value == -200, "read_preserving12");

	result = read_preserving(ptr, stream, &check, &pos);
	test(result == 0, "read_preserving13");
	test(check, "read_preserving14");

	RETURN;
}

static int test_readstring(void)
{
	addr pos;

	readstring(&pos, "100");
	test(GetType(pos) == LISPTYPE_FIXNUM, "readstring1");
	test(RefFixnum(pos) == 100, "readstring2");

	pos = readr(":HELLO");
	test(keywordp(pos), "readstring3");

	RETURN;
}


/*
 *  macro character
 */
static int test_double_quote_reader(void)
{
	addr pos;

	test(readstring(&pos, "\"Hello\"") == 0, "double_quote_reader1");
	test(string_equal_char(pos, "Hello"), "double_quote_reader2");
	test(readstring(&pos, "   \"\"   ") == 0, "double_quote_reader3");
	test(string_equal_char(pos, ""), "double_quote_reader4");
	test(readstring(&pos, "  \"ab\\\"cd\"  ") == 0, "double_quote_reader5");
	test(string_equal_char(pos, "ab\"cd"), "double_quote_reader6");

	RETURN;
}

static int test_single_quote_reader(void)
{
	addr pos, left, right;

	test(readstring(&pos, "'Hello") == 0, "single_quote_reader1");
	GetCons(pos, &left, &right);
	test(check_symbol(left, LISP_COMMON, "QUOTE"), "single_quote_reader2");
	test(right != Nil, "single_quote_reader3");
	GetCons(right, &left, &right);
	test(check_symbol(left, LISP_PACKAGE, "HELLO"), "single_quote_reader4");
	test(right == Nil, "single_quote_reader5");

	RETURN;
}

static int test_parensis_open_reader(void)
{
	addr pos, left, right;

	test(readstring(&pos, "()") == 0, "parensis_open_reader1");
	test(pos == Nil, "parensis_open_reader2");

	test(readstring(&pos, "(100)") == 0, "parensis_open_reader3");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 100, "parensis_open_reader4");
	test(right == Nil, "parensis_open_reader5");

	test(readstring(&pos, "(10 hello)") == 0, "parensis_open_reader6");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 10, "parensis_open_reader7");
	GetCons(right, &left, &right);
	test(check_symbol(left, LISP_PACKAGE, "HELLO"), "parensis_open_reader8");
	test(right == Nil, "parensis_open_reader9");

	test(readstring(&pos, "(10 . hello)") == 0, "parensis_open_reader10");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 10, "parensis_open_reader11");
	test(check_symbol(right, LISP_PACKAGE, "HELLO"), "parensis_open_reader12");

	test(readstring(&pos, "(10 20 . hello)") == 0, "parensis_open_reader13");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 10, "parensis_open_reader14");
	GetCons(right, &left, &right);
	test(RefFixnum(left) == 20, "parensis_open_reader15");
	test(check_symbol(right, LISP_PACKAGE, "HELLO"), "parensis_open_reader16");

	test(readstring(&pos, "(10 hello (:aaa . \"str\") 40)") == 0,
			"parensis_open_reader17");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 10, "parensis_open_reader18");
	GetCons(right, &left, &right);
	test(check_symbol(left, LISP_PACKAGE, "HELLO"), "parensis_open_reader19");
	GetCons(right, &left, &pos);
	GetCons(left, &left, &right);
	test(check_symbol(left, LISP_KEYWORD, "AAA"), "parensis_open_reader20");
	test(string_equal_char(right, "str"), "parensis_open_reader21");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 40, "parensis_open_reader22");
	test(right == Nil, "parensis_open_reader23");

	RETURN;
}

static int test_semicolon_reader(void)
{
	addr pos;

	test(readstring(&pos, "   ;; Hello\n100") == 0, "semicolon_reader1");
	test(RefFixnum(pos) == 100, "semicolon_reader2");

	RETURN;
}

static int test_backquote_reader(void)
{
	addr pos;

	test(readstring(&pos, "`hello") == 0, "backquote_reader1");
	test(quote_back_p(pos), "backquote_reader2");
	getvalue_quote(pos, &pos);
	test(equal(pos, readr("(quote hello)")), "backquote_reader3");

	RETURN;
}


/*
 *  dispatch character
 */
static int test_equal_dispatch(void)
{
	addr pos, left, right;

	test(readstring(&pos, "#1= hello") == 0, "equal_dispatch1");
	test(symbolp(pos), "equal_dispatch2");

	test(readstring(&pos, "(#1= hello #1#)") == 0, "equal_dispatch3");
	test(consp(pos), "equal_dispatch4");
	GetCons(pos, &left, &right);
	test(left == readr("hello"), "equal_dispatch5");
	test(consp(right), "equal_dispatch6");
	GetCons(right, &pos, &right);
	test(pos == left, "equal_dispatch7");
	test(right == Nil, "equal_dispatch8");

	test(readstring(&pos, "(#1= (10 20 30) #1#)") == 0, "equal_dispatch9");
	test(consp(pos), "equal_dispatch10");
	GetCons(pos, &left, &right);
	test(length_list_unsafe(left) == 3, "equal_dispatch11");
	test(consp(right), "equal_dispatch12");
	GetCons(right, &pos, &right);
	test(pos == left, "equal_dispatch13");
	test(right == Nil, "equal_dispatch14");

	test(readstring(&pos, "#1= (10 20 #1#)") == 0, "equal_dispatch15");
	test(consp(pos), "equal_dispatch16");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 10, "equal_dispatch17");
	GetCons(right, &left, &right);
	test(RefFixnum(left) == 20, "equal_dispatch18");
	GetCons(right, &left, &right);
	test(left == pos, "equal_dispatch19");
	test(right == Nil, "equal_dispatch20");

	test(readstring(&pos, "#1= (30 (40 . #1#))") == 0, "equal_dispatch21");
	test(consp(pos), "equal_dispatch22");
	GetCons(pos, &left, &right);
	test(RefFixnum(left) == 30, "equal_dispatch23");
	GetCons(right, &left, &right);
	test(right == Nil, "equal_dispatch24");
	test(consp(left), "equal_dispatch25");
	GetCons(left, &left, &right);
	test(RefFixnum(left) == 40, "equal_dispatch26");
	test(right == pos, "equal_dispatch27");

	RETURN;
}

static int test_single_quote_dispatch(void)
{
	addr pos, left, right;

	test(readstring(&pos, "#'Hello") == 0, "single_quote_dispatch1");
	GetCons(pos, &left, &right);
	test(check_symbol(left, LISP_COMMON, "FUNCTION"), "single_quote_dispatch2");
	test(right != Nil, "single_quote_dispatch3");
	GetCons(right, &left, &right);
	test(check_symbol(left, LISP_PACKAGE, "HELLO"), "single_quote_dispatch4");
	test(right == Nil, "single_quote_dispatch5");

	RETURN;
}

static int test_parensis_open_dispatch(void)
{
	addr pos, check;

	test(! readstring(&pos, "#()"), "parensis_open_dispatch1");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch2");
	test(lenarrayr(pos) == 0, "parensis_open_dispatch3");
	test(! readstring(&pos, "#(nil t 10)"), "parensis_open_dispatch4");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch5");
	test(lenarrayr(pos) == 3, "parensis_open_dispatch6");
	getarray(pos, 0, &check);
	test(check == Nil, "parensis_open_dispatch7");
	getarray(pos, 1, &check);
	test(check == T, "parensis_open_dispatch8");
	getarray(pos, 2, &check);
	test(RefFixnum(check) == 10, "parensis_open_dispatch9");

	test(! readstring(&pos, "#0()"), "parensis_open_dispatch10");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch11");
	test(lenarrayr(pos) == 0, "parensis_open_dispatch12");

	test(! readstring(&pos, "#2(t t)"), "parensis_open_dispatch13");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch14");
	test(lenarrayr(pos) == 2, "parensis_open_dispatch15");
	getarray(pos, 0, &check);
	test(check == T, "parensis_open_dispatch16");
	getarray(pos, 1, &check);
	test(check == T, "parensis_open_dispatch17");

	test(! readstring(&pos, "#4(10 t)"), "parensis_open_dispatch18");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch19");
	test(lenarrayr(pos) == 4, "parensis_open_dispatch20");
	getarray(pos, 0, &check);
	test(RefFixnum(check) == 10, "parensis_open_dispatch21");
	getarray(pos, 1, &check);
	test(check == T, "parensis_open_dispatch22");
	getarray(pos, 2, &check);
	test(check == T, "parensis_open_dispatch23");
	getarray(pos, 3, &check);
	test(check == T, "parensis_open_dispatch24");

	test(! readstring(&pos, "#2= #(t #2#)"), "parensis_open_dispatch25");
	test(GetType(pos) == LISPTYPE_VECTOR, "parensis_open_dispatch26");
	test(lenarrayr(pos) == 2, "parensis_open_dispatch27");
	getarray(pos, 0, &check);
	test(check == T, "parensis_open_dispatch28");
	getarray(pos, 1, &check);
	test(check == pos, "parensis_open_dispatch29");

	RETURN;
}

static int test_asterisk_dispatch(void)
{
	addr pos;
	size_t size;

	test(! readstring(&pos, "#*"), "asterisk_dispatch1");
	test(bitmemoryp(pos), "asterisk_dispatch2");
	bitmemory_length(pos, &size);
	test(size == 0, "asterisk_dispatch3");

	test(! readstring(&pos, "#*1011"), "asterisk_dispatch4");
	test(bitmemoryp(pos), "asterisk_dispatch5");
	bitmemory_length(pos, &size);
	test(size == 4, "asterisk_dispatch6");
	test(bitmemory_refint(pos, 0) == 1, "asterisk_dispatch7");
	test(bitmemory_refint(pos, 1) == 0, "asterisk_dispatch8");
	test(bitmemory_refint(pos, 2) == 1, "asterisk_dispatch9");
	test(bitmemory_refint(pos, 3) == 1, "asterisk_dispatch10");

	test(! readstring(&pos, "#*111000111000111000"), "asterisk_dispatch11");
	test(bitmemoryp(pos), "asterisk_dispatch12");
	bitmemory_length(pos, &size);
	test(size == 18, "asterisk_dispatch13");
	test(bitmemory_refint(pos, 0) == 1, "asterisk_dispatch14");
	test(bitmemory_refint(pos, 1) == 1, "asterisk_dispatch15");
	test(bitmemory_refint(pos, 2) == 1, "asterisk_dispatch16");
	test(bitmemory_refint(pos, 3) == 0, "asterisk_dispatch17");
	test(bitmemory_refint(pos, 4) == 0, "asterisk_dispatch18");
	test(bitmemory_refint(pos, 5) == 0, "asterisk_dispatch19");
	test(bitmemory_refint(pos, 6) == 1, "asterisk_dispatch20");
	test(bitmemory_refint(pos, 7) == 1, "asterisk_dispatch21");
	test(bitmemory_refint(pos, 8) == 1, "asterisk_dispatch22");
	test(bitmemory_refint(pos, 9) == 0, "asterisk_dispatch23");
	test(bitmemory_refint(pos, 10) == 0, "asterisk_dispatch24");
	test(bitmemory_refint(pos, 11) == 0, "asterisk_dispatch25");
	test(bitmemory_refint(pos, 12) == 1, "asterisk_dispatch26");
	test(bitmemory_refint(pos, 13) == 1, "asterisk_dispatch27");
	test(bitmemory_refint(pos, 14) == 1, "asterisk_dispatch28");
	test(bitmemory_refint(pos, 15) == 0, "asterisk_dispatch29");
	test(bitmemory_refint(pos, 16) == 0, "asterisk_dispatch30");
	test(bitmemory_refint(pos, 17) == 0, "asterisk_dispatch31");

	RETURN;
}

static int test_colon_dispatch(void)
{
	addr pos, check;

	test(! readstring(&pos, "#:hello"), "colon_dispatch1");
	test(symbolp(pos), "colon_dispatch2");
	GetPackageSymbol(pos, &check);
	test(check == Nil, "colon_dispatch3");
	GetNameSymbol(pos, &check);
	test(string_equal_char(check, "HELLO"), "colon_dispatch4");

	test(! readstring(&pos, "#:|Hello|"), "colon_dispatch5");
	test(symbolp(pos), "colon_dispatch6");
	GetPackageSymbol(pos, &check);
	test(check == Nil, "colon_dispatch7");
	GetNameSymbol(pos, &check);
	test(string_equal_char(check, "Hello"), "colon_dispatch8");

	RETURN;
}

static int test_backslash_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#\\a"), "backslash_dispatch1");
	test(RefCharacter(pos) == 'a', "backslash_dispatch2");
	test(! readstring(&pos, "#\\A"), "backslash_dispatch3");
	test(RefCharacter(pos) == 'A', "backslash_dispatch4");
	test(! readstring(&pos, "#\\SpaCE"), "backslash_dispatch5");
	test(RefCharacter(pos) == ' ', "backslash_dispatch6");

	RETURN;
}

static int test_or_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#|aaa|#100"), "or_dispatch1");
	test(RefFixnum(pos) == 100, "or_dispatch2");
	test(! readstring(&pos, "#|aaa#|bbb  |#ccc |#200"), "or_dispatch3");
	test(RefFixnum(pos) == 200, "or_dispatch4");

	RETURN;
}

static int test_plus_dispatch(void)
{
	addr control, list, symbol, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	list_heap(&list, readr(":aaa"), readr(":bbb"), readr(":ccc"), NULL);
	GetConst(SPECIAL_FEATURES, &symbol);
	pushspecial_control(ptr, symbol, list);

	test(! readstring(&pos, "#+aaa 100 200"), "plus_dispatch1");
	test(RefFixnum(pos) == 100, "plus_dispatch2");
	test(! readstring(&pos, "#-aaa 100 200"), "plus_dispatch3");
	test(RefFixnum(pos) == 200, "plus_dispatch4");
	test(! readstring(&pos, "#+zzz 100 200"), "plus_dispatch5");
	test(RefFixnum(pos) == 200, "plus_dispatch6");
	test(! readstring(&pos, "#-zzz 100 200"), "plus_dispatch7");
	test(RefFixnum(pos) == 100, "plus_dispatch8");

	test(! readstring(&pos, "#+(and aaa bbb) 100 200"), "plus_dispatch9");
	test(RefFixnum(pos) == 100, "plus_dispatch10");
	test(! readstring(&pos, "#+(and zzz bbb) 100 200"), "plus_dispatch11");
	test(RefFixnum(pos) == 200, "plus_dispatch12");
	test(! readstring(&pos, "#-(and aaa bbb) 100 200"), "plus_dispatch13");
	test(RefFixnum(pos) == 200, "plus_dispatch14");
	test(! readstring(&pos, "#-(and zzz bbb) 100 200"), "plus_dispatch15");
	test(RefFixnum(pos) == 100, "plus_dispatch16");

	test(! readstring(&pos, "#+(or (not xxx) zzz) 100 200"), "plus_dispatch17");
	test(RefFixnum(pos) == 100, "plus_dispatch18");
	test(! readstring(&pos, "#+(or xxx zzz) 100 200"), "plus_dispatch19");
	test(RefFixnum(pos) == 200, "plus_dispatch20");
	test(! readstring(&pos, "#-(or (not xxx) zzz) 100 200"), "plus_dispatch21");
	test(RefFixnum(pos) == 200, "plus_dispatch22");
	test(! readstring(&pos, "#-(or xxx zzz) 100 200"), "plus_dispatch23");
	test(RefFixnum(pos) == 100, "plus_dispatch24");

	free_control(ptr, control);

	RETURN;
}

static int test_dot_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#.100"), "dot_dispatch1");
	test(RefFixnum(pos) == 100, "dot_dispatch2");

	RETURN;
}

static int test_radix_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#4r100"), "radix_dispatch1");
	test(RefFixnum(pos) == 16, "radix_dispatch2");

	test(! readstring(&pos, "#5r-1234"), "radix_dispatch3");
	test(RefFixnum(pos) == -194, "radix_dispatch4");

	test(! readstring(&pos, "#17rgF21"), "radix_dispatch5");
	test(RefFixnum(pos) == 82978, "radix_dispatch6");

	RETURN;
}

static int test_binary_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#b1011"), "binary_dispatch1");
	test(RefFixnum(pos) == 11, "binary_dispatch2");
	test(! readstring(&pos, "#b-1101"), "binary_dispatch3");
	test(RefFixnum(pos) == -13, "binary_dispatch4");

	RETURN;
}

static int test_octal_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#o7654"), "octal_dispatch1");
	test(RefFixnum(pos) == 4012, "octal_dispatch2");
	test(! readstring(&pos, "#o-1236"), "octal_dispatch3");
	test(RefFixnum(pos) == -670, "octal_dispatch4");

	RETURN;
}

static int test_hexadecimal_dispatch(void)
{
	addr pos;

	test(! readstring(&pos, "#xFEab"), "hexadecimal_dispatch1");
	test(RefFixnum(pos) == 65195, "hexadecimal_dispatch2");
	test(! readstring(&pos, "#x-123F"), "hexadecimal_dispatch3");
	test(RefFixnum(pos) == -4671, "hexadecimal_dispatch4");

	RETURN;
}

static int test_complex_dispatch(void)
{
	addr pos, check;

	test(! readstring(&pos, "#c(10 20)"), "complex_dispatch1");
	test(complexp(pos), "complex_dispatch2");
	GetRealComplex(pos, &check);
	test(RefFixnum(check) == 10, "complex_dispatch3");
	GetImagComplex(pos, &check);
	test(RefFixnum(check) == 20, "complex_dispatch4");
	test(! readstring(&pos, "#C(-10.0 20.0)"), "complex_dispatch5");
	test(complexp(pos), "complex_dispatch6");
	GetRealComplex(pos, &check);
	test(RefSingleFloat(check) == -10.0f, "complex_dispatch7");
	GetImagComplex(pos, &check);
	test(RefSingleFloat(check) == 20.0f, "complex_dispatch8");

	RETURN;
}

static int test_pathname_dispatch(void)
{
	addr pos, check;

	test(! readstring(&pos, "#p\"\""), "pathname_dispatch1");
	test(GetType(pos) == LISPTYPE_PATHNAME, "pathname_dispatch2");
	test(! readstring(&pos, "#p\"file.txt\""), "pathname_dispatch3");
	test(GetType(pos) == LISPTYPE_PATHNAME, "pathname_dispatch4");
	GetPathname(pos, PATHNAME_INDEX_NAME, &check);
	test(string_equalp_char(check, "file"), "pathname_dispatch5");
	GetPathname(pos, PATHNAME_INDEX_TYPE, &check);
	test(string_equalp_char(check, "txt"), "pathname_dispatch6");

	RETURN;
}


/*
 *  basic object
 */
static int test_basic_token(void)
{
	addr pos;

	test(! readstring(&pos, "100"), "basic_token1");
	test(GetType(pos) == LISPTYPE_FIXNUM, "basic_token2");
	test(RefFixnum(pos) == 100, "basic_token3");
	test(! readstring(&pos, "-9999999999999999999999999"), "basic_token4");
	test(GetType(pos) == LISPTYPE_BIGNUM, "basic_token5");
	test(! readstring(&pos, "1.23d4"), "basic_token6");
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "basic_token7");
	test(RefDoubleFloat(pos) == 1.23e4, "basic_token8");
	test(! readstring(&pos, "2/3"), "basic_token9");
	test(GetType(pos) == LISPTYPE_RATIO, "basic_token10");

	RETURN;
}


/*
 *  main
 */
static int testbreak_readtable(void)
{
	in_package_lisp_package();
	/* chartable */
	TestBreak(test_init_readtable);
	/* readlabel */
	TestBreak(test_readlabel_heap);
	/* readinfo */
	TestBreak(test_readinfo_local);
	TestBreak(test_readinfo_symbol);
	TestBreak(test_getreadinfo);
	TestBreak(test_pushreadinfo);
	TestBreak(test_pushreadinfo_recursive);
	TestBreak(test_getpackage_readinfo);
	TestBreak(test_clear_readinfo);
	/* readtype */
	TestBreak(test_make_readtype);
	TestBreak(test_copy_readtype);
	TestBreak(test_array_readtype);
	TestBreak(test_macro_readtype);
	TestBreak(test_default_array_readtype);
	TestBreak(test_dispatch_character);
	TestBreak(test_default_dispatch_readtype);
	TestBreak(test_make_array_readtype);
	TestBreak(test_make_table_readtype);
	TestBreak(test_make_dispatch_readtype);
	/* readtable */
	TestBreak(test_copy_array_readtable);
	TestBreak(test_copy_table_readtable);
	TestBreak(test_copy_dispatch_readtable);
	TestBreak(test_copy_readtable);
	TestBreak(test_copy_readtable_heap);
	TestBreak(test_readtable_heap);
	TestBreak(test_setreadtype_readtable);
	TestBreak(test_make_dispatch_macro_character);
	TestBreak(test_get_default_dispatch_sharp);
	TestBreak(test_get_default_dispatch_macro);
	/* macro_character_execute */
	TestBreak(test_readtype_readtable);
	TestBreak(test_macro_character_call);
	TestBreak(test_macro_character_execute);
	TestBreak(test_get_dispatch_macro_character);
	TestBreak(test_rem_dispatch_macro_character);
	TestBreak(test_set_dispatch_macro_character);
	TestBreak(test_get_default_macro_character);
	TestBreak(test_get_macro_character);
	TestBreak(test_set_macro_character);
	TestBreak(test_readtype_whitespace);
	TestBreak(test_readtype_constituent);
	TestBreak(test_readtype_termmacro);
	TestBreak(test_readtype_sharpmacro);
	TestBreak(test_delete_dispatch_macro);
	TestBreak(test_delete_readtype);
	TestBreak(test_setreadtype_default);
	TestBreak(test_setdispatch_default);
	TestBreak(test_set_syntax_from_default);
	TestBreak(test_copy_dispatch_macro);
	TestBreak(test_set_syntax_from_char);
	/* tokentype */
	TestBreak(test_checktable_char);
	TestBreak(test_checktable_base);
	TestBreak(test_checktable_sign);
	TestBreak(test_checktable_exponent);
	TestBreak(test_checktable_potential);
	TestBreak(test_checktable_firstpotential);
	TestBreak(test_checktable_isdigit);
	TestBreak(test_checktable_isalpha);
	TestBreak(test_tokentype);
	TestBreak(test_checktoken);
	/* maketoken */
	TestBreak(test_getreadbase);
	TestBreak(test_maketoken_package);
	TestBreak(test_maketoken_normal);
	TestBreak(test_maketoken_gensym);
	TestBreak(test_maketoken);
	/* pushchar_readtable */
	TestBreak(test_charmode_readtable);
	TestBreak(test_tokenmode_readtable);
	TestBreak(test_setpackage_readtable);
	/* read */
	TestBreak(test_getcase_readtable);
	TestBreak(test_getreadtable);
	TestBreak(test_read_stream);
	TestBreak(test_read_preserving);
	TestBreak(test_readstring);
	/* macro character */
	TestBreak(test_double_quote_reader);
	TestBreak(test_single_quote_reader);
	TestBreak(test_parensis_open_reader);
	TestBreak(test_semicolon_reader);
	TestBreak(test_backquote_reader);
	/* dispatch character */
	TestBreak(test_equal_dispatch);
	TestBreak(test_single_quote_dispatch);
	TestBreak(test_parensis_open_dispatch);
	TestBreak(test_asterisk_dispatch);
	TestBreak(test_colon_dispatch);
	TestBreak(test_backslash_dispatch);
	TestBreak(test_or_dispatch);
	TestBreak(test_plus_dispatch);
	TestBreak(test_dot_dispatch);
	TestBreak(test_radix_dispatch);
	TestBreak(test_binary_dispatch);
	TestBreak(test_octal_dispatch);
	TestBreak(test_hexadecimal_dispatch);
	TestBreak(test_complex_dispatch);
	//TestBreak(test_array_dispatch); /* TODO */
	TestBreak(test_pathname_dispatch);
	//TestBreak(test_structure_dispatch); /* TODO */
	/* basic object */
	TestBreak(test_basic_token);

	return 0;
}

int test_readtable(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_readtable();
		build_eval_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_readtable();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

