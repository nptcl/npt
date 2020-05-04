#include "declare.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "control.h"
#include "copy.h"
#include "degrade.h"
#include "equal.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  Debug
 */
#if 0
static void set_index_optimize_declare(addr pos,
		OptimizeType value, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	Check(! (0 <= value && value <= 3), "range error");
	SetEvalDeclareOptimize(pos, index, value);
}
static void set_optimize_debug_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_DEBUG);
}
static void set_optimize_space_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SPACE);
}
#endif

static int get_type_declare(addr pos, addr symbol, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	CheckType(symbol, LISPTYPE_SYMBOL);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &pos);
	return getplist(pos, symbol, ret);
}
static int get_ftype_declare(addr pos, addr callname, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &pos);
	return getplist_callname(pos, callname, ret);
}

static int eq_constant_declare(addr pos, addr symbol,
		constindex constant, enum EVAL_DECLARE declare)
{
	Check(! eval_declare_p(pos), "type error");
	CheckType(symbol, LISPTYPE_SYMBOL);
	GetEvalDeclare(pos, declare, &pos);
	if (getplist(pos, symbol, &pos)) return 0;
	GetConstant(constant, &symbol);

	return pos == symbol;
}

static int eq_callname_declare(addr pos, addr callname,
		constindex constant, enum EVAL_DECLARE declare)
{
	addr value;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetEvalDeclare(pos, declare, &pos);
	if (getplist_callname(pos, callname, &pos)) return 0;
	GetConstant(constant, &value);

	return pos == value;
}

static int check_inline_declare(addr pos, addr callname)
{
	return eq_callname_declare(pos, callname,
			CONSTANT_COMMON_INLINE,
			EVAL_DECLARE_INLINE);
}

static int check_notinline_declare(addr pos, addr callname)
{
	return eq_callname_declare(pos, callname,
			CONSTANT_COMMON_NOTINLINE,
			EVAL_DECLARE_INLINE);
}

static int check_ignore_value_declare(addr pos, addr symbol)
{
	return eq_constant_declare(pos, symbol,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static int check_ignorable_value_declare(addr pos, addr symbol)
{
	return eq_constant_declare(pos, symbol,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static int check_ignore_function_declare(addr pos, addr callname)
{
	return eq_callname_declare(pos, callname,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static int check_ignorable_function_declare(addr pos, addr callname)
{
	return eq_callname_declare(pos, callname,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static int check_callname_declare(addr pos, addr callname, enum EVAL_DECLARE declare)
{
	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetEvalDeclare(pos, declare, &pos);
	return find_list_callname_unsafe(callname, pos);
}

static int check_special_declare(addr pos, addr symbol)
{
	return check_constant_declare(pos, symbol, EVAL_DECLARE_SPECIAL);
}

static int check_dynamic_value_declare(addr pos, addr symbol)
{
	return check_constant_declare(pos, symbol, EVAL_DECLARE_DYNAMIC_VALUE);
}

static int check_dynamic_function_declare(addr pos, addr callname)
{
	return check_callname_declare(pos, callname, EVAL_DECLARE_DYNAMIC_FUNCTION);
}


/*
 *  declarations
 */
static int test_make_eval_declare(void)
{
	addr pos;

	make_eval_declare(NULL, &pos);
	test(GetType(pos) == LISPTYPE_EVAL, "make_eval_declare1");
	test(RefEvalType(pos) == EVAL_TYPE_DECLARE, "make_eval_declare2");

	RETURN;
}

static int test_eval_declare_alloc_optimize(void)
{
	int i, check;
	addr pos;
	OptimizeType *ptr;

	eval_declare_alloc_optimize(NULL, &pos, 2);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_declare_alloc_optimize1");
	test(RefEvalType(pos) == EVAL_TYPE_DECLARE, "eval_declare_alloc_optimize2");

	ptr = PtrEvalDeclare_Low(pos);
	check = 1;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		if (ptr[i] != 2) check = 0;
	}
	test(check, "eval_declare_alloc_optimize3");

	RETURN;
}

static int test_eval_declare_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	eval_declare_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_declare_alloc1");
	test(! GetStatusDynamic(pos), "eval_declare_alloc2");
	test(RefEvalType(pos) == EVAL_TYPE_DECLARE, "eval_declare_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	eval_declare_alloc(local, &pos);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_declare_alloc4");
	test(GetStatusDynamic(pos), "eval_declare_alloc5");
	test(RefEvalType(pos) == EVAL_TYPE_DECLARE, "eval_declare_alloc6");
	rollback_local(local, stack);

	eval_declare_heap(&pos);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_declare_alloc7");
	test(! GetStatusDynamic(pos), "eval_declare_alloc8");
	test(RefEvalType(pos) == EVAL_TYPE_DECLARE, "eval_declare_alloc9");

	RETURN;
}

static int test_empty_declare(void)
{
	addr pos;

	eval_declare_heap(&pos);
	test(empty_declare(pos), "empty_declare1");
	SetEvalDeclare(pos, EVAL_DECLARE_IGNORE_VALUE, T);
	test(! empty_declare(pos), "empty_declare2");

	eval_declare_heap(&pos);
	set_optimize_debug_declare(pos, 0);
	test(! empty_declare(pos), "empty_declare3");

	RETURN;
}

static int test_empty_nil_declare(void)
{
	addr pos;

	eval_declare_heap(&pos);
	test(empty_nil_declare(pos), "empty_nil_declare1");
	SetEvalDeclare(pos, EVAL_DECLARE_IGNORE_VALUE, T);
	test(! empty_nil_declare(pos), "empty_nil_declare2");
	test(empty_nil_declare(Nil), "empty_nil_declare3");

	RETURN;
}

static int test_apply_array_declare(void)
{
	int i, check;
	addr pos;
	OptimizeType opt[EVAL_OPTIMIZE_SIZE];

	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		opt[i] = -1;
	apply_array_declare(opt, Nil);
	check = 1;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		if (opt[i] != -1)
			check = 0;
	}
	test(check, "apply_array_declare1");

	eval_declare_heap(&pos);
	set_optimize_debug_declare(pos, 3);
	apply_array_declare(opt, pos);
	test(opt[EVAL_OPTIMIZE_DEBUG] == 3, "apply_array_declare2");

	RETURN;
}

static int test_RefEvalDeclare(void)
{
	addr pos, check;

	eval_declare_heap(&pos);
	SetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, T);
	test(RefEvalDeclare(pos, EVAL_DECLARE_DECLARATION), "RefEvalDeclare1");
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &check);
	test(check == T, "RefEvalDeclare2");
	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, &check);
	test(check == Nil, "RefEvalDeclare3");

	RETURN;
}

static int test_RefEvalDeclareOptimize(void)
{
	addr pos;
	OptimizeType check;

	eval_declare_heap(&pos);
	SetEvalDeclareOptimize(pos, EVAL_OPTIMIZE_DEBUG, 1);
	SetEvalDeclareOptimize(pos, EVAL_OPTIMIZE_SAFETY, 2);
	test(RefEvalDeclareOptimize(pos, EVAL_OPTIMIZE_DEBUG) == 1,
			"RefEvalDeclareOptimize1");
	test(RefEvalDeclareOptimize(pos, EVAL_OPTIMIZE_SAFETY) == 2,
			"RefEvalDeclareOptimize2");
	test(RefEvalDeclareOptimize(pos, EVAL_OPTIMIZE_SPACE) < 0,
			"RefEvalDeclareOptimize3");
	GetEvalDeclareOptimize(pos, EVAL_OPTIMIZE_DEBUG, &check);
	test(check == 1, "RefEvalDeclareOptimize4");
	GetEvalDeclareOptimize(pos, EVAL_OPTIMIZE_SAFETY, &check);
	test(check == 2, "RefEvalDeclareOptimize5");
	GetEvalDeclareOptimize(pos, EVAL_OPTIMIZE_SPACE, &check);
	test(check < 0, "RefEvalDeclareOptimize6");

	RETURN;
}


/*
 *  access
 */
static int test_set_type_declare_heap(void)
{
	addr pos, sym1, sym2, sym3, type1, type2, list, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &sym1);
	internchar(LISP_SYSTEM, "BBB", &sym2);
	internchar(LISP_SYSTEM, "CCC", &sym3);
	GetTypeTable(&type1, Atom);
	GetTypeTable(&type2, List);
	set_type_declare_heap(pos, sym1, type1);
	set_type_declare_heap(pos, sym2, type2);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);
	test(list != Nil, "set_type_declare_heap1");
	test(getplist(list, sym1, &check) == 0, "set_type_declare_heap2");
	test(check == type1, "set_type_declare_heap3");
	test(getplist(list, sym2, &check) == 0, "set_type_declare_heap4");
	test(check == type2, "set_type_declare_heap5");
	test(getplist(list, sym3, &check), "set_type_declare_heap6");

	test(get_type_declare(pos, sym1, &check) == 0, "set_type_declare_heap7");
	test(check == type1, "set_type_declare_heap8");
	test(get_type_declare(pos, sym2, &check) == 0, "set_type_declare_heap9");
	test(check == type2, "set_type_declare_heap10");
	test(get_type_declare(pos, sym3, &check), "set_type_declare_heap11");

	RETURN;
}

static int test_push_type_declare_heap(void)
{
	addr pos, symbol, type1, type2, check, type;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	GetTypeTable(&type1, Atom);
	GetTypeTable(&type2, List);
	push_type_declare_heap(pos, symbol, type1);
	test(get_type_declare(pos, symbol, &check) == 0, "push_type_declare_heap1");
	test(check == type1, "push_type_declare_heap2");

	push_type_declare_heap(pos, symbol, type2);
	test(get_type_declare(pos, symbol, &check) == 0, "push_type_declare_heap3");
	test(RefLispDecl(check) == LISPDECL_AND, "push_type_declare_heap4");
	GetArrayType(check, 0, &check);
	GetArrayA4(check, 0, &type);
	test(type == type1, "push_type_declare_heap5");
	GetArrayA4(check, 1, &type);
	test(type == type2, "push_type_declare_heap6");

	RETURN;
}

static int test_set_ftype_declare_heap(void)
{
	addr pos, sym1, sym2, sym3, type1, type2, list, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &sym1);
	internchar(LISP_SYSTEM, "BBB", &sym2);
	internchar(LISP_SYSTEM, "CCC", &sym3);
	parse_callname_error(&sym1, sym1);
	parse_callname_error(&sym2, sym2);
	parse_callname_error(&sym3, sym3);
	GetTypeTable(&type1, Atom);
	GetTypeTable(&type2, List);
	set_ftype_declare_heap(pos, sym1, type1);
	set_ftype_declare_heap(pos, sym2, type2);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &list);
	test(list != Nil, "set_ftype_declare_heap1");
	test(getplist(list, sym1, &check) == 0, "set_ftype_declare_heap2");
	test(check == type1, "set_ftype_declare_heap3");
	test(getplist(list, sym2, &check) == 0, "set_ftype_declare_heap4");
	test(check == type2, "set_ftype_declare_heap5");
	test(getplist(list, sym3, &check), "set_ftype_declare_heap6");

	test(get_ftype_declare(pos, sym1, &check) == 0, "set_ftype_declare_heap7");
	test(check == type1, "set_ftype_declare_heap8");
	test(get_ftype_declare(pos, sym2, &check) == 0, "set_ftype_declare_heap9");
	test(check == type2, "set_ftype_declare_heap10");
	test(get_ftype_declare(pos, sym3, &check), "set_ftype_declare_heap11");

	RETURN;
}

static int test_push_ftype_declare_heap(void)
{
	addr pos, symbol, type1, type2, check, type;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	GetTypeTable(&type1, Atom);
	GetTypeTable(&type2, List);
	push_ftype_declare_heap(pos, symbol, type1);
	test(get_ftype_declare(pos, symbol, &check) == 0, "push_ftype_declare_heap1");
	test(check == type1, "push_ftype_declare_heap2");

	push_ftype_declare_heap(pos, symbol, type2);
	test(get_ftype_declare(pos, symbol, &check) == 0, "push_ftype_declare_heap3");
	test(RefLispDecl(check) == LISPDECL_AND, "push_ftype_declare_heap4");
	GetArrayType(check, 0, &check);
	GetArrayA4(check, 0, &type);
	test(type == type1, "push_ftype_declare_heap5");
	GetArrayA4(check, 1, &type);
	test(type == type2, "push_ftype_declare_heap6");

	RETURN;
}

static int test_plist_constant_declare_heap(void)
{
	addr pos, symbol, key, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	plist_constant_declare_heap(pos, symbol,
			CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE);
	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, &check);
	test(check != Nil, "plist_constant_declare_heap1");
	test(getplist(check, symbol, &check) == 0, "plist_constant_declare_heap2");
	GetConstant(CONSTANT_COMMON_INLINE, &key);
	test(check == key, "plist_constant_declare_heap3");

	test(eq_constant_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE),
			"eq_constant_declare1");
	test(! eq_constant_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_SPECIAL),
			"eq_constant_declare2");
	test(! eq_constant_declare(pos, symbol,
				CONSTANT_COMMON_NOTINLINE, EVAL_DECLARE_INLINE),
			"eq_constant_declare3");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(! eq_constant_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE),
			"eq_constant_declare4");

	RETURN;
}

static int test_plist_callname_declare_heap(void)
{
	addr pos, symbol, key, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	plist_callname_declare_heap(pos, symbol,
			CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE);
	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, &check);
	test(check != Nil, "plist_callname_declare_heap1");
	test(getplist_callname(check, symbol, &check) == 0,
			"plist_callname_declare_heap2");
	GetConstant(CONSTANT_COMMON_INLINE, &key);
	test(check == key, "plist_callname_declare_heap3");

	test(eq_callname_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE),
			"eq_callname_declare1");
	test(! eq_callname_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_SPECIAL),
			"eq_callname_declare2");
	test(! eq_callname_declare(pos, symbol,
				CONSTANT_COMMON_NOTINLINE, EVAL_DECLARE_INLINE),
			"eq_callname_declare3");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! eq_callname_declare(pos, symbol,
				CONSTANT_COMMON_INLINE, EVAL_DECLARE_INLINE),
			"eq_callname_declare4");

	RETURN;
}

static int test_push_inline_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_inline_declare_heap(pos, symbol);
	test(check_inline_declare(pos, symbol), "push_inline_declare1");
	test(! check_notinline_declare(pos, symbol), "push_inline_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_inline_declare(pos, symbol), "push_inline_declare3");
	test(! check_notinline_declare(pos, symbol), "push_inline_declare4");

	RETURN;
}

static int test_push_notinline_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_notinline_declare_heap(pos, symbol);
	test(! check_inline_declare(pos, symbol), "push_notinline_declare1");
	test(check_notinline_declare(pos, symbol), "push_notinline_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_inline_declare(pos, symbol), "push_notinline_declare3");
	test(! check_notinline_declare(pos, symbol), "push_notinline_declare4");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_inline_declare_heap(pos, symbol);
	test(check_inline_declare(pos, symbol), "push_notinline_declare5");

	RETURN;
}

static int test_push_ignore_value_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_ignore_value_declare_heap(pos, symbol);
	test(check_ignore_value_declare(pos, symbol),
			"push_ignore_value_declare1");
	test(! check_ignorable_value_declare(pos, symbol),
			"push_ignore_value_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(! check_ignore_value_declare(pos, symbol),
			"push_ignore_value_declare3");
	test(! check_ignorable_value_declare(pos, symbol),
			"push_ignore_value_declare4");

	RETURN;
}

static int test_push_ignorable_value_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_ignorable_value_declare_heap(pos, symbol);
	test(! check_ignore_value_declare(pos, symbol),
			"push_ignorable_value_declare1");
	test(check_ignorable_value_declare(pos, symbol),
			"push_ignorable_value_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(! check_ignore_value_declare(pos, symbol),
			"push_ignorable_value_declare3");
	test(! check_ignorable_value_declare(pos, symbol),
			"push_ignorable_value_declare4");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_ignore_value_declare_heap(pos, symbol);
	test(check_ignore_value_declare(pos, symbol),
			"push_ignorable_value_declare5");

	RETURN;
}

static int test_push_ignore_function_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_ignore_function_declare_heap(pos, symbol);
	test(check_ignore_function_declare(pos, symbol),
			"push_ignore_function_declare1");
	test(! check_ignorable_function_declare(pos, symbol),
			"push_ignore_function_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_ignore_function_declare(pos, symbol),
			"push_ignore_function_declare3");
	test(! check_ignorable_function_declare(pos, symbol),
			"push_ignore_function_declare4");

	RETURN;
}

static int test_push_ignorable_function_declare_heap(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_ignorable_function_declare_heap(pos, symbol);
	test(! check_ignore_function_declare(pos, symbol),
			"push_ignorable_function_declare1");
	test(check_ignorable_function_declare(pos, symbol),
			"push_ignorable_function_declare2");
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_ignore_function_declare(pos, symbol),
			"push_ignorable_function_declare3");
	test(! check_ignorable_function_declare(pos, symbol),
			"push_ignorable_function_declare4");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_ignore_function_declare_heap(pos, symbol);
	test(check_ignore_function_declare(pos, symbol),
			"push_ignorable_function_declare5");

	RETURN;
}

static int test_push_constant_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(check != Nil, "push_constant_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_constant_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_constant_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(check_constant_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_constant_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(check_constant_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_constant_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	test(! check_constant_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_constant_declare3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(! check_constant_declare(pos, symbol, EVAL_DECLARE_DYNAMIC_VALUE),
			"check_constant_declare4");

	RETURN;
}

static int test_push_callname_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_callname_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(check != Nil, "push_callname_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	push_callname_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_callname_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_callname_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_callname_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(check_callname_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_callname_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(check_callname_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_callname_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_callname_declare(pos, symbol, EVAL_DECLARE_SPECIAL),
			"check_callname_declare3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_callname_declare(pos, symbol, EVAL_DECLARE_DYNAMIC_VALUE),
			"check_callname_declare4");

	RETURN;
}

static int test_push_special_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_special_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(check != Nil, "push_special_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_special_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_special_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_special_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &check);
	test(length_list_unsafe(check) == 2, "push_special_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(check_special_declare(pos, symbol), "check_special_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(check_special_declare(pos, symbol), "check_special_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	test(! check_special_declare(pos, symbol), "check_special_declare3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(! check_dynamic_value_declare(pos, symbol), "check_special_declare4");

	RETURN;
}

static int test_push_dynamic_value_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_VALUE, &check);
	test(check != Nil, "push_dynamic_value_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_VALUE, &check);
	test(length_list_unsafe(check) == 2, "push_dynamic_value_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_VALUE, &check);
	test(length_list_unsafe(check) == 2, "push_dynamic_value_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(check_dynamic_value_declare(pos, symbol), "check_dynamic_value_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(check_dynamic_value_declare(pos, symbol), "check_dynamic_value_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	test(! check_dynamic_value_declare(pos, symbol), "check_dynamic_value_declare3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(! check_special_declare(pos, symbol), "check_dynamic_value_declare4");

	RETURN;
}

static int test_push_dynamic_function_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_FUNCTION, &check);
	test(check != Nil, "push_dynamic_function_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_FUNCTION, &check);
	test(length_list_unsafe(check) == 2, "push_dynamic_function_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_FUNCTION, &check);
	test(length_list_unsafe(check) == 2, "push_dynamic_function_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(check_dynamic_function_declare(pos, symbol),
			"check_dynamic_function_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(check_dynamic_function_declare(pos, symbol),
			"check_dynamic_function_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	parse_callname_error(&symbol, symbol);
	test(! check_dynamic_function_declare(pos, symbol),
			"check_dynamic_function_declare3");

	RETURN;
}

static int test_push_declaration_declare_heap(void)
{
	addr pos, symbol, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_declaration_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &check);
	test(check != Nil, "push_declaration_declare_heap1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_declaration_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &check);
	test(length_list_unsafe(check) == 2, "push_declaration_declare_heap2");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_declaration_declare_heap(pos, symbol);
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &check);
	test(length_list_unsafe(check) == 2, "push_declaration_declare_heap3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(check_declaration_declare(pos, symbol), "check_declaration_declare1");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(check_declaration_declare(pos, symbol), "check_declaration_declare2");

	internchar(LISP_SYSTEM, "CCC", &symbol);
	test(! check_declaration_declare(pos, symbol), "check_declaration_declare3");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(! check_special_declare(pos, symbol), "check_declaration_declare4");

	RETURN;
}


/*
 *  getall
 */
static int test_getall_declaration_declare(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_declaration_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_declaration_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "CCC", &symbol);
	push_declaration_declare_heap(pos, symbol);
	getall_declaration_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "getall_declaration_declare1");
	test(find_list_eq_unsafe(symbol, pos), "getall_declaration_declare2");

	RETURN;
}

static int test_getall_inline_declare(void)
{
	addr pos, symbol, check, key;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_inline_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	push_notinline_declare_heap(pos, symbol);

	getall_inline_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "getall_inline_declare1");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "getall_inline_declare2");
	GetConstant(CONSTANT_COMMON_INLINE, &key);
	test(check == key, "getall_inline_declare3");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "getall_inline_declare4");
	GetConstant(CONSTANT_COMMON_NOTINLINE, &key);
	test(check == key, "getall_inline_declare5");

	RETURN;
}

static int test_getall_special_declare(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_special_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_special_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "CCC", &symbol);
	push_special_declare_heap(pos, symbol);
	getall_special_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "getall_special_declare1");
	test(find_list_eq_unsafe(symbol, pos), "getall_special_declare2");

	RETURN;
}

static int test_getall_type_value_declare(void)
{
	addr pos, symbol, type, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	GetTypeTable(&type, Atom);
	push_type_declare_heap(pos, symbol, type);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	GetTypeTable(&type, List);
	push_type_declare_heap(pos, symbol, type);

	getall_type_value_declare(pos, &pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(getplist(pos, symbol, &check) == 0, "getall_type_value_declare1");
	test(RefLispDecl(check) == LISPDECL_ATOM, "getall_type_value_declare2");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(getplist(pos, symbol, &check) == 0, "getall_type_value_declare3");
	test(RefLispDecl(check) == LISPDECL_LIST, "getall_type_value_declare4");

	RETURN;
}

static int test_getall_type_function_declare(void)
{
	addr pos, symbol, type, check;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	GetTypeTable(&type, Atom);
	push_ftype_declare_heap(pos, symbol, type);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	GetTypeTable(&type, List);
	push_ftype_declare_heap(pos, symbol, type);

	getall_type_function_declare(pos, &pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "getall_type_function_declare1");
	test(RefLispDecl(check) == LISPDECL_ATOM, "getall_type_function_declare2");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "getall_type_function_declare3");
	test(RefLispDecl(check) == LISPDECL_LIST, "getall_type_function_declare4");

	RETURN;
}

static int test_getall_dynamic_value_declare(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "CCC", &symbol);
	push_dynamic_value_declare_heap(pos, symbol);
	getall_dynamic_value_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "getall_dynamic_value_declare1");
	test(find_list_eq_unsafe(symbol, pos), "getall_dynamic_value_declare2");

	RETURN;
}

static int test_getall_dynamic_function_declare(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "CCC", &symbol);
	parse_callname_error(&symbol, symbol);
	push_dynamic_function_declare_heap(pos, symbol);
	getall_dynamic_function_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "getall_dynamic_function_declare1");
	test(find_list_eq_unsafe(symbol, pos), "getall_dynamic_function_declare2");

	RETURN;
}

static int test_getall_ignore_value_declare(void)
{
	addr pos, symbol, check, key;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	push_ignore_value_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	push_ignorable_value_declare_heap(pos, symbol);

	getall_ignore_value_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "getall_ignore_value_declare1");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	test(getplist(pos, symbol, &check) == 0, "getall_ignore_value_declare2");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "getall_ignore_value_declare3");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	test(getplist(pos, symbol, &check) == 0, "getall_ignore_value_declare4");
	GetConstant(CONSTANT_COMMON_IGNORABLE, &key);
	test(check == key, "getall_ignore_value_declare5");

	RETURN;
}

static int test_getall_ignore_function_declare(void)
{
	addr pos, symbol, check, key;

	eval_declare_heap(&pos);
	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	push_ignore_function_declare_heap(pos, symbol);
	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	push_ignorable_function_declare_heap(pos, symbol);

	getall_ignore_function_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "getall_ignore_function_declare1");

	internchar(LISP_SYSTEM, "AAA", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0,
			"getall_ignore_function_declare2");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "getall_ignore_function_declare3");

	internchar(LISP_SYSTEM, "BBB", &symbol);
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0,
			"getall_ignore_function_declare4");
	GetConstant(CONSTANT_COMMON_IGNORABLE, &key);
	test(check == key, "getall_ignore_function_declare5");

	RETURN;
}


/*
 *  build_declare
 */
static int test_getroot_declare(void)
{
	addr pos, check;

	getroot_declare(&pos);
	test(eval_declare_p(pos), "getroot_declare1");
	test(get_optimize_space_declare(pos) == 1, "getroot_declare2");

	eval_declare_heap(&pos);
	setroot_declare(pos);
	getroot_declare(&check);
	test(pos == check, "getroot_declare3");

	build_declare();

	RETURN;
}

static int test_build_declare(void)
{
	addr pos;

	build_declare();
	getroot_declare(&pos);
	test(get_optimize_space_declare(pos) == 1, "build_declare1");
	test(get_optimize_space_declare(pos) == 1, "build_declare2");

	RETURN;
}

static int test_copy_optimize_declare(void)
{
	addr pos;
	OptimizeType optimize[EVAL_OPTIMIZE_SIZE];

	build_declare();
	getroot_declare(&pos);
	set_optimize_space_declare(pos, 3);
	copy_optimize_declare(optimize);
	test(optimize[EVAL_OPTIMIZE_SPACE] == 3, "copy_optimize_declare1");
	test(optimize[EVAL_OPTIMIZE_SAFETY] == 1, "copy_optimize_declare2");

	RETURN;
}


/*
 *  parse-declaration
 */
static int test_check_variable(void)
{
	addr symbol;

	internchar(LISP_PACKAGE, "HELLO", &symbol);
	check_variable(symbol);
	test(1, "check_variable1");

	RETURN;
}

static int test_check_callname_heap(void)
{
	addr symbol, pos, check;

	internchar(LISP_PACKAGE, "HELLO", &symbol);
	check_callname_heap(&pos, symbol);
	test(GetType(pos) == LISPTYPE_CALLNAME, "check_callname_heap1");
	GetCallName(pos, &check);
	test(check == symbol, "check_callname_heap2");

	RETURN;
}

static int test_decl_type(void)
{
	int result;
	addr pos, key, key1, key2, cons, check;

	eval_declare_heap(&pos);
	internchar(LISP_COMMON, "INTEGER", &key);
	internchar(LISP_PACKAGE, "BBB", &key1);
	internchar(LISP_PACKAGE, "CCC", &key2);
	list_heap(&cons, key, key1, key2, NULL);
	result = decl_type(Execute_Thread, Nil,  pos, cons);
	test(result == 0, "decl_type1");
	getall_type_value_declare(pos, &cons);
	test(length_list_unsafe(cons) == 4, "decl_type2");
	test(getplist(cons, key1, &check) == 0, "decl_type3");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_type4");
	test(getplist(cons, key2, &check) == 0, "decl_type5");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_type6");

	RETURN;
}

static int test_decl_ftype(void)
{
	int result;
	addr pos, key, key1, key2, cons, check, call1, call2;

	eval_declare_heap(&pos);
	internchar(LISP_COMMON, "INTEGER", &key);
	internchar(LISP_PACKAGE, "BBB", &key1);
	internchar(LISP_PACKAGE, "CCC", &key2);
	parse_callname_error(&call1, key1);
	parse_callname_error(&call2, key2);
	list_heap(&cons, key, key1, key2, NULL);
	result = decl_ftype(Execute_Thread, Nil, pos, cons);
	getall_type_function_declare(pos, &cons);
	test(result == 0, "decl_ftype1");
	test(length_list_unsafe(cons) == 4, "decl_ftype2");
	test(getplist_callname(cons, call1, &check) == 0, "decl_ftype3");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_ftype4");
	test(getplist_callname(cons, call2, &check) == 0, "decl_ftype5");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_ftype6");

	RETURN;
}

static int test_decl_special(void)
{
	addr pos, key1, key2, cons;

	eval_declare_heap(&pos);
	internchar(LISP_PACKAGE, "AAA", &key1);
	internchar(LISP_PACKAGE, "BBB", &key2);
	list_heap(&cons, key1, key2, key2, NULL);
	decl_special(pos, cons);
	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "decl_special1");
	test(find_list_eq_unsafe(key1, cons), "decl_special2");
	test(find_list_eq_unsafe(key2, cons), "decl_special3");

	RETURN;
}

static int test_decl_inline(void)
{
	addr pos, key1, key2, key3, call1, call2, call3, cons, check, key;

	eval_declare_heap(&pos);
	internchar(LISP_PACKAGE, "AAA", &key1);
	internchar(LISP_PACKAGE, "BBB", &key2);
	internchar(LISP_PACKAGE, "CCC", &key3);
	parse_callname_error(&call1, key1);
	parse_callname_error(&call2, key2);
	parse_callname_error(&call3, key3);
	list_heap(&cons, key1, key2, key2, NULL);
	decl_inline(pos, cons);
	getall_inline_declare(pos, &cons);
	test(length_list_unsafe(cons) == 4, "decl_inline1");
	GetConstant(CONSTANT_COMMON_INLINE, &key);
	test(getplist_callname(cons, call1, &check) == 0, "decl_inline2");
	test(key == check, "decl_inline3");
	test(getplist_callname(cons, call2, &check) == 0, "decl_inline4");
	test(key == check, "decl_inline5");

	list_heap(&cons, key2, key3, NULL);
	decl_notinline(pos, cons);
	getall_inline_declare(pos, &cons);
	test(length_list_unsafe(cons) == 6, "decl_notinline1");
	GetConstant(CONSTANT_COMMON_INLINE, &key);
	test(getplist_callname(cons, call1, &check) == 0, "decl_notinline2");
	test(key == check, "decl_notinline3");
	GetConstant(CONSTANT_COMMON_NOTINLINE, &key);
	test(getplist_callname(cons, call2, &check) == 0, "decl_notinline4");
	test(key == check, "decl_notinline5");
	test(getplist_callname(cons, call3, &check) == 0, "decl_notinline6");
	test(key == check, "decl_notinline7");

	RETURN;
}

static int test_decl_declaration(void)
{
	addr pos, key1, key2, cons;

	eval_declare_heap(&pos);
	internchar(LISP_PACKAGE, "AAA", &key1);
	internchar(LISP_PACKAGE, "BBB", &key2);
	list_heap(&cons, key1, key2, key2, NULL);
	decl_declaration(pos, cons);
	getall_declaration_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "decl_declaration1");
	test(find_list_eq_unsafe(key1, cons), "decl_declaration2");
	test(find_list_eq_unsafe(key2, cons), "decl_declaration3");

	RETURN;
}

static int test_function_callname_p(void)
{
	addr cons, type, symbol;

	internchar(LISP_COMMON, "FUNCTION", &type);
	internchar(LISP_PACKAGE, "VARIABLE", &symbol);
	list_heap(&cons, type, symbol, NULL);
	test(function_callname_p(&cons, cons), "function_callname_p1");
	test(GetType(cons) == LISPTYPE_CALLNAME, "function_callname_p2");
	GetCallName(cons, &cons);
	test(symbol == cons, "function_callname_p3");

	test(! function_callname_p(&cons, Nil), "function_callname_p4");
	consnil_heap(&cons);
	test(! function_callname_p(&cons, cons), "function_callname_p5");
	conscar_heap(&cons, type);
	test(! function_callname_p(&cons, cons), "function_callname_p6");
	cons_heap(&cons, symbol, T);
	cons_heap(&cons, type, cons);
	test(! function_callname_p(&cons, cons), "function_callname_p7");
	list_heap(&cons, symbol, symbol, NULL);
	test(! function_callname_p(&cons, cons), "function_callname_p8");

	RETURN;
}

static int test_decl_ignore(void)
{
	addr pos, cons, check, key;

	eval_declare_heap(&pos);
	readstring(&cons, "(aaa bbb (function ccc))");
	decl_ignore(pos, cons);
	getall_ignore_value_declare(pos, &cons);
	test(length_list_unsafe(cons) == 4, "decl_ignore1");

	readstring(&check, "aaa");
	test(getplist(cons, check, &check) == 0, "decl_ignore2");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "decl_ignore3");
	readstring(&check, "bbb");
	test(getplist(cons, check, &check) == 0, "decl_ignore4");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "decl_ignore5");
	readstring(&check, "ccc");
	test(getplist(cons, check, &check), "decl_ignore6");

	getall_ignore_function_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "decl_ignore7");
	readstring(&check, "ccc");
	parse_callname_error(&check, check);
	test(getplist_callname(cons, check, &check) == 0, "decl_ignore8");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "decl_ignore9");

	readstring(&cons, "((function (setf eee)) aaa ddd)");
	decl_ignorable(pos, cons);
	getall_ignore_value_declare(pos, &cons);
	test(length_list_unsafe(cons) == 6, "decl_ignorable1");

	readstring(&check, "aaa");
	test(getplist(cons, check, &check) == 0, "decl_ignorable2");
	GetConstant(CONSTANT_COMMON_IGNORABLE, &key);
	test(check == key, "decl_ignorable3");
	readstring(&check, "bbb");
	test(getplist(cons, check, &check) == 0, "decl_ignorable4");
	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	test(check == key, "decl_ignorable5");
	readstring(&check, "ddd");
	test(getplist(cons, check, &check) == 0, "decl_ignorable6");
	GetConstant(CONSTANT_COMMON_IGNORABLE, &key);
	test(check == key, "decl_ignorable7");

	getall_ignore_function_declare(pos, &cons);
	test(length_list_unsafe(cons) == 4, "decl_ignorable8");
	readstring(&check, "(setf eee)");
	parse_callname_error(&check, check);
	test(getplist_callname(cons, check, &check) == 0, "decl_ignorable9");
	GetConstant(CONSTANT_COMMON_IGNORABLE, &key);
	test(check == key, "decl_ignorable10");

	RETURN;
}

static int test_decl_dynamic_extent(void)
{
	addr pos, cons, check;

	eval_declare_heap(&pos);
	readstring(&cons, "(aaa bbb (function ccc))");
	decl_dynamic_extent(pos, cons);
	getall_dynamic_value_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "decl_dynamic_extent1");
	readstring(&check, "aaa");
	test(find_list_eq_unsafe(check, cons), "decl_dynamic_extent2");
	readstring(&check, "bbb");
	test(find_list_eq_unsafe(check, cons), "decl_dynamic_extent3");

	getall_dynamic_function_declare(pos, &cons);
	test(length_list_unsafe(cons) == 1, "decl_dynamic_extent4");
	readstring(&check, "ccc");
	parse_callname_error(&check, check);
	test(find_list_callname_unsafe(check, cons), "decl_dynamic_extent5");

	RETURN;
}

static int test_check_optimize_symbol(void)
{
	addr check;

	readstring(&check, "compilation-speed");
	test(check_optimize_symbol(check) == EVAL_OPTIMIZE_COMPILATION,
			"check_optimize_symbol1");
	readstring(&check, "debug");
	test(check_optimize_symbol(check) == EVAL_OPTIMIZE_DEBUG,
			"check_optimize_symbol2");
	readstring(&check, "speed");
	test(check_optimize_symbol(check) == EVAL_OPTIMIZE_SPEED,
			"check_optimize_symbol3");
	readstring(&check, "safety");
	test(check_optimize_symbol(check) == EVAL_OPTIMIZE_SAFETY,
			"check_optimize_symbol4");
	readstring(&check, "space");
	test(check_optimize_symbol(check) == EVAL_OPTIMIZE_SPACE,
			"check_optimize_symbol5");

	RETURN;
}

static int test_decl_optimize_symbol(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	internchar(LISP_COMMON, "COMPILATION-SPEED", &symbol);
	decl_optimize_symbol(pos, symbol);
	test(get_optimize_compilation_declare(pos) == 3, "decl_optimize_symbol1");

	RETURN;
}

static int test_decl_optimize_cons(void)
{
	addr pos, cons;

	eval_declare_heap(&pos);
	readstring(&cons, "(speed)");
	decl_optimize_cons(pos, cons);
	test(get_optimize_speed_declare(pos) == 3, "decl_optimize_cons1");

	readstring(&cons, "(safety 1)");
	decl_optimize_cons(pos, cons);
	test(get_optimize_safety_declare(pos) == 1, "decl_optimize_cons2");

	RETURN;
}

static int test_decl_optimize(void)
{
	addr pos, cons;

	eval_declare_heap(&pos);
	readstring(&cons, "(speed)");
	decl_optimize(pos, cons);
	test(get_optimize_speed_declare(pos) == 3, "decl_optimize1");

	readstring(&cons, "((safety 1))");
	decl_optimize(pos, cons);
	test(get_optimize_safety_declare(pos) == 1, "decl_optimize2");

	readstring(&cons, "(space (debug 3) (compilation-speed 2))");
	decl_optimize(pos, cons);
	test(get_optimize_space_declare(pos) == 3, "decl_optimize3");
	test(get_optimize_debug_declare(pos) == 3, "decl_optimize4");
	test(get_optimize_compilation_declare(pos) == 2, "decl_optimize5");

	RETURN;
}

static int test_declaration_p(void)
{
	addr pos, symbol;

	eval_declare_heap(&pos);
	setroot_declare(pos);
	eval_declare_heap(&pos);
	readstring(&symbol, "aaa");
	test(! declaration_p(pos, symbol), "declaration_p1");

	push_declaration_declare_heap(pos, symbol);
	test(declaration_p(pos, symbol), "declaration_p2");

	setroot_declare(pos);
	eval_declare_heap(&pos);
	test(declaration_p(pos, symbol), "declaration_p3");

	readstring(&symbol, "bbb");
	push_declaration_declare_heap(pos, symbol);

	readstring(&symbol, "aaa");
	test(declaration_p(pos, symbol), "declaration_p4");
	readstring(&symbol, "bbb");
	test(declaration_p(pos, symbol), "declaration_p5");

	/* free */
	eval_declare_heap(&pos);
	setroot_declare(pos);

	RETURN;
}

static int test_decl_otherwise(void)
{
	int result;
	addr pos, symbol, cons, check;

	eval_declare_heap(&pos);
	setroot_declare(pos);
	eval_declare_heap(&pos);

	readstring(&symbol, "aaa");
	push_declaration_declare_heap(pos, symbol);
	result = decl_otherwise(Execute_Thread, Nil, pos, symbol, Nil);
	test(result == 0, "decl_otherwise1");

	readstring(&symbol, "integer");
	readstring(&cons, "(aaa bbb)");
	result = decl_otherwise(Execute_Thread, Nil, pos, symbol, cons);
	test(result == 0, "decl_otherwise2");
	getall_type_value_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "decl_otherwise3");
	readstring(&symbol, "aaa");
	test(getplist(pos, symbol, &check) == 0, "decl_otherwise4");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_otherwise5");
	readstring(&symbol, "bbb");
	test(getplist(pos, symbol, &check) == 0, "decl_otherwise6");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "decl_otherwise7");

	RETURN;
}

static int test_push_declaim(void)
{
	int result;
	addr pos, symbol, cons;

	eval_declare_heap(&pos);
	readstring(&symbol, "special");
	readstring(&cons, "(aaa bbb)");
	result = push_declaim(Execute_Thread, Nil, pos, symbol, cons);
	test(result == 0, "push_declaim1");
	getall_special_declare(pos, &pos);
	test(length_list_unsafe(pos) == 2, "push_declaim2");

	RETURN;
}

static int test_push_declare(void)
{
	int result;
	addr pos, symbol, cons;

	eval_declare_heap(&pos);
	readstring(&symbol, "special");
	readstring(&cons, "(aaa bbb)");
	result = push_declare(Execute_Thread, Nil, pos, symbol, cons);
	test(result == 0, "push_declare1");
	getall_special_declare(pos, &pos);
	test(length_list_unsafe(pos) == 2, "push_declare2");

	RETURN;
}

static int test_parse_declare_form(void)
{
	int result;
	addr pos, cons;

	readstring(&cons, "((special aaa bbb) (inline ccc))");
	pos = NULL;
	result = parse_declare_form(Execute_Thread, Nil, cons, &pos, push_declare);
	test(result == 0, "parse_declare_form1");
	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "parse_declare_form2");
	getall_inline_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "parse_declare_form3");

	RETURN;
}

static int test_parse_declaim_heap(void)
{
	int result;
	addr pos, cons;

	readstring(&cons,
			"((type integer aaa) (ftype function bbb)"
			"(special ccc ddd) (inline eee) (notinline fff)"
			"(declaration hello) (optimize speed)"
			"(hello zzz) (integer xxx))");
	pos = NULL;
	result = parse_declaim_heap(Execute_Thread, Nil, cons, &pos);
	test(result == 0, "parse_declaim_heap1");
	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "parse_declaim_heap2");

	RETURN;
}

static int test_parse_declare_heap(void)
{
	int result;
	addr pos, cons;

	readstring(&cons,
			"((type integer aaa) (ftype function bbb)"
			"(special ccc ddd) (inline eee) (notinline fff)"
			"(ignore ggg) (ignorable hh ii)"
			"(dynamic-extent jj (function kk))"
			"(optimize speed)"
			"(integer xxx))");
	pos = NULL;
	result = parse_declare_heap(Execute_Thread, Nil, cons, &pos);
	test(result == 0, "parse_declare_heap1");
	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 2, "parse_declare_heap2");

	RETURN;
}


/*
 *  declare_body_documentation
 */
static int test_declare_split(void)
{
	addr cons, decl, body, check;

	readstring(&cons,
			"((declare (ignore hello) (special a))"
			" (declare (inline aaa))"
			" hello"
			" (cons 10 20))");
	declare_split(cons, &decl, &body);
	readstring(&check, "((ignore hello) (special a) (inline aaa))");
	test(equal_function(decl, check), "declare_split1");
	readstring(&check, "(hello (cons 10 20))");
	test(equal_function(body, check), "declare_split2");

	readstring(&cons, "((cons 10 20) hello)");
	declare_split(cons, &decl, &body);
	test(decl == Nil, "declare_split3");
	readstring(&check, "((cons 10 20) hello)");
	test(equal_function(body, check), "declare_split4");

	RETURN;
}

static int test_declare_body(void)
{
	int result;
	addr cons, decl, body, check;

	readstring(&cons,
			"((declare (ignore hello) (special a))"
			" (declare (inline aaa))"
			" hello"
			" (cons 10 20))");
	decl = NULL;
	result = declare_body(Execute_Thread, Nil, cons, &decl, &body);
	test(result == 0, "declare_body1");
	readstring(&check, "((ignore hello) (special a) (inline aaa))");
	test(GetType(decl) == LISPTYPE_EVAL, "declare_body2");
	readstring(&check, "(hello (cons 10 20))");
	test(equal_function(body, check), "declare_body3");

	readstring(&cons, "((cons 10 20) hello)");
	result = declare_body(Execute_Thread, Nil, cons, &decl, &body);
	test(result == 0, "declare_body4");
	test(decl == Nil, "declare_body5");
	readstring(&check, "((cons 10 20) hello)");
	test(equal_function(body, check), "declare_body6");

	RETURN;
}

static int test_declare_body_documentation(void)
{
	int result;
	addr cons, doc, decl, body;

	doc = decl = body = NULL;
	result = declare_body_documentation(Execute_Thread, Nil, Nil, &doc, &decl, &body);
	test(result == 0, "declare_body_documentation1");
	test(doc == Nil, "declare_body_documentation2");
	test(decl == Nil && body == Nil, "declare_body_documentation3");

	readstring(&cons, "(\"Hello\")");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc == Nil, "declare_body_documentation4");
	test(decl == Nil, "declare_body_documentation5");
	test(body != Nil, "declare_body_documentation6");

	readstring(&cons, "(\"Hello\" 100 200)");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(string_equal_char(doc, "Hello"), "declare_body_documentation7");
	test(decl == Nil, "declare_body_documentation8");
	test(body != Nil, "declare_body_documentation9");

	readstring(&cons, "((declare (ignore hello)))");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc == Nil, "declare_body_documentation10");
	test(decl != Nil, "declare_body_documentation11");
	test(body == Nil, "declare_body_documentation12");

	readstring(&cons, "((declare (ignore hello)) \"Hello\" 10 20 30)");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc != Nil, "declare_body_documentation13");
	test(decl != Nil, "declare_body_documentation14");
	test(body != Nil, "declare_body_documentation15");

	readstring(&cons, "((declare (ignore hello)) \"Hello\")");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc == Nil, "declare_body_documentation16");
	test(decl != Nil, "declare_body_documentation17");
	test(body != Nil, "declare_body_documentation18");

	readstring(&cons, "((declare (ignore hello)) 10 20 30)");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc == Nil, "declare_body_documentation19");
	test(decl != Nil, "declare_body_documentation20");
	test(body != Nil, "declare_body_documentation21");

	readstring(&cons, "(10 20 30)");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(doc == Nil, "declare_body_documentation22");
	test(decl == Nil, "declare_body_documentation23");
	test(body != Nil, "declare_body_documentation24");

	readstring(&cons, "(100)");
	doc = decl = body = NULL;
	declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(singlep(cons), "declare_body_documentation25-error");
	GetCar(cons, &cons);
	test(GetType(cons) == LISPTYPE_FIXNUM, "declare_body_documentation26-error");
	test(RefFixnum(cons) == 100, "declare_body_documentation27-error");

	RETURN;
}

static int test_declare_body_documentation_error(void)
{
	int result;
	addr cons, doc, decl, body;

	readstring(&cons, "((declare (optimize speed)) (progn 10))");
	doc = decl = body = NULL;
	result = declare_body_documentation(Execute_Thread, Nil, cons, &doc, &decl, &body);
	test(result == 0, "declare_body_documentation_error1");
	test(doc == Nil, "declare_body_documentation_error2");
	test(eval_declare_p(decl), "declare_body_documentation_error3");
	test(length_list_unsafe(body) == 1, "declare_body_documentation_error4");

	RETURN;
}


/*
 *  copy eval-declare
 */
static int test_copy_declare_type_v(void)
{
	addr cons, pos, source, symbol, check;

	readstring(&cons, "((type integer aaa) (type string bbb))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	eval_declare_heap(&pos);
	copy_declare_type_v(NULL, EVAL_DECLARE_TYPE_VALUE, source, pos);
	getall_type_value_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "copy_declare_type_v1");
	readstring(&symbol, "aaa");
	test(getplist(pos, symbol, &check) == 0, "copy_declare_type_v2");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "copy_declare_type_v3");
	readstring(&symbol, "bbb");
	test(getplist(pos, symbol, &check) == 0, "copy_declare_type_v4");
	test(RefLispDecl(check) == LISPDECL_STRING, "copy_declare_type_v5");

	RETURN;
}

static int test_copy_declare_type_f(void)
{
	addr cons, pos, source, symbol, check;

	readstring(&cons, "((ftype integer aaa) (ftype string bbb))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	eval_declare_heap(&pos);
	copy_declare_type_f(NULL, EVAL_DECLARE_TYPE_FUNCTION, source, pos);
	getall_type_function_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "copy_declare_type_f1");
	readstring(&symbol, "aaa");
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "copy_declare_type_f2");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "copy_declare_type_f3");
	readstring(&symbol, "bbb");
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "copy_declare_type_f4");
	test(RefLispDecl(check) == LISPDECL_STRING, "copy_declare_type_f5");

	RETURN;
}

static int test_copy_declare_push_v(void)
{
	addr cons, pos, source, symbol;

	readstring(&cons, "((special aaa bbb ccc))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	pos = NULL;
	eval_declare_heap(&pos);
	copy_declare_push_v(NULL, EVAL_DECLARE_SPECIAL, source, pos);
	getall_special_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "copy_declare_push_v1");
	readstring(&symbol, "aaa");
	test(find_list_eq_unsafe(symbol, pos), "copy_declare_push_v2");
	readstring(&symbol, "bbb");
	test(find_list_eq_unsafe(symbol, pos), "copy_declare_push_v3");
	readstring(&symbol, "ccc");
	test(find_list_eq_unsafe(symbol, pos), "copy_declare_push_v4");

	RETURN;
}

static int test_copy_declare_push_f(void)
{
	addr cons, pos, source, symbol;

	readstring(&cons, "((dynamic-extent #'aaa #'(setf bbb) #'ccc))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	eval_declare_heap(&pos);
	copy_declare_push_f(NULL, EVAL_DECLARE_DYNAMIC_FUNCTION, source, pos);
	getall_dynamic_function_declare(pos, &pos);
	test(length_list_unsafe(pos) == 3, "copy_declare_push_f1");
	readstring(&symbol, "aaa");
	parse_callname_error(&symbol, symbol);
	test(find_list_callname_unsafe(symbol, pos), "copy_declare_push_f2");
	readstring(&symbol, "(setf bbb)");
	parse_callname_error(&symbol, symbol);
	test(find_list_callname_unsafe(symbol, pos), "copy_declare_push_f3");
	readstring(&symbol, "ccc");
	parse_callname_error(&symbol, symbol);
	test(find_list_callname_unsafe(symbol, pos), "copy_declare_push_f4");

	RETURN;
}

static int test_copy_declare_plist_v(void)
{
	addr cons, pos, source, symbol, check, key;

	readstring(&cons, "((ignore aaa bbb))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	eval_declare_heap(&pos);
	copy_declare_plist_v(NULL, EVAL_DECLARE_IGNORE_VALUE, source, pos);

	GetConstant(CONSTANT_COMMON_IGNORE, &key);
	getall_ignore_value_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "copy_declare_plist_v1");
	readstring(&symbol, "aaa");
	test(getplist(pos, symbol, &check) == 0, "copy_declare_plist_v2");
	test(check == key, "copy_declare_plist_v3");
	readstring(&symbol, "bbb");
	test(getplist(pos, symbol, &check) == 0, "copy_declare_plist_v4");
	test(check == key, "copy_declare_plist_v5");

	RETURN;
}

static int test_copy_declare_plist_f(void)
{
	addr cons, pos, source, symbol, check, key;

	readstring(&cons, "((inline aaa bbb))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	eval_declare_heap(&pos);
	copy_declare_plist_f(NULL, EVAL_DECLARE_INLINE, source, pos);

	GetConstant(CONSTANT_COMMON_INLINE, &key);
	getall_inline_declare(pos, &pos);
	test(length_list_unsafe(pos) == 4, "copy_declare_plist_f1");
	readstring(&symbol, "aaa");
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "copy_declare_plist_f2");
	test(check == key, "copy_declare_plist_f3");
	readstring(&symbol, "bbb");
	parse_callname_error(&symbol, symbol);
	test(getplist_callname(pos, symbol, &check) == 0, "copy_declare_plist_f4");
	test(check == key, "copy_declare_plist_f5");

	RETURN;
}

static int test_copy_eval_declare_alloc(void)
{
	addr cons, pos, source, symbol;

	readstring(&cons, "((special aaa) (inline bbb) (optimize speed))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	copy_eval_declare_alloc(NULL, &pos, source);
	test(get_optimize_speed_declare(pos) == 3, "copy_eval_declare_alloc1");

	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 1, "copy_eval_declare_alloc2");
	readstring(&symbol, "aaa");
	test(find_list_eq_unsafe(symbol, cons), "copy_eval_declare_alloc3");

	RETURN;
}

static int test_copy_eval_declare_local(void)
{
	addr cons, pos, source, symbol;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	readstring(&cons, "((special aaa) (inline bbb) (optimize speed))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	copy_eval_declare_local(local, &pos, source);
	test(get_optimize_speed_declare(pos) == 3, "copy_eval_declare_local1");

	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 1, "copy_eval_declare_local2");
	readstring(&symbol, "aaa");
	test(find_list_eq_unsafe(symbol, cons), "copy_eval_declare_local3");
	rollback_local(local, stack);

	RETURN;
}

static int test_copy_eval_declare_heap(void)
{
	addr cons, pos, source, symbol;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	readstring(&cons, "((special aaa) (inline bbb) (optimize speed))");
	source = NULL;
	parse_declare_heap(Execute_Thread, Nil, cons, &source);
	copy_eval_declare_heap(&pos, source);
	test(get_optimize_speed_declare(pos) == 3, "copy_eval_declare_heap1");

	getall_special_declare(pos, &cons);
	test(length_list_unsafe(cons) == 1, "copy_eval_declare_heap2");
	readstring(&symbol, "aaa");
	test(find_list_eq_unsafe(symbol, cons), "copy_eval_declare_heap3");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  main
 */
static int testbreak_declare(void)
{
	/* declarations */
	TestBreak(test_make_eval_declare);
	TestBreak(test_eval_declare_alloc_optimize);
	TestBreak(test_eval_declare_alloc);
	TestBreak(test_empty_declare);
	TestBreak(test_empty_nil_declare);
	TestBreak(test_apply_array_declare);
	TestBreak(test_RefEvalDeclare);
	TestBreak(test_RefEvalDeclareOptimize);
	/* access */
	TestBreak(test_set_type_declare_heap);
	TestBreak(test_push_type_declare_heap);
	TestBreak(test_set_ftype_declare_heap);
	TestBreak(test_push_ftype_declare_heap);
	TestBreak(test_plist_constant_declare_heap);
	TestBreak(test_plist_callname_declare_heap);
	TestBreak(test_push_inline_declare_heap);
	TestBreak(test_push_notinline_declare_heap);
	TestBreak(test_push_ignore_value_declare_heap);
	TestBreak(test_push_ignorable_value_declare_heap);
	TestBreak(test_push_ignore_function_declare_heap);
	TestBreak(test_push_ignorable_function_declare_heap);
	TestBreak(test_push_constant_declare_heap);
	TestBreak(test_push_callname_declare_heap);
	TestBreak(test_push_special_declare_heap);
	TestBreak(test_push_dynamic_value_declare_heap);
	TestBreak(test_push_dynamic_function_declare_heap);
	TestBreak(test_push_declaration_declare_heap);
	/* getall */
	TestBreak(test_getall_declaration_declare);
	TestBreak(test_getall_inline_declare);
	TestBreak(test_getall_special_declare);
	TestBreak(test_getall_type_value_declare);
	TestBreak(test_getall_type_function_declare);
	TestBreak(test_getall_dynamic_value_declare);
	TestBreak(test_getall_dynamic_function_declare);
	TestBreak(test_getall_ignore_value_declare);
	TestBreak(test_getall_ignore_function_declare);
	/* build_declare */
	TestBreak(test_getroot_declare);
	TestBreak(test_build_declare);
	TestBreak(test_copy_optimize_declare);
	/* parse-declaration */
	TestBreak(test_check_variable);
	TestBreak(test_check_callname_heap);
	TestBreak(test_decl_type);
	TestBreak(test_decl_ftype);
	TestBreak(test_decl_special);
	TestBreak(test_decl_inline);
	TestBreak(test_decl_declaration);
	TestBreak(test_function_callname_p);
	TestBreak(test_decl_ignore);
	TestBreak(test_decl_dynamic_extent);
	TestBreak(test_check_optimize_symbol);
	TestBreak(test_decl_optimize_symbol);
	TestBreak(test_decl_optimize_cons);
	TestBreak(test_decl_optimize);
	TestBreak(test_declaration_p);
	TestBreak(test_decl_otherwise);
	TestBreak(test_push_declaim);
	TestBreak(test_push_declare);
	TestBreak(test_parse_declare_form);
	TestBreak(test_parse_declaim_heap);
	TestBreak(test_parse_declare_heap);
	/* declare_body_documentation */
	TestBreak(test_declare_split);
	TestBreak(test_declare_body);
	TestBreak(test_declare_body_documentation);
	TestBreak(test_declare_body_documentation_error);
	/* copy eval-declare */
	TestBreak(test_copy_declare_type_v);
	TestBreak(test_copy_declare_type_f);
	TestBreak(test_copy_declare_push_v);
	TestBreak(test_copy_declare_push_f);
	TestBreak(test_copy_declare_plist_v);
	TestBreak(test_copy_declare_plist_f);
	TestBreak(test_copy_eval_declare_alloc);
	TestBreak(test_copy_eval_declare_local);
	TestBreak(test_copy_eval_declare_heap);

	return 0;
}

int test_declare(void)
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
		build_reader();
		build_pathname();
		build_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_declare();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

