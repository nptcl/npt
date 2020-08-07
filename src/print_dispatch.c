#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "equal.h"
#include "execute.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "print.h"
#include "print_dispatch.h"
#include "real_equal.h"
#include "symbol.h"
#include "type_table.h"
#include "type_typep.h"

/*
 *  access
 */
_g void getlistprintdispatch(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	GetListPrintDispatch_Low(pos, ret);
}

_g void setlistprintdispatch(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetListPrintDispatch_Low(pos, value);
}

_g void gettypeprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetTypePrintTable_Low(pos, ret);
}

_g void settypeprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypePrintTable_Low(pos, value);
}

_g void getspecifierprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetSpecifierPrintTable_Low(pos, ret);
}

_g void setspecifierprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSpecifierPrintTable_Low(pos, value);
}

_g void getfunctionprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetFunctionPrintTable_Low(pos, ret);
}

_g void setfunctionprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionPrintTable_Low(pos, value);
}

_g void getpriorityprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetPriorityPrintTable_Low(pos, ret);
}

_g void setpriorityprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetPriorityPrintTable_Low(pos, value);
}


/*
 *  object
 */
_g int print_dispatch_p(addr pos)
{
	return GetType(pos) == LISPTYPE_PRINT_DISPATCH;
}

_g void pprint_dispatch_heap(addr *ret)
{
	heap_array2(ret, LISPTYPE_PRINT_DISPATCH, PrintDispatch_size);
}

static void print_table_heap(addr *ret)
{
	heap_array2(ret, LISPSYSTEM_PRINT_TABLE, PrintTable_size);
}

static void copy_print_table(addr *ret, addr var)
{
	addr x, pos;

	print_table_heap(&x);
	GetTypePrintTable(var, &pos);
	SetTypePrintTable(x, pos);
	GetSpecifierPrintTable(var, &pos);
	SetSpecifierPrintTable(x, pos);
	GetFunctionPrintTable(var, &pos);
	SetFunctionPrintTable(x, pos);
	GetPriorityPrintTable(var, &pos);
	SetPriorityPrintTable(x, pos);
	*ret = x;
}

static void copy_pprint_dispatch(addr var, addr *ret)
{
	addr list, root, x;

	/* copy list */
	CheckType(var, LISPTYPE_PRINT_DISPATCH);
	GetListPrintDispatch(var, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &x, &list);
		copy_print_table(&x, x);
		cons_heap(&root, x, root);
	}
	nreverse(&root, root);

	/* make result */
	pprint_dispatch_heap(&x);
	SetListPrintDispatch(x, root);
	*ret = x;
}

static int find_print_dispatch_p_(LocalRoot local, addr a, addr b, int *ret)
{
	if (a == Nil)
		return Result(ret, 1);
	else
		return less_real_(local, a, b, ret);
}

static int find_print_dispatch(Execute ptr, addr var, addr list, addr *ret)
{
	int check;
	addr pos, type, value, a, b;

	GetListPrintDispatch(list, &list);
	a = value = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetTypePrintTable(pos, &type);
		Return(typep_clang(ptr, var, type, &check));
		if (check) {
			GetPriorityPrintTable(pos, &b);
			Return(find_print_dispatch_p_(ptr->local, a, b, &check));
			if (check) {
				a = b;
				value = pos;
			}
		}
	}

	return Result(ret, value);
}

_g int find_function_print_dispatch(Execute ptr, addr var, addr table, addr *ret)
{
	Return(find_print_dispatch(ptr, var, table, &var));
	if (var != Nil)
		GetFunctionPrintTable(var, &var);

	return Result(ret, var);
}

static int delete_print_dispatch_p_(LocalRoot local,
		addr pos, addr spec, addr priority, int *ret)
{
	int check;
	addr a;

	/* specifier */
	GetSpecifierPrintTable(pos, &a);
	Return(equal_function_(a, spec, &check));
	if (! check)
		return Result(ret, 0);
	/* priority */
	if (priority == Unbound)
		return Result(ret, 1);
	GetPriorityPrintTable(pos, &a);

	return equal_real_(local, a, priority, ret);
}

static int delete_print_dispatch_(LocalRoot local, addr spec, addr priority, addr table)
{
	int delp, check;
	addr root, list, pos;

	GetListPrintDispatch(table, &list);
	delp = 0;
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(delete_print_dispatch_p_(local, pos, spec, priority, &check));
		if (check) {
			delp = 1;
			continue;
		}
		cons_heap(&root, pos, root);
	}
	if (delp) {
		nreverse(&root, root);
		SetListPrintDispatch(table, root);
	}

	return 0;
}

static void set_print_dispatch(addr spec, addr type,
		addr call, addr priority, addr table)
{
	addr x, list;

	/* print-table */
	print_table_heap(&x);
	SetTypePrintTable(x, type);
	SetSpecifierPrintTable(x, spec);
	SetFunctionPrintTable(x, call);
	SetPriorityPrintTable(x, priority);
	/* print-dispatch */
	GetListPrintDispatch(table, &list);
	cons_heap(&list, x, list);
	SetListPrintDispatch(table, list);
}


/*
 *  common
 */
_g int copy_pprint_dispatch_common_(Execute ptr, addr var, addr *ret)
{
	if (var == Unbound || var == Nil) {
		Return(pprint_dispatch_print_(ptr, &var));
	}
	copy_pprint_dispatch(var, ret);
	return 0;
}

_g int pprint_dispatch_common_(Execute ptr, addr var, addr table, addr *x, addr *y)
{
	if (table == Unbound) {
		Return(pprint_dispatch_print_(ptr, &table));
	}
	CheckType(table, LISPTYPE_PRINT_DISPATCH);
	Return(find_print_dispatch(ptr, var, table, &var));
	if (var != Nil) {
		GetFunctionPrintTable(var, x);
		*y = T;
	}
	else {
		GetConst(SYSTEM_WRITE_DEFAULT, &var);
		Return(getfunction_global_(var, x));
		*y = Nil;
	}

	return 0;
}

_g int set_pprint_dispatch_print_(LocalRoot local,
		addr spec, addr type, addr call, addr priority, addr table)
{
	CheckType(type, LISPTYPE_TYPE);
	Check(! functionp(call), "type error: function");
	CheckType(table, LISPTYPE_PRINT_DISPATCH);

	Return(delete_print_dispatch_(local, spec, priority, table));
	if (call != Nil) {
		if (priority == Unbound)
			fixnum_heap(&priority, 0);
		Check(! integerp(priority), "type error: priority");
		set_print_dispatch(spec, type, call, priority, table);
	}

	return 0;
}


/*
 *  *default-print-dispatch*
 */
static void build_print_dispatch_empty(void)
{
	addr symbol, pos;

	/* system::*empty-print-dispatch* */
	pprint_dispatch_heap(&pos);
	GetConst(SYSTEM_EMPTY_PRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, pos);
}

static void build_print_dispatch_table(addr dispatch)
{
	addr symbol, pos;

	copy_pprint_dispatch(dispatch, &pos);
	/* system::*default-print-dispatch* */
	GetConst(SYSTEM_DEFAULT_PRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, dispatch);
	/* common-lisp::*print-pprint-dispatch* */
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &symbol);
	SetValueSymbol(symbol, pos);
}

static int build_print_dispatch_cons_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   'cons
	 *   type #'pprint-fill -10 dispatch)
	 */
	addr spec, type, call, priority;

	GetConst(COMMON_CONS, &spec);
	GetTypeTable(&type, Cons);
	GetConst(COMMON_PPRINT_FILL, &call);
	GetFunctionSymbol(call, &call);
	fixnum_heap(&priority, -10);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static void make_print_dispatch_function(addr *ret, constindex name, pointer id)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(name, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, id);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DispatchFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* result */
	*ret = pos;
}

static int build_print_dispatch_vector_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *  '(and vector (not string) (not bit-vector))
	 *  type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, type3;

	/* spec */
	GetConst(COMMON_VECTOR, &type1);
	GetConst(COMMON_NOT, &type);
	GetConst(COMMON_STRING, &type2);
	list_heap(&type2, type, type2, NULL);
	GetConst(COMMON_BIT_VECTOR, &type3);
	list_heap(&type3, type, type3, NULL);
	GetConst(COMMON_AND, &type);
	list_heap(&spec, type, type1, type2, type3, NULL);
	/* type */
	GetTypeTable(&type1, Vector);
	GetTypeTable(&type3, Asterisk);
	type1not_heap(LISPDECL_STRING, type3, &type2);
	type1not_heap(LISPDECL_BIT_VECTOR, type3, &type3);
	type3and_heap(type1, type2, type3, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_VECTOR,
			p_pprint_dispatch_vector);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_quote_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql quote) (cons t null))
	 *   type #<FUNCTION> -4 dispatch)
	 */
	addr spec, type, call, priority;
	addr cons, car, eql, quote, null;

	/* spec */
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_NULL, &null);
	list_heap(&spec, cons, T, null, NULL);
	list_heap(&car, eql, quote, NULL);
	list_heap(&spec, cons, car, spec, NULL);
	/* type */
	GetTypeTable(&type, T);
	GetTypeTable(&null, Null);
	type2_heap(LISPDECL_CONS, type, null, &type);
	type_eql_heap(quote, &car);
	type2_heap(LISPDECL_CONS, car, type, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_QUOTE,
			p_pprint_dispatch_quote);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_call_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (and symbol (satisfies fboundp)))
	 *   type #<FUNCTION> -5 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, type3, fboundp;

	/* spec */
	GetConst(COMMON_SATISFIES, &type3);
	GetConst(COMMON_FBOUNDP, &fboundp);
	list_heap(&type3, type3, fboundp, NULL);
	GetConst(COMMON_AND, &type1);
	GetConst(COMMON_SYMBOL, &type2);
	list_heap(&type2, type1, type2, type3, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_satisfies_heap(fboundp, &type3);
	GetTypeTable(&type2, Symbol);
	type2and_heap(type2, type3, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_CALL,
			p_pprint_dispatch_call);
	/* set */
	fixnum_heap(&priority, -5);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_defun_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql defun))
	 *   type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, defun;

	/* spec */
	GetConst(COMMON_EQL, &type1);
	GetConst(COMMON_DEFUN, &defun);
	list_heap(&type2, type1, defun, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_eql_heap(defun, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_DEFUN,
			p_pprint_dispatch_defun);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

static int build_print_dispatch_let_(LocalRoot local, addr dispatch)
{
	/* (system::set-pprint-dispatch
	 *   '(cons (eql let))
	 *   type #<FUNCTION> 0 dispatch)
	 */
	addr spec, type, call, priority;
	addr type1, type2, let;

	/* spec */
	GetConst(COMMON_EQL, &type1);
	GetConst(COMMON_LET, &let);
	list_heap(&type2, type1, let, NULL);
	GetConst(COMMON_CONS, &type1);
	list_heap(&spec, type1, type2, NULL);
	/* type */
	type_eql_heap(let, &type1);
	GetTypeTable(&type2, Asterisk);
	type2_heap(LISPDECL_CONS, type1, type2, &type);
	/* function */
	make_print_dispatch_function(&call,
			CONSTANT_SYSTEM_DISPATCH_LET,
			p_pprint_dispatch_let);
	/* set */
	fixnum_heap(&priority, 0);
	return set_pprint_dispatch_print_(local, spec, type, call, priority, dispatch);
}

_g int build_print_dispatch_(void)
{
	LocalRoot local;
	addr dispatch;

	local = Local_Thread;
	pprint_dispatch_heap(&dispatch);
	/* build */
	Return(build_print_dispatch_cons_(local, dispatch));
	Return(build_print_dispatch_vector_(local, dispatch));
	Return(build_print_dispatch_quote_(local, dispatch));
	Return(build_print_dispatch_call_(local, dispatch));
	Return(build_print_dispatch_defun_(local, dispatch));
	Return(build_print_dispatch_let_(local, dispatch));
	/* set variable */
	build_print_dispatch_empty();
	build_print_dispatch_table(dispatch);

	return 0;
}

