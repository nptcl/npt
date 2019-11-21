#include "cons.h"
#include "equal.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "print_dispatch.h"
#include "real.h"
#include "symbol.h"
#include "type_typep.h"

/*
 *  access
 */
void getlistprintdispatch(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	GetListPrintDispatch_Low(pos, ret);
}

void setlistprintdispatch(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PRINT_DISPATCH);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetListPrintDispatch_Low(pos, value);
}

void gettypeprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetTypePrintTable_Low(pos, ret);
}

void settypeprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypePrintTable_Low(pos, value);
}

void getspecifierprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetSpecifierPrintTable_Low(pos, ret);
}

void setspecifierprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSpecifierPrintTable_Low(pos, value);
}

void getfunctionprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetFunctionPrintTable_Low(pos, ret);
}

void setfunctionprinttable(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionPrintTable_Low(pos, value);
}

void getpriorityprinttable(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_TABLE);
	GetPriorityPrintTable_Low(pos, ret);
}

void setpriorityprinttable(addr pos, addr value)
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
	nreverse_list_unsafe(&root, root);

	/* make result */
	pprint_dispatch_heap(&x);
	SetListPrintDispatch(x, root);
	*ret = x;
}

static int find_print_dispatch(LocalRoot local, addr var, addr list, addr *ret)
{
	int check;
	addr pos, type, result, a, b;

	GetListPrintDispatch(list, &list);
	a = result = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetTypePrintTable(pos, &type);
		if (typep_clang(var, type, &check))
			return 1;
		if (check) {
			GetPriorityPrintTable(pos, &b);
			if (a == Nil || less_real_clang(local, a, b)) {
				a = b;
				result = pos;
			}
		}
	}
	*ret = result;

	return 0;
}

_g int find_function_print_dispatch(LocalRoot local, addr var, addr table, addr *ret)
{
	if (find_print_dispatch(local, var, table, &var))
		return 1;
	if (var != Nil)
		GetFunctionPrintTable(var, &var);
	*ret = var;

	return 0;
}

static int delete_print_dispatch_p(LocalRoot local, addr pos, addr spec, addr priority)
{
	addr a;

	/* specifier */
	GetSpecifierPrintTable(pos, &a);
	if (! equal_function(a, spec))
		return 0;
	/* priority */
	if (priority == Unbound)
		return 1;
	GetPriorityPrintTable(pos, &a);
	return equal_real(local, a, priority);
}

static void delete_print_dispatch(LocalRoot local, addr spec, addr priority, addr table)
{
	int delp;
	addr root, list, pos;

	GetListPrintDispatch(table, &list);
	delp = 0;
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (delete_print_dispatch_p(local, pos, spec, priority)) {
			delp = 1;
			continue;
		}
		cons_heap(&root, pos, root);
	}
	if (delp) {
		nreverse_list_unsafe(&root, root);
		SetListPrintDispatch(table, root);
	}
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
_g void copy_pprint_dispatch_common(addr var, addr *ret)
{
	if (var == Nil)
		pprint_dispatch_heap(ret);
	else
		copy_pprint_dispatch(var, ret);
}

_g int pprint_dispatch_common(Execute ptr, addr var, addr table, addr *x, addr *y)
{
	CheckType(table, LISPTYPE_PRINT_DISPATCH);
	if (find_print_dispatch(ptr->local, var, table, &var))
		return 1;
	if (var != Nil) {
		GetFunctionPrintTable(var, x);
		*y = T;
	}
	else {
		GetConst(SYSTEM_WRITE_DEFAULT, &var);
		getfunctioncheck_local(ptr, var, x);
		*y = Nil;
	}

	return 0;
}

_g void set_pprint_dispatch_common(LocalRoot local,
		addr spec, addr type, addr call, addr priority, addr table)
{
	CheckType(type, LISPTYPE_TYPE);
	Check(! functionp(call), "type error: function");
	CheckType(table, LISPTYPE_PRINT_DISPATCH);

	delete_print_dispatch(local, spec, priority, table);
	if (call != Nil) {
		if (priority == Unbound)
			fixnum_heap(&priority, 0);
		Check(! integerp(priority), "type error: priority");
		set_print_dispatch(spec, type, call, priority, table);
	}
}

