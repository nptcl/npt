#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "eval.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "eval_scope.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "object.h"
#include "sequence.h"
#include "symbol.h"
#include "type.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_value.h"

static int scope_eval(Execute ptr, addr *ret, addr eval);

/*
 *  memory
 */
static void eval_scope_heap(addr *ret, size_t size)
{
	Check(0xFF < sizeof(struct eval_scope), "struct size error");
	Check(0xFF < 2UL + size, "size argument error");
	eval_heap(ret, EVAL_TYPE_SCOPE,
			(byte)(2UL + size),
			(byte)sizeof(struct eval_scope));
}

static void eval_scope_size(addr *ret, size_t size,
		enum EVAL_PARSE parse, addr type, addr value)
{
	addr pos;

	eval_scope_heap(&pos, size);
	SetEvalScopeType(pos, parse);
	SetEvalScopeThe(pos, type);
	SetEvalScopeValue(pos, value);
	*ret = pos;
}

static void make_eval_scope(addr *ret, enum EVAL_PARSE parse, addr type, addr value)
{
	eval_scope_size(ret, 0, parse, type, value);
}


/*
 *  eval-scope
 */
struct eval_scope *structevalscope(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return StructEvalScope_Low(pos);

}
enum EVAL_PARSE refevalscopetype(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeType_Low(pos);
}
void getevalscopetype(addr pos, enum EVAL_PARSE *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeType_Low(pos, ret);
}
void setevalscopetype(addr pos, enum EVAL_PARSE value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeType_Low(pos, value);
}
addr refevalscopethe(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeThe_Low(pos);
}
void getevalscopethe(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeThe_Low(pos, ret);
}
void setevalscopethe(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeThe_Low(pos, value);
}
addr refevalscopevalue(addr pos)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeValue_Low(pos);
}
void getevalscopevalue(addr pos, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue_Low(pos, ret);
}
void setevalscopevalue(addr pos, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeValue_Low(pos, value);
}
addr refevalscopeindex(addr pos, size_t index)
{
	Check(! eval_scope_p(pos), "type error");
	return RefEvalScopeIndex_Low(pos, index);
}
void getevalscopeindex(addr pos, size_t index, addr *ret)
{
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeIndex_Low(pos, index, ret);
}
void setevalscopeindex(addr pos, size_t index, addr value)
{
	Check(! eval_scope_p(pos), "type error");
	SetEvalScopeIndex_Low(pos, index, value);
}


/*
 *  scope constant
 */
static void scope_nil(addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_nil(&eval);
	make_eval_scope(ret, EVAL_PARSE_NIL, eval, Nil);
}

static void scope_t(addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_t(&eval);
	make_eval_scope(ret, EVAL_PARSE_T, eval, T);
}

static void scope_integer(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_integer(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_INTEGER, type, eval);
}

static void scope_rational(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_rational(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_RATIONAL, type, eval);
}

static void scope_complex(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_complex(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_COMPLEX, type, eval);
}

static void scope_character(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_character(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_CHARACTER, type, eval);
}

static void scope_array(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_array(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_ARRAY, type, eval);
}

static void scope_vector(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_vector(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_VECTOR, type, eval);
}

static void scope_bitvector(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_bitvector(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_BITVECTOR, type, eval);
}

static void scope_string(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_string(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_STRING, type, eval);
}

static void scope_float(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_float(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_FLOAT, type, eval);
}

static void scope_quote(addr *ret, addr eval)
{
	addr value, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &value);
	type_value(&type, value);
	make_eval_scope(ret, EVAL_PARSE_QUOTE, type, value);
}

static void scope_pathname(addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_pathname(&type, eval);
	make_eval_scope(ret, EVAL_PARSE_PATHNAME, type, eval);
}

static int scope_allcons(Execute ptr, addr *retcons, addr *rettype, addr cons)
{
	addr root, expr;

	for (root = expr = Nil; cons != Nil; ) {
		GetCons(cons, &expr, &cons);
		if (scope_eval(ptr, &expr, expr)) return 1;
		cons_heap(&root, expr, root);
	}
	if (rettype) {
		if (expr == Nil)
			type_value_nil(rettype);
		else
			GetEvalScopeThe(expr, rettype);
	}
	nreverse_list_unsafe(retcons, root);

	return 0;
}

static int scope_progn(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	if (scope_allcons(ptr, &eval, &type, eval)) return 1;
	make_eval_scope(ret, EVAL_PARSE_PROGN, type, eval);

	return 0;
}

static void scope_declaim(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	apply_declaim_stack(ptr, eval);
	type_value_nil(&type);
	make_eval_scope(ret, EVAL_PARSE_DECLAIM, type, eval);
}


/*
 *  apply_declare
 */
static int specialp_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* special declaration */
	GetConst(SYSTEM_TYPE_SCOPE, &key);
	if (getplist(table, key, &value) == 0 && find_list_eq_unsafe(symbol, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (getplistplist(table, key, symbol, &value) == 0) {
		*ret = getspecialp_tablevalue(value);
		return 1;
	}

	return 0;
}
static int specialp_tablevalue(Execute ptr, addr stack, addr symbol)
{
	int result;
	addr global_stack;

	/* symbol */
	if (specialp_symbol(symbol)) {
		return 1;
	}

	/* global stack */
	getglobal_eval(ptr, &global_stack);
	if (specialp_stack_tablevalue(global_stack, symbol, &result)) {
		if (result)
			return result;
		/* If symbol is lexical scope, find current stack. */
	}

	/* local stack */
	while (stack != Nil) {
		if (specialp_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* lexical */
	return 0;
}

static int find_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (ret == NULL) ret = &key;
	return getplistplist(stack, key, symbol, ret) == 0;
}

static int find_tablefunction(addr stack, addr call, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (ret == NULL) ret = &key;
	return getplistplist_callname(stack, key, call, ret) == 0;
}

static void check_value_scope(Execute ptr, addr stack, addr symbol, addr *ret)
{
	int specialp;
	addr pos;

	specialp = specialp_tablevalue(ptr, stack, symbol);
	make_tablevalue(NULL, symbol, &pos);
	setspecialp_tablevalue(pos, specialp);
	*ret = pos;
}

static void check_value_declare(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablevalue(stack, key, NULL)) {
			check_value_scope(ptr, stack, key, &key);
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}
}

static int globalp_stack_tablefunction(addr stack, addr call)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* free declaration */
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		return 1;
	}
	/* table value */
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		return 1;
	}

	return 0;
}

static int globalp_tablefunction(Execute ptr, addr stack, addr call)
{
	addr value, global_stack;

	/* local scope */
	while (stack != Nil) {
		if (globalp_stack_tablefunction(stack, call)) {
			return globalp_stack_eval(stack);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global scope */
	getglobal_eval(ptr, &global_stack);
	if (globalp_stack_tablefunction(global_stack, call)) {
		return 1;
	}

	/* symbol */
	getfunction_callname_global(call, &value);
	if (value != Unbound) {
		return 1;
	}

	/* global */
	return 1;
}

static void check_function_scope(Execute ptr, addr stack, addr call, addr *ret)
{
	int globalp;
	addr pos;

	copylocal_object(NULL, &call, call);
	globalp = globalp_tablefunction(ptr, stack, call);
	make_tablefunction(NULL, call, &pos);
	setglobalp_tablefunction(pos, globalp);
	*ret = pos;
}

static void check_function_declare(Execute ptr, addr stack, addr cons, addr *root)
{
	addr key, value;

	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (! find_tablefunction(stack, key, NULL)) {
			check_function_scope(ptr, stack, key, &key);
			copylocal_object(NULL, &value, value);
			cons_heap(&key, key, value);
			cons_heap(root, key, *root);
		}
	}
}

static void check_declare_stack(Execute ptr, addr stack, addr decl, addr *ret)
{
	addr root, cons;

	/* check */
	root = Nil;
	getall_type_value_declare(decl, &cons);
	check_value_declare(ptr, stack, cons, &root);
	getall_type_function_declare(decl, &cons);
	check_function_declare(ptr, stack, cons, &root);
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void apply_declare(Execute ptr, addr stack, addr decl, addr *ret)
{
	if (decl == Nil) {
		*ret = Nil;
		return;
	}
	check_declare_stack(ptr, stack, decl, ret);
	apply_declare_stack(ptr->local, stack, decl);
}


/*
 *  let
 */
struct let_struct {
	addr stack, args, decl, doc, cons, free, the;
};

static void check_scope_variable(addr symbol)
{
	Check(! symbolp(symbol), "type error");
	if (keywordp(symbol))
		fmte("Keyword ~S can't be use a variable.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		fmte("The constant of symbol ~S can't use a variable.", symbol, NULL);
}

static int let_init(Execute ptr, struct let_struct *str)
{
	addr args, root, var, init;
	LocalRoot local;

	local = ptr->local;
	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		check_scope_variable(var);
		if (scope_eval(ptr, &init, init)) return 1;
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(&str->args, root);

	return 0;
}

static int make_tablevalue_stack(LocalRoot local, addr *ret, addr stack, addr symbol)
{
	addr table, key, pos, aster;

	CheckType(symbol, LISPTYPE_SYMBOL);
	GetEvalStackTable(stack, &table);
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (getplistplist(table, key, symbol, ret) == 0) return 0;

	make_tablevalue(local, symbol, &pos);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	if (setplistplist_alloc(local, table, key, symbol, pos, &table))
		SetEvalStackTable(stack, table);
	*ret = pos;

	return 1;
}

static void let_maketable(LocalRoot local, struct let_struct *str)
{
	addr stack, args, var;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCar(var, &var);
		make_tablevalue_stack(local, &var, stack, var);
	}
}

static int dynamic_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_VALUE, &key);
	if (getplist(table, key, &value) == 0 && find_list_eq_unsafe(symbol, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (getplistplist(table, key, symbol, &value) == 0) {
		*ret = getdynamic_tablevalue(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablevalue(addr stack, addr symbol)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablevalue(addr stack, addr symbol, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_VALUE, &key);
	if (getplistplist(table, key, symbol, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (getplistplist(table, key, symbol, &value) == 0) {
		*ret = getignore_tablevalue(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablevalue(addr stack, addr symbol)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int type_free_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_VALUE, &key);
	return getplistplist(stack, key, symbol, ret) == 0;
}

static int type_boundary_tablevalue(addr stack, addr symbol, addr *ret)
{
	if (! find_tablevalue(stack, symbol, &symbol)) return 0;
	gettype_tablevalue(symbol, ret);
	return 1;
}

static void type_tablevalue(Execute ptr, LocalRoot local,
		addr stack, addr symbol, int specialp, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablevalue(stack, symbol, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablevalue(stack, symbol, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			if (! specialp) goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	getglobal_eval(ptr, &stack);
	/* free declaration */
	if (type_free_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_value_symbol(symbol, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse_list_unsafe(ret, root);
}

static int type_and_array(LocalRoot local, addr cons, addr *ret)
{
	addr array, type, pos;
	size_t size;

	/* array size */
	size = 0;
	type = Nil;
	for (array = cons; array != Nil; ) {
		GetCons(array, &pos, &array);
		if (! type_astert_p(pos)) {
			type = pos;
			size++;
		}
	}
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		CheckType(type, LISPTYPE_TYPE);
		copylocal_object(local, ret, type);
		return 0;
	}

	/* type-and */
	vector4_alloc(local, &array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_astert_p(pos)) {
			copylocal_object(local, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_alloc(local, LISPDECL_AND, array, ret);
	return 0;
}

static void push_tablevalue_alloc(Execute ptr, LocalRoot local,
		addr stack, addr symbol, addr *ret)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr pos, type;

	/* scope */
	specialp = specialp_tablevalue(ptr, stack, symbol);
	dynamic = dynamic_tablevalue(stack, symbol);
	ignore = ignore_tablevalue(stack, symbol);
	type_tablevalue(ptr, local, stack, symbol, specialp, &type);
	if (type_and_array(local, type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_stack(local, &pos, stack, symbol);
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);
	*ret = pos;
}
static void push_tablevalue_local(Execute ptr, addr stack, addr symbol, addr *ret)
{
	push_tablevalue_alloc(ptr, ptr->local, stack, symbol, ret);
}
static void push_tablevalue_heap(Execute ptr, addr stack, addr symbol, addr *ret)
{
	push_tablevalue_alloc(ptr, NULL, stack, symbol, ret);
}

static int checktype_p(addr left, addr right, int *check)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	switch (subtypep_result(left, right, 1)) {
		case SUBTYPEP_INCLUDE:
			/* type check can skip. */
			*check = 0;
			return 0;

		case SUBTYPEP_EXCLUDE:
			/* error, output to warning mesasge. */
			*check = 1;
			return 1;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_INVALID:
		default:
			/* type check must execute. */
			*check = 1;
			return 0;
	}
}

static void checktype_value(addr value, addr init)
{
	int check;
	addr type;

	gettype_tablevalue(value, &type);
	GetEvalScopeThe(init, &init);
	if (checktype_p(init, type, &check))
		fmtw("Type conflict occured.", NULL);
	setcheck_tablevalue(value, check);
}

static void let_applytable(Execute ptr, struct let_struct *str)
{
	addr stack, args, var, init;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		push_tablevalue_local(ptr, stack, var, &var);
		checktype_value(var, init);
	}
}

static void ignore_checkvalue(addr stack)
{
	enum IgnoreType ignore;
	int reference;
	addr key, table, symbol, value;

	GetConst(SYSTEM_TABLE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	getplist(table, key, &table);
	while (table != Nil) {
		GetCons(table, &symbol, &table);
		CheckType(table, LISPTYPE_CONS);
		GetCons(table, &value, &table);
		/* check ignore */
		ignore = getignore_tablevalue(value);
		reference = getreference_tablevalue(value);

		if (ignore == IgnoreType_None && (! reference)) {
			fmtw("Unused variable ~S.", symbol, NULL);
		}
		if (ignore == IgnoreType_Ignore && reference) {
			fmtw("Ignore variable ~S used.", symbol, NULL);
		}
	}
}

static void tablevalue_update(addr table, addr *ret, addr var)
{
	if (getplist(table, var, &var))
		fmte("tablevalue_update error.", NULL);
	copy_tablevalue(NULL, ret, var);
}

static void let_update(Execute ptr, struct let_struct *str)
{
	addr stack, args, key, root, var, init;

	stack = str->stack;
	args = str->args;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_VALUE, &key);
	getplist(stack, key, &stack);

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		tablevalue_update(stack, &var, var);
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(&str->args, root);
}

static int let_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	if (let_init(ptr, str)) return 1;
	let_maketable(ptr->local, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	let_applytable(ptr, str);
	if (scope_allcons(ptr, &str->cons, &str->the, str->cons)) return 1;
	ignore_checkvalue(stack);
	let_update(ptr, str);

	return 0;
}

static int let_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	if (let_execute(ptr, str)) return 1;
	freestack_eval(ptr, str->stack);

	return 0;
}

static int scope_let(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	memset(&str, 0, sizeoft(struct let_struct));
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	if (let_call(ptr, &str)) return 1;
	eval_scope_size(&eval, 4, EVAL_PARSE_LET, str.the, eval);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	*ret = eval;

	return 0;
}

static void ifdeclvalue(Execute ptr, addr stack, addr var, addr decl, addr *ret)
{
	addr pos, aster;
	LocalRoot local;

	local = ptr->local;
	make_tablevalue_stack(local, &pos, stack, var);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	apply_declare_value_stack(local, stack, var, decl);
	push_tablevalue_local(ptr, stack, var, &pos);
	if (ret) *ret = pos;
}

static int leta_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, root, var, init;
	LocalRoot local;

	local = ptr->local;
	stack = str->stack;
	args = str->args;
	decl = str->decl;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		check_scope_variable(var);
		if (scope_eval(ptr, &init, init)) return 1;
		ifdeclvalue(ptr, stack, var, decl, NULL);
		cons_local(local, &var, var, init);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(&str->args, root);

	return 0;
}

static void find_tablevalue_error(addr stack, addr symbol, addr *ret)
{
	if (! find_tablevalue(stack, symbol, ret))
		fmte("Cannot find table value ~S.", symbol, NULL);
}

static void leta_checktype(Execute ptr, struct let_struct *str)
{
	addr stack, args, var, init;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		find_tablevalue_error(stack, var, &var);
		checktype_value(var, init);
	}
}

static int leta_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	if (leta_init(ptr, str)) return 1;
	apply_declare(ptr, stack, str->decl, &str->free);
	leta_checktype(ptr, str);
	if (scope_allcons(ptr, &str->cons, &str->the, str->cons)) return 1;
	ignore_checkvalue(stack);
	let_update(ptr, str);

	return 0;
}

static int leta_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	if (leta_execute(ptr, str)) return 1;
	freestack_eval(ptr, str->stack);

	return 0;
}

static int scope_leta(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	memset(&str, 0, sizeoft(struct let_struct));
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	if (leta_call(ptr, &str)) return 1;
	eval_scope_size(&eval, 4, EVAL_PARSE_LETA, str.the, eval);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	*ret = eval;

	return 0;
}


/*
 *  symbol
 */
static void warning_global_lexical(addr symbol)
{
	/* fmtw("Undefined variable ~S.", symbol, NULL); */
}

static int symbol_global_tablevalue(Execute ptr, addr symbol, addr *ret)
{
	int specialp;
	addr stack;

	getglobal_eval(ptr, &stack);
	if (! find_tablevalue(stack, symbol, ret)) {
		/* heap object */
		push_tablevalue_heap(ptr, stack, symbol, ret);
		specialp = getspecialp_tablevalue(*ret);
		if (! specialp)
			warning_global_lexical(symbol);
		return specialp;
	}
	return getspecialp_tablevalue(*ret);
}

static void push_closure_value(addr stack, addr symbol, addr value)
{
	addr key, table, temp;

	GetConst(SYSTEM_CLOSURE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist(table, key, symbol, &temp)) {
		/* not found */
		if (setplistplist_heap_force(table, key, symbol, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static int symbol_tablevalue(Execute ptr, addr stack, addr symbol, addr *ret)
{
	addr table, key, next;

	/* global */
	if (stack == Nil) {
		return symbol_global_tablevalue(ptr, symbol, ret);
	}

	/* local */
	GetConst(SYSTEM_TABLE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist(table, key, symbol, ret) == 0) {
		return getspecialp_tablevalue(*ret);
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (symbol_tablevalue(ptr, next, symbol, ret)) {
		return 1; /* special */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_value(stack, symbol, *ret);
	}

	return 0; /* lexical */
}

static int find_symbol_scope(Execute ptr, addr symbol, addr *ret)
{
	int specialp;
	addr stack, value;

	getstack_eval(ptr, &stack);
	specialp = symbol_tablevalue(ptr, stack, symbol, &value);
	setreference_tablevalue(value, 1);
	copy_tablevalue(NULL, ret, value);

	return specialp;
}

static void scope_symbol_heap(addr *ret, addr type, addr symbol)
{
	eval_scope_size(ret, 1, EVAL_PARSE_SYMBOL, type, symbol);
}

static void make_scope_symbol(Execute ptr, addr symbol, addr *ret)
{
	addr value, type, pos;

	Check(! symbolp(symbol), "type error");
	find_symbol_scope(ptr, symbol, &value);
	gettype_tablevalue(value, &type);
	scope_symbol_heap(&pos, type, symbol);
	SetEvalScopeIndex(pos, 0, value);
	*ret = pos;
}

static int symbol_macrolet_global_p(Execute ptr, addr symbol, addr *ret)
{
	addr stack, table, key;

	/* global stack */
	getglobal_eval(ptr, &stack);
	GetEvalStackTable(stack, &table);

	/* global special */
	GetConst(SYSTEM_TABLE_VALUE, &key);
	if (getplistplist(table, key, symbol, &key) == 0) {
		if (getspecialp_tablevalue(key)) {
			return 0; /* special variable */
		}
	}

	/* symbol special */
	if (specialp_symbol(symbol)) {
		return 0;
	}

	/* define-symbol-macro */
	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	if (getplistplist(table, key, symbol, ret) == 0) {
		return 1; /* define-symbol-macro */
	}

	/* symbol info */
	evalsymbol_macro_symbol(symbol, &table);
	if (table != Unbound) {
		*ret = table;
		return 1;
	}

	return 0;
}

static int symbol_macrolet_p(Execute ptr, addr symbol, addr *ret)
{
	addr stack, table, key;

	Check(! symbolp(symbol), "type error");

	/* local */
	getstack_eval(ptr, &stack);
	while (stack != Nil) {
		/* local variable */
		GetConst(SYSTEM_TABLE_VALUE, &key);
		GetEvalStackTable(stack, &table);
		if (getplistplist(table, key, symbol, &key) == 0) {
			return 0; /* shadow */
		}

		/* symbol-macrolet */
		GetConst(SYSTEM_SYMBOL_MACROLET, &key);
		GetEvalStackTable(stack, &table);
		if (getplistplist(table, key, symbol, ret) == 0) {
			return 1; /* symbol-macrolet */
		}

		/* next */
		GetEvalStackNext(stack, &stack);
	}

	/* global */
	return symbol_macrolet_global_p(ptr, symbol, ret);
}

static int replace_symbol_macrolet(Execute ptr, addr *ret, addr form)
{
	addr hook, call, env;

	/* hook */
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	getspecialcheck_local(ptr, hook, &hook);
	/* symbol-macro-expander */
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	/* call */
	if (consp(form)) {
		GetCons(form, &form, &env);
	}
	else
		env = Nil;
	if (callclang_funcall(ptr, &form, hook, call, form, env, NULL)) return 1;
	return scope_eval(ptr, ret, form);
}

static void make_scope_keyword(Execute ptr, addr symbol, addr *ret)
{
	addr type;

	GetTypeTable(&type, Keyword);
	scope_symbol_heap(ret, type, symbol);
}

static int scope_symbol(Execute ptr, addr *ret, addr eval)
{
	addr form;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	if (keywordp(eval))
		make_scope_keyword(ptr, eval, ret);
	else if (symbol_macrolet_p(ptr, eval, &form))
		return replace_symbol_macrolet(ptr, ret, form);
	else
		make_scope_symbol(ptr, eval, ret);

	return 0;
}

static int setq_cons(Execute ptr, addr cons, addr *ret, addr *type)
{
	addr root, var, form;

	if (cons == Nil) {
		GetTypeTable(type, Null);
		*ret = Nil;
		return 0;
	}

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(var, &var, &form);
		find_symbol_scope(ptr, var, &var);
		if (scope_eval(ptr, &form, form)) return 1;
		GetEvalScopeThe(form, type);
		checktype_value(var, form);

		cons_heap(&var, var, form);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int scope_setq(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &cons);
	if (setq_cons(ptr, cons, &cons, &type)) return 1;
	make_eval_scope(ret, EVAL_PARSE_SETQ, type, cons);

	return 0;
}


/*
 *  function
 */
static int dynamic_stack_tablefunction(addr stack, addr call, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_FUNCTION, &key);
	if (getplist(table, key, &value) == 0
			&& find_list_callname_unsafe(call, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		*ret = getdynamic_tablefunction(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablefunction(addr stack, addr call)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablefunction(addr stack, addr call, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		*ret = getignore_tablefunction(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablefunction(addr stack, addr call)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int inline_stack_tablefunction(addr stack, addr call, enum InlineType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* inline, notinline declaration */
	GetConst(SYSTEM_INLINE, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_INLINE, &check);
		if (check == value) {
			*ret = InlineType_Inline;
			return 1;
		}
		GetConst(COMMON_NOTINLINE, &check);
		if (check == value) {
			*ret = InlineType_NotInline;
			return 1;
		}
		/* through */
	}
	/* table value */
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, &value) == 0) {
		*ret = getinline_tablefunction(value);
		return 1;
	}

	return 0;
}
static enum InlineType inline_tablefunction(Execute ptr, addr stack, addr call)
{
	enum InlineType result;

	/* local stack */
	while (stack != Nil) {
		if (inline_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	getglobal_eval(ptr, &stack);
	if (inline_stack_tablefunction(stack, call, &result)) {
		return result;
	}

	/* not inline or notinline */
	return InlineType_None;
}

static void gettype_global_callname(LocalRoot local, addr call, addr *ret)
{
	enum CALLNAME_TYPE check;

	GetCallNameType(call, &check);
	GetCallName(call, &call);
	if (check == CALLNAME_SYMBOL)
		gettype_function_symbol(call, ret);
	else
		gettype_setf_symbol(call, ret);
}

static int type_free_tablefunction(addr stack, addr call, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	return getplistplist_callname(stack, key, call, ret) == 0;
}

static int type_boundary_tablefunction(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, &call)) return 0;
	gettype_tablefunction(call, ret);
	return 1;
}

static void type_tablefunction(Execute ptr, LocalRoot local,
		addr stack, addr call, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablefunction(stack, call, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablefunction(stack, call, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	getglobal_eval(ptr, &stack);
	/* free declaration */
	if (type_free_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_global_callname(local, call, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse_list_unsafe(ret, root);
}

static int make_tablefunction_stack(LocalRoot local, addr *ret, addr stack, addr call)
{
	addr table, key, pos, aster;

	CheckType(call, LISPTYPE_CALLNAME);
	GetEvalStackTable(stack, &table);
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	if (getplistplist_callname(table, key, call, ret) == 0) return 0;

	copylocal_object(local, &call, call);
	make_tablefunction(local, call, &pos);
	GetTypeTable(&aster, Function);
	settype_tablefunction(pos, aster);
	if (setplistplist_callname_alloc(local, table, key, call, pos, &table))
		SetEvalStackTable(stack, table);
	*ret = pos;

	return 1;
}

static void push_tablefunction_alloc(Execute ptr, LocalRoot local,
		addr stack, addr call, addr *ret)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr pos, type;

	/* scope */
	globalp = globalp_tablefunction(ptr, stack, call);
	dynamic = dynamic_tablefunction(stack, call);
	ignore = ignore_tablefunction(stack, call);
	Inline = inline_tablefunction(ptr, stack, call);
	type_tablefunction(ptr, local, stack, call, &type);
	if (type_and_array(local, type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	make_tablefunction_stack(local, &pos, stack, call);
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);
	*ret = pos;
}
static void push_tablefunction_local(Execute ptr, addr stack, addr call, addr *ret)
{
	push_tablefunction_alloc(ptr, ptr->local, stack, call, ret);
}
static void push_tablefunction_heap(Execute ptr, addr stack, addr call, addr *ret)
{
	push_tablefunction_alloc(ptr, NULL, stack, call, ret);
}

static void callname_global_tablefunction(Execute ptr, addr *ret, addr call)
{
	addr stack;

	getglobal_eval(ptr, &stack);
	if (! find_tablefunction(stack, call, ret))
		push_tablefunction_heap(ptr, stack, call, ret);
}

static void push_closure_function(addr stack, addr call, addr value)
{
	addr key, table, temp;

	GetConst(SYSTEM_CLOSURE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist_callname(table, key, call, &temp)) {
		/* not found */
		copylocal_object(NULL, &call, call);
		if (setplistplist_callname_heap_force(table, key, call, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static int callname_tablefunction(Execute ptr, addr stack, addr call, addr *ret)
{
	addr key, table, next;

	/* global */
	if (stack == Nil) {
		callname_global_tablefunction(ptr, ret, call);
		return 1; /* global-scope */
	}

	/* local */
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist_callname(table, key, call, ret) == 0) {
		return getglobalp_tablefunction(*ret);
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (callname_tablefunction(ptr, next, call, ret)) {
		return 1; /* global-scope */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_function(stack, call, *ret);
	}

	return 0; /* local-scope */
}

static void scope_function(Execute ptr, addr *ret, addr eval)
{
	addr call, value, type, stack;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &call);
	/* callname */
	getstack_eval(ptr, &stack);
	callname_tablefunction(ptr, stack, call, &value);
	setreference_tablefunction(value, 1);
	copy_tablefunction(NULL, &value, value);
	gettype_tablefunction(value, &type);
	make_eval_scope(ret, EVAL_PARSE_FUNCTION, type, value);
}


/*
 *  lambda
 */
struct lambda_struct {
	addr stack, call, table, args, decl, doc, cons, clos, free, the, form;
	unsigned globalp;
	enum EVAL_PARSE eval;
};

static void init_lambda_struct(struct lambda_struct *str,
		enum EVAL_PARSE eval, int globalp)
{
	memset(str, 0, sizeoft(struct lambda_struct));
	str->stack = str->call = str->table = str->args =
		str->decl = str->doc = str->cons = str->clos =
		str->free = str->the = str->form = Nil;
	str->globalp = globalp;
	str->eval = eval;
}

static void lambda_init_var(Execute ptr, addr stack, addr args, addr decl)
{
	addr var;

	while (args != Nil) {
		GetCons(args, &var, &args);
		ifdeclvalue(ptr, stack, var, decl, NULL);
	}
}

static int lambda_init_opt(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init, svar;
	LocalRoot local;

	local = Local_Thread;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		if (scope_eval(ptr, &init, init)) return 1;
		ifdeclvalue(ptr, stack, var, decl, NULL);
		if (svar != Nil)
			ifdeclvalue(ptr, stack, svar, decl, NULL);
		list_local(local, &var, var, init, svar, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int lambda_init_key(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, name, init, svar;
	LocalRoot local;

	local = Local_Thread;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		if (scope_eval(ptr, &init, init)) return 1;
		ifdeclvalue(ptr, stack, var, decl, NULL);
		if (svar != Nil)
			ifdeclvalue(ptr, stack, svar, decl, NULL);
		list_local(local, &var, var, name, init, svar, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int lambda_init_aux(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init;
	LocalRoot local;

	local = Local_Thread;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		if (scope_eval(ptr, &init, init)) return 1;
		ifdeclvalue(ptr, stack, var, decl, NULL);
		list_local(local, &var, var, init, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int lambda_init(Execute ptr, struct lambda_struct *str)
{
	addr stack, decl, args, var, opt, rest, key, allow, aux;

	stack = str->stack;
	decl = str->decl;
	args = str->args;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* scope */
	lambda_init_var(ptr, stack, var, decl);
	if (lambda_init_opt(ptr, stack, opt, decl, &opt)) return 1;
	if (rest != Nil)
		ifdeclvalue(ptr, stack, rest, decl, NULL);
	if (lambda_init_key(ptr, stack, key, decl, &key)) return 1;
	if (lambda_init_aux(ptr, stack, aux, decl, &aux)) return 1;
	list_local(ptr->local, &str->args, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static void lambda_tablevalue_var(LocalRoot local, addr stack, addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		find_tablevalue_error(stack, var, &var);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_tablevalue_opt(LocalRoot local, addr stack, addr args, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		find_tablevalue_error(stack, var, &var);
		if (svar != Nil)
			find_tablevalue_error(stack, svar, &svar);
		checktype_value(var, init);
		list_local(local, &var, var, init, svar, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_tablevalue_key(LocalRoot local, addr stack, addr args, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		find_tablevalue_error(stack, var, &var);
		if (svar != Nil)
			find_tablevalue_error(stack, svar, &svar);
		checktype_value(var, init);
		list_local(local, &var, var, name, init, svar, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_tablevalue_aux(LocalRoot local, addr stack, addr args, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		find_tablevalue_error(stack, var, &var);
		checktype_value(var, init);
		list_local(local, &var, var, init, NULL);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_tablevalue(LocalRoot local, struct lambda_struct *str)
{
	addr stack, args, var, opt, rest, key, allow, aux;

	stack = str->stack;
	args = str->args;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* tablevalue */
	lambda_tablevalue_var(local, stack, var, &var);
	lambda_tablevalue_opt(local, stack, opt, &opt);
	if (rest != Nil)
		find_tablevalue_error(stack, rest, &rest);
	lambda_tablevalue_key(local, stack, key, &key);
	lambda_tablevalue_aux(local, stack, aux, &aux);
	list_local(local, &str->args, var, opt, rest, key, allow, aux, NULL);
}

static void type_ordinary_var(LocalRoot local, addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		gettype_tablevalue(var, &var);
		copylocal_object(local, &var, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void type_ordinary_opt(LocalRoot local, addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCar(var, &var);
		gettype_tablevalue(var, &var);
		copylocal_object(local, &var, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void type_ordinary_rest(LocalRoot local, addr rest, addr *ret)
{
	if (rest != Nil)
		GetTypeTable(ret, T);
}

static void type_ordinary_key(LocalRoot local, addr args, addr allow, addr *ret)
{
	addr root, var, name;

	if (allow != Nil) {
		/* &allow-other-keys */
		*ret = T;
		return;
	}
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &name);
		GetCar(name, &name);
		gettype_tablevalue(var, &var);
		copylocal_object(local, &var, var);
		cons_alloc(local, &var, name, var);
		cons_alloc(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void make_type_ordinary(LocalRoot local, addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, array;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	/* arg-typespec */
	type_ordinary_var(local, var, &var);
	type_ordinary_opt(local, opt, &opt);
	type_ordinary_rest(local, rest, &rest);
	type_ordinary_key(local, key, allow, &key);

	/* type-function vector */
	vector2_alloc(local, &array, 4);
	SetArrayA2(array, 0, var);
	SetArrayA2(array, 1, opt);
	SetArrayA2(array, 2, rest);
	SetArrayA2(array, 3, key); /* &key or &allow-other-keys */
	*ret = array;
}

static void lambda_type_incomplete(LocalRoot local, addr args, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	make_type_ordinary(local, args, &args);
	type3_local(local, LISPDECL_FUNCTION, args, aster, Nil, ret);
}

static void lambda_declare(Execute ptr, struct lambda_struct *str)
{
	addr table, type;
	LocalRoot local;

	/* incomplete type */
	local = ptr->local;
	lambda_type_incomplete(local, str->args, &type);
	str->the = type;

	/* tablefunction */
	if (str->call != Nil) {
		make_tablefunction_stack(local, &table, str->stack, str->call);
		setglobalp_tablefunction(table, 0);
		settype_tablefunction(table, type);
		str->table = table;
	}
}

static int lambda_progn(Execute ptr, struct lambda_struct *str)
{
	addr the, type;

	if (scope_allcons(ptr, &str->cons, &type, str->cons)) return 1;
	/* (function [args] *) -> (function [args] [values]) */
	the = str->the;
	SetArrayType(the, 1, type);
	copylocal_object(NULL, &the, the);
	if (str->table != Nil)
		settype_tablefunction(str->table, the);
	str->the = the;

	return 0;
}

static void lambda_update_var(addr *ret, addr args)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		copylocal_tablevalue(NULL, &var, var);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_update_opt(addr *ret, addr args)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		copylocal_tablevalue(NULL, &var, var);
		if (svar != Nil)
			copylocal_tablevalue(NULL, &svar, svar);
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_update_key(addr *ret, addr args)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		copylocal_tablevalue(NULL, &var, var);
		if (svar != Nil)
			copylocal_tablevalue(NULL, &svar, svar);
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_update_aux(addr *ret, addr args)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		copylocal_tablevalue(NULL, &var, var);
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void lambda_update(struct lambda_struct *str)
{
	addr var, opt, rest, key, allow, aux;

	/* destructuring-bind */
	List_bind(str->args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* args copy */
	lambda_update_var(&var, var);
	lambda_update_opt(&opt, opt);
	if (rest != Nil)
		copylocal_tablevalue(NULL, &rest, rest);
	lambda_update_key(&key, key);
	lambda_update_aux(&aux, aux);
	list_heap(&str->args, var, opt, rest, key, allow, aux, NULL);
	/* tablefunction copy */
	if (str->table != Nil)
		copy_tablefunction(NULL, &str->table, str->table);
}

static void getclosure(addr table, void (*copycall)(LocalRoot, addr *, addr),
		enum CONSTANT_INDEX index, addr *ret)
{
	addr key, cons, root, name, value;

	GetConstant(index, &key);
	if (getplist(table, key, &cons)) {
		*ret = Nil;
		return;
	}
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &name, &cons);
		GetCons(cons, &value, &cons);
		(*copycall)(NULL, &value, value);
		cons_heap(&root, value, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void getclosure_block(addr table, addr *ret)
{
	addr key, cons;

	GetConst(SYSTEM_CLOSURE_BLOCK, &key);
	if (getplist(table, key, &cons)) {
		*ret = Nil;
		return;
	}
	copyhard_object(NULL, ret, cons);
}

static void lambda_closure(Execute ptr, struct lambda_struct *str)
{
	addr array, stack, table, pos;

	vector2_heap(&array, EvalClosure_Size);
	getstack_eval(ptr, &stack);
	GetEvalStackTable(stack, &table);
	/* value */
	getclosure(table, copy_tablevalue, CONSTANT_SYSTEM_CLOSURE_VALUE, &pos);
	SetArrayA2(array, EvalClosure_Value, pos);
	/* function */
	getclosure(table, copy_tablefunction, CONSTANT_SYSTEM_CLOSURE_FUNCTION, &pos);
	SetArrayA2(array, EvalClosure_Function, pos);
	/* tagbody */
	getclosure(table, copy_tabletagbody, CONSTANT_SYSTEM_CLOSURE_TAGBODY, &pos);
	SetArrayA2(array, EvalClosure_TagBody, pos);
	/* block */
	getclosure_block(table, &pos);
	SetArrayA2(array, EvalClosure_Block, pos);
	/* result */
	str->clos = array;
}

static int lambda_execute(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack, eval;

	/* lambda */
	stack = str->stack;
	if (lambda_init(ptr, str)) return 1;
	apply_declare(ptr, stack, str->decl, &str->free);
	lambda_tablevalue(ptr->local, str);
	lambda_declare(ptr, str);
	if (lambda_progn(ptr, str)) return 1;
	ignore_checkvalue(stack);
	lambda_update(str);
	lambda_closure(ptr, str);

	/* eval */
	eval_scope_size(&eval, EvalLambda_Size, str->eval, str->the, Nil);
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	SetEvalScopeIndex(eval, EvalLambda_Table, str->table);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	SetEvalScopeIndex(eval, EvalLambda_Form, str->form);
	*ret = eval;

	return 0;
}

static int lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	str->stack = newstack_lambda(ptr);
	if (lambda_execute(ptr, str, ret)) return 1;
	freestack_eval(ptr, str->stack);
	str->stack = NULL;

	return 0;
}

static int scope_lambda(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	init_lambda_struct(&str, EVAL_PARSE_LAMBDA, 0);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	GetEvalParse(eval, 4, &str.form);
	return lambda_object(ptr, &str, ret);
}


/*
 *  defun
 */
static void defun_update(Execute ptr, struct lambda_struct *str)
{
	addr stack, table;

	getglobal_eval(ptr, &stack);
	push_tablefunction_heap(ptr, stack, str->call, &table);
	settype_tablefunction(table, str->the);
}

static void defun_the(addr eval, struct lambda_struct *str)
{
	addr cdr, type, null, setf, call;

	switch (RefCallNameType(str->call)) {
		case CALLNAME_SYMBOL:
			GetTypeTable(&type, Symbol);
			break;

		case CALLNAME_SETF:
			/* (setf hello) -> (cons (eql setf) (cons (eql hello) null)) */
			GetConst(COMMON_SETF, &setf);
			type_eql_heap(setf, &setf);
			GetCallName(str->call, &call);
			type_eql_heap(call, &call);
			GetTypeTable(&null, Null);
			type2_heap(LISPDECL_CONS, call, null, &cdr);
			type2_heap(LISPDECL_CONS, setf, cdr, &type);
			break;

		default:
			fmte("callname error.", NULL);
			return;
	}
	SetEvalScopeThe(eval, type);
}

static int scope_defun(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	init_lambda_struct(&str, EVAL_PARSE_DEFUN, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	if (lambda_object(ptr, &str, &eval)) return 1;
	defun_update(ptr, &str);
	defun_the(eval, &str);
	*ret = eval;

	return 0;
}


/*
 *  macro-lambda
 */
static int macro_init_args(Execute ptr, addr, addr, addr, addr *);
static int macro_init_var(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, var;
	LocalRoot local;

	local = ptr->local;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			if (macro_init_args(ptr, stack, var, decl, &var)) return 1;
		}
		else {
			ifdeclvalue(ptr, stack, var, decl, NULL);
		}
		cons_alloc(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static void macro_init_rest(Execute ptr, addr stack, addr rest, addr decl)
{
	if (rest != Nil) {
		GetCar(rest, &rest);
		ifdeclvalue(ptr, stack, rest, decl, NULL);
	}
}

static int macro_init_args(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	if (macro_init_var(ptr, stack, var, decl, &var)) return 1;
	if (lambda_init_opt(ptr, stack, opt, decl, &opt)) return 1;
	macro_init_rest(ptr, stack, rest, decl);
	if (lambda_init_key(ptr, stack, key, decl, &key)) return 1;
	if (lambda_init_aux(ptr, stack, aux, decl, &aux)) return 1;
	if (whole != Nil)
		ifdeclvalue(ptr, stack, whole, decl, NULL);
	if (env != Nil)
		ifdeclvalue(ptr, stack, env, decl, NULL);
	list_local(ptr->local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int macro_init(Execute ptr, struct lambda_struct *str)
{
	return macro_init_args(ptr, str->stack, str->args, str->decl, &str->args);
}

static void macro_tablevalue_args(Execute ptr, addr stack, addr args, addr *ret);
static void macro_tablevalue_var(Execute ptr, addr stack, addr args, addr *ret)
{
	addr root, var;
	LocalRoot local;

	local = Local_Thread;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var))
			macro_tablevalue_args(ptr, stack, var, &var);
		else
			find_tablevalue_error(stack, var, &var);
		cons_local(local, &root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void macro_tablevalue_rest(LocalRoot local, addr stack, addr rest, addr *ret)
{
	addr car, cdr;

	if (rest != Nil) {
		GetCons(rest, &car, &cdr);
		find_tablevalue_error(stack, car, &car);
		cons_local(local, &rest, car, cdr);
	}
	*ret = rest;
}

static void macro_tablevalue_args(Execute ptr, addr stack, addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;
	LocalRoot local;

	local = ptr->local;
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	macro_tablevalue_var(ptr, stack, var, &var);
	lambda_tablevalue_opt(local, stack, opt, &opt);
	macro_tablevalue_rest(local, stack, rest, &rest);
	lambda_tablevalue_key(local, stack, key, &key);
	lambda_tablevalue_aux(local, stack, aux, &aux);
	if (whole != Nil)
		find_tablevalue_error(stack, whole, &whole);
	if (env != Nil)
		find_tablevalue_error(stack, env, &env);
	list_local(ptr->local, ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

static void macro_tablevalue(Execute ptr, struct lambda_struct *str)
{
	macro_tablevalue_args(ptr, str->stack, str->args, &str->args);
}

static int macro_progn(Execute ptr, struct lambda_struct *str)
{
	return scope_allcons(ptr, &str->cons, NULL, str->cons);
}

static void macro_update_args(addr *ret, addr args);
static void macro_update_var(addr *ret, addr args)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var))
			macro_update_args(&var, var);
		else
			copylocal_tablevalue(NULL, &var, var);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void macro_update_rest(addr *ret, addr rest)
{
	addr car, cdr;

	if (rest != Nil) {
		GetCons(rest, &car, &cdr);
		copylocal_tablevalue(NULL, &car, car);
		cons_heap(&rest, car, cdr);
	}
	*ret = rest;
}

static void macro_update_args(addr *ret, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	macro_update_var(&var, var);
	lambda_update_opt(&opt, opt);
	macro_update_rest(&rest, rest);
	lambda_update_key(&key, key);
	lambda_update_aux(&aux, aux);
	if (whole != Nil)
		copylocal_tablevalue(NULL, &whole, whole);
	if (env != Nil)
		copylocal_tablevalue(NULL, &env, env);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

static void macro_update(struct lambda_struct *str)
{
	macro_update_args(&str->args, str->args);
}

static int macro_execute(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack, eval;

	/* lambda */
	stack = str->stack;
	if (macro_init(ptr, str)) return 1;
	apply_declare(ptr, stack, str->decl, &str->free);
	macro_tablevalue(ptr, str);
	if (macro_progn(ptr, str)) return 1;
	ignore_checkvalue(stack);
	macro_update(str);
	lambda_closure(ptr, str);

	/* eval */
	eval_scope_size(&eval, EvalLambda_Size, str->eval, str->the, Nil);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	*ret = eval;

	return 0;
}

static int macro_lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	str->stack = newstack_lambda(ptr);
	if (macro_execute(ptr, str, ret)) return 1;
	freestack_eval(ptr, str->stack);
	str->stack = NULL;

	return 0;
}

static void macro_lambda_the(addr eval)
{
	addr type;
	GetTypeCompiled(&type, MacroFunction);
	SetEvalScopeThe(eval, type);
}

static int scope_macro_lambda(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	init_lambda_struct(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	if (macro_lambda_object(ptr, &str, &eval)) return 1;
	macro_lambda_the(eval);
	*ret = eval;

	return 0;
}


/*
 *  defmacro
 */
static void scope_defmacro(Execute ptr, addr *ret, addr eval)
{
	addr symbol, lambda, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &lambda);

	GetTypeTable(&type, Symbol);
	eval_scope_size(&eval, 2, EVAL_PARSE_DEFMACRO, type, Nil);
	SetEvalScopeIndex(eval, 0, symbol);
	SetEvalScopeIndex(eval, 1, lambda);
	*ret = eval;
}


/*
 *  deftype
 */
static int scope_deftype(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	init_lambda_struct(&str, EVAL_PARSE_DEFTYPE, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	if (macro_lambda_object(ptr, &str, &eval)) return 1;
	SetEvalScopeIndex(eval, EvalLambda_Call, str.call);
	*ret = eval;

	return 0;
}


/*
 *  destructuring-bind
 */
static int scope_destructuring_bind(Execute ptr, addr *ret, addr eval)
{
	addr args, expr, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &args);
	if (scope_eval(ptr, &expr, expr)) return 1;
	if (scope_eval(ptr, &args, args)) return 1;
	GetEvalScopeThe(args, &type);
	eval_scope_size(&eval, 2, EVAL_PARSE_DESTRUCTURING_BIND, type, Nil);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, args);
	*ret = eval;

	return 0;
}


/*
 *  define-symbol-macro
 */
static void push_symbol_macrolet(addr stack, addr symbol, addr form, addr env)
{
	addr key, table, temp;

	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist(table, key, symbol, &temp)) {
		/* not found */
		cons_heap(&form, form, env);
		if (setplistplist_heap(table, key, symbol, form, &table))
			SetEvalStackTable(stack, table);
	}
}

static void scope_define_symbol_macro(Execute ptr, addr *ret, addr eval)
{
	addr stack, symbol, form, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &form);
	GetEvalParse(eval, 2, &body);

	getglobal_eval(ptr, &stack);
	push_symbol_macrolet(stack, symbol, form, Nil); /* null env */
	GetTypeTable(&type, Symbol);
	eval_scope_size(&eval, 3, EVAL_PARSE_DEFINE_SYMBOL_MACRO, type, Nil);
	SetEvalScopeIndex(eval, 0, symbol);
	SetEvalScopeIndex(eval, 1, form);
	SetEvalScopeIndex(eval, 2, body);
	*ret = eval;
}


/*
 *  symbol-macrolet
 */
static void apply_symbol_macrolet(Execute ptr, addr stack, addr args)
{
	addr list, symbol, form, env;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &symbol, &form, &env, NULL);
		push_symbol_macrolet(stack, symbol, form, env);
	}
}

static int symbol_macrolet_execute(Execute ptr,
		addr args, addr decl, addr cons, addr *ret, addr *type, addr *free)
{
	addr stack;

	stack = newstack_nil(ptr);
	apply_declare(ptr, stack, decl, free);
	apply_symbol_macrolet(ptr, stack, args);
	if (scope_allcons(ptr, ret, type, cons)) return 1;
	freestack_eval(ptr, stack);

	return 0;
}

static int scope_symbol_macrolet(Execute ptr, addr *ret, addr eval)
{
	addr args, decl, cons, type, free;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);

	/* symbol-macrolet -> locally */
	if (symbol_macrolet_execute(ptr, args, decl, cons, &cons, &type, &free)) return 1;
	eval_scope_size(&eval, 3, EVAL_PARSE_LOCALLY, type, eval);
	SetEvalScopeIndex(eval, 0, decl);
	SetEvalScopeIndex(eval, 1, cons);
	SetEvalScopeIndex(eval, 2, free);
	*ret = eval;

	return 0;
}


/*
 *  flet
 */
static int flet_call(Execute ptr, addr *ret, addr list)
{
	struct lambda_struct str;

	init_lambda_struct(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	return lambda_object(ptr, &str, ret);
}

static int flet_init(Execute ptr, struct let_struct *str)
{
	addr args, root, call, eval;
	LocalRoot local;

	local = ptr->local;
	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		if (flet_call(ptr, &eval, eval)) return 1;
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		cons_local(local, &call, call, eval);
		cons_local(local, &root, call, root);
	}
	nreverse_list_unsafe(&str->args, root);

	return 0;
}

static void flet_maketable(LocalRoot local, struct let_struct *str)
{
	addr stack, args, call;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCar(call, &call);
		make_tablefunction_stack(local, &call, stack, call);
	}
}

static void checktype_function(addr table, addr eval)
{
	int check;

	gettype_tablefunction(table, &table);
	GetEvalScopeThe(eval, &eval);
	(void)checktype_p(eval, table, &check);
	if (check)
		fmte("Invalid function type.", NULL);
}

static void flet_applytable(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		push_tablefunction_local(ptr, stack, call, &call);
		checktype_function(call, eval);
	}
}

static void ignore_checkfunction(addr stack)
{
	enum IgnoreType ignore;
	int reference;
	addr key, table, call, symbol, value;

	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	getplist(table, key, &table);
	while (table != Nil) {
		GetCons(table, &call, &table);
		CheckType(table, LISPTYPE_CONS);
		GetCons(table, &value, &table);
		/* check ignore */
		ignore = getignore_tablefunction(value);
		reference = getreference_tablefunction(value);
		GetCallName(call, &symbol);

		if (ignore == IgnoreType_None && (! reference)) {
			fmtw("Unused variable ~S.", symbol, NULL);
		}
		if (ignore == IgnoreType_Ignore && reference) {
			fmtw("Ignore variable ~S used.", symbol, NULL);
		}
	}
}

static void tablefunction_update(addr table, addr *ret, addr call)
{
	if (getplist_callname(table, call, &call))
		fmte("tablefunction_update error.", NULL);
	copy_tablefunction(NULL, ret, call);
}

static void flet_update(Execute ptr, struct let_struct *str)
{
	addr stack, args, key, root, call, eval;

	stack = str->stack;
	args = str->args;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_FUNCTION, &key);
	getplist(stack, key, &stack);

	for (root = Nil; args != Nil; ) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		tablefunction_update(stack, &call, call);
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse_list_unsafe(&str->args, root);
}

static int flet_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	if (flet_init(ptr, str)) return 1;
	flet_maketable(ptr->local, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	flet_applytable(ptr, str);
	if (scope_allcons(ptr, &str->cons, &str->the, str->cons)) return 1;
	ignore_checkfunction(stack);
	flet_update(ptr, str);

	return 0;
}

static int flet_object(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	if (flet_execute(ptr, str)) return 1;
	freestack_eval(ptr, str->stack);

	return 0;
}

static int scope_flet(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	memset(&str, 0, sizeoft(struct let_struct));
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	if (flet_object(ptr, &str)) return 1;
	eval_scope_size(&eval, 4, EVAL_PARSE_FLET, str.the, eval);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	*ret = eval;

	return 0;
}


/*
 *  labels
 */
static void ifdeclcall(Execute ptr, addr stack, addr call, addr decl, addr *ret)
{
	addr pos;

	make_tablefunction_stack(ptr->local, &pos, stack, call);
	apply_declare_function_stack(ptr->local, stack, call, decl);
	push_tablefunction_local(ptr, stack, call, &pos);
	if (ret) *ret = pos;
}

static int labels_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, root, call, eval;
	LocalRoot local;

	local = ptr->local;
	stack = str->stack;
	args = str->args;
	decl = str->decl;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		if (flet_call(ptr, &eval, eval)) return 1;
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		ifdeclcall(ptr, stack, call, decl, NULL);
		cons_local(local, &call, call, eval);
		cons_local(local, &root, call, root);
	}
	nreverse_list_unsafe(&str->args, root);

	return 0;
}

static void find_tablefunction_error(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, ret))
		fmte("Cannot find table function ~S.", call, NULL);
}

static void labels_checktype(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		find_tablefunction_error(stack, call, &call);
		checktype_function(call, eval);
	}
}

static int labels_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	if (labels_init(ptr, str)) return 1;
	apply_declare(ptr, stack, str->decl, &str->free);
	labels_checktype(ptr, str);
	if (scope_allcons(ptr, &str->cons, &str->the, str->cons)) return 1;
	ignore_checkfunction(stack);
	flet_update(ptr, str);

	return 0;
}

static int labels_object(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	if (labels_execute(ptr, str)) return 1;
	freestack_eval(ptr, str->stack);

	return 0;
}

static int scope_labels(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	memset(&str, 0, sizeoft(struct let_struct));
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	if (labels_object(ptr, &str)) return 1;
	eval_scope_size(&eval, 4, EVAL_PARSE_LABELS, str.the, eval);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	*ret = eval;

	return 0;
}


/*
 *  call
 */
static int call_first(Execute ptr, addr *ret, addr first)
{
	switch (RefEvalParseType(first)) {
		case EVAL_PARSE_FUNCTION:
			scope_function(ptr, ret, first);
			break;

		case EVAL_PARSE_LAMBDA:
			return scope_lambda(ptr, ret, first);

		default:
			fmte("Invalid parse object.", NULL);
			break;
	}

	return 0;
}

static int check_tablecall(Execute ptr, addr eval, addr right, addr *ret)
{
	int check;
	addr table;

	/* tablecall */
	if (scope_eval(ptr, &eval, eval)) return 1;
	make_tablecall(NULL, &table);
	copylocal_object(NULL, &right, right);
	settype_tablecall(table, right);
	setvalue_tablecall(table, eval);
	/* checktype */
	GetEvalScopeThe(eval, &eval);
	if (checktype_p(eval, right, &check))
		fmtw("Type conflict occured.", NULL);
	setcheck_tablecall(table, check);
	/* result */
	*ret = table;

	return 0;
}

static int callargs_var(Execute ptr, addr array, addr *args, addr *root)
{
	addr eval, right;

	GetArrayA2(array, 0, &array);
	while (array != Nil) {
		if (*args == Nil) return 1;
		GetCons(*args, &eval, args);
		GetCons(array, &right, &array);
		if (check_tablecall(ptr, eval, right, &eval)) return 1;
		cons_heap(root, eval, *root);
	}

	return 0;
}

static int callargs_opt(Execute ptr, addr array, addr *args, addr *root)
{
	addr eval, right;

	GetArrayA2(array, 1, &array);
	while (*args != Nil && array != Nil) {
		GetCons(*args, &eval, args);
		GetCons(array, &right, &array);
		if (check_tablecall(ptr, eval, right, &eval)) return 1;
		cons_heap(root, eval, *root);
	}

	return 0;
}

static void callargs_keyvalue(int keyvalue, addr cons, addr *ret)
{
	if (keyvalue == 0) {
		/* name */
		GetCar(cons, &cons);
		type_eql_heap(cons, ret);
	}
	else {
		/* type */
		GetCdr(cons, &cons);
		copylocal_object(NULL, ret, cons);
	}
}

static void callargs_key(int keyvalue, addr cons, addr *ret)
{
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
	size = length_list_unsafe(cons);
	if (size == 1) {
		GetCar(cons, &pos);
		callargs_keyvalue(keyvalue, pos, ret);
		return;
	}

	/* or */
	vector4_heap(&array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		callargs_keyvalue(keyvalue, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_heap(LISPDECL_OR, array, ret);
}

static void type_and_nil(LocalRoot local, addr type1, addr type2, addr *ret)
{
	int check1, check2;

	check1 = (type1 == Nil);
	check2 = (type2 == Nil);
	if (check1 && check2) {
		GetTypeTable(ret, Asterisk);
		return;
	}
	if (check1) {
		*ret = type2;
		return;
	}
	if (check2) {
		*ret = type1;
		return;
	}
	else {
		type2and_alloc(local, type1, type2, ret);
		return;
	}
}

static int callargs_restkey(Execute ptr,
		addr array, addr *args, addr *root, int *result)
{
	int keyvalue;
	addr rest, key, eval, type1, type2, right;

	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);
	if (rest == Nil && key == Nil) {
		*result = *args != Nil;
		return 0;
	}
	for (keyvalue = 0; *args != Nil; keyvalue = (! keyvalue)) {
		GetCons(*args, &eval, args);
		type1 = type2 = Nil;
		/* &rest */
		if (rest != Nil)
			copylocal_object(NULL, &type1, rest);
		/* &key */
		if (key != Nil)
			callargs_key(keyvalue, key, &type2);
		/* result */
		type_and_nil(NULL, type1, type2, &right);
		if (check_tablecall(ptr, eval, right, &eval)) return 1;
		cons_heap(root, eval, *root);
	}

	/* error check */
	if (key != Nil && keyvalue)
		fmte("Invalid keyword argument.", NULL);
	*result = 0;

	return 0;
}

static int callargs_check(Execute ptr, addr array, addr args, addr *ret)
{
	int check;
	addr root;

	root = Nil;
	/* var */
	if (callargs_var(ptr, array, &args, &root)) goto toofew;
	/* opt */
	if (args == Nil) goto final;
	if (callargs_opt(ptr, array, &args, &root)) return 1;
	/* rest, key */
	if (args == Nil) goto final;
	if (callargs_restkey(ptr, array, &args, &root, &check)) return 1;
	if (check) goto toomany;
	goto final;

toofew:
	fmtw("Too few arguments.", NULL);
	goto final;
toomany:
	fmtw("Too many arguments.", NULL);
	goto final;
final:
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int asterisk_function_argument(addr type, addr *ret)
{
	if (type_asterisk_p(type)) {
		return 1;
	}
	if (type_function_p(type)) {
		GetArrayType(type, 0, &type); /* arguement */
		if (type_asterisk_p(type)) return 1;
		*ret = type;
		return 0;
	}
	infobit(type);
	Abort("type error");
	return 0;
}

static int callargs_nocheck(Execute ptr, addr args, addr *ret)
{
	addr root, eval, type;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		GetTypeTable(&type, Asterisk);
		if (check_tablecall(ptr, eval, type, &eval)) return 1;
		cons_heap(&root, eval, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int call_args(Execute ptr, addr *ret, addr first, addr args)
{
	GetEvalScopeThe(first, &first);
	if (asterisk_function_argument(first, &first)) {
		if (callargs_nocheck(ptr, args, ret)) return 1;
	}
	else {
		if (callargs_check(ptr, first, args, ret)) return 1;
	}

	return 0;
}

static void call_result(addr *ret, addr first)
{
	addr type;

	GetEvalScopeThe(first, &type);
	CheckType(type, LISPTYPE_TYPE);
	if (type_asterisk_p(type)) {
		*ret = type;
		return;
	}
	Check(! type_function_p(type), "type decl error");
	GetArrayType(type, 1, ret); /* values */
	CheckType(*ret, LISPTYPE_TYPE);
}

static int scope_call(Execute ptr, addr *ret, addr eval)
{
	addr first, args, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &first);
	GetEvalParse(eval, 1, &args);

	if (call_first(ptr, &first, first)) return 1;
	if (call_args(ptr, &args, first, args)) return 1;
	call_result(&type, first);

	eval_scope_size(&eval, 2, EVAL_PARSE_CALL, type, eval);
	SetEvalScopeIndex(eval, 0, first);
	SetEvalScopeIndex(eval, 1, args);
	*ret = eval;

	return 0;
}

static int values_args(Execute ptr, addr args, addr *retargs, addr *rettype)
{
	addr root, var, rest, eval, type;

	/* progn and typelist */
	for (root = var = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		if (scope_eval(ptr, &eval, eval)) return 1;
		GetEvalScopeThe(eval, &type);
		/* push */
		cons_heap(&root, eval, root);
		cons_heap(&var, type, var);
	}
	nreverse_list_unsafe(retargs, root);
	nreverse_list_unsafe(&var, var);

	/* (values ... &rest nil) */
	GetTypeTable(&rest, Nil);
	type_values_heap(var, Nil, rest, Nil, rettype);

	return 0;
}

static int scope_values(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	if (values_args(ptr, eval, &cons, &type)) return 1;
	make_eval_scope(ret, EVAL_PARSE_VALUES, type, cons);

	return 0;
}

static void the_check(Execute ptr, addr eval, addr right, addr *ret)
{
	int check;
	addr left;

	GetEvalScopeThe(eval, &left);
	if (checktype_p(left, right, &check))
		fmtw("Type conflict occured.", NULL);
	*ret = check? T: Nil;
}

static int scope_the(Execute ptr, addr *ret, addr eval)
{
	addr type, form, check;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &type);
	GetEvalParse(eval, 1, &form);

	if (scope_eval(ptr, &form, form)) return 1;
	the_check(ptr, form, type, &check);

	eval_scope_size(&eval, 1, EVAL_PARSE_THE, type, form);
	SetEvalScopeIndex(eval, 0, check);
	*ret = eval;

	return 0;
}

static int locally_execute(Execute ptr,
		addr decl, addr cons, addr *ret, addr *type, addr *free)
{
	addr stack;

	stack = newstack_nil(ptr);
	apply_declare(ptr, stack, decl, free);
	if (scope_allcons(ptr, ret, type, cons)) return 1;
	freestack_eval(ptr, stack);

	return 0;
}

static int scope_locally(Execute ptr, addr *ret, addr eval)
{
	addr decl, cons, type, free;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);

	if (locally_execute(ptr, decl, cons, &cons, &type, &free)) return 1;
	eval_scope_size(&eval, 3, EVAL_PARSE_LOCALLY, type, eval);
	SetEvalScopeIndex(eval, 0, decl);
	SetEvalScopeIndex(eval, 1, cons);
	SetEvalScopeIndex(eval, 2, free);
	*ret = eval;

	return 0;
}

static int scope_if(Execute ptr, addr *ret, addr eval)
{
	addr expr, then, last, type1, type2, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &then);
	GetEvalParse(eval, 2, &last);

	if (scope_eval(ptr, &expr, expr)) return 1;
	if (scope_eval(ptr, &then, then)) return 1;
	if (scope_eval(ptr, &last, last)) return 1;
	GetEvalScopeThe(then, &type1);
	GetEvalScopeThe(last, &type2);
	type2or_heap(type1, type2, &type);

	eval_scope_size(&eval, 3, EVAL_PARSE_IF, type, eval);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, then);
	SetEvalScopeIndex(eval, 2, last);
	*ret = eval;

	return 0;
}

static int scope_unwind_protect(Execute ptr, addr *ret, addr eval)
{
	addr protect, cleanup, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &protect);
	GetEvalParse(eval, 1, &cleanup);

	if (scope_eval(ptr, &protect, protect)) return 1;
	if (scope_allcons(ptr, &cleanup, NULL, cleanup)) return 1;
	GetEvalScopeThe(protect, &type);

	eval_scope_size(&eval, 2, EVAL_PARSE_UNWIND_PROTECT, type, eval);
	SetEvalScopeIndex(eval, 0, protect);
	SetEvalScopeIndex(eval, 1, cleanup);
	*ret = eval;

	return 0;
}


/*
 *  tagbody
 */
static void push_tabletagbody(addr stack, addr tag)
{
	addr value, key, table;

	make_tabletagbody(NULL, &value, tag);
	GetConst(SYSTEM_TABLE_TAGBODY, &key);
	GetEvalStackTable(stack, &table);
	if (setplistplist_eql_heap(table, key, tag, value, &table))
		SetEvalStackTable(stack, table);
}

static void tagbody_push(addr stack, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Check(RefEvalParseType(pos) != EVAL_PARSE_TAG, "type error");
		GetEvalParse(pos, 0, &pos);
		push_tabletagbody(stack, pos);
	}
}

static int tagbody_allcons(Execute ptr, addr body, addr *ret)
{
	addr root, pos;

	for (root = Nil; body != Nil; ) {
		GetCons(body, &pos, &body);
		if (RefEvalParseType(pos) == EVAL_PARSE_TAG) {
			GetEvalParse(pos, 0, &pos);
			make_eval_scope(&pos, EVAL_PARSE_TAG, Nil, pos);
		}
		else {
			if (scope_eval(ptr, &pos, pos)) return 1;
		}
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static void tagbody_check(addr stack)
{
	addr cons, key, value;

	GetConst(SYSTEM_TABLE_TAGBODY, &cons);
	GetEvalStackTable(stack, &stack);
	getplist(stack, cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (getreference_tabletagbody(value) == 0) {
			/* fmtw("Unused tag ~S.", key, NULL); */
		}
	}
}

static int tagbody_execute(Execute ptr, addr tag, addr body, addr *ret)
{
	addr stack;

	stack = newstack_tagbody(ptr);
	tagbody_push(stack, tag);
	if (tagbody_allcons(ptr, body, ret)) return 1;
	tagbody_check(stack);
	freestack_eval(ptr, stack);

	return 0;
}

static int scope_tagbody(Execute ptr, addr *ret, addr eval)
{
	addr tag, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &body);

	if (tagbody_execute(ptr, tag, body, &body)) return 1;
	GetTypeTable(&type, Null);
	eval_scope_size(&eval, 2, EVAL_PARSE_TAGBODY, type, eval);
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, body);
	*ret = eval;

	return 0;
}

static int find_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_TAGBODY, &key);
	if (ret == NULL) ret = &key;
	return getplistplist_eql(stack, key, tag, ret) == 0;
}

static void push_closure_tagbody(addr stack, addr tag, addr value)
{
	addr key, table, temp;

	GetConst(SYSTEM_CLOSURE_TAGBODY, &key);
	GetEvalStackTable(stack, &table);
	if (getplistplist_eql(table, key, tag, &temp)) {
		/* not found */
		if (setplistplist_eql_heap(table, key, tag, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static int go_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (find_tabletagbody(stack, tag, ret)) {
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! go_tabletagbody(next, tag, ret)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_tagbody(stack, tag, *ret);
	}

	return 1;
}

static void go_execute(Execute ptr, addr *ret, addr tag)
{
	addr stack, table;

	getstack_eval(ptr, &stack);
	if (! go_tabletagbody(stack, tag, &table))
		fmte("Tag ~S is not found.", tag, NULL);
	setreference_tabletagbody(table, 1);
	*ret = table;
}

static void scope_go(Execute ptr, addr *ret, addr eval)
{
	addr tag, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	go_execute(ptr, &tag, tag);
	GetTypeTable(&type, Nil);
	make_eval_scope(ret, EVAL_PARSE_GO, type, tag);
}


/*
 *  block
 */
static void push_tableblock(addr stack, addr name)
{
	addr key, table;

	GetConst(SYSTEM_TABLE_BLOCK, &key);
	GetEvalStackTable(stack, &table);
	if (setplist_heap(table, key, name, &table))
		SetEvalStackTable(stack, table);
}

static int block_execute(Execute ptr,
		addr name, addr cons, addr *retcons, addr *rettype)
{
	addr stack;

	stack = newstack_block(ptr);
	push_tableblock(stack, name);
	if (scope_allcons(ptr, retcons, rettype, cons)) return 1;
	freestack_eval(ptr, stack);

	return 0;
}

static int scope_block(Execute ptr, addr *ret, addr eval)
{
	addr name, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &cons);

	if (block_execute(ptr, name, cons, &cons, &type)) return 1;
	/* type -> (or block return-from1 return-from2 ...) */
	GetTypeTable(&type, Asterisk);
	eval_scope_size(&eval, 2, EVAL_PARSE_BLOCK, type, eval);
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, cons);
	*ret = eval;

	return 0;
}

static void push_closure_block(addr stack, addr name)
{
	addr key, table;

	GetConst(SYSTEM_CLOSURE_BLOCK, &key);
	GetEvalStackTable(stack, &table);
	if (pushnewplist_heap(table, key, name, &table))
		SetEvalStackTable(stack, table);
}

static int find_tableblock(addr stack, addr name)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_BLOCK, &key);
	return getplist(stack, key, &key) == 0 && key == name;
}

static int name_tableblock(addr stack, addr name)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (find_tableblock(stack, name)) {
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! name_tableblock(next, name)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_block(stack, name);
	}

	return 1;
}

static int return_from_execute(Execute ptr, addr name, addr form, addr *ret)
{
	addr stack;

	getstack_eval(ptr, &stack);
	if (! name_tableblock(stack, name))
		fmte("Cannot find block name ~S.", name, NULL);
	return scope_eval(ptr, ret, form);
}

static int scope_return_from(Execute ptr, addr *ret, addr eval)
{
	addr name, form, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &form);

	if (return_from_execute(ptr, name, form, &form)) return 1;
	GetTypeTable(&type, Nil);
	eval_scope_size(&eval, 2, EVAL_PARSE_RETURN_FROM, type, eval);
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, form);
	*ret = eval;

	return 0;
}

static int scope_catch(Execute ptr, addr *ret, addr eval)
{
	addr tag, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	if (scope_eval(ptr, &tag, tag)) return 1;
	if (scope_allcons(ptr, &cons, &type, cons)) return 1;
	GetTypeTable(&type, Asterisk);
	eval_scope_size(&eval, 2, EVAL_PARSE_CATCH, type, eval);
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, cons);
	*ret = eval;

	return 0;
}

static int scope_throw(Execute ptr, addr *ret, addr eval)
{
	addr tag, form, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &form);

	if (scope_eval(ptr, &tag, tag)) return 1;
	if (scope_eval(ptr, &form, form)) return 1;
	GetTypeTable(&type, Nil);
	eval_scope_size(&eval, 2, EVAL_PARSE_THROW, type, eval);
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, form);
	*ret = eval;

	return 0;
}

static int eval_when_check(Execute ptr, addr compilep, addr loadp, addr evalp)
{
	addr when, check;

	/* eval :execute */
	getevalwhen_eval(ptr, &when);
	GetConst(COMMON_EVAL, &check);
	if (when == check && evalp == T) return 1;

	/* toplevel */
	gettoplevel_eval(ptr, &check);
	if (check == Nil) return 0;

	/* load :load-toplevel */
	GetConst(COMMON_LOAD, &check);
	if (when == check && loadp == T) return 1;

	/* compile :compile-toplevel */
	GetConst(COMMON_COMPILE, &check);
	if (when == check && compilep == T) return 1;

	return 0;
}

static int scope_eval_when(Execute ptr, addr *ret, addr eval)
{
	addr cons, type, compilep, loadp, evalp;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &cons);
	GetEvalParse(eval, 1, &compilep);
	GetEvalParse(eval, 2, &loadp);
	GetEvalParse(eval, 3, &evalp);

	if (eval_when_check(ptr, compilep, loadp, evalp)) {
		if (scope_allcons(ptr, &cons, &type, cons)) return 1;
		make_eval_scope(&eval, EVAL_PARSE_PROGN, type, cons);
	}
	else {
		type0_heap(LISPDECL_EMPTY, &type);
		make_eval_scope(&eval, EVAL_PARSE_NIL, type, Nil);
	}
	*ret = eval;

	return 0;
}

/* multiple-value-bind */
struct mvbind_struct {
	addr stack, args, decl, doc, cons, free, the;
};

static void mvbind_maketable(Execute ptr, struct mvbind_struct *str)
{
	addr stack, decl, args, var;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		check_scope_variable(var);
		ifdeclvalue(ptr, stack, var, decl, NULL);
	}
}

static void mvbind_update(Execute ptr, struct mvbind_struct *str)
{
	addr stack, args, key, root, var;

	stack = str->stack;
	args = str->args;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TABLE_VALUE, &key);
	getplist(stack, key, &stack);

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		tablevalue_update(stack, &var, var);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(&str->args, root);
}

static int mvbind_execute(Execute ptr, struct mvbind_struct *str)
{
	addr stack;

	stack = str->stack;
	mvbind_maketable(ptr, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	if (scope_allcons(ptr, &str->cons, &str->the, str->cons)) return 1;
	ignore_checkvalue(stack);
	mvbind_update(ptr, str);

	return 0;
}

static int scope_multiple_value_bind(Execute ptr, addr *ret, addr eval)
{
	struct mvbind_struct str;
	addr expr;

	Check(! eval_parse_p(eval), "type error");
	memset(&str, 0, sizeoft(struct mvbind_struct));
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);

	if (scope_eval(ptr, &expr, expr)) return 1;
	str.stack = newstack_nil(ptr);
	if (mvbind_execute(ptr, &str)) return 1;
	freestack_eval(ptr, str.stack);

	eval_scope_size(&eval, 6, EVAL_PARSE_MULTIPLE_VALUE_BIND, str.the, eval);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, expr);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.doc);
	SetEvalScopeIndex(eval, 4, str.cons);
	SetEvalScopeIndex(eval, 5, str.free);
	*ret = eval;

	return 0;
}


/* multiple-value-call */
static int function_result_type(addr expr, addr *ret)
{
	GetEvalScopeThe(expr, &expr);
	if (! type_function_p(expr)) return 1;
	GetArrayType(expr, 1, ret); /* result */

	return 0;
}

static int scope_multiple_value_call(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);

	if (scope_eval(ptr, &expr, expr)) return 1;
	if (scope_allcons(ptr, &cons, NULL, cons)) return 1;
	if (function_result_type(expr, &type))
		GetTypeTable(&type, Asterisk);
	eval_scope_size(&eval, 2, EVAL_PARSE_MULTIPLE_VALUE_CALL, type, eval);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* multiple-value-prog1 */
static int scope_multiple_value_prog1(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);

	if (scope_eval(ptr, &expr, expr)) return 1;
	if (scope_allcons(ptr, &cons, NULL, cons)) return 1;
	GetEvalScopeThe(expr, &type);
	eval_scope_size(&eval, 2, EVAL_PARSE_MULTIPLE_VALUE_PROG1, type, eval);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	*ret = eval;

	return 0;
}

/* nth-value */
static int scope_nth_value(Execute ptr, addr *ret, addr eval)
{
	addr nth, expr, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &nth);
	GetEvalParse(eval, 1, &expr);

	if (scope_eval(ptr, &nth, nth)) return 1;
	if (scope_eval(ptr, &expr, expr)) return 1;
	GetEvalScopeThe(expr, &type);
	eval_scope_size(&eval, 2, EVAL_PARSE_NTH_VALUE, type, eval);
	SetEvalScopeIndex(eval, 0, nth);
	SetEvalScopeIndex(eval, 1, expr);
	*ret = eval;

	return 0;
}

/* progv */
static int scope_progv(Execute ptr, addr *ret, addr eval)
{
	addr symbols, values, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbols);
	GetEvalParse(eval, 1, &values);
	GetEvalParse(eval, 2, &body);

	if (scope_eval(ptr, &symbols, symbols)) return 1;
	if (scope_eval(ptr, &values, values)) return 1;
	if (scope_allcons(ptr, &body, &type, body)) return 1;
	eval_scope_size(&eval, 3, EVAL_PARSE_PROGV, type, eval);
	SetEvalScopeIndex(eval, 0, symbols);
	SetEvalScopeIndex(eval, 1, values);
	SetEvalScopeIndex(eval, 2, body);
	*ret = eval;

	return 0;
}


/*
 *  eval-scope
 */
static int scope_eval_downcase(Execute ptr, addr *ret, addr eval)
{
	switch (RefEvalParseType(eval)) {
		case EVAL_PARSE_NIL:
			scope_nil(ret, eval);
			break;

		case EVAL_PARSE_T:
			scope_t(ret, eval);
			break;

		case EVAL_PARSE_INTEGER:
			scope_integer(ret, eval);
			break;

		case EVAL_PARSE_RATIONAL:
			scope_rational(ret, eval);
			break;

		case EVAL_PARSE_COMPLEX:
			scope_complex(ret, eval);
			break;

		case EVAL_PARSE_CHARACTER:
			scope_character(ret, eval);
			break;

		case EVAL_PARSE_ARRAY:
			scope_array(ret, eval);
			break;

		case EVAL_PARSE_VECTOR:
			scope_vector(ret, eval);
			break;

		case EVAL_PARSE_BITVECTOR:
			scope_bitvector(ret, eval);
			break;

		case EVAL_PARSE_STRING:
			scope_string(ret, eval);
			break;

		case EVAL_PARSE_FLOAT:
			scope_float(ret, eval);
			break;

		case EVAL_PARSE_QUOTE:
			scope_quote(ret, eval);
			break;

		case EVAL_PARSE_PATHNAME:
			scope_pathname(ret, eval);
			break;

		case EVAL_PARSE_DECLAIM:
			scope_declaim(ptr, ret, eval);
			break;

		case EVAL_PARSE_LET:
			return scope_let(ptr, ret, eval);

		case EVAL_PARSE_LETA:
			return scope_leta(ptr, ret, eval);

		case EVAL_PARSE_SYMBOL:
			return scope_symbol(ptr, ret, eval);

		case EVAL_PARSE_SETQ:
			return scope_setq(ptr, ret, eval);

		case EVAL_PARSE_FUNCTION:
			scope_function(ptr, ret, eval);
			break;

		case EVAL_PARSE_LAMBDA:
			return scope_lambda(ptr, ret, eval);

		case EVAL_PARSE_DEFUN:
			return scope_defun(ptr, ret, eval);

		case EVAL_PARSE_MACRO_LAMBDA:
			return scope_macro_lambda(ptr, ret, eval);

		case EVAL_PARSE_DEFMACRO:
			scope_defmacro(ptr, ret, eval);
			break;

		case EVAL_PARSE_DEFTYPE:
			return scope_deftype(ptr, ret, eval);

		case EVAL_PARSE_DESTRUCTURING_BIND:
			return scope_destructuring_bind(ptr, ret, eval);

		case EVAL_PARSE_DEFINE_SYMBOL_MACRO:
			scope_define_symbol_macro(ptr, ret, eval);
			break;

		case EVAL_PARSE_SYMBOL_MACROLET:
			return scope_symbol_macrolet(ptr, ret, eval);

		case EVAL_PARSE_FLET:
			return scope_flet(ptr, ret, eval);

		case EVAL_PARSE_LABELS:
			return scope_labels(ptr, ret, eval);

		case EVAL_PARSE_CALL:
			return scope_call(ptr, ret, eval);

		case EVAL_PARSE_VALUES:
			return scope_values(ptr, ret, eval);

		case EVAL_PARSE_THE:
			return scope_the(ptr, ret, eval);

		case EVAL_PARSE_IF:
			return scope_if(ptr, ret, eval);

		case EVAL_PARSE_UNWIND_PROTECT:
			return scope_unwind_protect(ptr, ret, eval);

		case EVAL_PARSE_TAGBODY:
			return scope_tagbody(ptr, ret, eval);

		case EVAL_PARSE_GO:
			scope_go(ptr, ret, eval);
			break;

		case EVAL_PARSE_BLOCK:
			return scope_block(ptr, ret, eval);

		case EVAL_PARSE_RETURN_FROM:
			return scope_return_from(ptr, ret, eval);

		case EVAL_PARSE_CATCH:
			return scope_catch(ptr, ret, eval);

		case EVAL_PARSE_THROW:
			return scope_throw(ptr, ret, eval);

		case EVAL_PARSE_MULTIPLE_VALUE_BIND:
			return scope_multiple_value_bind(ptr, ret, eval);

		case EVAL_PARSE_MULTIPLE_VALUE_CALL:
			return scope_multiple_value_call(ptr, ret, eval);

		case EVAL_PARSE_MULTIPLE_VALUE_PROG1:
			return scope_multiple_value_prog1(ptr, ret, eval);

		case EVAL_PARSE_NTH_VALUE:
			return scope_nth_value(ptr, ret, eval);

		case EVAL_PARSE_PROGV:
			return scope_progv(ptr, ret, eval);

		default:
			fmte("Invalid eval-parse type.", NULL);
			break;
	}

	return 0;
}

static int scope_eval_downlevel(Execute ptr, addr *ret, addr eval)
{
	addr symbol, value;

	symbol_toplevel_eval(&symbol);
	getspecialcheck_local(ptr, symbol, &value);
	setspecial_local(ptr, symbol, Nil);
	if (scope_eval_downcase(ptr, ret, eval)) return 1;
	setspecial_local(ptr, symbol, value);

	return 0;
}

static int scope_eval(Execute ptr, addr *ret, addr eval)
{
	Check(GetType(eval) != LISPTYPE_EVAL, "type error");
	Check(RefEvalType(eval) != EVAL_TYPE_PARSE, "eval type error");

	/* toplevel form */
	switch (RefEvalParseType(eval)) {
		case EVAL_PARSE_PROGN:
			return scope_progn(ptr, ret, eval);

		case EVAL_PARSE_LOCALLY:
			return scope_locally(ptr, ret, eval);

		case EVAL_PARSE_EVAL_WHEN:
			return scope_eval_when(ptr, ret, eval);

		default:
			return scope_eval_downlevel(ptr, ret, eval);
	}

	return 0;
}

int eval_scope(Execute ptr, addr *ret, addr eval)
{
	addr control;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	init_eval_stack(ptr);
	if (scope_eval(ptr, ret, eval))
		return runcode_free_control(ptr, control);
	free_eval_stack(ptr);
	return free_control(ptr, control);
}

