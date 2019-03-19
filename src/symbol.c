#include "bignum.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "number.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"

#ifdef LISP_THREAD_ENABLE
#define SYMSTACK_SIZE       4
#else
#define SYMSTACK_SIZE       1
#endif

static int InitObject = 0;
static rwlocklite MutexSymbol;

int init_symbol(void)
{
	if (InitObject) {
		Debug("InitObject error.");
		return 1;
	}
	if (make_rwlocklite(&MutexSymbol)) {
		Debug("make_mutexlite error");
		return 1;
	}
	InitObject = 1;

	return 0;
}

void free_symbol(void)
{
	if (InitObject) {
		destroy_rwlocklite(&MutexSymbol);
		InitObject = 0;
	}
}

void build_symbol(void)
{
	addr symbol, value;

	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	fixnum_heap(&value, 0);
	SetValueSymbol(symbol, value);
}


/*
 *  symbol
 */
addr symbol_allocr(LocalRoot local)
{
	int i;
	addr pos, make, stack;

	/* symbol object */
	alloc_symbol(local, &pos);

	/* Unbound */
	SetValueSymbol_Low(pos, Nil);
	SetFunctionSymbol_Low(pos, Unbound);

	/* stack */
	alloc_array4(local, &make, LISPSYSTEM_SYMSTACK, SYMSTACK_SIZE);
	for (i = 0; i < SYMSTACK_SIZE; i++) {
		alloc_array2(local, &stack, LISPSYSTEM_SYMARRAY, SYMBOL_STACK_SIZE);
		SetArrayA4(make, i, stack);
	}
	SetStackSymbol_Low(pos, make);

	return pos;
}
void symbol_alloc(LocalRoot local, addr *ret)
{
	*ret = symbol_allocr(local);
}
addr symbol_heapr(void)
{
	return symbol_allocr(NULL);
}
void symbol_heap(addr *ret)
{
	*ret = symbol_allocr(NULL);
}
addr symbol_localr(LocalRoot local)
{
	Check(local == NULL, "local error");
	return symbol_allocr(local);
}
void symbol_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	*ret = symbol_allocr(local);
}

int symbolp(addr pos)
{
	return GetType(pos) == LISPTYPE_SYMBOL || pos == Nil || pos == T;
}

int keywordp(addr pos)
{
	addr keyword;

	if (GetType(pos) != LISPTYPE_SYMBOL) return 0;
	GetConst(PACKAGE_KEYWORD, &keyword);
	Check(GetType(keyword) != LISPTYPE_PACKAGE, "package error");
	GetPackageSymbol(pos, &pos);

	return pos == keyword;
}

static void setcheck_symbol(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		fmte("Cannot set the constant variable ~S.", symbol, NULL);
}

#define SetSymbol(s,i,v) { \
	Check(! IsSymbol(s), "type error"); \
	setcheck_symbol(symbol); \
	SetArrayA2(s, i, v); \
}
#define RefSymbol(s,i) { \
	Check(! IsSymbol(s), "type error"); \
	return RefArrayA2(s, i); \
}
#define GetSymbol(s,i,v) { \
	Check(! IsSymbol(s), "type error"); \
	GetArrayA2(s, i, v); \
}

addr refname_symbol(addr symbol)
{
	RefSymbol(symbol, SYMBOL_INDEX_NAME);
}
void getname_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_NAME, value);
}
void setname_symbol(addr symbol, addr value)
{
	Check(! stringp(value), "type error");
	SetSymbol(symbol, SYMBOL_INDEX_NAME, value);
}

addr refvalue_symbol(addr symbol)
{
	GetSymbol(symbol, SYMBOL_INDEX_VALUE, &symbol);
	if (symbol == Nil)
		return Unbound;
	else
		return RefCar(symbol);
}
void getvalue_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_VALUE, &symbol);
	if (symbol == Nil)
		*value = Unbound;
	else
		GetCar(symbol, value);
}
void setvalue_symbol(addr symbol, addr value)
{
	addr cons;

	GetSymbol(symbol, SYMBOL_INDEX_VALUE, &cons);
	if (cons == Nil) {
		conscar_heap(&cons, value);
		SetSymbol(symbol, SYMBOL_INDEX_VALUE, cons);
	}
	else {
		SetCar(cons, value);
	}
}

addr reffunction_symbol(addr symbol)
{
	RefSymbol(symbol, SYMBOL_INDEX_FUNCTION);
}
void getfunction_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_FUNCTION, value);
}
void setfunction_symbol(addr symbol, addr value)
{
	SetSymbol(symbol, SYMBOL_INDEX_FUNCTION, value);
}

addr refpackage_symbol(addr symbol)
{
	RefSymbol(symbol, SYMBOL_INDEX_PACKAGE);
}
void getpackage_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_PACKAGE, value);
}
void setpackage_symbol(addr symbol, addr value)
{
	Check(value != Nil && GetType(value) != LISPTYPE_PACKAGE, "type error");
	SetSymbol(symbol, SYMBOL_INDEX_PACKAGE, value);
}

addr refplist_symbol(addr symbol)
{
	RefSymbol(symbol, SYMBOL_INDEX_PLIST);
}
void getplist_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_PLIST, value);
}
void setplist_symbol(addr symbol, addr value)
{
	Check(! listp(value), "type error");
	SetSymbol(symbol, SYMBOL_INDEX_PLIST, value);
}

static void getinfo_constant(addr symbol, enum CONSTANT_INDEX index, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, index, &symbol)? Nil: symbol;
}
static void setinfo_constant(addr symbol, enum CONSTANT_INDEX index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	setcheck_symbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}
static void reminfo_constant(addr symbol, enum CONSTANT_INDEX index)
{
	addr plist;

	CheckSymbol(symbol);
	setcheck_symbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

/* type */
void gettype_value_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, ret);
}
void settype_value_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, value);
}
void remtype_value_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_VALUE);
}

void gettype_function_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, ret);
}
void settype_function_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
void remtype_function_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION);
}

void gettype_setf_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF, ret);
}
void settype_setf_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_SETF, value);
}
void remtype_setf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_SETF);
}

/* inline */
int inlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
void setinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
int notinlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
void setnotinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
void reminline_function_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION);
}

int inlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
void setinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
int notinlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
void setnotinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
void reminline_setf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF);
}

/* setf */
addr refsetf_symbol(addr symbol)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	return getplist_constant(symbol, CONSTANT_COMMON_SETF, &symbol)? Unbound: symbol;
}
void getsetf_symbol(addr symbol, addr *value)
{
	*value = refsetf_symbol(symbol);
}
void getsetfcheck_symbol(addr symbol, addr *value)
{
	*value = refsetf_symbol(symbol);
	if (*value == Unbound)
		undefined_function_setf(symbol);
}
void setsetf_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SETF, value);
}
void remsetf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_SETF);
}

/* setf-macro */
addr refsetfmacro_symbol(addr symbol)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	return getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER,
			&symbol)? Unbound: symbol;
}
void getsetfmacro_symbol(addr symbol, addr *value)
{
	*value = refsetfmacro_symbol(symbol);
}
void setsetfmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
void remsetfmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER);
}


/* macro */
addr refmacro_symbol(addr symbol)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	return getplist_constant(symbol, CONSTANT_COMMON_DEFMACRO, &symbol)?
		Unbound: symbol;
}
void getmacro_symbol(addr symbol, addr *value)
{
	*value = refmacro_symbol(symbol);
}
void setmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
void remmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFMACRO);
}

/* symbol-macro */
addr refsymbol_macro_symbol(addr symbol)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	return getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, &symbol)?
		Unbound: symbol;
}
void evalsymbol_macro_symbol(addr symbol, addr *ret)
{
	*ret = refsymbol_macro_symbol(symbol);
	if (*ret != Unbound)
		GetCar(*ret, ret);
}
void formsymbol_macro_symbol(addr symbol, addr *ret)
{
	*ret = refsymbol_macro_symbol(symbol);
	if (*ret != Unbound)
		GetCdr(*ret, ret);
}
void setsymbol_macro_symbol(addr symbol, addr eval, addr form)
{
	cons_heap(&eval, eval, form);
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, eval);
}
void remsymbol_macro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO);
}

/* scope */
addr refscope_symbol(addr symbol)
{
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, &symbol);
	return symbol;
}
void getscope_symbol(addr symbol, addr *value)
{
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
void setscope_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
void setspecial_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, T);
}
void setlexical_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, Nil);
}
int specialp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	return refscope_symbol(symbol) != Nil;
}
int lexicalp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	return refscope_symbol(symbol) == Nil;
}

void set_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, T);
}
int get_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, &symbol);
	return symbol == T;
}

void getdocument_variable_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, ret);
}
void setdocument_variable_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, value);
}
void getdocument_type_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, ret);
}
void setdocument_type_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, value);
}

void getdeftype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, ret);
}
void setdeftype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(value != Nil && GetType(value) != LISPTYPE_FUNCTION, "type error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, value);
}
void remdeftype_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE);
}

static void setinfo_force(addr symbol, enum CONSTANT_INDEX index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_force(symbol, plist);
}

void getsymboltype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, ret);
}

void setsymboltype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_force(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, value);
}

void getlisttype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, ret);
}

void setlisttype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_force(symbol, CONSTANT_SYSTEM_TYPE_LIST, value);
}


/*
 *  symstack
 */
static void realloc_symbol(addr *stack, size_t size1, size_t size2)
{
	addr make, old, temp;
	byte32 size32;
	size_t i;

	old = *stack;
	size32 = (byte32)size2;
	if ((size_t)size32 <= size1)
		Abort("size error");

	heap_array4(&make, LISPSYSTEM_SYMARRAY, size32);
	for (i = 0; i < size1; i++) {
		GetArrayA4(old, i, &temp);
		SetArrayA4(make, i, temp);
		SetArrayA4(old, i, Nil);
	}
	*stack = make;
}

static void symstack(size_t index, addr symbol, addr *ret)
{
	addr stack, child;
	size_t i, size, size2;

	Check(GetStatusDynamic(symbol), "dynamic error.");
	rdlock_rwlocklite(&MutexSymbol);
	stack = PtrArrayA2(symbol)[SYMBOL_INDEX_STACK];
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = PtrArrayA4(stack)[index];
		unrdlock_rwlocklite(&MutexSymbol);
		return;
	}
	else {
		/* write lock */
		unrdlock_rwlocklite(&MutexSymbol);
		wrlock_rwlocklite(&MutexSymbol);
		/* reload */
		stack = PtrArrayA2(symbol)[SYMBOL_INDEX_STACK];
		size = GetLenArrayA4(stack);
		if (index < size) {
			*ret = PtrArrayA4(stack)[index];
			unwrlock_rwlocklite(&MutexSymbol);
			return;
		}

		/* size extension */
		for (size2 = size; size2 <= index; )
			size2 <<= 1;
		realloc_symbol(&stack, size, size2);
		PtrArrayA2(symbol)[SYMBOL_INDEX_STACK] = stack;
		for (i = size; i < size2; i++) {
			heap_array2(&child, LISPSYSTEM_SYMSTACK, SYMBOL_STACK_SIZE);
			SetArrayA4(stack, i, child);
		}
		*ret = PtrArrayA4(stack)[index];
		unwrlock_rwlocklite(&MutexSymbol);
	}
}

static void pushsymlocal(Execute ptr, addr pos, addr value, int index)
{
	addr cons, root, stack;

	Check(! IsSymbol(pos), "type error");
	setcheck_symbol(pos);
	Check(SYMBOL_STACK_SIZE <= index, "index error");

	symstack(ptr->index, pos, &root);
	GetArrayA2(root, index, &stack);
	cons_local(ptr->local, &cons, value, stack);
	SetArrayA2_force(root, index, cons);
}
void pushlexical_closure_unsafe(Execute ptr, addr pos, addr cons)
{
	pushsymlocal(ptr, pos, cons, SYMBOL_STACK_LEXICAL);
}
void pushlexical_unsafe(Execute ptr, addr pos, addr value)
{
	conscar_heap(&value, value);
	pushsymlocal(ptr, pos, value, SYMBOL_STACK_LEXICAL);
}
void pushspecial_unsafe(Execute ptr, addr pos, addr value)
{
	pushsymlocal(ptr, pos, value, SYMBOL_STACK_SPECIAL);
}
void pushfunction_unsafe(Execute ptr, addr pos, addr value)
{
	pushsymlocal(ptr, pos, value, SYMBOL_STACK_FUNCTION);
}
void pushsetf_unsafe(Execute ptr, addr pos, addr value)
{
	pushsymlocal(ptr, pos, value, SYMBOL_STACK_SETF);
}

static void popsymlocal(Execute ptr, addr pos, int index)
{
	addr root, stack;

	Check(! IsSymbol(pos), "type error");
	setcheck_symbol(pos);
	Check(SYMBOL_STACK_SIZE <= index, "index error");

	symstack(ptr->index, pos, &root);
	GetArrayA2(root, index, &stack);
	Check(stack == Nil, "symstack is already empty.");
	GetCdr(stack, &stack);
	SetArrayA2_force(root, index, stack);
}
void poplexical_unsafe(Execute ptr, addr pos)
{
	popsymlocal(ptr, pos, SYMBOL_STACK_LEXICAL);
}
void popspecial_unsafe(Execute ptr, addr pos)
{
	popsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL);
}
void popfunction_unsafe(Execute ptr, addr pos)
{
	popsymlocal(ptr, pos, SYMBOL_STACK_FUNCTION);
}
void popsetf_unsafe(Execute ptr, addr pos)
{
	popsymlocal(ptr, pos, SYMBOL_STACK_SETF);
}

static void snapshot_symlocal(Execute ptr, addr pos, int index, addr *ret)
{
	Check(! IsSymbol(pos), "type error");
	Check(SYMBOL_STACK_SIZE <= index, "index error");
	symstack(ptr->index, pos, &pos);
	GetArrayA2(pos, index, ret);
}
void snapshot_lexical_local(Execute ptr, addr pos, addr *ret)
{
	snapshot_symlocal(ptr, pos, SYMBOL_STACK_LEXICAL, ret);
}
void snapshot_special_local(Execute ptr, addr pos, addr *ret)
{
	snapshot_symlocal(ptr, pos, SYMBOL_STACK_SPECIAL, ret);
}
void snapshot_function_local(Execute ptr, addr pos, addr *ret)
{
	snapshot_symlocal(ptr, pos, SYMBOL_STACK_FUNCTION, ret);
}
void snapshot_setf_local(Execute ptr, addr pos, addr *ret)
{
	snapshot_symlocal(ptr, pos, SYMBOL_STACK_SETF, ret);
}

static void rollback_symlocal(Execute ptr, addr pos, int index, addr cons)
{
#ifdef LISP_DEBUG
	addr root;
#endif
	Check(! IsSymbol(pos), "type error");
	Check(SYMBOL_STACK_SIZE <= index, "index error");
	symstack(ptr->index, pos, &pos);
	if (cons == Nil) {
		SetArrayA2(pos, index, Nil);
		return;
	}

#ifdef LISP_DEBUG
	GetArrayA2(pos, index, &root);
	while (root != cons) {
		Check(root == Nil, "rollback_symlocal error");
		GetCdr(root, &root);
	}
#endif
	SetArrayA2_force(pos, index, cons);
}
void rollback_lexical_local(Execute ptr, addr pos, addr cons)
{
	rollback_symlocal(ptr, pos, SYMBOL_STACK_LEXICAL, cons);
}
void rollback_special_local(Execute ptr, addr pos, addr cons)
{
	rollback_symlocal(ptr, pos, SYMBOL_STACK_SPECIAL, cons);
}
void rollback_function_local(Execute ptr, addr pos, addr cons)
{
	rollback_symlocal(ptr, pos, SYMBOL_STACK_FUNCTION, cons);
}
void rollback_setf_local(Execute ptr, addr pos, addr cons)
{
	rollback_symlocal(ptr, pos, SYMBOL_STACK_SETF, cons);
}

static void clearsymlocal(Execute ptr, addr pos, int index)
{
	addr root;

	Check(! IsSymbol(pos), "type error");
	setcheck_symbol(pos);
	Check(SYMBOL_STACK_SIZE <= index, "index error");

	symstack(ptr->index, pos, &root);
	SetArrayA2(root, index, Nil);
}
void clearlexical_local(Execute ptr, addr pos)
{
	clearsymlocal(ptr, pos, SYMBOL_STACK_LEXICAL);
}
void clearspecial_local(Execute ptr, addr pos)
{
	clearsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL);
}
void clearfunction_local(Execute ptr, addr pos)
{
	clearsymlocal(ptr, pos, SYMBOL_STACK_FUNCTION);
}
void clearsetf_local(Execute ptr, addr pos)
{
	clearsymlocal(ptr, pos, SYMBOL_STACK_SETF);
}

static int getsymlocal(Execute ptr, addr root, int index, addr *ret)
{
	Check(! IsSymbol(root), "type error");
	Check(SYMBOL_STACK_SIZE <= index, "index error");

	symstack(ptr->index, root, &root);
	GetArrayA2(root, index, &root);
	if (root == Nil) return 1;  /* not found */
	GetCar(root, ret);

	return 0;  /* found */
}

static int getsymlexical(Execute ptr, addr root, addr *ret)
{
	addr check;

	if (getsymlocal(ptr, root, SYMBOL_STACK_LEXICAL, &root))
		return 1;
	GetCar(root, &check);
	if (check == Unbound)
		return 1;
	*ret = root;

	return 0;
}

void conslexical_local(Execute ptr, addr pos, addr *ret)
{
	Check(! IsSymbol(pos), "type error");
	if (getsymlexical(ptr, pos, ret)) {
		GetSymbol(pos, SYMBOL_INDEX_VALUE, &pos);
		*ret = (pos == Nil)? Unbound: pos;
	}
}
void getlexical_local(Execute ptr, addr pos, addr *ret)
{
	addr cons;

	Check(! IsSymbol(pos), "type error");
	if (! getsymlexical(ptr, pos, &cons)) {
		GetCar(cons, ret);
		return;
	}
	if (getsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL, ret))
		GetValueSymbol(pos, ret);
}
void getspecial_local(Execute ptr, addr pos, addr *ret)
{
	Check(! IsSymbol(pos), "type error");
	if (getsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL, ret))
		GetValueSymbol(pos, ret);
}
void getfunction_local(Execute ptr, addr pos, addr *ret)
{
	Check(! IsSymbol(pos), "type error");
	if (getsymlocal(ptr, pos, SYMBOL_STACK_FUNCTION, ret))
		GetFunctionSymbol(pos, ret);
}
void getsetf_local(Execute ptr, addr pos, addr *ret)
{
	Check(! IsSymbol(pos), "type error");
	if (getsymlocal(ptr, pos, SYMBOL_STACK_SETF, ret))
		getsetf_symbol(pos, ret);
}

addr reflexical_local(Execute ptr, addr pos)
{
	getlexical_local(ptr, pos, &pos);
	return pos;
}
addr refspecial_local(Execute ptr, addr pos)
{
	getspecial_local(ptr, pos, &pos);
	return pos;
}
addr reffunction_local(Execute ptr, addr pos)
{
	getfunction_local(ptr, pos, &pos);
	return pos;
}
addr refsetf_local(Execute ptr, addr pos)
{
	getsetf_local(ptr, pos, &pos);
	return pos;
}

void conslexicalcheck_local(Execute ptr, addr pos, addr *ret)
{
	conslexical_local(ptr, pos, ret);
	if (*ret == Unbound) unbound_variable(pos);
}
void getlexicalcheck_local(Execute ptr, addr pos, addr *ret)
{
	getlexical_local(ptr, pos, ret);
	if (*ret == Unbound) unbound_variable(pos);
}
void getspecialcheck_local(Execute ptr, addr pos, addr *ret)
{
	getspecial_local(ptr, pos, ret);
	if (*ret == Unbound) unbound_variable(pos);
}
void getfunctioncheck_local(Execute ptr, addr pos, addr *ret)
{
	getfunction_local(ptr, pos, ret);
	if (*ret == Unbound) undefined_function(pos);
}
void getsetfcheck_local(Execute ptr, addr pos, addr *ret)
{
	addr setf;

	getsetf_local(ptr, pos, ret);
	if (*ret == Unbound) {
		GetConst(COMMON_SETF, &setf);
		list_heap(&pos, setf, pos, NULL);
		undefined_function(pos);
	}
}
addr reflexicalcheck_local(Execute ptr, addr pos)
{
	getlexicalcheck_local(ptr, pos, &pos);
	return pos;
}
addr refspecialcheck_local(Execute ptr, addr pos)
{
	getspecialcheck_local(ptr, pos, &pos);
	return pos;
}
addr reffunctioncheck_local(Execute ptr, addr pos)
{
	getfunctioncheck_local(ptr, pos, &pos);
	return pos;
}
addr refsetfcheck_local(Execute ptr, addr pos)
{
	getsetfcheck_local(ptr, pos, &pos);
	return pos;
}

static int setsymlocal(Execute ptr, addr root, int index, addr value)
{
	Check(! IsSymbol(root), "type error");
	setcheck_symbol(root);
	Check(SYMBOL_STACK_SIZE <= index, "index error");

	symstack(ptr->index, root, &root);
	GetArrayA2(root, index, &root);
	if (root == Nil) return 1;  /* not found */
	SetCar(root, value);

	return 0;  /* found */
}
void setlexical_local(Execute ptr, addr pos, addr value)
{
	addr cons;

	Check(! IsSymbol(pos), "type error");
	if (! getsymlexical(ptr, pos, &cons)) {
		SetCar(cons, value);
		return;
	}
	if (setsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL, value))
		SetValueSymbol(pos, value);
}
void setspecial_local(Execute ptr, addr pos, addr value)
{
	Check(! IsSymbol(pos), "type error");
	if (setsymlocal(ptr, pos, SYMBOL_STACK_SPECIAL, value))
		SetValueSymbol(pos, value);
}
void setfunction_local(Execute ptr, addr pos, addr value)
{
	Check(! IsSymbol(pos), "type error");
	if (setsymlocal(ptr, pos, SYMBOL_STACK_FUNCTION, value)) {
		Abort("setfunction error");
		return;
	}
}
void setsetf_local(Execute ptr, addr pos, addr value)
{
	Check(! IsSymbol(pos), "type error");
	if (setsymlocal(ptr, pos, SYMBOL_STACK_SETF, value)) {
		Abort("setsetf error");
		return;
	}
}


/*
 *  gensym
 */
int gensymp(addr pos)
{
	if (GetType(pos) != LISPTYPE_SYMBOL) return 0;
	GetPackageSymbol(pos, &pos);
	return pos == Nil;
}

void make_symbolchar(addr *ret, const char *str)
{
	addr pos, name;

	symbol_heap(&pos);
	strvect_char_heap(&name, str);
	SetNameSymbol(pos, name);
	*ret = pos;
}

static void make_gensym_argument(Execute ptr,
		const char *prefix1, addr prefix2, addr counter, addr *ret)
{
	addr symbol, value, queue, name, gensym;
	LocalRoot local;
	LocalStack stack;

	/* symbol-name */
	local = ptr->local;
	push_local(local, &stack);
	if (counter == NULL) {
		GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
		getspecialcheck_local(ptr, symbol, &value);
	}
	else {
		value = counter;
	}
	Check(! integerp(value), "type error");
	charqueue_local(local, &queue, 1 + 16);
	if (prefix1)
		pushchar_charqueue_local(local, queue, prefix1);
	else
		pushstring_charqueue_local(local, queue, prefix2);
	decimal_charqueue_integer_local(local, value, queue);
	make_charqueue_heap(queue, &name);
	rollback_local(local, stack);

	/* gensym */
	symbol_heap(&gensym);
	SetNameSymbol(gensym, name);
	*ret = gensym;

	/* (1+ *gensym-counter*) */
	if (counter == NULL) {
		oneplus_integer_common(local, value, &value);
		setspecial_local(ptr, symbol, value);
	}
}
void make_gensym(Execute ptr, addr *ret)
{
	make_gensym_argument(ptr, "G", NULL, NULL, ret);
}
void make_gensym_prefix(Execute ptr, addr prefix, addr *ret)
{
	Check(! stringp(prefix), "type error");
	make_gensym_argument(ptr, NULL, prefix, NULL, ret);
}
void make_gensym_integer(Execute ptr, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	make_gensym_argument(ptr, "G", NULL, value, ret);
}
void make_gensym_char(Execute ptr, const char *str, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	make_gensym_argument(ptr, str, NULL, value, ret);
}

void setcounter_gensym(Execute ptr, fixnum value)
{
	addr pos, symbol;

	Check(value < 0, "value error");
	fixnum_heap(&pos, value);
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	setspecial_local(ptr, symbol, pos);
}

