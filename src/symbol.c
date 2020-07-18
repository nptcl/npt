#include "bignum_output.h"
#include "character_queue.h"
#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "constant.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "number.h"
#include "object.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

#ifdef LISP_THREAD_ENABLE
#define SYMSTACK_SIZE       4
#else
#define SYMSTACK_SIZE       1
#endif

static int InitObject = 0;
static rwlocklite MutexSymbol;

_g int init_symbol(void)
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

_g void free_symbol(void)
{
	if (InitObject) {
		destroy_rwlocklite(&MutexSymbol);
		InitObject = 0;
	}
}

_g void build_symbol(void)
{
	addr symbol, value;

	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	fixnum_heap(&value, 0);
	SetValueSymbol(symbol, value);
}


/*
 *  symbol
 */
_g void symbol_alloc(LocalRoot local, addr *ret)
{
	int i;
	addr pos, make;

	/* symbol object */
	alloc_symbol(local, &pos);

	/* Unbound */
	SetValueSymbol_Low(pos, Nil);
	SetFunctionSymbol_Low(pos, Unbound);

	/* stack */
	alloc_array4(local, &make, LISPSYSTEM_SYMSTACK, SYMSTACK_SIZE);
	for (i = 0; i < SYMSTACK_SIZE; i++) {
		SetArrayA4(make, i, NULL);
	}
	SetSpecialSymbol_Low(pos, make);
	*ret = pos;
}

_g void symbol_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	symbol_alloc(local, ret);
}

_g void symbol_heap(addr *ret)
{
	symbol_alloc(NULL, ret);
}

_g int symbolp(addr pos)
{
	return GetType(pos) == LISPTYPE_SYMBOL || pos == Nil || pos == T;
}

_g int keywordp(addr pos)
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
	Check(! symbolp(s), "type error"); \
	setcheck_symbol(symbol); \
	SetArrayA2(s, i, v); \
}
#define GetSymbol(s,i,v) { \
	Check(! symbolp(s), "type error"); \
	GetArrayA2(s, i, v); \
}

_g void getname_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_NAME, value);
}
_g void setname_symbol(addr symbol, addr value)
{
	Check(! stringp(value), "type error");
	SetSymbol(symbol, SYMBOL_INDEX_NAME, value);
}

_g void getvalue_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_VALUE, &symbol);
	if (symbol == Nil)
		*value = Unbound;
	else
		GetCar(symbol, value);
}
_g void setvalue_symbol(addr symbol, addr value)
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

_g void getfunction_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_FUNCTION, value);
}
_g void setfunction_symbol(addr symbol, addr value)
{
	SetSymbol(symbol, SYMBOL_INDEX_FUNCTION, value);
}

_g void getpackage_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_PACKAGE, value);
}
_g void setpackage_symbol(addr symbol, addr value)
{
	Check(value != Nil && GetType(value) != LISPTYPE_PACKAGE, "type error");
	SetSymbol(symbol, SYMBOL_INDEX_PACKAGE, value);
}

_g void getplist_symbol(addr symbol, addr *value)
{
	GetSymbol(symbol, SYMBOL_INDEX_PLIST, value);
}
_g void setplist_symbol(addr symbol, addr value)
{
	Check(! listp(value), "type error");
	SetSymbol(symbol, SYMBOL_INDEX_PLIST, value);
}

static void getinfo_constant(addr symbol, constindex index, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, index, &symbol)? Nil: symbol;
}
static void setinfo_constant(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	setcheck_symbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}
static void reminfo_constant(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	setcheck_symbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

/* type */
_g void gettype_value_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, ret);
}
_g void settype_value_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, value);
}
_g void remtype_value_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_VALUE);
}

_g void gettype_function_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, ret);
}
_g void settype_function_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
_g void remtype_function_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION);
}

_g void gettype_setf_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF, ret);
}
_g void settype_setf_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_SETF, value);
}
_g void remtype_setf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_SETF);
}

/* inline */
_g int inlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
_g void setinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
_g int notinlinep_function_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
_g void setnotinline_function_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
_g void reminline_function_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION);
}

_g int inlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_INLINE, &check);
	return symbol == check;
}
_g void setinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_INLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
_g int notinlinep_setf_symbol(addr symbol)
{
	addr check;
	getinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, &symbol);
	GetConst(COMMON_NOTINLINE, &check);
	return symbol == check;
}
_g void setnotinline_setf_symbol(addr symbol)
{
	addr value;
	GetConst(COMMON_NOTINLINE, &value);
	setinfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
_g void reminline_setf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF);
}

/* setf */
_g void getsetf_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*value = getplist_constant(symbol, CONSTANT_COMMON_SETF, &symbol)? Unbound: symbol;
}
_g void setsetf_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SETF, value);
}
_g void remsetf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_SETF);
}

/* setf-macro */
_g void getsetfmacro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*value = getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER,
			&symbol)? Unbound: symbol;
}
_g void setsetfmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
_g void remsetfmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER);
}


/* macro */
_g void getmacro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*value = getplist_constant(symbol, CONSTANT_COMMON_DEFMACRO, &symbol)?
		Unbound: symbol;
}
_g void setmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
_g void remmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFMACRO);
}

/* symbol-macro */
static addr refsymbol_macro_symbol(addr symbol)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	return getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, &symbol)?
		Unbound: symbol;
}
_g void evalsymbol_macro_symbol(addr symbol, addr *ret)
{
	*ret = refsymbol_macro_symbol(symbol);
	if (*ret != Unbound)
		GetCar(*ret, ret);
}
_g void formsymbol_macro_symbol(addr symbol, addr *ret)
{
	*ret = refsymbol_macro_symbol(symbol);
	if (*ret != Unbound)
		GetCdr(*ret, ret);
}
_g void setsymbol_macro_symbol(addr symbol, addr eval, addr form)
{
	cons_heap(&eval, eval, form);
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, eval);
}
_g void remsymbol_macro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO);
}

/* compiler-macro-function */
_g void get_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

_g void set_compiler_macro_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

_g void get_setf_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

_g void set_setf_compiler_macro_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

/* scope */
_g void getscope_symbol(addr symbol, addr *value)
{
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
_g void setscope_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
_g void setspecial_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, T);
}
_g void setlexical_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, Nil);
}
_g int specialp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol != Nil;
}
_g int lexicalp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol == Nil;
}

_g void set_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, T);
}
_g int get_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, &symbol);
	return symbol == T;
}

_g void getdocument_variable_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, ret);
}
_g void setdocument_variable_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_VARIABLE, value);
}
_g void getdocument_type_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, ret);
}
_g void setdocument_type_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_DOCUMENTATION, value);
}

_g void getdeftype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, ret);
}
_g void setdeftype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(value != Nil && GetType(value) != LISPTYPE_FUNCTION, "type error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, value);
}
_g void remdeftype_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE);
}

static void setinfo_force(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_force(symbol, plist);
}

_g void getsymboltype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, ret);
}

_g void setsymboltype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_force(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, value);
}

_g void getlisttype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, ret);
}

_g void setlisttype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_force(symbol, CONSTANT_SYSTEM_TYPE_LIST, value);
}

_g void getclass_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_CLASS, ret);
}

_g void setclass_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_force(symbol, CONSTANT_COMMON_CLASS, value);
}

static void reminfo_force(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_force(symbol, plist);
}

_g void remclass_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_force(symbol, CONSTANT_COMMON_CLASS);
}

_g void getcombination_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_METHOD_COMBINATION, ret);
}

_g void setcombination_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_force(symbol, CONSTANT_COMMON_METHOD_COMBINATION, value);
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

	heap_array4(&make, LISPSYSTEM_SYMSTACK, size32);
	for (i = 0; i < size1; i++) {
		GetArrayA4(old, i, &temp);
		SetArrayA4(make, i, temp);
		SetArrayA4(old, i, Nil);
	}
	for (; i < size2; i++) {
		SetArrayA4(make, i, NULL);
	}
	*stack = make;
}

static void symstack(size_t index, addr symbol, addr *ret)
{
	addr stack, child;
	size_t i, size, size2;

	Check(GetStatusDynamic(symbol), "dynamic error.");
	rdlock_rwlocklite(&MutexSymbol);
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		unrdlock_rwlocklite(&MutexSymbol);
		return;
	}

	/* write lock */
	unrdlock_rwlocklite(&MutexSymbol);
	wrlock_rwlocklite(&MutexSymbol);
	/* reload */
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		unwrlock_rwlocklite(&MutexSymbol);
		return;
	}

	/* size extension */
	for (size2 = size; size2 <= index; )
		size2 <<= 1;
	realloc_symbol(&stack, size, size2);
	SetSpecialSymbol_Low(symbol, stack);
	for (i = size; i < size2; i++) {
		consnil_heap(&child);
		SetArrayA4(stack, i, child);
	}
	*ret = stack;
	unwrlock_rwlocklite(&MutexSymbol);
}

_g void getspecial_unsafe(Execute ptr, addr pos, addr *ret)
{
	Check(! symbolp(pos), "type error");
	setcheck_symbol(pos);
	symstack(ptr->index, pos, &pos);
	GetArrayA4(pos, ptr->index, ret);
}

_g void setspecial_unsafe(Execute ptr, addr pos, addr value)
{
	Check(! symbolp(pos), "type error");
	setcheck_symbol(pos);
	symstack(ptr->index, pos, &pos);
	SetArrayA4_force(pos, ptr->index, value);
}

_g void getspecial_local(Execute ptr, addr pos, addr *ret)
{
	Check(! symbolp(pos), "type error");
	if (GetStatusReadOnly(pos)) {
		GetValueSymbol(pos, ret);
		return;
	}
	getspecial_unsafe(ptr, pos, ret);
	if (*ret == NULL) {
		GetValueSymbol(pos, ret);
	}
}

_g void getspecialcheck_local(Execute ptr, addr pos, addr *ret)
{
	getspecial_local(ptr, pos, ret);
	if (*ret == Unbound)
		unbound_variable(pos);
}

_g void setspecial_local(Execute ptr, addr pos, addr value)
{
	addr check;

	Check(! symbolp(pos), "type error");
	getspecial_unsafe(ptr, pos, &check);
	if (check == NULL) {
		SetValueSymbol(pos, value);
	}
	else {
		setspecial_unsafe(ptr, pos, value);
	}
}

_g void getfunction_global(addr pos, addr *ret)
{
	GetFunctionSymbol(pos, ret);
	if (*ret == Unbound)
		undefined_function(pos);
}

_g void getsetf_global(addr pos, addr *ret)
{
	addr setf;

	getsetf_symbol(pos, ret);
	if (*ret == Unbound) {
		GetConst(COMMON_SETF, &setf);
		list_heap(&pos, setf, pos, NULL);
		undefined_function(pos);
	}
}


/*
 *  gensym
 */
_g int gensymp(addr pos)
{
	if (GetType(pos) != LISPTYPE_SYMBOL) return 0;
	GetPackageSymbol(pos, &pos);
	return pos == Nil;
}

_g void make_symbolchar(addr *ret, const char *str)
{
	addr pos, name;

	symbol_heap(&pos);
	strvect_char_heap(&name, str);
	SetNameSymbol(pos, name);
	*ret = pos;
}

static int make_gensym_argument_(Execute ptr,
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
	Return(decimal_charqueue_integer_local_(local, value, queue));
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

	return 0;
}
_g int make_gensym_(Execute ptr, addr *ret)
{
	return make_gensym_argument_(ptr, "G", NULL, NULL, ret);
}
_g int make_gensym_prefix_(Execute ptr, addr prefix, addr *ret)
{
	Check(! stringp(prefix), "type error");
	return make_gensym_argument_(ptr, NULL, prefix, NULL, ret);
}
_g int make_gensym_integer_(Execute ptr, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, "G", NULL, value, ret);
}
_g int make_gensym_char_(Execute ptr, const char *str, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, str, NULL, value, ret);
}

_g void setcounter_gensym(Execute ptr, fixnum value)
{
	addr pos, symbol;

	Check(value < 0, "value error");
	fixnum_heap(&pos, value);
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	setspecial_local(ptr, symbol, pos);
}

