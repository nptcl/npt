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

int init_symbol(void)
{
	if (InitObject) {
		Debug("InitObject error.");
		return 1;
	}
	if (lispd_make_rwlocklite(&MutexSymbol)) {
		Debug("lispd_make_mutexlite error");
		return 1;
	}
	InitObject = 1;

	return 0;
}

void free_symbol(void)
{
	if (InitObject) {
		lispd_destroy_rwlocklite(&MutexSymbol);
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
void symbol_heap(addr *ret)
{
	int i;
	addr pos, make;

	/* symbol object */
	heap_symbol(&pos);

	/* Unbound */
	SetValueSymbol_Low(pos, Unbound);
	SetFunctionSymbol_Low(pos, Unbound);

	/* stack */
	heap_array4(&make, LISPSYSTEM_SYMSTACK, SYMSTACK_SIZE);
	for (i = 0; i < SYMSTACK_SIZE; i++) {
		SetArrayA4(make, i, NULL);
	}
	SetSpecialSymbol_Low(pos, make);
	*ret = pos;
}

int symbolp(addr pos)
{
	return GetType(pos) == LISPTYPE_SYMBOL || pos == Nil || pos == T;
}

int keywordp(addr pos)
{
	addr keyword;

	if (GetType(pos) != LISPTYPE_SYMBOL)
		return 0;
	GetConst(PACKAGE_KEYWORD, &keyword);
	Check(GetType(keyword) != LISPTYPE_PACKAGE, "package error");
	GetPackageSymbol(pos, &pos);

	return pos == keyword;
}

static int setcheck_symbol_(addr symbol, size_t index, addr value)
{
	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	SetArrayA2(symbol, index, value);
	return 0;
}

/* name */
void getname_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetNameSymbol_Low(symbol, ret);
}
void setname_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetNameSymbol_Low(symbol, value);
}

/* value */
void getvalue_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetValueSymbol_Low(symbol, ret);
}
void setvalue_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetValueSymbol_Low(symbol, value);
}
int setvalue_symbol_(addr symbol, addr value)
{
	return setcheck_symbol_(symbol, SYMBOL_INDEX_VALUE, value);
}

/* function */
void getfunction_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetFunctionSymbol_Low(symbol, ret);
}
void setfunction_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	SetFunctionSymbol_Low(symbol, value);
}
int setfunction_symbol_(addr symbol, addr value)
{
	return setcheck_symbol_(symbol, SYMBOL_INDEX_FUNCTION, value);
}

/* package */
void getpackage_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetPackageSymbol_Low(symbol, ret);
}
void setpackage_symbol(addr symbol, addr value)
{
	Check(value != Nil && GetType(value) != LISPTYPE_PACKAGE, "type error");
	/* no-readonly-check */
	SetPackageSymbol_Low(symbol, value);
}

/* plist */
void getplist_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetPlistSymbol_Low(symbol, ret);
}
void setplist_symbol(addr symbol, addr value)
{
	Check(! listp(value), "type error");
	/* no-readonly-check */
	SetPlistSymbol_Low(symbol, value);
}

/* info */
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
	Check(GetStatusReadOnly(symbol), "readonly error");
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static void setinfo_nocheck_constant(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static int setinfo_constant_(addr symbol, constindex index, addr value)
{
	addr plist;

	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	GetInfoSymbol_Low(symbol, &plist);
	if (setplist_constant_heap(plist, index, value, &plist))
		SetInfoSymbol_Low(symbol, plist);

	return 0;
}

static void reminfo_constant(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	Check(GetStatusReadOnly(symbol), "readonly error");
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

static int reminfo_constant_(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set the constant variable ~S.", symbol, NULL);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);

	return 0;
}

static void reminfo_nocheck_constant(addr symbol, constindex index)
{
	addr plist;

	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &plist);
	if (remplist_constant(plist, index, &plist))
		SetInfoSymbol_Low(symbol, plist);
}

/* type value */
void gettype_value_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, ret);
}
void settype_value_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_VALUE, value);
}
int settype_value_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_VALUE, value);
}
void remtype_value_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_SYSTEM_VALUE);
}

/* type function */
void gettype_function_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, ret);
}
void settype_function_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
int settype_function_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_FUNCTION, value);
}
int remtype_function_symbol_(addr symbol)
{
	return reminfo_constant_(symbol, CONSTANT_SYSTEM_FUNCTION);
}

/* type setf */
void gettype_setf_symbol(addr symbol, addr *ret)
{
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF, ret);
}
void settype_setf_symbol(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	setinfo_constant(symbol, CONSTANT_SYSTEM_SETF, value);
}
int settype_setf_symbol_(addr symbol, addr value)
{
	Check(GetType(value) != LISPTYPE_TYPE, "type right error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_SETF, value);
}
int remtype_setf_symbol_(addr symbol)
{
	return reminfo_constant_(symbol, CONSTANT_SYSTEM_SETF);
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
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
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
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
}
void reminline_function_symbol(addr symbol)
{
	reminfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_FUNCTION);
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
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
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
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF, value);
}
void reminline_setf_symbol(addr symbol)
{
	reminfo_nocheck_constant(symbol, CONSTANT_SYSTEM_INLINE_SETF);
}

/* setf */
void getsetf_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_SETF, &symbol)? Unbound: symbol;
}
void setsetf_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SETF, value);
}
int setsetf_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_SETF, value);
}
void remsetf_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_SETF);
}

/* setf-macro */
void getsetfmacro_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER,
			&symbol)? Unbound: symbol;
}
void setsetfmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
int setsetfmacro_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER, value);
}
void remsetfmacro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SETF_EXPANDER);
}

/* macro */
void getmacro_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	GetInfoSymbol_Low(symbol, &symbol);
	*ret = getplist_constant(symbol, CONSTANT_COMMON_DEFMACRO, &symbol)?
		Unbound: symbol;
}
void setmacro_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
int setmacro_symbol_(addr symbol, addr value)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFMACRO, value);
}
void remmacro_symbol(addr symbol)
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
int setsymbol_macro_symbol_(addr symbol, addr eval, addr form)
{
	cons_heap(&eval, eval, form);
	return setinfo_constant_(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO, eval);
}
void remsymbol_macro_symbol(addr symbol)
{
	reminfo_constant(symbol, CONSTANT_COMMON_DEFINE_SYMBOL_MACRO);
}

/* compiler-macro-function */
void get_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

int set_compiler_macro_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_COMPILER_MACRO_FUNCTION, value);
}

void get_setf_compiler_macro_symbol(addr symbol, addr *value)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

int set_setf_compiler_macro_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	return setinfo_constant_(symbol,
			CONSTANT_SYSTEM_SETF_COMPILER_MACRO_FUNCTION, value);
}

/* scope */
void getscope_symbol(addr symbol, addr *value)
{
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
static void setscope_symbol(addr symbol, addr value)
{
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL, value);
}
void setspecial_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, T);
}
int setspecial_symbol_(addr symbol)
{
	return setinfo_constant_(symbol, CONSTANT_COMMON_SPECIAL, T);
}
void setlexical_symbol(addr symbol)
{
	CheckSymbol(symbol);
	setscope_symbol(symbol, Nil);
}
int specialp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol != Nil;
}
int lexicalp_symbol(addr symbol)
{
	CheckSymbol(symbol);
	getscope_symbol(symbol, &symbol);
	return symbol == Nil;
}

/* special-operator */
void set_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	setinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, T);
}
int get_special_operator(addr symbol)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_SPECIAL_OPERATOR_P, &symbol);
	return symbol != Nil;
}

/* document */
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

/* deftype */
void getdeftype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE, ret);
}
int setdeftype_symbol_(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(value != Nil && GetType(value) != LISPTYPE_FUNCTION, "type error");
	return setinfo_constant_(symbol, CONSTANT_SYSTEM_DEFTYPE, value);
}
void remdeftype_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_constant(symbol, CONSTANT_SYSTEM_DEFTYPE);
}

/* type-symbol */
void getsymboltype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, ret);
}

void setsymboltype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_TYPE_SYMBOL, value);
}

void getlisttype_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, ret);
}

void setlisttype_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	setinfo_nocheck_constant(symbol, CONSTANT_SYSTEM_TYPE_LIST, value);
}

/* clos */
void getclass_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_CLASS, ret);
}

void setclass_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_nocheck_constant(symbol, CONSTANT_COMMON_CLASS, value);
}

void remclass_symbol(addr symbol)
{
	CheckSymbol(symbol);
	reminfo_nocheck_constant(symbol, CONSTANT_COMMON_CLASS);
}

void getcombination_symbol(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getinfo_constant(symbol, CONSTANT_COMMON_METHOD_COMBINATION, ret);
}

void setcombination_symbol(addr symbol, addr value)
{
	CheckSymbol(symbol);
	Check(GetType(value) != LISPTYPE_CLOS, "type error");
	setinfo_nocheck_constant(symbol, CONSTANT_COMMON_METHOD_COMBINATION, value);
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
	lispd_rdlock_rwlocklite(&MutexSymbol);
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unrdlock_rwlocklite(&MutexSymbol);
		return;
	}

	/* write lock */
	lispd_unrdlock_rwlocklite(&MutexSymbol);
	lispd_wrlock_rwlocklite(&MutexSymbol);
	/* reload */
	GetSpecialSymbol_Low(symbol, &stack);
	size = GetLenArrayA4(stack);
	if (index < size) {
		*ret = stack;
		lispd_unwrlock_rwlocklite(&MutexSymbol);
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
	lispd_unwrlock_rwlocklite(&MutexSymbol);
}

void getspecial_unsafe(Execute ptr, addr pos, addr *ret)
{
	CheckSymbol(pos);
	symstack(ptr->index, pos, &pos);
	GetArrayA4(pos, ptr->index, ret);
}

void setspecial_unsafe(Execute ptr, addr pos, addr value)
{
	CheckSymbol(pos);
	symstack(ptr->index, pos, &pos);
	SetArrayA4_force(pos, ptr->index, value);
}

void getspecial_local(Execute ptr, addr pos, addr *ret)
{
	CheckSymbol(pos);
	if (GetStatusReadOnly(pos)) {
		GetValueSymbol(pos, ret);
		return;
	}
	getspecial_unsafe(ptr, pos, ret);
	if (*ret == NULL) {
		GetValueSymbol(pos, ret);
	}
}

int getspecialcheck_local_(Execute ptr, addr pos, addr *ret)
{
	getspecial_local(ptr, pos, ret);
	if (*ret == Unbound)
		return call_unbound_variable_(ptr, pos);

	return 0;
}

void setspecial_local(Execute ptr, addr pos, addr value)
{
	addr check;

	CheckSymbol(pos);
	getspecial_unsafe(ptr, pos, &check);
	if (check == NULL) {
		SetValueSymbol(pos, value);
	}
	else {
		setspecial_unsafe(ptr, pos, value);
	}
}

int getfunction_global_(addr pos, addr *ret)
{
	GetFunctionSymbol(pos, ret);
	if (*ret == Unbound)
		return call_undefined_function_(NULL, pos);

	return 0;
}

int getsetf_global_(addr pos, addr *ret)
{
	addr setf;

	getsetf_symbol(pos, ret);
	if (*ret == Unbound) {
		GetConst(COMMON_SETF, &setf);
		list_heap(&pos, setf, pos, NULL);
		return call_undefined_function_(NULL, pos);
	}

	return 0;
}

int alldelete_function_(addr pos)
{
	Check(! symbolp(pos), "type error");
	if (GetStatusReadOnly(pos))
		return fmte_("Cannot set the constant variable ~S.", pos, NULL);
	SetFunctionSymbol(pos, Unbound);
	remsetf_symbol(pos);
	remsetfmacro_symbol(pos);
	remmacro_symbol(pos);
	Return(remtype_function_symbol_(pos));
	Return(remtype_setf_symbol_(pos));

	return 0;
}


/*
 *  gensym
 */
int gensymp(addr pos)
{
	if (GetType(pos) != LISPTYPE_SYMBOL)
		return 0;
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
		Return(getspecialcheck_local_(ptr, symbol, &value));
	}
	else {
		symbol = NULL;
		value = counter;
	}
	Check(! integerp(value), "type error");
	charqueue_local(local, &queue, 1 + 16);
	if (prefix1) {
		Return(pushchar_charqueue_local_(local, queue, prefix1));
	}
	else {
		Return(pushstring_charqueue_local_(local, queue, prefix2));
	}
	Return(decimal_charqueue_integer_local_(local, value, queue));
	make_charqueue_heap(queue, &name);
	rollback_local(local, stack);

	/* gensym */
	symbol_heap(&gensym);
	SetNameSymbol(gensym, name);
	*ret = gensym;

	/* (1+ *gensym-counter*) */
	if (counter == NULL) {
		Return(oneplus_integer_common_(local, value, &value));
		setspecial_local(ptr, symbol, value);
	}

	return 0;
}
int make_gensym_(Execute ptr, addr *ret)
{
	return make_gensym_argument_(ptr, "G", NULL, NULL, ret);
}
int make_gensym_prefix_(Execute ptr, addr prefix, addr *ret)
{
	Check(! stringp(prefix), "type error");
	return make_gensym_argument_(ptr, NULL, prefix, NULL, ret);
}
int make_gensym_integer_(Execute ptr, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, "G", NULL, value, ret);
}
int make_gensym_char_(Execute ptr, const char *str, addr value, addr *ret)
{
	Check(! integerp(value), "type error");
	return make_gensym_argument_(ptr, str, NULL, value, ret);
}

void setcounter_gensym(Execute ptr, fixnum value)
{
	addr pos, symbol;

	Check(value < 0, "value error");
	fixnum_heap(&pos, value);
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	setspecial_local(ptr, symbol, pos);
}

