#ifndef __SYMBOL_HEADER__
#define __SYMBOL_HEADER__

#include "execute.h"
#include "typedef.h"

enum SYMBOL_STACK {
	SYMBOL_STACK_LEXICAL = 0,
	SYMBOL_STACK_SPECIAL,
	SYMBOL_STACK_FUNCTION,
	SYMBOL_STACK_SETF,
	SYMBOL_STACK_SIZE
};

#define RefNameSymbol_Low(s)        RefArrayA2((s), SYMBOL_INDEX_NAME)
#define GetNameSymbol_Low(s,v)      GetArrayA2((s), SYMBOL_INDEX_NAME, (v))
#define SetNameSymbol_Low(s,v)      SetArrayA2((s), SYMBOL_INDEX_NAME, (v))
#define RefValueSymbol_Low(s)       RefArrayA2((s), SYMBOL_INDEX_VALUE)
#define GetValueSymbol_Low(s,v)     GetArrayA2((s), SYMBOL_INDEX_VALUE, (v))
#define SetValueSymbol_Low(s,v)     SetArrayA2((s), SYMBOL_INDEX_VALUE, (v))
#define RefFunctionSymbol_Low(s)    RefArrayA2((s), SYMBOL_INDEX_FUNCTION)
#define GetFunctionSymbol_Low(s,v)  GetArrayA2((s), SYMBOL_INDEX_FUNCTION, (v))
#define SetFunctionSymbol_Low(s,v)  SetArrayA2((s), SYMBOL_INDEX_FUNCTION, (v))
#define RefPackageSymbol_Low(s)     RefArrayA2((s), SYMBOL_INDEX_PACKAGE)
#define GetPackageSymbol_Low(s,v)   GetArrayA2((s), SYMBOL_INDEX_PACKAGE, (v))
#define SetPackageSymbol_Low(s,v)   SetArrayA2((s), SYMBOL_INDEX_PACKAGE, (v))
#define RefPlistSymbol_Low(s)       RefArrayA2((s), SYMBOL_INDEX_PLIST)
#define GetPlistSymbol_Low(s,v)     GetArrayA2((s), SYMBOL_INDEX_PLIST, (v))
#define SetPlistSymbol_Low(s,v)     SetArrayA2((s), SYMBOL_INDEX_PLIST, (v))

#define RefStackSymbol_Low(s)       RefArrayA2((s), SYMBOL_INDEX_STACK)
#define GetStackSymbol_Low(s,v)     GetArrayA2((s), SYMBOL_INDEX_STACK, (v))
#define SetStackSymbol_Low(s,v)     SetArrayA2((s), SYMBOL_INDEX_STACK, (v))
#define RefInfoSymbol_Low(s)        RefArrayA2((s), SYMBOL_INDEX_INFO)
#define GetInfoSymbol_Low(s,v)      GetArrayA2((s), SYMBOL_INDEX_INFO, (v))
#define SetInfoSymbol_Low(s,v)      SetArrayA2((s), SYMBOL_INDEX_INFO, (v))
#define SetInfoSymbol_force(s,v)    SetArrayA2_force((s), SYMBOL_INDEX_INFO, (v))

#ifdef LISP_DEBUG
#define RefNameSymbol(s)            refname_symbol(s)
#define GetNameSymbol(s,v)          getname_symbol((s), (v))
#define SetNameSymbol(s,v)          setname_symbol((s), (v))
#define RefFunctionSymbol(s)        reffunction_symbol(s)
#define GetFunctionSymbol(s,v)      getfunction_symbol((s), (v))
#define SetFunctionSymbol(s,v)      setfunction_symbol((s), (v))
#define RefPackageSymbol(s)         refpackage_symbol(s)
#define GetPackageSymbol(s,v)       getpackage_symbol((s), (v))
#define SetPackageSymbol(s,v)       setpackage_symbol((s), (v))
#define RefPlistSymbol(s)           refplist_symbol(s)
#define GetPlistSymbol(s,v)         getplist_symbol((s), (v))
#define SetPlistSymbol(s,v)         setplist_symbol((s), (v))
#else
#define RefNameSymbol(s)            RefNameSymbol_Low(s)
#define GetNameSymbol(s,v)          GetNameSymbol_Low(s,v)
#define SetNameSymbol(s,v)          SetNameSymbol_Low(s,v)
#define RefFunctionSymbol(s)        RefFunctionSymbol_Low(s)
#define GetFunctionSymbol(s,v)      GetFunctionSymbol_Low(s,v)
#define SetFunctionSymbol(s,v)      SetFunctionSymbol_Low(s,v)
#define RefPackageSymbol(s)         RefPackageSymbol_Low(s)
#define GetPackageSymbol(s,v)       GetPackageSymbol_Low(s,v)
#define SetPackageSymbol(s,v)       SetPackageSymbol_Low(s,v)
#define RefPlistSymbol(s)           RefPlistSymbol_Low(s)
#define GetPlistSymbol(s,v)         GetPlistSymbol_Low(s,v)
#define SetPlistSymbol(s,v)         SetPlistSymbol_Low(s,v)
#endif

#define RefValueSymbol(s)           refvalue_symbol(s)
#define GetValueSymbol(s,v)         getvalue_symbol((s), (v))
#define SetValueSymbol(s,v)         setvalue_symbol((s), (v))
#define GetValueCheckSymbol(s,v)    { \
	GetValueSymbol((s), (v)); \
	if (*(addr *)(v) == Unbound) errorunbound(s); \
}
#define GetFunctionCheckSymbol(s,v) { \
	GetFunctionSymbol((s), (v)); \
	if (*(addr *)(v) == Unbound) undefined_function(s); \
}

int init_symbol(void);
void free_symbol(void);
void build_symbol(void);

addr symbol_heapr(void);
addr symbol_localr(LocalRoot local);
addr symbol_allocr(LocalRoot local);
void symbol_heap(addr *ret);
void symbol_local(LocalRoot local, addr *ret);
void symbol_alloc(LocalRoot local, addr *ret);
int symbolp(addr pos);
int keywordp(addr pos);

addr refname_symbol(addr symbol);
void getname_symbol(addr symbol, addr *value);
void setname_symbol(addr symbol, addr value);
addr refvalue_symbol(addr symbol);
void getvalue_symbol(addr symbol, addr *value);
void setvalue_symbol(addr symbol, addr value);
addr reffunction_symbol(addr symbol);
void getfunction_symbol(addr symbol, addr *value);
void setfunction_symbol(addr symbol, addr value);
addr refpackage_symbol(addr symbol);
void getpackage_symbol(addr symbol, addr *value);
void setpackage_symbol(addr symbol, addr value);
addr refplist_symbol(addr symbol);
void getplist_symbol(addr symbol, addr *value);
void setplist_symbol(addr symbol, addr value);

void gettype_value_symbol(addr symbol, addr *ret);
void settype_value_symbol(addr symbol, addr value);
void remtype_value_symbol(addr symbol);
void gettype_function_symbol(addr symbol, addr *ret);
void settype_function_symbol(addr symbol, addr value);
void remtype_function_symbol(addr symbol);
void gettype_setf_symbol(addr symbol, addr *ret);
void settype_setf_symbol(addr symbol, addr value);
void remtype_setf_symbol(addr symbol);
int inlinep_function_symbol(addr symbol);
void setinline_function_symbol(addr symbol);
int notinlinep_function_symbol(addr symbol);
void setnotinline_function_symbol(addr symbol);
void reminline_function_symbol(addr symbol);
int inlinep_setf_symbol(addr symbol);
void setinline_setf_symbol(addr symbol);
int notinlinep_setf_symbol(addr symbol);
void setnotinline_setf_symbol(addr symbol);
void reminline_setf_symbol(addr symbol);

addr refsetf_symbol(addr symbol);
void getsetf_symbol(addr symbol, addr *value);
void getsetfcheck_symbol(addr symbol, addr *value);
void setsetf_symbol(addr symbol, addr value);
void remsetf_symbol(addr symbol);
addr refsetfmacro_symbol(addr symbol);
void getsetfmacro_symbol(addr symbol, addr *value);
void setsetfmacro_symbol(addr symbol, addr value);
void remsetfmacro_symbol(addr symbol);
addr refmacro_symbol(addr symbol);
void getmacro_symbol(addr symbol, addr *value);
void setmacro_symbol(addr symbol, addr value);
void remmacro_symbol(addr symbol);
addr refsymbol_macro_symbol(addr symbol);
void evalsymbol_macro_symbol(addr symbol, addr *ret);
void formsymbol_macro_symbol(addr symbol, addr *ret);
void setsymbol_macro_symbol(addr symbol, addr eval, addr form);
void remsymbol_macro_symbol(addr symbol);
addr refscope_symbol(addr symbol);
void getscope_symbol(addr symbol, addr *value);
void setscope_symbol(addr symbol, addr value);
void setspecial_symbol(addr symbol);
void setlexical_symbol(addr symbol);
int specialp_symbol(addr symbol);
int lexicalp_symbol(addr symbol);
void set_special_operator(addr symbol);
int get_special_operator(addr symbol);
void getdocument_variable_symbol(addr symbol, addr *ret);
void setdocument_variable_symbol(addr symbol, addr value);
void getdocument_type_symbol(addr symbol, addr *ret);
void setdocument_type_symbol(addr symbol, addr value);
void getdeftype_symbol(addr symbol, addr *ret);
void setdeftype_symbol(addr symbol, addr value);
void remdeftype_symbol(addr symbol);
void getsymboltype_symbol(addr symbol, addr *ret);
void setsymboltype_symbol(addr symbol, addr value);
void getlisttype_symbol(addr symbol, addr *ret);
void setlisttype_symbol(addr symbol, addr value);

/* symstack */
void pushlexical_closure_unsafe(Execute ptr, addr pos, addr cons);
void pushlexical_unsafe(Execute ptr, addr pos, addr value);
void pushspecial_unsafe(Execute ptr, addr pos, addr value);
void pushfunction_unsafe(Execute ptr, addr pos, addr value);
void pushsetf_unsafe(Execute ptr, addr pos, addr value);
void poplexical_unsafe(Execute ptr, addr pos);
void popspecial_unsafe(Execute ptr, addr pos);
void popfunction_unsafe(Execute ptr, addr pos);
void popsetf_unsafe(Execute ptr, addr pos);
void snapshot_lexical_local(Execute ptr, addr pos, addr *ret);
void snapshot_special_local(Execute ptr, addr pos, addr *ret);
void snapshot_function_local(Execute ptr, addr pos, addr *ret);
void snapshot_setf_local(Execute ptr, addr pos, addr *ret);
void rollback_lexical_local(Execute ptr, addr pos, addr cons);
void rollback_special_local(Execute ptr, addr pos, addr cons);
void rollback_function_local(Execute ptr, addr pos, addr cons);
void rollback_setf_local(Execute ptr, addr pos, addr cons);
void clearlexical_local(Execute ptr, addr pos);
void clearspecial_local(Execute ptr, addr pos);
void clearfunction_local(Execute ptr, addr pos);
void clearsetf_local(Execute ptr, addr pos);

void conslexical_local(Execute ptr, addr pos, addr *ret);
void getlexical_local(Execute ptr, addr pos, addr *ret);
void getspecial_local(Execute ptr, addr pos, addr *ret);
void getfunction_local(Execute ptr, addr pos, addr *ret);
void getsetf_local(Execute ptr, addr pos, addr *ret);
addr reflexical_local(Execute ptr, addr pos);
addr refspecial_local(Execute ptr, addr pos);
addr reffunction_local(Execute ptr, addr pos);
addr refsetf_local(Execute ptr, addr pos);

void conslexicalcheck_local(Execute ptr, addr pos, addr *ret);
void getlexicalcheck_local(Execute ptr, addr pos, addr *ret);
void getspecialcheck_local(Execute ptr, addr pos, addr *ret);
void getfunctioncheck_local(Execute ptr, addr pos, addr *ret);
void getsetfcheck_local(Execute ptr, addr pos, addr *ret);
addr reflexicalcheck_local(Execute ptr, addr pos);
addr refspecialcheck_local(Execute ptr, addr pos);
addr reffunctioncheck_local(Execute ptr, addr pos);
addr refsetfcheck_local(Execute ptr, addr pos);

void setlexical_local(Execute ptr, addr pos, addr value);
void setspecial_local(Execute ptr, addr pos, addr value);
void setfunction_local(Execute ptr, addr pos, addr value);
void setsetf_local(Execute ptr, addr pos, addr value);

/* gensym */
int gensymp(addr pos);
void make_symbolchar(addr *ret, const char *str);
void make_gensym(Execute ptr, addr *ret);
void make_gensym_prefix(Execute ptr, addr prefix, addr *ret);
void make_gensym_integer(Execute ptr, addr value, addr *ret);
void make_gensym_char(Execute ptr, const char *str, addr value, addr *ret);
void setcounter_gensym(Execute ptr, fixnum value);

#endif

