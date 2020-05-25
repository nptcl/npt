#ifndef __SYMBOL_HEADER__
#define __SYMBOL_HEADER__

#include "execute.h"
#include "typedef.h"

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

_g int init_symbol(void);
_g void free_symbol(void);
_g void build_symbol(void);

_g addr symbol_heapr(void);
_g addr symbol_localr(LocalRoot local);
_g addr symbol_allocr(LocalRoot local);
_g void symbol_heap(addr *ret);
_g void symbol_local(LocalRoot local, addr *ret);
_g void symbol_alloc(LocalRoot local, addr *ret);
_g int symbolp(addr pos);
_g int keywordp(addr pos);

_g addr refname_symbol(addr symbol);
_g void getname_symbol(addr symbol, addr *value);
_g void setname_symbol(addr symbol, addr value);
_g addr refvalue_symbol(addr symbol);
_g void getvalue_symbol(addr symbol, addr *value);
_g void setvalue_symbol(addr symbol, addr value);
_g addr reffunction_symbol(addr symbol);
_g void getfunction_symbol(addr symbol, addr *value);
_g void setfunction_symbol(addr symbol, addr value);
_g addr refpackage_symbol(addr symbol);
_g void getpackage_symbol(addr symbol, addr *value);
_g void setpackage_symbol(addr symbol, addr value);
_g addr refplist_symbol(addr symbol);
_g void getplist_symbol(addr symbol, addr *value);
_g void setplist_symbol(addr symbol, addr value);

_g void gettype_value_symbol(addr symbol, addr *ret);
_g void settype_value_symbol(addr symbol, addr value);
_g void remtype_value_symbol(addr symbol);
_g void gettype_function_symbol(addr symbol, addr *ret);
_g void settype_function_symbol(addr symbol, addr value);
_g void remtype_function_symbol(addr symbol);
_g void gettype_setf_symbol(addr symbol, addr *ret);
_g void settype_setf_symbol(addr symbol, addr value);
_g void remtype_setf_symbol(addr symbol);
_g int inlinep_function_symbol(addr symbol);
_g void setinline_function_symbol(addr symbol);
_g int notinlinep_function_symbol(addr symbol);
_g void setnotinline_function_symbol(addr symbol);
_g void reminline_function_symbol(addr symbol);
_g int inlinep_setf_symbol(addr symbol);
_g void setinline_setf_symbol(addr symbol);
_g int notinlinep_setf_symbol(addr symbol);
_g void setnotinline_setf_symbol(addr symbol);
_g void reminline_setf_symbol(addr symbol);

_g addr refsetf_symbol(addr symbol);
_g void getsetf_symbol(addr symbol, addr *value);
_g void setsetf_symbol(addr symbol, addr value);
_g void remsetf_symbol(addr symbol);
_g addr refsetfmacro_symbol(addr symbol);
_g void getsetfmacro_symbol(addr symbol, addr *value);
_g void setsetfmacro_symbol(addr symbol, addr value);
_g void remsetfmacro_symbol(addr symbol);
_g addr refmacro_symbol(addr symbol);
_g void getmacro_symbol(addr symbol, addr *value);
_g void setmacro_symbol(addr symbol, addr value);
_g void remmacro_symbol(addr symbol);
_g addr refsymbol_macro_symbol(addr symbol);
_g void evalsymbol_macro_symbol(addr symbol, addr *ret);
_g void formsymbol_macro_symbol(addr symbol, addr *ret);
_g void setsymbol_macro_symbol(addr symbol, addr eval, addr form);
_g void remsymbol_macro_symbol(addr symbol);

_g void get_compiler_macro_symbol(addr symbol, addr *value);
_g void set_compiler_macro_symbol(addr symbol, addr form);
_g void get_setf_compiler_macro_symbol(addr symbol, addr *value);
_g void set_setf_compiler_macro_symbol(addr symbol, addr form);

_g addr refscope_symbol(addr symbol);
_g void getscope_symbol(addr symbol, addr *value);
_g void setscope_symbol(addr symbol, addr value);
_g void setspecial_symbol(addr symbol);
_g void setlexical_symbol(addr symbol);
_g int specialp_symbol(addr symbol);
_g int lexicalp_symbol(addr symbol);
_g void set_special_operator(addr symbol);
_g int get_special_operator(addr symbol);
_g void getdocument_variable_symbol(addr symbol, addr *ret);
_g void setdocument_variable_symbol(addr symbol, addr value);
_g void getdocument_type_symbol(addr symbol, addr *ret);
_g void setdocument_type_symbol(addr symbol, addr value);
_g void getdeftype_symbol(addr symbol, addr *ret);
_g void setdeftype_symbol(addr symbol, addr value);
_g void remdeftype_symbol(addr symbol);
_g void getsymboltype_symbol(addr symbol, addr *ret);
_g void setsymboltype_symbol(addr symbol, addr value);
_g void getlisttype_symbol(addr symbol, addr *ret);
_g void setlisttype_symbol(addr symbol, addr value);
_g void getclass_symbol(addr symbol, addr *ret);
_g void setclass_symbol(addr symbol, addr value);
_g void remclass_symbol(addr symbol);
_g void getcombination_symbol(addr symbol, addr *ret);
_g void setcombination_symbol(addr symbol, addr value);

/* symstack */
_g void pushspecial_unsafe(Execute ptr, addr pos, addr value);
_g void popspecial_unsafe(Execute ptr, addr pos);
_g void snapshot_special_local(Execute ptr, addr pos, addr *ret);
_g void rollback_special_local(Execute ptr, addr pos, addr cons);
_g void clearspecial_local(Execute ptr, addr pos);
_g void getspecial_local(Execute ptr, addr pos, addr *ret);
_g addr refspecial_local(Execute ptr, addr pos);
_g void getspecialcheck_local(Execute ptr, addr pos, addr *ret);
_g addr refspecialcheck_local(Execute ptr, addr pos);
_g void setspecial_local(Execute ptr, addr pos, addr value);

_g void getfunction_global(addr pos, addr *ret);
_g void getsetf_global(addr pos, addr *ret);

/* gensym */
_g int gensymp(addr pos);
_g void make_symbolchar(addr *ret, const char *str);
_g void make_gensym(Execute ptr, addr *ret);
_g void make_gensym_prefix(Execute ptr, addr prefix, addr *ret);
_g void make_gensym_integer(Execute ptr, addr value, addr *ret);
_g void make_gensym_char(Execute ptr, const char *str, addr value, addr *ret);
_g void setcounter_gensym(Execute ptr, fixnum value);

#endif

