#ifndef __SYMBOL_HEADER__
#define __SYMBOL_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_symbol _n(init_symbol)
#define free_symbol _n(free_symbol)
#define build_symbol _n(build_symbol)
#define symbol_heap _n(symbol_heap)
#define symbolp _n(symbolp)
#define keywordp _n(keywordp)
#define getname_symbol _n(getname_symbol)
#define setname_symbol _n(setname_symbol)
#define getvalue_symbol _n(getvalue_symbol)
#define setvalue_symbol _n(setvalue_symbol)
#define setvalue_symbol_ _n(setvalue_symbol_)
#define getfunction_symbol _n(getfunction_symbol)
#define setfunction_symbol _n(setfunction_symbol)
#define setfunction_symbol_ _n(setfunction_symbol_)
#define getpackage_symbol _n(getpackage_symbol)
#define setpackage_symbol _n(setpackage_symbol)
#define getplist_symbol _n(getplist_symbol)
#define setplist_symbol _n(setplist_symbol)
#define gettype_value_symbol _n(gettype_value_symbol)
#define settype_value_symbol _n(settype_value_symbol)
#define settype_value_symbol_ _n(settype_value_symbol_)
#define remtype_value_symbol _n(remtype_value_symbol)
#define gettype_function_symbol _n(gettype_function_symbol)
#define settype_function_symbol _n(settype_function_symbol)
#define settype_function_symbol_ _n(settype_function_symbol_)
#define remtype_function_symbol_ _n(remtype_function_symbol_)
#define gettype_setf_symbol _n(gettype_setf_symbol)
#define settype_setf_symbol _n(settype_setf_symbol)
#define settype_setf_symbol_ _n(settype_setf_symbol_)
#define remtype_setf_symbol_ _n(remtype_setf_symbol_)
#define inlinep_function_symbol _n(inlinep_function_symbol)
#define setinline_function_symbol _n(setinline_function_symbol)
#define notinlinep_function_symbol _n(notinlinep_function_symbol)
#define setnotinline_function_symbol _n(setnotinline_function_symbol)
#define reminline_function_symbol _n(reminline_function_symbol)
#define inlinep_setf_symbol _n(inlinep_setf_symbol)
#define setinline_setf_symbol _n(setinline_setf_symbol)
#define notinlinep_setf_symbol _n(notinlinep_setf_symbol)
#define setnotinline_setf_symbol _n(setnotinline_setf_symbol)
#define reminline_setf_symbol _n(reminline_setf_symbol)
#define getsetf_symbol _n(getsetf_symbol)
#define setsetf_symbol _n(setsetf_symbol)
#define setsetf_symbol_ _n(setsetf_symbol_)
#define remsetf_symbol _n(remsetf_symbol)
#define getsetfmacro_symbol _n(getsetfmacro_symbol)
#define setsetfmacro_symbol _n(setsetfmacro_symbol)
#define setsetfmacro_symbol_ _n(setsetfmacro_symbol_)
#define remsetfmacro_symbol _n(remsetfmacro_symbol)
#define getmacro_symbol _n(getmacro_symbol)
#define setmacro_symbol _n(setmacro_symbol)
#define setmacro_symbol_ _n(setmacro_symbol_)
#define remmacro_symbol _n(remmacro_symbol)
#define evalsymbol_macro_symbol _n(evalsymbol_macro_symbol)
#define formsymbol_macro_symbol _n(formsymbol_macro_symbol)
#define setsymbol_macro_symbol_ _n(setsymbol_macro_symbol_)
#define remsymbol_macro_symbol _n(remsymbol_macro_symbol)
#define get_compiler_macro_symbol _n(get_compiler_macro_symbol)
#define set_compiler_macro_symbol_ _n(set_compiler_macro_symbol_)
#define get_setf_compiler_macro_symbol _n(get_setf_compiler_macro_symbol)
#define set_setf_compiler_macro_symbol_ _n(set_setf_compiler_macro_symbol_)
#define getscope_symbol _n(getscope_symbol)
#define setspecial_symbol _n(setspecial_symbol)
#define setspecial_symbol_ _n(setspecial_symbol_)
#define setlexical_symbol _n(setlexical_symbol)
#define specialp_symbol _n(specialp_symbol)
#define lexicalp_symbol _n(lexicalp_symbol)
#define set_special_operator _n(set_special_operator)
#define get_special_operator _n(get_special_operator)
#define getdocument_variable_symbol _n(getdocument_variable_symbol)
#define setdocument_variable_symbol _n(setdocument_variable_symbol)
#define getdocument_type_symbol _n(getdocument_type_symbol)
#define setdocument_type_symbol _n(setdocument_type_symbol)
#define getdeftype_symbol _n(getdeftype_symbol)
#define setdeftype_symbol_ _n(setdeftype_symbol_)
#define remdeftype_symbol _n(remdeftype_symbol)
#define getsymboltype_symbol _n(getsymboltype_symbol)
#define setsymboltype_symbol _n(setsymboltype_symbol)
#define getlisttype_symbol _n(getlisttype_symbol)
#define setlisttype_symbol _n(setlisttype_symbol)
#define getclass_symbol _n(getclass_symbol)
#define setclass_symbol _n(setclass_symbol)
#define remclass_symbol _n(remclass_symbol)
#define getcombination_symbol _n(getcombination_symbol)
#define setcombination_symbol _n(setcombination_symbol)
#define getspecial_unsafe _n(getspecial_unsafe)
#define setspecial_unsafe _n(setspecial_unsafe)
#define getspecial_local _n(getspecial_local)
#define getspecialcheck_local_ _n(getspecialcheck_local_)
#define setspecial_local _n(setspecial_local)
#define getfunction_global_ _n(getfunction_global_)
#define getsetf_global_ _n(getsetf_global_)
#define gensymp _n(gensymp)
#define make_symbolchar _n(make_symbolchar)
#define make_gensym_ _n(make_gensym_)
#define make_gensym_prefix_ _n(make_gensym_prefix_)
#define make_gensym_integer_ _n(make_gensym_integer_)
#define make_gensym_char_ _n(make_gensym_char_)
#define setcounter_gensym _n(setcounter_gensym)

#define GetNameSymbol_Low(s,v)      GetArrayA2((s), SYMBOL_INDEX_NAME, (v))
#define SetNameSymbol_Low(s,v)      SetArrayA2((s), SYMBOL_INDEX_NAME, (v))
#define GetValueSymbol_Low(s,v)     GetArrayA2((s), SYMBOL_INDEX_VALUE, (v))
#define SetValueSymbol_Low(s,v)     SetArrayA2((s), SYMBOL_INDEX_VALUE, (v))
#define GetFunctionSymbol_Low(s,v)  GetArrayA2((s), SYMBOL_INDEX_FUNCTION, (v))
#define SetFunctionSymbol_Low(s,v)  SetArrayA2((s), SYMBOL_INDEX_FUNCTION, (v))
#define GetPackageSymbol_Low(s,v)   GetArrayA2((s), SYMBOL_INDEX_PACKAGE, (v))
#define SetPackageSymbol_Low(s,v)   SetArrayA2_force((s), SYMBOL_INDEX_PACKAGE, (v))
#define GetPlistSymbol_Low(s,v)     GetArrayA2((s), SYMBOL_INDEX_PLIST, (v))
#define SetPlistSymbol_Low(s,v)     SetArrayA2_force((s), SYMBOL_INDEX_PLIST, (v))

#define GetSpecialSymbol_Low(s,v)   GetArrayA2((s), SYMBOL_INDEX_SPECIAL, (v))
#define SetSpecialSymbol_Low(s,v)   SetArrayA2((s), SYMBOL_INDEX_SPECIAL, (v))
#define GetInfoSymbol_Low(s,v)      GetArrayA2((s), SYMBOL_INDEX_INFO, (v))
#define SetInfoSymbol_Low(s,v)      SetArrayA2_force((s), SYMBOL_INDEX_INFO, (v))

#ifdef LISP_DEBUG
#define GetNameSymbol(s,v)          getname_symbol((s), (v))
#define SetNameSymbol(s,v)          setname_symbol((s), (v))
#define GetFunctionSymbol(s,v)      getfunction_symbol((s), (v))
#define SetFunctionSymbol(s,v)      setfunction_symbol((s), (v))
#define GetPackageSymbol(s,v)       getpackage_symbol((s), (v))
#define SetPackageSymbol(s,v)       setpackage_symbol((s), (v))
#define GetPlistSymbol(s,v)         getplist_symbol((s), (v))
#define SetPlistSymbol(s,v)         setplist_symbol((s), (v))
#else
#define GetNameSymbol(s,v)          GetNameSymbol_Low(s,v)
#define SetNameSymbol(s,v)          SetNameSymbol_Low(s,v)
#define GetFunctionSymbol(s,v)      GetFunctionSymbol_Low(s,v)
#define SetFunctionSymbol(s,v)      SetFunctionSymbol_Low(s,v)
#define GetPackageSymbol(s,v)       GetPackageSymbol_Low(s,v)
#define SetPackageSymbol(s,v)       SetPackageSymbol_Low(s,v)
#define GetPlistSymbol(s,v)         GetPlistSymbol_Low(s,v)
#define SetPlistSymbol(s,v)         SetPlistSymbol_Low(s,v)
#endif

#define GetValueSymbol(s,v)         getvalue_symbol((s), (v))
#define SetValueSymbol(s,v)         setvalue_symbol((s), (v))

_g int init_symbol(void);
_g void free_symbol(void);
_g void build_symbol(void);

_g void symbol_heap(addr *ret);
_g int symbolp(addr pos);
_g int keywordp(addr pos);

_g void getname_symbol(addr symbol, addr *ret);
_g void setname_symbol(addr symbol, addr value);
_g void getvalue_symbol(addr symbol, addr *ret);
_g void setvalue_symbol(addr symbol, addr value);
_g int setvalue_symbol_(addr symbol, addr value);
_g void getfunction_symbol(addr symbol, addr *ret);
_g void setfunction_symbol(addr symbol, addr value);
_g int setfunction_symbol_(addr symbol, addr value);
_g void getpackage_symbol(addr symbol, addr *ret);
_g void setpackage_symbol(addr symbol, addr value);
_g void getplist_symbol(addr symbol, addr *ret);
_g void setplist_symbol(addr symbol, addr value);

_g void gettype_value_symbol(addr symbol, addr *ret);
_g void settype_value_symbol(addr symbol, addr value);
_g int settype_value_symbol_(addr symbol, addr value);
_g void remtype_value_symbol(addr symbol);

_g void gettype_function_symbol(addr symbol, addr *ret);
_g void settype_function_symbol(addr symbol, addr value);
_g int settype_function_symbol_(addr symbol, addr value);
_g int remtype_function_symbol_(addr symbol);

_g void gettype_setf_symbol(addr symbol, addr *ret);
_g void settype_setf_symbol(addr symbol, addr value);
_g int settype_setf_symbol_(addr symbol, addr value);
_g int remtype_setf_symbol_(addr symbol);

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

_g void getsetf_symbol(addr symbol, addr *ret);
_g void setsetf_symbol(addr symbol, addr value);
_g int setsetf_symbol_(addr symbol, addr value);
_g void remsetf_symbol(addr symbol);

_g void getsetfmacro_symbol(addr symbol, addr *ret);
_g void setsetfmacro_symbol(addr symbol, addr value);
_g int setsetfmacro_symbol_(addr symbol, addr value);
_g void remsetfmacro_symbol(addr symbol);

_g void getmacro_symbol(addr symbol, addr *ret);
_g void setmacro_symbol(addr symbol, addr value);
_g int setmacro_symbol_(addr symbol, addr value);
_g void remmacro_symbol(addr symbol);

_g void evalsymbol_macro_symbol(addr symbol, addr *ret);
_g void formsymbol_macro_symbol(addr symbol, addr *ret);
_g int setsymbol_macro_symbol_(addr symbol, addr eval, addr form);
_g void remsymbol_macro_symbol(addr symbol);

_g void get_compiler_macro_symbol(addr symbol, addr *ret);
_g int set_compiler_macro_symbol_(addr symbol, addr value);
_g void get_setf_compiler_macro_symbol(addr symbol, addr *ret);
_g int set_setf_compiler_macro_symbol_(addr symbol, addr value);

_g void getscope_symbol(addr symbol, addr *ret);
_g void setspecial_symbol(addr symbol);
_g int setspecial_symbol_(addr symbol);
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
_g int setdeftype_symbol_(addr symbol, addr value);
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
_g void getspecial_unsafe(Execute ptr, addr pos, addr *ret);
_g void setspecial_unsafe(Execute ptr, addr pos, addr value);
_g void getspecial_local(Execute ptr, addr pos, addr *ret);
_g int getspecialcheck_local_(Execute ptr, addr pos, addr *ret);
_g void setspecial_local(Execute ptr, addr pos, addr value);

_g int getfunction_global_(addr pos, addr *ret);
_g int getsetf_global_(addr pos, addr *ret);

/* gensym */
_g int gensymp(addr pos);
_g void make_symbolchar(addr *ret, const char *str);
_g int make_gensym_(Execute ptr, addr *ret);
_g int make_gensym_prefix_(Execute ptr, addr prefix, addr *ret);
_g int make_gensym_integer_(Execute ptr, addr value, addr *ret);
_g int make_gensym_char_(Execute ptr, const char *str, addr value, addr *ret);
_g void setcounter_gensym(Execute ptr, fixnum value);

#endif

