#ifndef __FUNCTION_HEADER__
#define __FUNCTION_HEADER__

#include "execute.h"
#include "local.h"
#include "memory.h"
#include "pointer.h"
#include "typedef.h"

#define function_alloc _n(function_alloc)
#define function_local _n(function_local)
#define function_heap _n(function_heap)
#define function_empty_heap _n(function_empty_heap)
#define macro_alloc _n(macro_alloc)
#define macro_local _n(macro_local)
#define macro_heap _n(macro_heap)
#define compiled_alloc _n(compiled_alloc)
#define compiled_local _n(compiled_local)
#define compiled_heap _n(compiled_heap)
#define compiled_macro_alloc _n(compiled_macro_alloc)
#define compiled_macro_local _n(compiled_macro_local)
#define compiled_macro_heap _n(compiled_macro_heap)
#define compiled_system _n(compiled_system)
#define compiled_setf_system _n(compiled_setf_system)
#define compiled_macro_system _n(compiled_macro_system)
#define setcompiled_code _n(setcompiled_code)
#define getcompiled_code _n(getcompiled_code)
#define setcompiled_macro _n(setcompiled_macro)
#define setcompiled_none _n(setcompiled_none)
#define setcompiled_any _n(setcompiled_any)
#define setcompiled_empty _n(setcompiled_empty)
#define setcompiled_dynamic _n(setcompiled_dynamic)
#define setcompiled_rest _n(setcompiled_rest)
#define setcompiled_var1 _n(setcompiled_var1)
#define setcompiled_var2 _n(setcompiled_var2)
#define setcompiled_var3 _n(setcompiled_var3)
#define setcompiled_var4 _n(setcompiled_var4)
#define setcompiled_var5 _n(setcompiled_var5)
#define setcompiled_var6 _n(setcompiled_var6)
#define setcompiled_opt1 _n(setcompiled_opt1)
#define setcompiled_opt2 _n(setcompiled_opt2)
#define setcompiled_opt3 _n(setcompiled_opt3)
#define setcompiled_opt4 _n(setcompiled_opt4)
#define setcompiled_opt5 _n(setcompiled_opt5)
#define setcompiled_var1opt1 _n(setcompiled_var1opt1)
#define setcompiled_var2opt1 _n(setcompiled_var2opt1)
#define setcompiled_var3opt1 _n(setcompiled_var3opt1)
#define setcompiled_var4opt1 _n(setcompiled_var4opt1)
#define setcompiled_var5opt1 _n(setcompiled_var5opt1)
#define setcompiled_var1opt2 _n(setcompiled_var1opt2)
#define setcompiled_var2opt2 _n(setcompiled_var2opt2)
#define setcompiled_var2opt3 _n(setcompiled_var2opt3)
#define setcompiled_var1rest _n(setcompiled_var1rest)
#define setcompiled_var2rest _n(setcompiled_var2rest)
#define setcompiled_var3rest _n(setcompiled_var3rest)
#define setcompiled_var4rest _n(setcompiled_var4rest)
#define setcompiled_opt1rest _n(setcompiled_opt1rest)
#define setcompiled_var1dynamic _n(setcompiled_var1dynamic)
#define setcompiled_var2dynamic _n(setcompiled_var2dynamic)
#define setcompiled_var3dynamic _n(setcompiled_var3dynamic)
#define setcompiled_var4dynamic _n(setcompiled_var4dynamic)
#define setcompiled_opt1dynamic _n(setcompiled_opt1dynamic)
#define setcompiled_extend_dynamic _n(setcompiled_extend_dynamic)
#define setcompiled_extend_rest _n(setcompiled_extend_rest)
#define setcompiled_extend_empty _n(setcompiled_extend_empty)
#define setcompiled_extend_var1 _n(setcompiled_extend_var1)
#define setcompiled_extend_var2 _n(setcompiled_extend_var2)
#define setcompiled_extend_var3 _n(setcompiled_extend_var3)
#define structfunction _n(structfunction)
#define getcodefunction _n(getcodefunction)
#define setcodefunction _n(setcodefunction)
#define getnamefunction _n(getnamefunction)
#define setnamefunction _n(setnamefunction)
#define getdatafunction _n(getdatafunction)
#define setdatafunction _n(setdatafunction)
#define gettype_function _n(gettype_function)
#define settype_function _n(settype_function)
#define getdocumentation_function _n(getdocumentation_function)
#define setdocumentation_function _n(setdocumentation_function)
#define getlambda_expression_function _n(getlambda_expression_function)
#define setlambda_expression_function _n(setlambda_expression_function)
#define getdefunform_function _n(getdefunform_function)
#define setdefunform_function _n(setdefunform_function)
#define functionp _n(functionp)
#define funcall_function_p _n(funcall_function_p)
#define macro_function_p _n(macro_function_p)
#define interpreted_function_p _n(interpreted_function_p)
#define interpreted_funcall_function_p _n(interpreted_funcall_function_p)
#define interpreted_macro_function_p _n(interpreted_macro_function_p)
#define compiled_function_p _n(compiled_function_p)
#define compiled_funcall_function_p _n(compiled_funcall_function_p)
#define compiled_macro_function_p _n(compiled_macro_function_p)
#define settrace_function _n(settrace_function)
#define tracep_function _n(tracep_function)
#define build_function _n(build_function)
#define init_function _n(init_function)

enum FUNCTION_INDEX {
	FUNCTION_INDEX_CODE,
	FUNCTION_INDEX_NAME,
	FUNCTION_INDEX_DATA,
	FUNCTION_INDEX_TABLE,
	FUNCTION_INDEX_SIZE
};

struct function_struct {
	unsigned macro : 1;
	unsigned compiled : 1;
	unsigned trace : 1;
	pointer index;
};

#define PtrFunction_Low(x)			PtrBodySSa(x, FUNCTION_INDEX_SIZE)
#define StructFunction_Low(x)		((struct function_struct *)PtrFunction_Low(x))
#define GetCodeFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_CODE,(v))
#define SetCodeFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_CODE,(v))
#define GetNameFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_NAME,(v))
#define SetNameFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_NAME,(v))
#define GetDataFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_DATA,(v))
#define SetDataFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_DATA,(v))
#define GetTableFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_TABLE,(v))
#define SetTableFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_TABLE,(v))

#ifdef LISP_DEBUG
#define StructFunction(x)			structfunction(x)
#define GetCodeFunction(x,v)		getcodefunction(x,v)
#define SetCodeFunction(x,v)		setcodefunction(x,v)
#define GetNameFunction(x,v)		getnamefunction(x,v)
#define SetNameFunction(x,v)		setnamefunction(x,v)
#define GetDataFunction(x,v)		getdatafunction(x,v)
#define SetDataFunction(x,v)		setdatafunction(x,v)
#else
#define StructFunction(x)			StructFunction_Low(x)
#define GetCodeFunction(x,v)		GetCodeFunction_Low(x,v)
#define SetCodeFunction(x,v)		SetCodeFunction_Low(x,v)
#define GetNameFunction(x,v)		GetNameFunction_Low(x,v)
#define SetNameFunction(x,v)		setnamefunction(x,v)
#define GetDataFunction(x,v)		GetDataFunction_Low(x,v)
#define SetDataFunction(x,v)		SetDataFunction_Low(x,v)
#endif

void function_alloc(LocalRoot local, addr *ret, addr name, addr code);
void function_local(LocalRoot local, addr *ret, addr name, addr code);
void function_heap(addr *ret, addr name, addr code);
void function_empty_heap(addr *ret, addr name);
void macro_alloc(LocalRoot local, addr *ret, addr name, addr code);
void macro_local(LocalRoot local, addr *ret, addr name, addr code);
void macro_heap(addr *ret, addr name, addr code);

void compiled_alloc(LocalRoot local, addr *ret, addr name);
void compiled_local(LocalRoot local, addr *ret, addr name);
void compiled_heap(addr *ret, addr name);
void compiled_macro_alloc(LocalRoot local, addr *ret, addr name);
void compiled_macro_local(LocalRoot local, addr *ret, addr name);
void compiled_macro_heap(addr *ret, addr name);

void compiled_system(addr *ret, addr name);
void compiled_setf_system(addr *ret, addr symbol);
void compiled_macro_system(addr *ret, addr name);

void setcompiled_code(addr pos, pointer p);
void getcompiled_code(addr pos, pointer *ret);
void setcompiled_macro(addr pos, pointer p);
void setcompiled_none(addr pos, pointer p);
void setcompiled_any(addr pos, pointer p);
void setcompiled_empty(addr pos, pointer p);
void setcompiled_dynamic(addr pos, pointer p);
void setcompiled_rest(addr pos, pointer p);
void setcompiled_var1(addr pos, pointer p);
void setcompiled_var2(addr pos, pointer p);
void setcompiled_var3(addr pos, pointer p);
void setcompiled_var4(addr pos, pointer p);
void setcompiled_var5(addr pos, pointer p);
void setcompiled_var6(addr pos, pointer p);
void setcompiled_opt1(addr pos, pointer p);
void setcompiled_opt2(addr pos, pointer p);
void setcompiled_opt3(addr pos, pointer p);
void setcompiled_opt4(addr pos, pointer p);
void setcompiled_opt5(addr pos, pointer p);
void setcompiled_var1opt1(addr pos, pointer p);
void setcompiled_var2opt1(addr pos, pointer p);
void setcompiled_var3opt1(addr pos, pointer p);
void setcompiled_var4opt1(addr pos, pointer p);
void setcompiled_var5opt1(addr pos, pointer p);
void setcompiled_var1opt2(addr pos, pointer p);
void setcompiled_var2opt2(addr pos, pointer p);
void setcompiled_var2opt3(addr pos, pointer p);
void setcompiled_var1rest(addr pos, pointer p);
void setcompiled_var2rest(addr pos, pointer p);
void setcompiled_var3rest(addr pos, pointer p);
void setcompiled_var4rest(addr pos, pointer p);
void setcompiled_opt1rest(addr pos, pointer p);
void setcompiled_var1dynamic(addr pos, pointer p);
void setcompiled_var2dynamic(addr pos, pointer p);
void setcompiled_var3dynamic(addr pos, pointer p);
void setcompiled_var4dynamic(addr pos, pointer p);
void setcompiled_opt1dynamic(addr pos, pointer p);
void setcompiled_extend_dynamic(addr pos, pointer p);
void setcompiled_extend_rest(addr pos, pointer p);
void setcompiled_extend_empty(addr pos, pointer p);
void setcompiled_extend_var1(addr pos, pointer p);
void setcompiled_extend_var2(addr pos, pointer p);
void setcompiled_extend_var3(addr pos, pointer p);

struct function_struct *structfunction(addr pos);
void getcodefunction(addr pos, addr *ret);
void setcodefunction(addr pos, addr value);
void getnamefunction(addr pos, addr *ret);
void setnamefunction(addr pos, addr value);
void getdatafunction(addr pos, addr *ret);
void setdatafunction(addr pos, addr value);

void gettype_function(addr pos, addr *ret);
void settype_function(addr pos, addr value);
void getdocumentation_function(addr pos, addr *ret);
void setdocumentation_function(addr pos, addr value);
void getlambda_expression_function(addr pos, addr *ret);
void setlambda_expression_function(addr pos, addr value);
void getdefunform_function(addr pos, addr *ret);
void setdefunform_function(addr pos, addr value);

int functionp(addr pos);
int funcall_function_p(addr pos);
int macro_function_p(addr pos);
int interpreted_function_p(addr pos);
int interpreted_funcall_function_p(addr pos);
int interpreted_macro_function_p(addr pos);
int compiled_function_p(addr pos);
int compiled_funcall_function_p(addr pos);
int compiled_macro_function_p(addr pos);
void settrace_function(addr pos);
int tracep_function(addr pos);

void build_function(void);
void init_function(void);

#endif

