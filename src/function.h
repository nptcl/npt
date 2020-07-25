#ifndef __FUNCTION_HEADER__
#define __FUNCTION_HEADER__

#include "execute.h"
#include "local.h"
#include "memory.h"
#include "pointer.h"
#include "typedef.h"

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

_g void function_alloc(LocalRoot local, addr *ret, addr name, addr code);
_g void function_local(LocalRoot local, addr *ret, addr name, addr code);
_g void function_heap(addr *ret, addr name, addr code);
_g void function_heap_for_develop(addr *ret, addr name);
_g void macro_alloc(LocalRoot local, addr *ret, addr name, addr code);
_g void macro_local(LocalRoot local, addr *ret, addr name, addr code);
_g void macro_heap(addr *ret, addr name, addr code);

_g void compiled_alloc(LocalRoot local, addr *ret, addr name);
_g void compiled_local(LocalRoot local, addr *ret, addr name);
_g void compiled_heap(addr *ret, addr name);
_g void compiled_macro_alloc(LocalRoot local, addr *ret, addr name);
_g void compiled_macro_local(LocalRoot local, addr *ret, addr name);
_g void compiled_macro_heap(addr *ret, addr name);

_g void compiled_system(addr *ret, addr name);
_g void compiled_setf_system(addr *ret, addr symbol);
_g void compiled_macro_system(addr *ret, addr name);

_g void setcompiled_code(addr pos, pointer p);
_g void getcompiled_code(addr pos, pointer *ret);
_g void setcompiled_macro(addr pos, pointer p);
_g void setcompiled_none(addr pos, pointer p);
_g void setcompiled_any(addr pos, pointer p);
_g void setcompiled_empty(addr pos, pointer p);
_g void setcompiled_dynamic(addr pos, pointer p);
_g void setcompiled_rest(addr pos, pointer p);
_g void setcompiled_var1(addr pos, pointer p);
_g void setcompiled_var2(addr pos, pointer p);
_g void setcompiled_var3(addr pos, pointer p);
_g void setcompiled_var4(addr pos, pointer p);
_g void setcompiled_var5(addr pos, pointer p);
_g void setcompiled_var6(addr pos, pointer p);
_g void setcompiled_opt1(addr pos, pointer p);
_g void setcompiled_opt2(addr pos, pointer p);
_g void setcompiled_opt3(addr pos, pointer p);
_g void setcompiled_opt4(addr pos, pointer p);
_g void setcompiled_opt5(addr pos, pointer p);
_g void setcompiled_var1opt1(addr pos, pointer p);
_g void setcompiled_var2opt1(addr pos, pointer p);
_g void setcompiled_var3opt1(addr pos, pointer p);
_g void setcompiled_var4opt1(addr pos, pointer p);
_g void setcompiled_var5opt1(addr pos, pointer p);
_g void setcompiled_var1opt2(addr pos, pointer p);
_g void setcompiled_var2opt2(addr pos, pointer p);
_g void setcompiled_var2opt3(addr pos, pointer p);
_g void setcompiled_var1rest(addr pos, pointer p);
_g void setcompiled_var2rest(addr pos, pointer p);
_g void setcompiled_opt1rest(addr pos, pointer p);
_g void setcompiled_var1dynamic(addr pos, pointer p);
_g void setcompiled_var2dynamic(addr pos, pointer p);
_g void setcompiled_var3dynamic(addr pos, pointer p);
_g void setcompiled_var4dynamic(addr pos, pointer p);
_g void setcompiled_opt1dynamic(addr pos, pointer p);
_g void setcompiled_extend_dynamic(addr pos, pointer p);
_g void setcompiled_extend_rest(addr pos, pointer p);

_g struct function_struct *structfunction(addr pos);
_g void getcodefunction(addr pos, addr *ret);
_g void setcodefunction(addr pos, addr value);
_g void getnamefunction(addr pos, addr *ret);
_g void setnamefunction(addr pos, addr value);
_g void getdatafunction(addr pos, addr *ret);
_g void setdatafunction(addr pos, addr value);

_g void gettype_function(addr pos, addr *ret);
_g void settype_function(addr pos, addr value);
_g void getdocumentation_function(addr pos, addr *ret);
_g void setdocumentation_function(addr pos, addr value);
_g void getlambda_expression_function(addr pos, addr *ret);
_g void setlambda_expression_function(addr pos, addr value);
_g void getdefunform_function(addr pos, addr *ret);
_g void setdefunform_function(addr pos, addr value);

_g int functionp(addr pos);
_g int funcall_function_p(addr pos);
_g int macro_function_p(addr pos);
_g int interpreted_function_p(addr pos);
_g int interpreted_funcall_function_p(addr pos);
_g int interpreted_macro_function_p(addr pos);
_g int compiled_function_p(addr pos);
_g int compiled_funcall_function_p(addr pos);
_g int compiled_macro_function_p(addr pos);
_g void settrace_function(addr pos);
_g int tracep_function(addr pos);

#endif

