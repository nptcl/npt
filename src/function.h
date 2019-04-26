#ifndef __FUNCTION_HEADER__
#define __FUNCTION_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "memory.h"

enum CALLNAME_TYPE {
	CALLNAME_ERROR = 0,
	CALLNAME_SYMBOL,
	CALLNAME_SETF,
	CALLNAME_SIZE
};

enum FUNCTION_INDEX {
	FUNCTION_INDEX_CODE,
	FUNCTION_INDEX_NAME,
	FUNCTION_INDEX_DATA,
	FUNCTION_INDEX_TABLE,
	FUNCTION_INDEX_CLOSURE_VALUE,
	FUNCTION_INDEX_CLOSURE_FUNCTION,
	FUNCTION_INDEX_CLOSURE_TAGBODY,
	FUNCTION_INDEX_CLOSURE_BLOCK,
	FUNCTION_INDEX_SIZE
};

struct function_struct {
	unsigned macro : 1;
	unsigned compiled : 1;
	unsigned recursive : 1;
	unsigned system : 1;
	struct callbind_struct call;
};

#define RefCallName_Low(s)          RefArrayA2((s),0)
#define GetCallName_Low(s,v)        GetArrayA2((s),0,(v))
#define SetCallName_Low(s,v)        SetArrayA2((s),0,(v))
#define RefCallNameType_Low(s)      ((enum CALLNAME_TYPE)GetUser(s))
#define GetCallNameType_Low(s,v)    (*(v) = RefCallNameType_Low(s))
#define SetCallNameType_Low(s,v)    SetUser((s), (byte)(v))

#define PtrFunction_Low(x)			PtrBodySSa(x, FUNCTION_INDEX_SIZE)
#define StructFunction_Low(x)		((struct function_struct *)PtrFunction_Low(x))
#define RefFunction_Low(x)			RefArraySS((x),FUNCTION_INDEX_CODE)
#define GetFunction_Low(x,v)		GetArraySS((x),FUNCTION_INDEX_CODE,(v))
#define SetFunction_Low(x,v)		SetArraySS((x),FUNCTION_INDEX_CODE,(v))
#define RefNameFunction_Low(x)		RefArraySS((x),FUNCTION_INDEX_NAME)
#define GetNameFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_NAME,(v))
#define SetNameFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_NAME,(v))
#define RefDataFunction_Low(x)		RefArraySS((x),FUNCTION_INDEX_DATA)
#define GetDataFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_DATA,(v))
#define SetDataFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_DATA,(v))
#define RefTableFunction_Low(x)		RefArraySS((x),FUNCTION_INDEX_TABLE)
#define GetTableFunction_Low(x,v)	GetArraySS((x),FUNCTION_INDEX_TABLE,(v))
#define SetTableFunction_Low(x,v)	SetArraySS((x),FUNCTION_INDEX_TABLE,(v))
#define CallBindCompiled_Low(x)		(&(StructFunction_Low(x)->call))

#define GetClosureValueFunction_Low(x,v)	\
	GetArraySS((x),FUNCTION_INDEX_CLOSURE_VALUE,(v))
#define SetClosureValueFunction_Low(x,v)	\
	SetArraySS((x),FUNCTION_INDEX_CLOSURE_VALUE,(v))
#define GetClosureFunctionFunction_Low(x,v)	\
	GetArraySS((x),FUNCTION_INDEX_CLOSURE_FUNCTION,(v))
#define SetClosureFunctionFunction_Low(x,v)	\
	SetArraySS((x),FUNCTION_INDEX_CLOSURE_FUNCTION,(v))
#define GetClosureTagbodyFunction_Low(x,v)	\
	GetArraySS((x),FUNCTION_INDEX_CLOSURE_TAGBODY,(v))
#define SetClosureTagbodyFunction_Low(x,v)	\
	SetArraySS((x),FUNCTION_INDEX_CLOSURE_TAGBODY,(v))
#define GetClosureBlockFunction_Low(x,v)	\
	GetArraySS((x),FUNCTION_INDEX_CLOSURE_BLOCK,(v))
#define SetClosureBlockFunction_Low(x,v)	\
	SetArraySS((x),FUNCTION_INDEX_CLOSURE_BLOCK,(v))

#ifdef LISP_DEBUG
#define RefCallName(s)              refcallname(s)
#define GetCallName(s,v)            getcallname(s,v)
#define SetCallName(s,v)            setcallname(s,v)
#define RefCallNameType(s)          refcallnametype(s)
#define GetCallNameType(s,v)        getcallnametype(s,v)
#define SetCallNameType(s,v)        setcallnametype(s,v)

#define StructFunction(x)			structfunction(x)
#define RefFunction(x)				reffunction(x)
#define GetFunction(x,v)			getfunction(x,v)
#define SetFunction(x,v)			setfunction(x,v)
#define RefNameFunction(x)			refnamefunction(x)
#define GetNameFunction(x,v)		getnamefunction(x,v)
#define SetNameFunction(x,v)		setnamefunction(x,v)
#define RefDataFunction(x)			refdatafunction(x)
#define GetDataFunction(x,v)		getdatafunction(x,v)
#define SetDataFunction(x,v)		setdatafunction(x,v)
#define CallBindCompiled(x)			callbindcompiled(x)

#define GetClosureValueFunction(x,v)	getclosure_value_function(x,v)
#define SetClosureValueFunction(x,v)	setclosure_value_function(x,v)
#define GetClosureFunctionFunction(x,v)	getclosure_function_function(x,v)
#define SetClosureFunctionFunction(x,v)	setclosure_function_function(x,v)
#define GetClosureTagbodyFunction(x,v)	getclosure_tagbody_function(x,v)
#define SetClosureTagbodyFunction(x,v)	setclosure_tagbody_function(x,v)
#define GetClosureBlockFunction(x,v)	getclosure_block_function(x,v)
#define SetClosureBlockFunction(x,v)	setclosure_block_function(x,v)
#else
#define RefCallName(s)              RefCallName_Low(s)
#define GetCallName(s,v)            GetCallName_Low(s,v)
#define SetCallName(s,v)            SetCallName_Low(s,v)
#define RefCallNameType(s)          RefCallNameType_Low(s)
#define GetCallNameType(s,v)        GetCallNameType_Low(s,v)
#define SetCallNameType(s,v)        SetCallNameType_Low(s,v)

#define StructFunction(x)			StructFunction_Low(x)
#define RefFunction(x)				RefFunction_Low(x)
#define GetFunction(x,v)			GetFunction_Low(x,v)
#define SetFunction(x,v)			SetFunction_Low(x,v)
#define RefNameFunction(x)			RefNameFunction_Low(x)
#define GetNameFunction(x,v)		GetNameFunction_Low(x,v)
#define SetNameFunction(x,v)		setnamefunction(x,v)
#define RefDataFunction(x)			RefDataFunction_Low(x)
#define GetDataFunction(x,v)		GetDataFunction_Low(x,v)
#define SetDataFunction(x,v)		SetDataFunction_Low(x,v)
#define CallBindCompiled(x)			CallBindCompiled_Low(x)

#define GetClosureValueFunction(x,v)	GetClosureValueFunction_Low(x,v)
#define SetClosureValueFunction(x,v)	SetClosureValueFunction_Low(x,v)
#define GetClosureFunctionFunction(x,v)	GetClosureFunctionFunction_Low(x,v)
#define SetClosureFunctionFunction(x,v)	SetClosureFunctionFunction_Low(x,v)
#define GetClosureTagbodyFunction(x,v)	GetClosureTagbodyFunction_Low(x,v)
#define SetClosureTagbodyFunction(x,v)	SetClosureTagbodyFunction_Low(x,v)
#define GetClosureBlockFunction(x,v)	GetClosureBlockFunction_Low(x,v)
#define SetClosureBlockFunction(x,v)	SetClosureBlockFunction_Low(x,v)
#endif

/* callname */
void make_callname_alloc(LocalRoot local, addr *ret);
addr callname_allocr(LocalRoot local, addr name, enum CALLNAME_TYPE type);
addr callname_localr(LocalRoot local, addr name, enum CALLNAME_TYPE type);
addr callname_heapr(addr name, enum CALLNAME_TYPE type);
void callname_alloc(LocalRoot local, addr *ret, addr name, enum CALLNAME_TYPE type);
void callname_local(LocalRoot local, addr *ret, addr name, enum CALLNAME_TYPE type);
void callname_heap(addr *ret, addr name, enum CALLNAME_TYPE type);

int parse_callname_alloc(LocalRoot local, addr *ret, addr name);
int parse_callname_local(LocalRoot local, addr *ret, addr name);
int parse_callname_heap(addr *ret, addr name);
void parse_callname_error(addr *ret, addr name);
void setf_callname_alloc(LocalRoot local, addr *ret, addr symbol);
void setf_callname_local(LocalRoot local, addr *ret, addr symbol);
void setf_callname_heap(addr *ret, addr symbol);
int parse_setcallname(addr pos, addr name);
enum CALLNAME_TYPE parse_callname(addr name, addr *ret);
int function_name_p(addr name);
int callnamep(addr pos);
int symbol_callname_p(addr call);
int setf_callname_p(addr call);
addr refcallname(addr pos);
void getcallname(addr pos, addr *value);
void setcallname(addr pos, addr value);
enum CALLNAME_TYPE refcallnametype(addr pos);
void getcallnametype(addr pos, enum CALLNAME_TYPE *value);
void setcallnametype(addr pos, enum CALLNAME_TYPE value);
int callname_constant_p(addr pos);
int equal_callname(addr left, addr right);
enum CALLNAME_TYPE getfunction_callname_global(addr pos, addr *ret);
enum CALLNAME_TYPE getfunction_callname_local(Execute ptr, addr pos, addr *ret);
enum CALLNAME_TYPE getfunctioncheck_callname_local(Execute ptr, addr pos, addr *ret);
void setfunction_callname_global(addr pos, addr value);

void copy_callname_alloc(LocalRoot local, addr *ret, addr pos);
void copy_callname_local(LocalRoot local, addr *ret, addr pos);
void copy_callname_heap(addr *ret, addr pos);

addr refcallname_local(Execute ptr, addr pos);
void getcallname_local(Execute ptr, addr pos, addr *value);
void setcallname_local(Execute ptr, addr pos, addr value);
addr refcallnamecheck_local(Execute ptr, addr pos);
void getcallnamecheck_local(Execute ptr, addr pos, addr *value);

void setcallname_global(addr pos, addr value);
addr refcallname_global(addr pos);
void getcallname_global(addr pos, addr *value);
addr refcallnamecheck_global(addr pos);
void getcallnamecheck_global(addr pos, addr *value);

void name_callname_alloc(LocalRoot local, addr pos, addr *ret);
void name_callname_local(LocalRoot local, addr pos, addr *ret);
void name_callname_heap(addr pos, addr *ret);

/* function */
addr function_allocr(LocalRoot local, addr name, addr code);
addr function_localr(LocalRoot local, addr name, addr code);
addr function_heapr(addr name, addr code);
void function_alloc(LocalRoot local, addr *ret, addr name, addr code);
void function_local(LocalRoot local, addr *ret, addr name, addr code);
void function_heap(addr *ret, addr name, addr code);
addr macro_allocr(LocalRoot local, addr name, addr code);
addr macro_localr(LocalRoot local, addr name, addr code);
addr macro_heapr(addr name, addr code);
void macro_alloc(LocalRoot local, addr *ret, addr name, addr code);
void macro_local(LocalRoot local, addr *ret, addr name, addr code);
void macro_heap(addr *ret, addr name, addr code);

addr compiled_allocr(LocalRoot local, addr name);
addr compiled_localr(LocalRoot local, addr name);
addr compiled_heapr(addr name);
void compiled_alloc(LocalRoot local, addr *ret, addr name);
void compiled_local(LocalRoot local, addr *ret, addr name);
void compiled_heap(addr *ret, addr name);
addr compiled_macro_allocr(LocalRoot local, addr name);
addr compiled_macro_localr(LocalRoot local, addr name);
addr compiled_macro_heapr(addr name);
void compiled_macro_alloc(LocalRoot local, addr *ret, addr name);
void compiled_macro_local(LocalRoot local, addr *ret, addr name);
void compiled_macro_heap(addr *ret, addr name);

void setcompiled_system(addr pos, calltype call);
void getcompiled_system(addr pos, calltype *ret);
void setcompiled_type(addr pos, void *call);
void getcompiled_type(addr pos, void **ret);
void setcompiled_macro(addr pos, callbind_macro call);
void setcompiled_none(addr pos, callbind_none call);
void setcompiled_any(addr pos, callbind_any call);
void setcompiled_empty(addr pos, callbind_empty call);
void setcompiled_dynamic(addr pos, callbind_dynamic call);
void setcompiled_rest(addr pos, callbind_rest call);
void setcompiled_var1(addr pos, callbind_var1 call);
void setcompiled_var2(addr pos, callbind_var2 call);
void setcompiled_var3(addr pos, callbind_var3 call);
void setcompiled_var4(addr pos, callbind_var4 call);
void setcompiled_var5(addr pos, callbind_var5 call);
void setcompiled_var6(addr pos, callbind_var6 call);
void setcompiled_opt1(addr pos, callbind_opt1 call);
void setcompiled_opt2(addr pos, callbind_opt2 call);
void setcompiled_opt3(addr pos, callbind_opt3 call);
void setcompiled_opt4(addr pos, callbind_opt4 call);
void setcompiled_opt5(addr pos, callbind_opt5 call);
void setcompiled_var1opt1(addr pos, callbind_var1opt1 call);
void setcompiled_var2opt1(addr pos, callbind_var2opt1 call);
void setcompiled_var3opt1(addr pos, callbind_var3opt1 call);
void setcompiled_var4opt1(addr pos, callbind_var4opt1 call);
void setcompiled_var5opt1(addr pos, callbind_var5opt1 call);
void setcompiled_var1opt2(addr pos, callbind_var1opt2 call);
void setcompiled_var2opt2(addr pos, callbind_var2opt2 call);
void setcompiled_var1rest(addr pos, callbind_var1rest call);
void setcompiled_var2rest(addr pos, callbind_var2rest call);
void setcompiled_var1dynamic(addr pos, callbind_var1dynamic call);
void setcompiled_var2dynamic(addr pos, callbind_var2dynamic call);
void setcompiled_var3dynamic(addr pos, callbind_var3dynamic call);
void setcompiled_var4dynamic(addr pos, callbind_var4dynamic call);

void function_heap_for_develop(addr *ret, addr name);

struct function_struct *structfunction(addr pos);
addr reffunction(addr pos);
void getfunction(addr pos, addr *ret);
void setfunction(addr pos, addr value);
struct callbind_struct *callbindcompiled(addr pos);
addr refnamefunction(addr pos);
void getnamefunction(addr pos, addr *ret);
void setnamefunction(addr pos, addr value);
addr refdatafunction(addr pos);
void getdatafunction(addr pos, addr *ret);
void setdatafunction(addr pos, addr value);

void getclosure_value_function(addr pos, addr *ret);
void setclosure_value_function(addr pos, addr value);
void getclosure_function_function(addr pos, addr *ret);
void setclosure_function_function(addr pos, addr value);
void getclosure_tagbody_function(addr pos, addr *ret);
void setclosure_tagbody_function(addr pos, addr value);
void getclosure_block_function(addr pos, addr *ret);
void setclosure_block_function(addr pos, addr value);
void pushclosure_value_function(addr pos, addr key, addr value);
void pushclosure_function_function(addr pos, addr key, addr value);
void pushclosure_tagbody_function(addr pos, addr key, addr value);
void pushclosure_block_function(addr pos, addr value);

void gettype_function(addr pos, addr *ret);
void settype_function(addr pos, addr value);
void getdocumentation_function(addr pos, addr *ret);
void setdocumentation_function(addr pos, addr value);
void getlambda_expression_function(addr pos, addr *ret);
void setlambda_expression_function(addr pos, addr value);

void setsystem_function(addr pos);
int functionp(addr pos);
int funcall_function_p(addr pos);
int macro_function_p(addr pos);
int interpreted_function_p(addr pos);
int interpreted_funcall_function_p(addr pos);
int interpreted_macro_function_p(addr pos);
int compiled_function_p(addr pos);
int compiled_funcall_function_p(addr pos);
int compiled_macro_function_p(addr pos);
int system_function_p(addr pos);
void setrecursive_function(addr pos);
int recursivep_function(addr pos);

#endif

