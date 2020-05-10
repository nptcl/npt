#ifndef __CODE_OBJECT_HEADER__
#define __CODE_OBJECT_HEADER__

#include "constant.h"
#include "pointer.h"
#include "typedef.h"

enum Code_Index {
	Code_Array,
	Code_Call,
	Code_Argument,
	Code_Size
};

struct code_struct {
	unsigned p_control : 1;
	unsigned p_args : 1;
	callbind_code *sys;
	addr *args;
	size_t size;
};

#define RefArrayCode		RefArraySS
#define GetArrayCode		GetArraySS
#define SetArrayCode		SetArraySS
#define PtrBodyCode(p)		PtrBodySSa((p), Code_Size)
#define StructCode(p)		((struct code_struct *)PtrBodyCode(p))
#define PtrCallCode(p)		((void *)PtrBodyB4(p))
#define RefArgumentCall		RefArrayA4
#define GetArgumentCall		GetArrayA4
#define SetArgumentCall		SetArrayA4

_g void code_heap(addr *ret, addr codeA4);
_g void function_empty_heap(addr *ret, addr name);
_g void getarray_code(addr pos, addr *ret);
_g void getargs_code(addr pos, addr *ret);
_g void setpointer_code(addr code);
_g void setpointer_code_call(addr code);
_g void allpointer_code(addr code);

#endif

