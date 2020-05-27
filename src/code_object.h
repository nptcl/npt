#ifndef __CODE_OBJECT_HEADER__
#define __CODE_OBJECT_HEADER__

#include "constant.h"
#include "pointer.h"
#include "typedef.h"

enum Code_Index {
	Code_Array,
	Code_Call,
	Code_Size
};

struct code_value {
	callbind_code call;
	CodeValue value;
};

struct code_struct {
	unsigned p_control : 1;
	unsigned p_args : 1;
	struct code_value *sys;
	size_t size;
};

#define RefArrayCode		RefArraySS
#define GetArrayCode		GetArraySS
#define SetArrayCode		SetArraySS
#define PtrBodyCode(p)		PtrBodySSa((p), Code_Size)
#define StructCode(p)		((struct code_struct *)PtrBodyCode(p))
#define StructCallCode(p)	((struct code_value *)PtrBodyB4(p))
#define RefArgumentCall		RefArrayA4
#define GetArgumentCall		GetArrayA4
#define SetArgumentCall		SetArrayA4

_g void code_heap(addr *ret, addr codeA4);
_g void function_empty_heap(addr *ret, addr name);
_g void getarray_code(addr pos, addr *ret);
_g void update_code(addr code);

#endif

