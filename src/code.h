#ifndef __CODE_HEADER__
#define __CODE_HEADER__

#include "build.h"
#include "constant.h"
#include "pointer.h"

enum Code_Index {
	Code_Array,
	Code_Call,
	Code_Argument,
	Code_Size
};

struct code_struct {
	unsigned p_control : 1;
	unsigned p_return : 1;
	unsigned p_push : 1;
	unsigned p_argument : 1;
	const pointer *call;
	size_t size;
};

#define RefArrayCode		RefArraySS
#define GetArrayCode		GetArraySS
#define SetArrayCode		SetArraySS
#define PtrBodyCode(p)		PtrBodySSa((p), Code_Size)
#define StructCode(p)		((struct code_struct *)PtrBodyCode(p))
#define PtrCallCode(p)		((pointer *)PtrBodyB4(p))
#define RefArgumentCall		RefArrayA4
#define GetArgumentCall		GetArrayA4
#define SetArgumentCall		SetArrayA4

_g void code_heap(addr *ret, addr codeA4);
_g void code_empty_heap(addr *ret);
_g void function_empty_heap(addr *ret, addr name);
_g const pointer *getcalltype_code(addr pos);
_g void getarray_code(addr pos, addr *ret);
_g void getargs_code(addr pos, addr *ret);

_g void init_code(void);
_g void build_code(void);
_g void defcode_constant(constindex index, pointer p);

#endif

