#ifndef __CODE_OBJECT_HEADER__
#define __CODE_OBJECT_HEADER__

#include "constant.h"
#include "pointer.h"
#include "typedef.h"

#define code_heap _n(code_heap)
#define getarray_code _n(getarray_code)
#define update_code _n(update_code)

enum Code_Index {
	Code_Array,
	Code_Call,
	Code_Size
};

struct code_value {
	callbind_code call;
	pointer id;
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

void code_heap(addr *ret, addr codeA4);
void getarray_code(addr pos, addr *ret);
void update_code(addr code);

#endif

