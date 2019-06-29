#ifndef __CODE_HEADER__
#define __CODE_HEADER__

#include "build.h"
#include "pointer.h"

enum CodeType {
	CodeType_Default,
	CodeType_Return,
	CodeType_Argument,
	CodeType_Push,
	CodeType_Remove,
	CodeType_Close,
	CodeType_Protect,
	CodeType_TagBody,
	CodeType_Block,
	CodeType_Catch,
	CodeType_Condition,
	CodeType_Restart,
	CodeType_Size
};

_g void init_code(void);
_g void build_code(void);
_g void code_heap(addr *ret, addr codeA4);
_g void code_empty_heap(addr *ret);
_g void function_empty_heap(addr *ret, addr name);
_g const pointer *getcalltype_code(addr pos);
_g void getarray_code(addr pos, addr *ret);
_g void getargs_code(addr pos, addr *ret);

_g enum CodeType gettype_code(addr pos);
_g void settype_code(addr pos, enum CodeType type);
_g void getinfo_code(addr pos, addr *ret);
_g void setinfo_code(addr pos, addr value);
_g void syscall_code(LocalRoot local, addr *ret, pointer call, addr value);

#endif

