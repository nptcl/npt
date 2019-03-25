#ifndef __CODE_HEADER__
#define __CODE_HEADER__

#include "build.h"

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

void build_code(void);
void code_heap(addr *ret, addr codeA4);
const calltype *getcalltype_code(addr pos);
void getarray_code(addr pos, addr *ret);
void getargs_code(addr pos, addr *ret);

enum CodeType gettype_code(addr pos);
void settype_code(addr pos, enum CodeType type);
void getinfo_code(addr pos, addr *ret);
void setinfo_code(addr pos, addr value);
void syscall_code(LocalRoot local, addr *ret, callbind_empty call, addr value);

#endif

