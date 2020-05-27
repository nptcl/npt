#ifndef __CODE_INIT_HEADER__
#define __CODE_INIT_HEADER__

#include "constant.h"
#include "pointer.h"
#include "typedef.h"

enum CodeValueType {
	CodeValueType_Addr,
	CodeValueType_Index,
	CodeValueType_Fixnum,
	CodeValueType_Character,
	CodeValueType_Null
};
__extern byte CodeValueArray[p_size_code];

_g void init_code_init(void);
_g void build_code_init(void);
_g void defcode_constant(constindex index, pointer p);

#endif

