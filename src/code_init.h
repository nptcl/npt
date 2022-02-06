#ifndef __CODE_INIT_HEADER__
#define __CODE_INIT_HEADER__

#include "constant.h"
#include "pointer.h"
#include "typedef.h"

#define init_code_init _n(init_code_init)
#define build_code_init _n(build_code_init)
#define defcode_constant _n(defcode_constant)
#define CodeValueArray _n(CodeValueArray)

enum CodeValueType {
	CodeValueType_Addr,
	CodeValueType_Index,
	CodeValueType_Fixnum,
	CodeValueType_FixnumNull,
	CodeValueType_Character,
	CodeValueType_Null
};
extern byte CodeValueArray[p_size_code];

#define GetCodeValueArray(x,y) (*(y) = (enum CodeValueType)CodeValueArray[x])

void init_code_init(void);
void build_code_init(void);
void defcode_constant(constindex index, pointer p);

#endif

