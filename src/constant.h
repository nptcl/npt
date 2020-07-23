#ifndef __CONSTANT_HEADER__
#define __CONSTANT_HEADER__

#include "build.h"
#include "constant_table.h"
#include "define.h"
#include "local.h"
#include "memory.h"

#define RetConstant(i)		RetArrayA4(LispRoot(CONST),(i))
#define GetConstant(i,v)	GetArrayA4(LispRoot(CONST),(i),(v))
#define SetConstant(i,v)	SetArrayA4(LispRoot(CONST),(i),(v))

#define GetConst(i,v)		GetConstant(CONSTANT_##i, (v))
#define SetConst(i,v)		SetConstant(CONSTANT_##i, (v))

_g void build_constant(void);
_g int intern_symbol_header_(void);
_g int specialconstant_(constindex index, const char *package, const char *name);
_g void gensymconstant(constindex index, const char *name);
_g int keywordconstant_(constindex index, const char *name);
_g int commonconstant_(constindex index, const char *name);

_g int symbolchar_common_(constindex index, const char *name);
_g int symbolchar_keyword_(constindex index, const char *name);
_g void quotelist_heap(addr *ret, addr name);
_g void pushconst_heap(addr *ret, constindex index);
#define PushConst(a,b) pushconst_heap((a),CONSTANT_##b)

#endif

