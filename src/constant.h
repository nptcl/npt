#ifndef __CONSTANT_HEADER__
#define __CONSTANT_HEADER__

#include "build.h"
#include "constant_table.h"
#include "define.h"
#include "local.h"
#include "memory.h"

#define build_constant _n(build_constant)
#define intern_symbol_header_ _n(intern_symbol_header_)
#define specialconstant_ _n(specialconstant_)
#define gensymconstant _n(gensymconstant)
#define keywordconstant_ _n(keywordconstant_)
#define commonconstant_ _n(commonconstant_)
#define symbolchar_common_ _n(symbolchar_common_)
#define symbolchar_keyword_ _n(symbolchar_keyword_)
#define quotelist_heap _n(quotelist_heap)
#define pushconst_heap _n(pushconst_heap)

#define RetConstant(i)		RetArrayA4(LispRoot(CONST),(i))
#define GetConstant(i,v)	GetArrayA4(LispRoot(CONST),(i),(v))
#define SetConstant(i,v)	SetArrayA4(LispRoot(CONST),(i),(v))

#define GetConst(i,v)		GetConstant(CONSTANT_##i, (v))
#define SetConst(i,v)		SetConstant(CONSTANT_##i, (v))

void build_constant(void);
int intern_symbol_header_(void);
int specialconstant_(constindex index, const char *package, const char *name);
void gensymconstant(constindex index, const char *name);
int keywordconstant_(constindex index, const char *name);
int commonconstant_(constindex index, const char *name);

int symbolchar_common_(constindex index, const char *name);
int symbolchar_keyword_(constindex index, const char *name);
void quotelist_heap(addr *ret, addr name);
void pushconst_heap(addr *ret, constindex index);
#define PushConst(a,b) pushconst_heap((a),CONSTANT_##b)

#endif

