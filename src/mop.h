#ifndef __MOP_HEADER__
#define __MOP_HEADER__

#include "constant.h"
#include "typedef.h"

void export_mop(addr symbol);

#define ClosKeyTypeTable(r,a,b) \
	keytypetable(CONSTANT_CLOSKEY_##a, TypeTable_##b, (r))

#define ArgumentMethod_var(a,b) \
	mop_argument_method_var((a), CONSTANT_CLOS_##b)
#define ArgumentMethod_var1(a,b) \
	mop_argument_method_var1((a), CONSTANT_CLOS_##b)
#define ArgumentMethod_var1rest(a,b) \
	mop_argument_method_var1rest((a), CONSTANT_CLOS_##b)

void mop_argument_generic_var1(addr *ret);
void mop_argument_generic_var2(addr *ret);
void mop_argument_generic_var3(addr *ret);
void mop_argument_generic_var4(addr *ret);
void mop_argument_generic_var5(addr *ret);
void mop_argument_generic_var3opt1(addr *ret);
void mop_argument_generic_var1rest(addr *ret);
void mop_argument_generic_var2rest(addr *ret);
void mop_argument_generic_var1rest1key0(addr *ret);
void mop_argument_generic_var2rest1key0(addr *ret);
void mop_argument_generic_var4rest1key0(addr *ret);

void mop_argument_method_var(addr *ret, constindex index);
void mop_argument_method_var1(addr *ret, constindex var1);
void mop_argument_method_var1rest(addr *ret, constindex var1);

#endif

