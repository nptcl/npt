#ifndef __MOP_HEADER__
#define __MOP_HEADER__

#include "constant.h"
#include "typedef.h"

#define mop_export_symbol_ _n(mop_export_symbol_)
#define mop_argument_generic_var1 _n(mop_argument_generic_var1)
#define mop_argument_generic_var2 _n(mop_argument_generic_var2)
#define mop_argument_generic_var3 _n(mop_argument_generic_var3)
#define mop_argument_generic_var4 _n(mop_argument_generic_var4)
#define mop_argument_generic_var5 _n(mop_argument_generic_var5)
#define mop_argument_generic_var1opt1 _n(mop_argument_generic_var1opt1)
#define mop_argument_generic_var3opt1 _n(mop_argument_generic_var3opt1)
#define mop_argument_generic_var1rest _n(mop_argument_generic_var1rest)
#define mop_argument_generic_var2rest _n(mop_argument_generic_var2rest)
#define mop_argument_generic_var1rest1key0 _n(mop_argument_generic_var1rest1key0)
#define mop_argument_generic_var2rest1key0 _n(mop_argument_generic_var2rest1key0)
#define mop_argument_generic_var4rest1key0 _n(mop_argument_generic_var4rest1key0)
#define mop_argument_method_var _n(mop_argument_method_var)
#define mop_argument_method_var1 _n(mop_argument_method_var1)
#define mop_argument_method_var1opt1 _n(mop_argument_method_var1opt1)
#define mop_argument_method_var1rest _n(mop_argument_method_var1rest)
#define mop_argument_method_var2 _n(mop_argument_method_var2)
#define mop_argument_method_var2rest _n(mop_argument_method_var2rest)
#define mop_argument_method_print_object _n(mop_argument_method_print_object)

int mop_export_symbol_(addr symbol);

#define ClosKeyTypeTable(r,a,b) \
	keytypetable(CONSTANT_CLOSKEY_##a, TypeTable_##b, (r))

#define ArgumentMethod_var(a,b) \
	mop_argument_method_var((a), CONSTANT_CLOS_##b)
#define ArgumentMethod_var1(a,b) \
	mop_argument_method_var1((a), CONSTANT_CLOS_##b)
#define ArgumentMethod_var1opt1(a,b,c) \
	mop_argument_method_var1opt1((a), CONSTANT_CLOS_##b, CONSTANT_CLOS_##c)
#define ArgumentMethod_var1rest(a,b) \
	mop_argument_method_var1rest((a), CONSTANT_CLOS_##b)

void mop_argument_generic_var1(addr *ret);
void mop_argument_generic_var2(addr *ret);
void mop_argument_generic_var3(addr *ret);
void mop_argument_generic_var4(addr *ret);
void mop_argument_generic_var5(addr *ret);
void mop_argument_generic_var1opt1(addr *ret);
void mop_argument_generic_var3opt1(addr *ret);
void mop_argument_generic_var1rest(addr *ret);
void mop_argument_generic_var2rest(addr *ret);
void mop_argument_generic_var1rest1key0(addr *ret);
void mop_argument_generic_var2rest1key0(addr *ret);
void mop_argument_generic_var4rest1key0(addr *ret);

void mop_argument_method_var(addr *ret, constindex index);
void mop_argument_method_var1(addr *ret, constindex var1);
void mop_argument_method_var1opt1(addr *ret, constindex var1, constindex opt1);
void mop_argument_method_var1rest(addr *ret, constindex var1);
void mop_argument_method_var2(addr *ret, constindex var1, constindex var2);
void mop_argument_method_var2rest(addr *ret, constindex var1, constindex var2);
void mop_argument_method_print_object(addr *ret, addr pos);

#endif

