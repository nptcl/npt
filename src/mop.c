/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "cons.h"
#include "lambda.h"
#include "mop.h"
#include "package.h"
#include "symbol.h"
#include "type_table.h"

_g void export_mop(addr symbol)
{
	addr package;

	Check(! symbolp(symbol), "type error");
	GetConst(PACKAGE_CLOS, &package);
	export_package(package, symbol);
}


/*
 *  type
 */
static void mop_argument_generic_var(addr *ret, unsigned n)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = n;
	*ret = pos;
}

_g void mop_argument_generic_var1(addr *ret)
{
	mop_argument_generic_var(ret, 1);
}
_g void mop_argument_generic_var2(addr *ret)
{
	mop_argument_generic_var(ret, 2);
}
_g void mop_argument_generic_var3(addr *ret)
{
	mop_argument_generic_var(ret, 3);
}
_g void mop_argument_generic_var4(addr *ret)
{
	mop_argument_generic_var(ret, 4);
}
_g void mop_argument_generic_var5(addr *ret)
{
	mop_argument_generic_var(ret, 5);
}

_g void mop_argument_generic_var3opt1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 3;
	str->opt = 1;
	*ret = pos;
}

_g void mop_argument_generic_var1rest(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 1;
	str->rest = 1;
	*ret = pos;
}

_g void mop_argument_generic_var2rest(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 2;
	str->rest = 1;
	*ret = pos;
}

static void mop_argument_generic_varnrest1key0(addr *ret, unsigned var)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = var;
	str->keyp = 1;
	str->rest = 1;
	str->allow = 1;
	*ret = pos;
}

_g void mop_argument_generic_var1rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 1);
}

_g void mop_argument_generic_var2rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 2);
}

_g void mop_argument_generic_var4rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 4);
}

_g void mop_argument_method_var(addr *ret, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	list_heap(ret, Nil, pos, NULL);
}

_g void mop_argument_method_var1(addr *ret, constindex var1)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	mop_argument_method_var(&list, var1);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

_g void mop_argument_method_var1rest(addr *ret, constindex var1)
{
	addr pos;
	mop_argument_method_var1(&pos, var1);
	ArgumentStruct(pos)->rest = 1;
	*ret = pos;
}


/*
 *  function
 */
_g void init_mop_reader(void);
_g void init_mop_class(void);
_g void init_mop_generic(void);
_g void init_mop_protocols(void);

_g void build_mop_reader(Execute ptr);
_g void build_mop_class(Execute ptr);
_g void build_mop_generic(Execute ptr);
_g void build_mop_protocols(Execute ptr);

_g void init_metaobject_protocol(void)
{
	init_mop_class();
	init_mop_reader();
	init_mop_generic();
	init_mop_protocols();
}

_g void build_metaobject_protocol(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	build_mop_class(ptr);
	build_mop_reader(ptr);
	build_mop_generic(ptr);
	build_mop_protocols(ptr);
}

