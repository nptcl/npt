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

void export_mop(addr symbol)
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

void mop_argument_generic_var1(addr *ret)
{
	mop_argument_generic_var(ret, 1);
}

void mop_argument_generic_var2(addr *ret)
{
	mop_argument_generic_var(ret, 2);
}

void mop_argument_generic_var3(addr *ret)
{
	mop_argument_generic_var(ret, 3);
}
void mop_argument_generic_var4(addr *ret)
{
	mop_argument_generic_var(ret, 4);
}
void mop_argument_generic_var5(addr *ret)
{
	mop_argument_generic_var(ret, 5);
}

void mop_argument_generic_var1rest(addr *ret)
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

void mop_argument_generic_var2rest(addr *ret)
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

void mop_argument_generic_var1rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 1);
}

void mop_argument_generic_var2rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 2);
}

void mop_argument_generic_var4rest1key0(addr *ret)
{
	mop_argument_generic_varnrest1key0(ret, 4);
}

void mop_argument_method_var(addr *ret, constindex index)
{
	addr pos;
	GetConstant(index, &pos);
	list_heap(ret, Nil, pos, NULL);
}

void mop_argument_method_var1(addr *ret, constindex var1)
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

void mop_argument_method_var1rest(addr *ret, constindex var1)
{
	addr pos;
	mop_argument_method_var1(&pos, var1);
	ArgumentStruct(pos)->rest = 1;
	*ret = pos;
}


/*
 *  intern
 */
void intern_mop_reader(Execute ptr);
void intern_mop_class(Execute ptr);
void intern_mop_generic(Execute ptr);

void intern_metaobject_protocol(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	intern_mop_class(ptr);
	intern_mop_reader(ptr);
	intern_mop_generic(ptr);
}

