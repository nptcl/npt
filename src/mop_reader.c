#include "clos.h"
#include "clos_class.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "execute.h"
#include "function.h"
#include "lambda.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  argument
 */
static void generic_var1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 1;
	*ret = pos;
}

static void method_var1(addr *ret, constindex var1)
{
	addr pos, list, type;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	GetConstant(var1, &type);
	list_heap(&list, Nil, type, NULL);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}
#define Method_var1(a,b) method_var1((a), CONSTANT_CLOS_##b)

/* (defmethod class-name ((class standard-class)) -> symbol */
static void method_class_name(Execute ptr, addr method, addr next, addr args)
{
	addr var;

	list_bind(args, &var, NULL);
	stdget_class_name(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_name_type(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, method, call;
	LocalRoot local;

	local = ptr->local;
	/* function */
	compiled_heap(&call, name);
	setcompiled_method(call, method_class_name);
	/* method */
	method_var1(&pos, index);
	method_instance_lambda(local, &method, Nil, pos);
	stdset_method_function(method, call);
	common_method_add(ptr, gen, method);
}

/* (defgeneric class-name (class)) -> symbol */
static void defgeneric_class_name(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_NAME, &symbol);
	generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_name_type(ptr, name, gen, CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_name_type(ptr, name, gen, CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
	/* lock */
	SetStatusReadOnly(gen);
}

/* (defgeneric (setf class-name) (symbol class)) -> symbol */
static void defgeneric_setf_class_name(Execute ptr)
{
	/* TODO */
}


/*
 *  intern
 */
void intern_mop_reader(void)
{
	Execute ptr = Execute_Thread;

	/* Classes */
	defgeneric_class_name(ptr);
	defgeneric_setf_class_name(ptr);
}

