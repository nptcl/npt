/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "clos.h"
#include "clos_common.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "execute.h"
#include "function.h"
#include "symbol.h"
#include "type_table.h"


/* (defun system::ensure-class (name &key
 *     :direct-superclasses supers
 *     :direct-slots slots
 *     &allow-other-keys) ...)
 *   -> class
 */
static void function_ensure_class(Execute ptr, addr pos, addr args)
{
	ensure_class_common(ptr, pos, args, &pos);
	setresult_control(ptr, pos);
}

static void type_ensure_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_CLASS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_ensure_class);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defgeneric ensure-class-using-class
 *     (class name &rest initargs &key) ...)
 *   -> class
 */
static void defgeneric_ensure_class_using_class(void)
{
	addr symbol;

	GetConst(CLOSNAME_ENSURE_CLASS_USING_CLASS, &symbol);
}


/*
 *  intern
 */
void intern_mop_class(void)
{
	defun_ensure_class();
	defgeneric_ensure_class_using_class();
}

