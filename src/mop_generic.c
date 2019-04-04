/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "function.h"
#include "mop.h"
#include "symbol.h"

/***********************************************************************
 *  no-applicable-method
 ***********************************************************************/
static void defgeneric_no_applicable_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_APPLICABLE_METHOD, &symbol);
	mop_argument_generic_var1rest(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  no-next-method
 ***********************************************************************/
static void defgeneric_no_next_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_NEXT_METHOD, &symbol);
	mop_argument_generic_var2rest(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
void intern_mop_generic(Execute ptr)
{
	defgeneric_no_applicable_method_mop(ptr);
	defgeneric_no_next_method_mop(ptr);
}

