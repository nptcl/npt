/*
 *  ANSI COMMON LISP: 8. Structures
 */
#include "common_header.h"
#include "structure.h"
#include "structure_common.h"

/*
 *  (defmacro defstruct (name [doc] slots*) ...) -> symbol
 */
static int function_defstruct(Execute ptr, addr form, addr env)
{
	Return(defstruct_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defstruct(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSTRUCT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defstruct);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  (defun copy-structure (structure) ...) -> structure
 */
static int function_copy_structure(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_structure(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StructureObject);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_structure(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_STRUCTURE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_structure);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_structure(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_structures(void)
{
	SetPointerCall(defmacro, macro, defstruct);
	SetPointerCall(defun, var1, copy_structure);
}

_g void build_common_structures(void)
{
	defmacro_defstruct();
	defun_copy_structure();
}

