/*
 *  ANSI COMMON LISP: 6. Iteration
 */
#include "call_iteration.h"
#include "common_header.h"
#include "cons.h"
#include "loop.h"

/* (defmacro do/do* (var end [declaration] [tag-statement]) ...) -> result */
static int function_do(Execute ptr, addr form, addr env)
{
	Return(do_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}

static int function_doa(Execute ptr, addr form, addr env)
{
	Return(doa_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_doa(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOA, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_doa);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dotimes (var count &optional result) [declaration] [body]) -> result */
static int function_dotimes(Execute ptr, addr form, addr env)
{
	Return(dotimes_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_dotimes(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOTIMES, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dotimes);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dolist (var value &optional result) [declaration] [body]) -> result */
static int function_dolist(Execute ptr, addr form, addr env)
{
	Return(dolist_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_dolist(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOLIST, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dolist);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro loop (&rest args) ...) */
static int function_loop(Execute ptr, addr form, addr env)
{
	Return(loop_common_(ptr, &form, form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_loop(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_LOOP, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_loop);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
void init_common_iteration(void)
{
	SetPointerCall(defmacro,  macro,  do);
	SetPointerCall(defmacro,  macro,  doa);
	SetPointerCall(defmacro,  macro,  dotimes);
	SetPointerCall(defmacro,  macro,  dolist);
	SetPointerCall(defmacro,  macro,  loop);
}

void build_common_iteration(void)
{
	defmacro_do();
	defmacro_doa();
	defmacro_dotimes();
	defmacro_dolist();
	defmacro_loop();
}

