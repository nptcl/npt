#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "execute.h"
#include "loop_special.h"
#include "symbol.h"
#include "typedef.h"

void loop_push_special(Execute ptr)
{
	addr pos;

	/* *loop-named* */
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-vars* */
	GetConst(SYSTEM_LOOP_VARS, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-init* */
	GetConst(SYSTEM_LOOP_INIT, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-final* */
	GetConst(SYSTEM_LOOP_FINAL, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-form* */
	GetConst(SYSTEM_LOOP_FORM, &pos);
	pushspecial_control(ptr, pos, Nil);
	/* *loop-let* */
	GetConst(SYSTEM_LOOP_LET, &pos);
	pushspecial_control(ptr, pos, Nil);
}


/*
 *  named
 */
void setnamed_loop(Execute ptr, addr value)
{
	addr pos;
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	setspecial_local(ptr, pos, value);
}

int getnamed_loop_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SYSTEM_LOOP_NAMED, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}


/*
 *  function
 */
static int getexpand_loop_(Execute ptr, constindex index, addr *ret)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	nreverse(ret, pos);
	setspecial_local(ptr, symbol, Unbound);

	return 0;
}

static int getpush_loop_(Execute ptr, constindex index, addr value)
{
	addr symbol, list;

	GetConstant(index, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, value, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  vars
 */
int getvars_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_VARS, ret);
}

int push_vars_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_VARS, value);
}


/*
 *  form
 */
int getform_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_FORM, ret);
}

int push_form_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_FORM, value);
}


/*
 *  initially
 */
int getinit_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_INIT, ret);
}

int push_init_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_INIT, value);
}


/*
 *  finallly
 */
int getfinal_expand_loop_(Execute ptr, addr *ret)
{
	return getexpand_loop_(ptr, CONSTANT_SYSTEM_LOOP_FINAL, ret);
}

int push_final_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_FINAL, value);
}


/*
 *  let
 */
int push_let_loop_(Execute ptr, addr value)
{
	return getpush_loop_(ptr, CONSTANT_SYSTEM_LOOP_LET, value);
}

int getlet_loop_(Execute ptr, addr *ret)
{
	addr pos;
	GetConst(SYSTEM_LOOP_LET, &pos);
	return getspecialcheck_local_(ptr, pos, ret);
}

