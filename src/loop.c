#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "hold.h"
#include "loop.h"
#include "loop_bind.h"
#include "loop_main.h"
#include "loop_parse.h"
#include "loop_special.h"
#include "loop_symbol.h"
#include "loop_variables.h"
#include "object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  extended
 */
static void loop_expand_macrolet(addr *ret, addr form)
{
	/* `(macrolet ((loop-finish () (quote (go end-loop))))
	 *    ,form)
	 */
	addr macrolet, loop_finish, quote, go, end_loop;

	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LOOP_FINISH, &loop_finish);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	list_heap(&go, go, end_loop, NULL);
	list_heap(&quote, quote, go, NULL);
	list_heap(&loop_finish, loop_finish, Nil, quote, NULL);
	list_heap(&loop_finish, loop_finish, NULL);
	list_heap(ret, macrolet, loop_finish, form, NULL);
}

static int loop_expand_symbol_(Execute ptr, addr *ret, addr form)
{
	/* `(let (value-loop it-loop function-loop ,@vars)
	 *    (declare (ignorable it-loop ,@vars))
	 *    ,form)
	 */
	addr value_loop, it_loop, function_loop;
	addr let, declare, ignorable, vars, x;

	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	Return(getvars_expand_loop_(ptr, &vars));

	lista_heap(&x, value_loop, it_loop, function_loop, vars, NULL);
	lista_heap(&ignorable, ignorable, it_loop, vars, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, x, declare, form, NULL);

	return 0;
}

static int loop_expand_let_(Execute ptr, addr *ret, addr form)
{
	addr list, pos, root, x;

	Return(getlet_loop_(ptr, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		root = Nil;
		while (pos != Nil) {
			GetCons(pos, &x, &pos);
			cons_heap(&root, x, root);
		}
		cons_heap(&root, form, root);
		nreverse(&form, root);
	}

	return Result(ret, form);
}

static void loop_expand_return_from(addr *ret, addr named)
{
	/*  (return-from ,named
	 *    (if function-loop
	 *      (funcall function-loop value-loop)
	 *      value-loop))
	 */
	addr return_from, if_symbol, funcall, function_loop, value_loop;

	GetConst(COMMON_RETURN_FROM, &return_from);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_FUNCALL, &funcall);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	list_heap(&funcall, funcall, function_loop, value_loop, NULL);
	list_heap(&if_symbol, if_symbol, function_loop, funcall, value_loop, NULL);
	list_heap(ret, return_from, named, if_symbol, NULL);
}

static int loop_expand_form_(Execute ptr, addr *ret)
{
	/* `(block ,named
	 *    (tagbody
	 *      (progn ,@initially)
	 *      next-loop
	 *      ,@form
	 *      (go next-loop)
	 *      end-loop
	 *      (progn ,@finally)
	 *      ,return-from))
	 */
	addr let, block, tagbody, progn, go;
	addr next_loop, end_loop, x, list;
	addr named, init, final, form;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_NEXT_LOOP, &next_loop);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	Return(getnamed_loop_(ptr, &named));
	Return(getinit_expand_loop_(ptr, &init));
	Return(getfinal_expand_loop_(ptr, &final));
	Return(getform_expand_loop_(ptr, &form));

	/* tagbody */
	conscar_heap(&list, tagbody);
	/* initially */
	if (init != Nil) {
		cons_heap(&init, progn, init);
		cons_heap(&list, init, list);
	}
	/* next-loop */
	cons_heap(&list, next_loop, list);
	/* form */
	while (form != Nil) {
		GetCons(form, &x, &form);
		cons_heap(&list, x, list);
	}
	/* go */
	list_heap(&go, go, next_loop, NULL);
	cons_heap(&list, go, list);
	/* end-loop */
	cons_heap(&list, end_loop, list);
	/* finally */
	if (final != Nil) {
		cons_heap(&final, progn, final);
		cons_heap(&list, final, list);
	}
	/* return-from */
	loop_expand_return_from(&x, named);
	cons_heap(&list, x, list);

	/* form */
	nreverse(&tagbody, list);
	list_heap(&form, block, named, tagbody, NULL);
	Return(loop_expand_let_(ptr, &form, form));
	Return(loop_expand_symbol_(ptr, &form, form));
	loop_expand_macrolet(ret, form);

	return 0;
}


/*
 *  compose
 */
static int loop_extended_common_(Execute ptr, addr *ret, addr vars, addr main)
{
	Return(loop_variables_(ptr, vars));
	Return(loop_main_(ptr, main));
	Return(loop_expand_form_(ptr, ret));

	return 0;
}


/*
 *  main
 */
static void loop_simple_common(addr *ret, addr form)
{
	/* `(do () (nil) ,@form) */
	addr x, y;

	GetConst(COMMON_DO, &x);
	list_heap(&y, Nil, NULL);
	lista_heap(ret, x, Nil, y, form, NULL);
}

static int loop_execute_common_(Execute ptr, addr *ret, addr list)
{
	addr named, vars, main;
	LocalHold hold;

	/* (loop) */
	Return_getcdr(list, &list);
	if (list == Nil) {
		loop_simple_common(ret, Nil);
		return 0;
	}

	/* clause */
	vars = main = Nil;
	Return(loop_parse_common_(ptr, &named, &vars, &main, &list));

	/* simple-loop */
	if (named == Nil && vars == Nil && main == Nil) {
		loop_simple_common(ret, list);
		return 0;
	}

	/* error */
	if (list != Nil)
		return fmte_("Invalid loop form ~S.", list, NULL);

	/* extended-loop */
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, vars, main, NULL);
	Return(loop_extended_common_(ptr, ret, vars, main));
	localhold_end(hold);

	return 0;
}

int loop_common_(Execute ptr, addr *ret, addr list)
{
	addr control;

	push_control(ptr, &control);
	loop_push_special(ptr);
	(void)loop_execute_common_(ptr, ret, list);
	return pop_control_(ptr, control);
}

