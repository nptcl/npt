#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "loop.h"
#include "loop_bind.h"
#include "loop_main.h"
#include "loop_parse.h"
#include "loop_symbol.h"
#include "loop_variables.h"
#include "object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  extended
 */
static void loop_macrolet(addr *form)
{
	/* `(macrolet ((loop-finish () (quote (go lisp-system::end-loop))))
	 *    ,form)
	 */
	addr macrolet, loop_finish, quote, go, end;

	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LOOP_FINISH, &loop_finish);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end);

	list_heap(&go, go, end, NULL);
	list_heap(&quote, quote, go, NULL);
	list_heap(&loop_finish, loop_finish, Nil, quote, NULL);
	list_heap(&loop_finish, loop_finish, NULL);
	list_heap(form, macrolet, loop_finish, *form, NULL);
}

static void loop_block_tagbody(addr *form,
		addr named, addr init, addr final, addr expr1, addr expr2)
{
	/* `(let (lisp-system::value-loop
	 *        lisp-system::it-loop)
	 *    (declare (ignorable lisp-system::it-loop))
	 *    (block ,named
	 *      (tagbody
	 *        (progn ,@initially)
	 *        lisp-system::next-loop
	 *        ,@expr1
	 *        ,@form
	 *        ,@expr2
	 *        (go lisp-system::next-loop)
	 *        lisp-system::end-loop
	 *        (progn ,@finally)
	 *        (if lisp-system::function-loop
	 *          (setq lisp-system::value-loop
	 *            (funcall lisp-system::function-loop lisp-system::value-loop)))
	 *        (return-from ,named lisp-system::value-loop))))
	 */
	addr let, block, tagbody, progn, go, retfrom, ifsym, setq, funcall;
	addr value, floop, it, next, end, x, list, declare, ignorable;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_RETURN_FROM, &retfrom);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(SYSTEM_VALUE_LOOP, &value);
	GetConst(SYSTEM_FUNCTION_LOOP, &floop);
	GetConst(SYSTEM_IT_LOOP, &it);
	GetConst(SYSTEM_NEXT_LOOP, &next);
	GetConst(SYSTEM_END_LOOP, &end);
	list_heap(&retfrom, retfrom, named, value, NULL);
	list_heap(&go, go, next, NULL);
	/* initially */
	if (init != Nil)
		cons_heap(&init, progn, init);
	if (final != Nil)
		cons_heap(&final, progn, final);
	/* tagbody */
	conscar_heap(&list, tagbody);
	if (init != Nil)
		cons_heap(&list, init, list);
	cons_heap(&list, next, list);
	while (expr1 != Nil) {
		getcons(expr1, &x, &expr1);
		cons_heap(&list, x, list);
	}
	while (*form != Nil) {
		getcons(*form, &x, form);
		cons_heap(&list, x, list);
	}
	while (expr2 != Nil) {
		getcons(expr2, &x, &expr2);
		cons_heap(&list, x, list);
	}
	cons_heap(&list, go, list);
	cons_heap(&list, end, list);
	if (final != Nil)
		cons_heap(&list, final, list);
	/* lisp-system::function-loop */
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_FUNCALL, &funcall);
	list_heap(&funcall, funcall, floop, value, NULL);
	list_heap(&setq, setq, value, funcall, NULL);
	list_heap(&ifsym, ifsym, floop, setq, NULL);
	cons_heap(&list, ifsym, list);
	/* return */
	cons_heap(&list, retfrom, list);
	nreverse_list_unsafe(&tagbody, list);
	list_heap(&block, block, named, tagbody, NULL);
	list_heap(&value, value, it, NULL);
	list_heap(&ignorable, ignorable, it, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(form, let, value, declare, block, NULL);
}

static void loop_function_loop(addr *form)
{
	addr let, floop;

	/* (let (lisp-system::function-loop) ,form) */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_FUNCTION_LOOP, &floop);
	list_heap(&floop, floop, NULL);
	list_heap(form, let, floop, *form, NULL);
}

static int loop_extended_common(Execute ptr,
		addr *ret, addr named, addr vars, addr body)
{
	addr form, init, final, with, expr1, expr2;
	struct loop_main str;

	/* initially, finally, with */
	init = final = with = expr1 = expr2 = form = Nil;
	loop_filter_initially(&vars, &init);
	loop_filter_initially(&body, &init);
	loop_filter_finally(&vars, &final);
	loop_filter_finally(&body, &final);
	if (loop_filter_with(ptr, &vars, &with))
		return 1;
	loop_push_for_as(ptr, &expr1, &expr2, vars);
	make_loop_main(&str, form, init, named);
	loop_push_main(&str, body);
	form = str.form;
	init = str.init;
	nreverse_list_unsafe(&init, init);
	nreverse_list_unsafe(&final, final);
	nreverse_list_unsafe(&with, with);
	nreverse_list_unsafe(&form, form);
	nreverse_list_unsafe(&expr1, expr1);
	/* loop macro */
	loop_block_tagbody(&form, named, init, final, expr1, expr2);
	loop_function_loop(&form);
	loop_macrolet(&form);
	loop_let_main(&form, body);
	loop_variables_with(ptr, &form, with);
	loop_variables_for_as(&form, vars);
	*ret = form;

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

_g int loop_common(Execute ptr, addr *ret, addr list)
{
	addr named, vars, body;

	/* (loop) */
	if (list == Nil) {
		loop_simple_common(ret, Nil);
		return 0;
	}

	/* clause */
	vars = body = Nil;
	loop_parse_common(ptr, &named, &vars, &body, &list);

	/* simple-loop */
	if (named == Nil && vars == Nil && body == Nil) {
		loop_simple_common(ret, list);
		return 0;
	}

	/* error */
	if (list != Nil) {
		fmte("Invalid loop form ~S.", list, NULL);
		return 0;
	}

	/* extended-loop */
	if (loop_extended_common(ptr, ret, named, vars, body))
		return 1;

	return 0;
}

