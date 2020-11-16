#include "condition.h"
#include "cons.h"
#include "control_callbind.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute.h"
#include "function.h"

/*
 *  call_compiled_function
 */
typedef const struct callbind_struct *CallStruct;
typedef int (*callbind_control)(Execute, addr, CallStruct);
static callbind_control CallBindTable[CallBind_size];

static int call_callbind_code(Execute ptr, addr pos, CallStruct call)
{
	Abort("Cannot call callbind_code in control.");
	return 0;
}

static int call_callbind_macro(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.macro)(ptr, var1, var2);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_none(Execute ptr, addr pos, CallStruct call)
{
	return (call->call.none)();
}

static int call_callbind_any(Execute ptr, addr pos, CallStruct call)
{
	return (call->call.any)(ptr);
}

static int call_callbind_empty(Execute ptr, addr pos, CallStruct call)
{
	addr check;

	GetControl(ptr->control, Control_Cons, &check);
	if (check != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.any)(ptr);
}

static int call_callbind_dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	return (call->call.dynamic)(ptr, cons);
}

static int call_callbind_rest(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	return (call->call.rest)(ptr, cons);
}

static int call_callbind_var1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.var1)(ptr, var1);
}

static int call_callbind_var2(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.var2)(ptr, var1, var2);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var3(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var3)(ptr, var1, var2, var3);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var4(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var4)(ptr, var1, var2, var3, var4);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var5(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var5)(ptr, var1, var2, var3, var4, var5);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_var6(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5, var6;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var5, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var6, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.var6)(ptr, var1, var2, var3, var4, var5, var6);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		opt1 = Unbound;
	else
		Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.opt1)(ptr, opt1);
}

static int call_callbind_opt2(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt2)(ptr, opt1, opt2);
}

static int call_callbind_opt3(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt3)(ptr, opt1, opt2, opt3);
}

static int call_callbind_opt4(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3, opt4;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt4, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt4)(ptr, opt1, opt2, opt3, opt4);
}

static int call_callbind_opt5(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, opt1, opt2, opt3, opt4, opt5;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons == Nil) {
		opt4 = opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt4, &cons);
	if (cons == Nil) {
		opt5 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt5, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
finish:
	return (call->call.opt5)(ptr, opt1, opt2, opt3, opt4, opt5);
}

static int call_callbind_var1opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var1opt1)(ptr, var1, opt1);
}

static int call_callbind_var2opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt1)(ptr, var1, var2, opt1);
}

static int call_callbind_var3opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var3opt1)(ptr, var1, var2, var3, opt1);
}

static int call_callbind_var4opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var4opt1)(ptr, var1, var2, var3, var4, opt1);
}

static int call_callbind_var5opt1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, var5, opt1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var4, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var5, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var5opt1)(ptr, var1, var2, var3, var4, var5, opt1);
}

static int call_callbind_var1opt2(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var1opt2)(ptr, var1, opt1, opt2);
}

static int call_callbind_var2opt2(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt2)(ptr, var1, var2, opt1, opt2);
}

static int call_callbind_var2opt3(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, opt1, opt2, opt3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		opt1 = opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt1, &cons);
	if (cons == Nil) {
		opt2 = opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt2, &cons);
	if (cons == Nil) {
		opt3 = Unbound;
		goto finish;
	}
	Inline_getcons(cons, &opt3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
finish:
	return (call->call.var2opt3)(ptr, var1, var2, opt1, opt2, opt3);
}

static int call_callbind_var1rest(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_heap(ptr, 1, &rest);
	return (call->call.var1rest)(ptr, var1, rest);
}

static int call_callbind_var2rest(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_heap(ptr, 2, &rest);
	return (call->call.var2rest)(ptr, var1, var2, rest);
}

static int call_callbind_opt1rest(Execute ptr, addr pos, CallStruct call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		Inline_getcar(cons, &opt1);
		getargs_list_control_heap(ptr, 1, &rest);
	}
	return (call->call.opt1rest)(ptr, opt1, rest);
}

static int call_callbind_var1dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var1);
	getargs_list_control_unsafe(ptr, 1, &rest);
	return (call->call.var1dynamic)(ptr, var1, rest);
}

static int call_callbind_var2dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var2);
	getargs_list_control_unsafe(ptr, 2, &rest);
	return (call->call.var2dynamic)(ptr, var1, var2, rest);
}

static int call_callbind_var3dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var3);
	getargs_list_control_unsafe(ptr, 3, &rest);
	return (call->call.var3dynamic)(ptr, var1, var2, var3, rest);
}

static int call_callbind_var4dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3, var4, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var3, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcar(cons, &var4);
	getargs_list_control_unsafe(ptr, 4, &rest);
	return (call->call.var4dynamic)(ptr, var1, var2, var3, var4, rest);
}

static int call_callbind_opt1dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr cons, opt1, rest;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		opt1 = Unbound;
		rest = Nil;
	}
	else {
		Inline_getcar(cons, &opt1);
		getargs_list_control_unsafe(ptr, 1, &rest);
	}
	return (call->call.opt1dynamic)(ptr, opt1, rest);
}

static int call_callbind_extend_dynamic(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	GetControl(ptr->control, Control_Cons, &cons);
	return (call->call.extend_dynamic)(cons);
}

static int call_callbind_extend_rest(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	return (call->call.extend_rest)(cons);
}

static int call_callbind_extend_empty(Execute ptr, addr pos, CallStruct call)
{
	addr cons;
	getargs_list_control_heap(ptr, 0, &cons);
	return (call->call.extend_empty)();
}

static int call_callbind_extend_var1(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	Inline_getcons(cons, &var1, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_var1)(var1);
}

static int call_callbind_extend_var2(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too many call argument ~S.", check, NULL);
	}
	return (call->call.extend_var2)(var1, var2);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

static int call_callbind_extend_var3(Execute ptr, addr pos, CallStruct call)
{
	addr check, cons, var1, var2, var3;

	GetControl(ptr->control, Control_Cons, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var1, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var2, &cons);
	if (cons == Nil)
		goto toofew;
	Inline_getcons(cons, &var3, &cons);
	if (cons != Nil) {
		GetNameFunction(pos, &check);
		return fmte_("Too few call argument ~S.", check, NULL);
	}
	return (call->call.extend_var3)(var1, var2, var3);

toofew:
	GetNameFunction(pos, &check);
	return fmte_("Too few call argument ~S.", check, NULL);
}

int call_compiled_function(Execute ptr, addr compiled)
{
	struct callbind_struct *str;
	callbind_control call;
	pointer p;

	p = StructFunction(compiled)->index;
	str = &(pointer_table[p]);
	Check(CallBind_size <= str->type, "index error");
	call = CallBindTable[str->type];
	Check(call == NULL, "call error. (build_control?)");
	return (*call)(ptr, compiled, str);
}

void init_callbind_control(void)
{
	CallBindTable[CallBind_code] = call_callbind_code;
	CallBindTable[CallBind_macro] = call_callbind_macro;
	CallBindTable[CallBind_none] = call_callbind_none;
	CallBindTable[CallBind_any] = call_callbind_any;
	CallBindTable[CallBind_empty] = call_callbind_empty;
	CallBindTable[CallBind_dynamic] = call_callbind_dynamic;
	CallBindTable[CallBind_rest] = call_callbind_rest;
	CallBindTable[CallBind_var1] = call_callbind_var1;
	CallBindTable[CallBind_var2] = call_callbind_var2;
	CallBindTable[CallBind_var3] = call_callbind_var3;
	CallBindTable[CallBind_var4] = call_callbind_var4;
	CallBindTable[CallBind_var5] = call_callbind_var5;
	CallBindTable[CallBind_var6] = call_callbind_var6;
	CallBindTable[CallBind_opt1] = call_callbind_opt1;
	CallBindTable[CallBind_opt2] = call_callbind_opt2;
	CallBindTable[CallBind_opt3] = call_callbind_opt3;
	CallBindTable[CallBind_opt4] = call_callbind_opt4;
	CallBindTable[CallBind_opt5] = call_callbind_opt5;
	CallBindTable[CallBind_var1opt1] = call_callbind_var1opt1;
	CallBindTable[CallBind_var2opt1] = call_callbind_var2opt1;
	CallBindTable[CallBind_var3opt1] = call_callbind_var3opt1;
	CallBindTable[CallBind_var4opt1] = call_callbind_var4opt1;
	CallBindTable[CallBind_var5opt1] = call_callbind_var5opt1;
	CallBindTable[CallBind_var1opt2] = call_callbind_var1opt2;
	CallBindTable[CallBind_var2opt2] = call_callbind_var2opt2;
	CallBindTable[CallBind_var2opt3] = call_callbind_var2opt3;
	CallBindTable[CallBind_var1rest] = call_callbind_var1rest;
	CallBindTable[CallBind_var2rest] = call_callbind_var2rest;
	CallBindTable[CallBind_opt1rest] = call_callbind_opt1rest;
	CallBindTable[CallBind_var1dynamic] = call_callbind_var1dynamic;
	CallBindTable[CallBind_var2dynamic] = call_callbind_var2dynamic;
	CallBindTable[CallBind_var3dynamic] = call_callbind_var3dynamic;
	CallBindTable[CallBind_var4dynamic] = call_callbind_var4dynamic;
	CallBindTable[CallBind_opt1dynamic] = call_callbind_opt1dynamic;
	CallBindTable[CallBind_extend_dynamic] = call_callbind_extend_dynamic;
	CallBindTable[CallBind_extend_rest] = call_callbind_extend_rest;
	CallBindTable[CallBind_extend_empty] = call_callbind_extend_empty;
	CallBindTable[CallBind_extend_var1] = call_callbind_extend_var1;
	CallBindTable[CallBind_extend_var2] = call_callbind_extend_var2;
	CallBindTable[CallBind_extend_var3] = call_callbind_extend_var3;
}

