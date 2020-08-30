#include <stdarg.h>
#include "clos.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "extern_control.h"
#include "extern_execute.h"
#include "extern_instance.h"
#include "extern_object.h"
#include "extern_sequence.h"
#include "extern_type.h"
#include "hold.h"
#include "symbol.h"

/*
 *  find-class
 */
void lisp0_find_class(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return;
	}
	clos_find_class_nil(symbol, ret);
}

int lisp0_find_class_(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return TypeError_(symbol, SYMBOL);
	}
	return clos_find_class_(symbol, ret);
}

int lisp0_find_class8_(addr *ret, const void *str)
{
	addr pos;
	lisp0_intern8_(&pos, NULL, str);
	return clos_find_class_(pos, ret);
}

int lisp0_find_class16_(addr *ret, const void *str)
{
	addr pos;
	lisp0_intern16_(&pos, NULL, str);
	return clos_find_class_(pos, ret);
}

int lisp0_find_class32_(addr *ret, const void *str)
{
	addr pos;
	lisp0_intern32_(&pos, NULL, str);
	return clos_find_class_(pos, ret);
}

void lisp_find_class(addr x, addr symbol)
{
	lisp0_find_class(&symbol, symbol);
	hold_set(x, symbol);
}

int lisp_find_class_(addr x, addr symbol)
{
	Return(lisp0_find_class_(&symbol, symbol));
	hold_set(x, symbol);
	return 0;
}

int lisp_find_class8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_find_class16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_find_class32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_find_class32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  make-instance
 */
int lisp0_instance_(addr *ret, addr instance, ...)
{
	addr list, call;
	va_list va;

	va_start(va, instance);
	lisp0_list_va(&list, va);
	va_end(va);
	hold_value(instance, &instance);
	cons_heap(&list, instance, list);

	GetConst(COMMON_MAKE_INSTANCE, &call);
	GetFunctionSymbol(call, &call);
	return callclang_apply(Execute_Thread, ret, call, list);
}

static int lisp0_instance_call_(addr *ret, addr instance, va_list va,
		int (*call0)(addr *, const void *))
{
	addr control, x, list, pos;
	const void *str;

	lisp_push_control(&control);
	x = Lisp_hold();
	list = Lisp_hold();

	/* list */
	hold_value(instance, &instance);
	lisp_cons(list, instance, NULL);
	for (;;) {
		/* key */
		str = va_arg(va, void *);
		if (str == NULL)
			break;
		if ((*call0)(&pos, str))
			goto escape;
		lisp_cons(list, pos, list);
		/* value */
		pos = va_arg(va, addr);
		if (pos == NULL) {
			lisp_cons(list, Nil, list);
			break;
		}
		hold_value(pos, &pos);
		lisp_cons(list, pos, list);
	}
	if (lisp_nreverse_(list, list))
		goto escape;

	/* make-instance */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	GetFunctionSymbol(pos, &pos);
	if (lisp_apply_(x, pos, list))
		goto escape;

	/* result */
	hold_value(x, ret);
escape:
	return lisp_pop_control_(control);
}

static int lisp0_instance8_call_(addr *ret, addr instance, va_list va)
{
	return lisp0_instance_call_(ret, instance, va, lisp0_eval8_);
}

static int lisp0_instance16_call_(addr *ret, addr instance, va_list va)
{
	return lisp0_instance_call_(ret, instance, va, lisp0_eval16_);
}

static int lisp0_instance32_call_(addr *ret, addr instance, va_list va)
{
	return lisp0_instance_call_(ret, instance, va, lisp0_eval32_);
}

int lisp0_instance8_(addr *ret, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance8_call_(ret, instance, va);
	va_end(va);

	return check;
}

int lisp0_instance16_(addr *ret, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance16_call_(ret, instance, va);
	va_end(va);

	return check;
}

int lisp0_instance32_(addr *ret, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance32_call_(ret, instance, va);
	va_end(va);

	return check;
}

int lisp_instance_(addr x, addr instance, ...)
{
	addr list, call;
	va_list va;

	va_start(va, instance);
	lisp0_list_va(&list, va);
	va_end(va);
	hold_value(instance, &instance);
	cons_heap(&list, instance, list);

	GetConst(COMMON_MAKE_INSTANCE, &call);
	GetFunctionSymbol(call, &call);
	return lisp_apply_(x, call, list);
}

int lisp_instance8_(addr x, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance8_call_(&instance, instance, va);
	va_end(va);
	if (check == 0)
		hold_set(x, instance);

	return check;
}

int lisp_instance16_(addr x, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance16_call_(&instance, instance, va);
	va_end(va);
	if (check == 0)
		hold_set(x, instance);

	return check;
}

int lisp_instance32_(addr x, addr instance, ...)
{
	int check;
	va_list va;

	va_start(va, instance);
	check = lisp0_instance32_call_(&instance, instance, va);
	va_end(va);
	if (check == 0)
		hold_set(x, instance);

	return check;
}


/*
 *  slot-exists-p
 */
int lisp_slot_exists_(addr instance, addr symbol, int *ret)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	*ret = clos_slot_exists_p(instance, symbol);
	return 0;
}

int lisp_slot_exists8_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval8_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}

int lisp_slot_exists16_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval16_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}

int lisp_slot_exists32_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval32_(&pos, str));
	return lisp_slot_exists_(instance, pos, ret);
}


/*
 *  slot-boundp
 */
int lisp_slot_boundp_(addr instance, addr symbol, int *ret)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_slot_boundp_(instance, symbol, ret);
}

int lisp_slot_boundp8_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval8_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}

int lisp_slot_boundp16_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval16_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}

int lisp_slot_boundp32_(addr instance, const void *str, int *ret)
{
	addr pos;
	Return(lisp0_eval32_(&pos, str));
	return lisp_slot_boundp_(instance, pos, ret);
}


/*
 *  slot-makunbound
 */
int lisp_slot_makunbound_(addr instance, addr symbol)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_slot_makunbound_(instance, symbol);
}

int lisp_slot_makunbound8_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval8_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}

int lisp_slot_makunbound16_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval16_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}

int lisp_slot_makunbound32_(addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval32_(&pos, str));
	return lisp_slot_makunbound_(instance, pos);
}


/*
 *  slot-value
 */
int lisp0_get_slot_(addr *ret, addr instance, addr symbol)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	return clos_check_(instance, symbol, ret);
}

int lisp0_get_slot8_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval8_(&pos, str));
	return lisp0_get_slot_(ret, instance, pos);
}

int lisp0_get_slot16_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval16_(&pos, str));
	return lisp0_get_slot_(ret, instance, pos);
}

int lisp0_get_slot32_(addr *ret, addr instance, const void *str)
{
	addr pos;
	Return(lisp0_eval32_(&pos, str));
	return lisp0_get_slot_(ret, instance, pos);
}

int lisp_get_slot_(addr x, addr instance, addr symbol)
{
	Return(lisp0_get_slot_(&instance, instance, symbol));
	hold_set(x, instance);
	return 0;
}

int lisp_get_slot8_(addr x, addr instance, const void *str)
{
	Return(lisp0_get_slot8_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}

int lisp_get_slot16_(addr x, addr instance, const void *str)
{
	Return(lisp0_get_slot16_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}

int lisp_get_slot32_(addr x, addr instance, const void *str)
{
	Return(lisp0_get_slot32_(&instance, instance, str));
	hold_set(x, instance);
	return 0;
}


/*
 *  setf slot-value
 */
int lisp_set_slot_(addr instance, addr symbol, addr value)
{
	hold_value(instance, &instance);
	hold_value(symbol, &symbol);
	hold_value(value, &value);
	return clos_set_(instance, symbol, value);
}

int lisp_set_slot8_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_eval8_(&pos, str));
	return lisp_set_slot_(instance, pos, value);
}

int lisp_set_slot16_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_eval16_(&pos, str));
	return lisp_set_slot_(instance, pos, value);
}

int lisp_set_slot32_(addr instance, const void *str, addr value)
{
	addr pos;
	Return(lisp0_eval32_(&pos, str));
	return lisp_set_slot_(instance, pos, value);
}

