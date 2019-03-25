#include "build.h"
#include "condition.h"
#include "extern.h"
#include "memory.h"
#include "object.h"
#include "type_constant.h"
#include "type_table.h"

static void lisp_typecheck_list(addr pos)
{
	if (! listp(pos))
		TypeError(pos, LIST);
}

static void lisp_typecheck_cons(addr pos)
{
	if (! consp(pos))
		TypeError(pos, CONS);
}

void lisp0_car(addr list, addr *ret)
{
	GetCar(list, ret);
}

void lisp0_cdr(addr list, addr *ret)
{
	GetCdr(list, ret);
}

void lisp0_carcdr(addr list, addr *car, addr *cdr)
{
	GetCons(list, car, cdr);
}

void lisp_car(addr list, addr *ret)
{
	lisp_typecheck_list(list);
	GetCar(list, ret);
}

void lisp_cdr(addr list, addr *ret)
{
	lisp_typecheck_list(list);
	GetCdr(list, ret);
}

void lisp_carcdr(addr list, addr *car, addr *cdr)
{
	lisp_typecheck_list(list);
	GetCons(list, car, cdr);
}

void lisp0_setf_car(addr cons, addr value)
{
	SetCar(cons, value);
}

void lisp0_setf_cdr(addr cons, addr value)
{
	SetCdr(cons, value);
}

void lisp0_setf_carcdr(addr cons, addr car, addr cdr)
{
	SetCons(cons, car, cdr);
}

void lisp_setf_car(addr cons, addr value)
{
	lisp_typecheck_cons(cons);
	SetCar(cons, value);
}

void lisp_setf_cdr(addr cons, addr value)
{
	lisp_typecheck_cons(cons);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	lisp_typecheck_cons(cons);
	SetCons(cons, car, cdr);
}

