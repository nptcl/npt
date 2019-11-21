#include <stdarg.h>
#include "build.h"
#include "condition.h"
#include "cons.h"
#include "memory.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"
#include "type_constant.h"
#include "type_table.h"

/*
 *  nil, t
 */
void lisp_get_nil(addr *ret)
{
	*ret = Nil;
}

void lisp_get_t(addr *ret)
{
	*ret = T;
}

addr lisp_ref_nil(void)
{
	return Nil;
}

addr lisp_ref_t(void)
{
	return T;
}


/*
 *  type
 */
int lisp_nil_p(addr x)
{
	return GetType(x) == LISPTYPE_NIL;
}

int lisp_t_p(addr x)
{
	return GetType(x) == LISPTYPE_T;
}

int lisp_character_p(addr x)
{
	return characterp(x);
}

int lisp_cons_p(addr x)
{
	return consp(x);
}

int lisp_list_p(addr x)
{
	return listp(x);
}

int lisp_string_p(addr x)
{
	return stringp(x);
}

int lisp_symbol_p(addr x)
{
	return symbolp(x);
}

int lisp0_symbol_p(addr x)
{
	return GetType(x) == LISPTYPE_SYMBOL;
}

int lisp0_array_p(addr x)
{
	return GetType(x) == LISPTYPE_ARRAY;
}

int lisp0_vector_p(addr x)
{
	return GetType(x) == LISPTYPE_VECTOR;
}

int lisp0_strvect_p(addr x)
{
	return GetType(x) == LISPTYPE_STRING;
}


/*
 *  cons
 */
void lisp_cons(addr *ret, addr car, addr cdr)
{
	cons_heap(ret, car, cdr);
}

void lisp_list(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_alloc_stdarg(NULL, ret, args);
	va_end(args);
}

void lisp_lista(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_safe(NULL, ret, args);
	va_end(args);
}

void lisp_vector(addr *ret, size_t size)
{
	vector_heap(ret, size);
}

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


/*
 *  vector
 */
static void lisp_typecheck_vector(addr pos)
{
	if (GetType(pos) != LISPTYPE_VECTOR)
		TypeError(pos, VECTOR);
}

void lisp0_getvector(addr vector, size_t index, addr *ret)
{
	getarray(vector, index, ret);
}

void lisp0_setvector(addr vector, size_t index, addr value)
{
	setarray(vector, index, value);
}

void lisp_getvector(addr vector, size_t index, addr *ret)
{
	lisp_typecheck_vector(vector);
	getarray(vector, index, ret);
}

void lisp_setvector(addr vector, size_t index, addr value)
{
	lisp_typecheck_vector(vector);
	setarray(vector, index, value);
}

