#include "extern_develop.h"
#include "extern_error.h"
#include "hold.h"
#include "object.h"
#include "sequence.h"
#include "strtype.h"
#include "strvect.h"
#include "typedef.h"
#include "unicode.h"

/*
 *  sequence
 */
void lisp0_cons(addr *ret, addr car, addr cdr)
{
	/* car */
	if (car == NULL)
		car = Nil;
	else
		hold_value(car, &car);
	/* cdr */
	if (cdr == NULL)
		cdr = Nil;
	else
		hold_value(cdr, &cdr);
	/* cons */
	cons_heap(ret, car, cdr);
}

void lisp_cons(addr x, addr car, addr cdr)
{
	lisp0_cons(&car, car, cdr);
	hold_set(x, car);
}

void lisp0_vector(addr *ret, size_t size)
{
	vector_heap(ret, size);
}

void lisp_vector(addr x, size_t size)
{
	addr pos;
	vector_heap(&pos, size);
	hold_set(x, pos);
}

void lisp0_list_va(addr *ret, va_list args)
{
	lisp0_list_va_alloc(NULL, ret, args);
}

void lisp0_lista_va(addr *ret, va_list args)
{
	lisp0_lista_va_alloc(NULL, ret, args);
}

void lisp0_list(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_list_va(ret, args);
	va_end(args);
}

void lisp_list(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_list_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

void lisp0_lista(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_lista_va(ret, args);
	va_end(args);
}

void lisp_lista(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_lista_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

int lisp0_getelt_(addr *ret, addr pos, size_t index)
{
	hold_value(pos, &pos);
	return getelt_sequence_(NULL, pos, index, ret);
}

int lisp_getelt_(addr x, addr pos, size_t index)
{
	Return(lisp0_getelt_(&pos, pos, index));
	hold_set(x, pos);
	return 0;
}

int lisp_setelt_(addr pos, size_t index, addr value)
{
	hold_value(pos, &pos);
	hold_value(value, &value);
	return setelt_sequence_(pos, index, value);
}

int lisp_length_(addr pos, size_t *ret)
{
	hold_value(pos, &pos);
	return length_sequence_(pos, 1, ret);
}

int lisp0_reverse_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	return reverse_sequence_heap_(ret, pos);
}

int lisp0_nreverse_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	return nreverse_sequence_(ret, pos);
}

int lisp_reverse_(addr x, addr pos)
{
	Return(lisp0_reverse_(&pos, pos));
	hold_set(x, pos);
	return 0;
}

int lisp_nreverse_(addr x, addr pos)
{
	Return(lisp0_nreverse_(&pos, pos));
	hold_set(x, pos);
	return 0;
}


/*
 *  cons
 */
void lisp0_car(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCar(list, ret);
}

void lisp0_cdr(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCdr(list, ret);
}

void lisp0_carcdr(addr *car, addr *cdr, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCons(list, car, cdr);
}

void lisp_car(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCar(list, &list);
	hold_set(x, list);
}

void lisp_cdr(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCdr(list, &list);
	hold_set(x, list);
}

void lisp_carcdr(addr x, addr y, addr list)
{
	addr car, cdr;

	hold_value(list, &list);
	if (! listp(list)) {
		Lisp_abort_type(list, LIST);
		return;
	}
	GetCons(list, &car, &cdr);
	hold_set(x, car);
	hold_set(y, cdr);
}

void lisp_setf_car(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(value, &value);
	SetCar(cons, value);
}

void lisp_setf_cdr(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(value, &value);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	hold_value(cons, &cons);
	if (! consp(cons)) {
		Lisp_abort_type(cons, CONS);
		return;
	}
	hold_value(car, &car);
	hold_value(cdr, &cdr);
	SetCons(cons, car, cdr);
}


/*
 *  string
 */
int lisp0_string8_(addr *ret, const void *str)
{
	return string8_null_heap_(ret, (const char *)str);
}

int lisp0_string16_(addr *ret, const void *str)
{
	return string16_null_heap_(ret, (const byte16 *)str);
}

int lisp0_string32_(addr *ret, const void *str)
{
	return string32_null_heap_(ret, (const unicode *)str);
}

int lisp_string8_(addr x, const void *str)
{
	addr pos;

	Return(string8_null_heap_(&pos, (const char *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string16_(addr x, const void *str)
{
	addr pos;

	Return(string16_null_heap_(&pos, (const byte16 *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string32_(addr x, const void *str)
{
	addr pos;

	Return(string32_null_heap_(&pos, (const unicode *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string_getc_(addr pos, size_t i, unicode *c)
{
	hold_value(pos, &pos);
	return string_getc_(pos, i, c);
}


/*
 *  strvect
 */
int lisp_strvect_getc(addr pos, size_t i, unicode *c)
{
	const unicode *body;
	size_t size;

	hold_value(pos, &pos);
	if (! strvectp(pos))
		return -1; /* type error */
	strvect_posbodylen(pos, &body, &size);
	if (size <= i)
		return 1; /* size error */
	*c = body[i];
	return 0;
}

int lisp_strvect_length(addr pos, size_t *ret)
{
	hold_value(pos, &pos);
	if (! strvectp(pos))
		return -1; /* type error */
	GetStringSize(pos, ret);
	return 0;
}

