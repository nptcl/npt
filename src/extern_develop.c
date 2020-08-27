#include <stdarg.h>
#include "cons.h"
#include "constant.h"
#include "extern_error.h"
#include "hold.h"
#include "object.h"
#include "local.h"
#include "typedef.h"

/*
 *  error
 */
void lisp_abort_type(addr value, constindex index)
{
	addr type;

	GetConstant(index, &type);
	lisp_abort8("type error: ~S must be a ~A type.", value, type, NULL);
}


/*
 *  list
 */
void lisp0_list_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr x, y, next;

	x = va_arg(args, addr);
	if (x == NULL) {
		*ret = Nil;
		return;
	}
	hold_value(x, &x);
	conscar_alloc(local, &y, x);
	*ret = y;

	for (;;) {
		x = va_arg(args, addr);
		if (x == NULL)
			break;
		hold_value(x, &x);
		conscar_alloc(local, &next, x);
		SetCdr(y, next);
		y = next;
	}
}

void lisp0_lista_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}
	hold_value(pos1, &pos1);

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}
	hold_value(pos2, &pos2);

	/* result */
	conscar_alloc(local, &cons, pos1);
	*ret = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			return;
		}
		hold_value(pos3, &pos3);

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

