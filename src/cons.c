#include <stdarg.h>
#include "condition.h"
#include "cons.h"

/*
 *  cons
 */
int consp_getcons(addr list, addr *car, addr *cdr)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCons(list, car, cdr);
	return 1;
}

int consp_getcar(addr list, addr *car)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCar(list, car);
	return 1;
}

int consp_getcdr(addr list, addr *cdr)
{
	if (GetType(list) != LISPTYPE_CONS)
		return 0;
	GetCdr(list, cdr);
	return 1;
}

int getcons_(addr list, addr *car, addr *cdr)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCons(list, car, cdr);
	return 0;
}

int getcar_(addr list, addr *car)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCar(list, car);
	return 0;
}

int getcdr_(addr list, addr *cdr)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	GetCdr(list, cdr);
	return 0;
}

int setcons_(addr cons, addr car, addr cdr)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCons(cons, car, cdr);
	return 0;
}

int setcar_(addr cons, addr car)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCar(cons, car);
	return 0;
}

int setcdr_(addr cons, addr cdr)
{
	if (! consp(cons))
		return TypeError_(cons, CONS);
	SetCdr(cons, cdr);
	return 0;
}


/*
 *  list
 */
void list_stdarg_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr left, right, next;

	left = va_arg(args, addr);
	if (left == NULL) {
		*ret = Nil;
		return;
	}
	conscar_alloc(local, &right, left);
	*ret = right;

	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL)
			break;
		conscar_alloc(local, &next, left);
		SetCdr(right, next);
		right = next;
	}
}

void list_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_stdarg_alloc(NULL, ret, args);
	va_end(args);
}

void list_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	list_stdarg_alloc(local, ret, args);
	va_end(args);
}

void list_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_stdarg_alloc(local, ret, args);
	va_end(args);
}

void pushva_heap(addr *list, ...)
{
	addr root, pos;
	va_list args;

	root = *list;
	va_start(args, list);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		cons_heap(&root, pos, root);
	}
	va_end(args);
	*list = root;
}


/*
 *  list*
 */
/* `(list* ,first ,@cons) */
int lista_safe_alloc_(LocalRoot local, addr *ret, addr first, addr cons)
{
	addr pos, root;

	for (root = Nil; cons != Nil; first = pos) {
		if (GetType(cons) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The argument ~S must be a list.", cons, NULL);
		}
		GetCons(cons, &pos, &cons);
		cons_alloc(local, &root, first, root);
	}

	/* nil */
	if (root == Nil)
		return Result(ret, first);

	/* loop */
	for (;;) {
		GetCdr(root, &pos);
		SetCdr(root, first);
		if (pos == Nil)
			break;
		first = root;
		root = pos;
	}

	return Result(ret, root);
}
int lista_safe_local_(LocalRoot local, addr *ret, addr first, addr cons)
{
	Check(local == NULL, "local error");
	return lista_safe_alloc_(local, ret, first, cons);
}
int lista_safe_heap_(addr *ret, addr first, addr cons)
{
	return lista_safe_alloc_(NULL, ret, first, cons);
}

static void lista_stdarg(LocalRoot local, addr *ret, va_list args, addr pos1)
{
	addr pos2, pos3, cons;

	/* nil */
	if (pos1 == NULL) {
		*ret = Nil;
		return;
	}

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}

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

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

void lista_stdarg_noerror(LocalRoot local, addr *ret, va_list args)
{
	addr pos1 = va_arg(args, addr);
	lista_stdarg(local, ret, args, pos1);
}

int lista_stdarg_safe_(LocalRoot local, addr *ret, va_list args)
{
	addr pos1;

	pos1 = va_arg(args, addr);
	/* nil */
	if (pos1 == NULL) {
		*ret = Nil;
		return fmte_("LIST* must be at least one argument.", NULL);
	}
	lista_stdarg(local, ret, args, pos1);

	return 0;
}

void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	Check(pos1 == NULL, "lista at least one argument.");
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}

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

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

void lista_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

void lista_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

void lista_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(NULL, ret, args);
	va_end(args);
}


/*
 *  bind
 */
void List_bind(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			Check(list != Nil, "list nil error");
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);
}

int list_bind_(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	pos = Nil;
	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			if (list != Nil)
				return fmte_("Too many list argument.", NULL);
			break;
		}
		if (list == Nil)
			return fmte_("Too few list argument.", NULL);
		Return_getcons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);

	return 0;
}

void Lista_bind(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		Check(list != Nil, "list nil error");
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);
}

int lista_bind_(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	pos = Nil;
	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		if (list != Nil)
			return fmte_("Too few argument.", NULL);
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		if (! consp(list))
			return fmte_("Too few argument.", NULL);
		Return_getcons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);

	return 0;
}


/*
 *  copy-tree
 */
void copy_tree_alloc(LocalRoot local, addr *ret, addr cdr)
{
	addr car;

	if (GetType(cdr) != LISPTYPE_CONS) {
		*ret = cdr;
	}
	else {
		GetCons(cdr, &car, &cdr);
		copy_tree_alloc(local, &car, car);
		copy_tree_alloc(local, &cdr, cdr);
		cons_alloc(local, ret, car, cdr);
	}
}

void copy_tree_local(LocalRoot local, addr *ret, addr list)
{
	Check(local == NULL, "local error");
	copy_tree_alloc(local, ret, list);
}

void copy_tree_heap(addr *ret, addr list)
{
	copy_tree_alloc(NULL, ret, list);
}

