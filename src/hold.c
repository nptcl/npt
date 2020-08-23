#include <stdarg.h>
#include "cons.h"
#include "control_object.h"
#include "heap.h"
#include "hold.h"
#include "local.h"
#include "symbol.h"

/*
 *  gchold
 */
static int setgchold_p(addr pos)
{
#ifdef LISP_DEBUG
	Check((pos != Unbound && pos[0] == 0xAA), "break local memory.");
#endif
	return (pos != Unbound) && (! GetStatusDynamic(pos));
}

_g void setgchold(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_GCHOLD);
	Check(! setgchold_p(value), "gchold error");
	SetArrayA2(pos, index, value);
}

_g void gchold_local(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFUL <= size, "size error");
	local_array2(local, ret, LISPSYSTEM_GCHOLD, (byte16)size);
}

static void gchold_heap(addr *ret, size_t size)
{
	Check(0xFFFFUL <= size, "size error");
	heap_array2(ret, LISPSYSTEM_GCHOLD, (byte16)size);
}

_g void gchold_push_local(LocalRoot local, addr pos)
{
	addr array;
	gchold_local(local, &array, 1);
	setgchold(array, 0, pos);
}

static void gchold_pushva_stdarg(LocalRoot local, va_list args)
{
	addr pos, array;
	size_t size, i;
	va_list dest;

	/* index */
	va_copy(dest, args);
	for (size = 0; ; size++) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
	}

	/* make */
	gchold_local(local, &array, size);
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(size <= i, "size error");
		setgchold(array, i, pos);
	}
	Check(size != i, "size error");
}

_g void gchold_pushva_local(LocalRoot local, ...)
{
	va_list args;

	va_start(args, local);
	gchold_pushva_stdarg(local, args);
	va_end(args);
}

static void gchold_pushva_force_stdarg(LocalRoot local, va_list args)
{
	addr pos, array;
	size_t size, i;
	va_list dest;

	/* index */
	va_copy(dest, args);
	size = 0;
	for (;;) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
		if (setgchold_p(pos))
			size++;
	}

	/* make */
	gchold_local(local, &array, size);
	i = 0;
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		if (setgchold_p(pos)) {
			Check(size <= i, "size error");
			setgchold(array, i, pos);
			i++;
		}
	}
	Check(size != i, "size error");
}

_g void gchold_pushva_force_local(LocalRoot local, ...)
{
	va_list args;

	va_start(args, local);
	gchold_pushva_force_stdarg(local, args);
	va_end(args);
}

static void gchold_special(Execute ptr, addr value)
{
	addr symbol, pos;

	GetConst(SYSTEM_GCHOLD, &symbol);
	if (existspecial_control(ptr, symbol)) {
		getspecial_local(ptr, symbol, &pos);
		cons_heap(&value, value, (pos == Unbound)? Nil: pos);
		setspecial_local(ptr, symbol, value);
	}
	else {
		conscar_heap(&value, value);
		pushspecial_control(ptr, symbol, value);
	}
}

_g void gchold_push_special(Execute ptr, addr pos)
{
	addr array;
	gchold_heap(&array, 1);
	setgchold(array, 0, pos);
	gchold_special(ptr, array);
}

_g void gchold_pushva_special(Execute ptr, ...)
{
	addr pos, array;
	size_t size, i;
	va_list args, dest;

	/* index */
	va_start(args, ptr);
	va_copy(dest, args);
	for (size = 0; ; size++) {
		pos = va_arg(dest, addr);
		if (pos == NULL)
			break;
	}

	/* make */
	gchold_heap(&array, size);
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(size <= i, "size error");
		setgchold(array, i, pos);
	}
	Check(size != (i + 1UL), "size error");
	va_end(args);

	/* push */
	gchold_special(ptr, array);
}


/*
 *  gchold
 */
_g LocalHold localhold_local(LocalRoot local)
{
	LocalStack stack;
	LocalHold ptr;

	push_local(local, &stack);
	ptr = (LocalHold)lowlevel_local(local, sizeoft(struct localhold));
	ptr->local = local;
	ptr->stack = stack;
	ptr->array = Nil;

	return ptr;
}

_g LocalHold localhold_local_push(LocalRoot local, addr pos)
{
	LocalHold hold;

	hold = localhold_local(local);
	localhold_push(hold, pos);

	return hold;
}

_g void localhold_push(LocalHold hold, addr pos)
{
	if (pos != Nil && pos != Unbound && pos != NULL)
		gchold_push_local(hold->local, pos);
}

_g void localhold_pushva(LocalHold hold, ...)
{
	va_list args;

	va_start(args, hold);
	gchold_pushva_stdarg(hold->local, args);
	va_end(args);
}

_g void localhold_pushva_force(LocalHold hold, ...)
{
	va_list args;

	va_start(args, hold);
	gchold_pushva_force_stdarg(hold->local, args);
	va_end(args);
}

_g LocalHold localhold_array(LocalRoot local, size_t size)
{
	LocalHold hold;

	hold = localhold_local(local);
	gchold_local(local, &(hold->array), size);

	return hold;
}

_g void localhold_end(LocalHold hold)
{
	rollback_local(hold->local, hold->stack);
}

_g void localhold_set(LocalHold hold, size_t index, addr value)
{
	CheckType(hold->array, LISPSYSTEM_GCHOLD);
	setgchold(hold->array, index, value);
}

_g void localhold_set_force(LocalHold hold, size_t index, addr value)
{
	CheckType(hold->array, LISPSYSTEM_GCHOLD);
	if (setgchold_p(value))
		setgchold(hold->array, index, value);
}


/*
 *  hold object
 */
_g void Hold_local(addr *ret, addr value)
{
	hold_value(value, &value);
	hold_local(Local_Thread, ret, value);
}

_g void hold_local(LocalRoot local, addr *ret, addr value)
{
	hold_value(value, &value);
	local_array2(local, ret, LISPSYSTEM_HOLD, 1);
}

_g int holdp(addr pos)
{
	return pos && (pos != Unbound) && GetType(pos) == LISPSYSTEM_HOLD;
}

_g void hold_set(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_HOLD);
	hold_value(value, &value);
	SetArrayA2(pos, 0, value);
}

_g void hold_set_null(addr pos, addr value)
{
	if (pos) {
		CheckType(pos, LISPSYSTEM_HOLD);
		hold_value(value, &value);
		SetArrayA2(pos, 0, value);
	}
}

_g void hold_get(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_HOLD);
	GetArrayA2(pos, 0, ret);
}

_g void hold_value(addr pos, addr *ret)
{
	if (holdp(pos))
		GetArrayA2(pos, 0, ret);
	else
		*ret = pos;
}

_g addr holdv(addr pos)
{
	if (holdp(pos)) {
		GetArrayA2(pos, 0, &pos);
		return pos;
	}
	else {
		return pos;
	}
}

