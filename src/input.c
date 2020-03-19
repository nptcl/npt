#include "code.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "file.h"
#include "gc.h"
#include "input.h"
#include "pointer.h"
#include "readtable.h"
#include "stream.h"

static int readlist_loop(Execute ptr, addr stream, addr *ret)
{
	addr list, x;
	int result;
	LocalHold hold;

	list = Nil;
	hold = LocalHold_array(ptr, 1);
	for (;;) {
		if (read_stream(ptr, stream, &result, &x))
			return 1;
		if (result < 0) {
			*ret = Unbound;
			return 0;
		}
		if (result)
			break;
		cons_heap(&list, x, list);
		localhold_set(hold, 0, list);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, list);

	return 0;
}

static int readlist_finalize(Execute ptr)
{
	addr stream;

	getdata_control(ptr, &stream);
	Check(! streamp(stream), "type error");
	close_stream(stream);

	return 0;
}

static int readlist_unwind_protect(Execute ptr, addr file, addr *ret)
{
	int check;
	addr stream, control, code;
	LocalHold hold;

	open_input_stream_error(ptr, &stream, file);
	hold = LocalHold_array(ptr, 1);
	localhold_push(hold, stream);
	/* finalize */
	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_readlist_finalize, stream);
	setfinalize_control(ptr, control, code);
	/* code */
	check = readlist_loop(ptr, stream, ret);
	localhold_set(hold, 0, *ret);
	Return(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

_g int readlist_input(Execute ptr, addr file, addr *ret)
{
	int check;
	addr list;

	check = readlist_unwind_protect(ptr, file, &list);
	if (check)
		return 1;
	if (list == Unbound)
		_fmte("Invalid file ~S.", file, NULL);
	*ret = list;

	return check;
}


/*
 *  initialize
 */
_g void init_input(void)
{
	SetPointerType(empty, readlist_finalize);
}

