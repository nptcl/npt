#include "code.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "file.h"
#include "input.h"
#include "pointer.h"
#include "readtable.h"
#include "stream.h"

static int readlist_loop(Execute ptr, addr stream, addr *ret)
{
	addr list, x;
	int result;

	list = Nil;
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
	}
	nreverse_list_unsafe(ret, list);

	return 0;
}

static void readlist_finalize(Execute ptr)
{
	addr stream;

	getdata_control(ptr, &stream);
	Check(! streamp(stream), "type error");
	close_stream(stream);
}

static int readlist_unwind_protect(Execute ptr, addr file, addr *ret)
{
	int check;
	addr stream, control, code;

	open_input_stream_error(ptr, &stream, file);
	/* finalize */
	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_readlist_finalize, stream);
	setfinalize_control(ptr, control, code);
	/* code */
	check = readlist_loop(ptr, stream, ret);
	return free_check_control(ptr, control, check);
}

_g int readlist_input(Execute ptr, addr file, addr *ret)
{
	int check;
	addr list;

	check = readlist_unwind_protect(ptr, file, &list);
	if (check)
		return 1;
	if (list == Unbound)
		fmte("Invalid file ~S.", file, NULL);
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

