#include "code_object.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "file_open.h"
#include "hold.h"
#include "input.h"
#include "pointer.h"
#include "reader.h"
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
	nreverse(ret, list);

	return 0;
}

static int readlist_unwind_protect_call_(
		Execute ptr, LocalHold hold, addr stream, addr *ret)
{
	addr pos;

	push_close_stream(ptr, stream);
	Return(readlist_loop(ptr, stream, &pos));
	localhold_set(hold, 0, pos);

	return Result(ret, pos);
}

static int readlist_unwind_protect_(Execute ptr, addr file, addr *ret)
{
	addr stream, control;
	LocalHold hold;

	Return(open_input_stream_error_(ptr, &stream, file));
	hold = LocalHold_array(ptr, 1);
	localhold_push(hold, stream);
	/* finalize */
	push_control(ptr, &control);
	(void)readlist_unwind_protect_call_(ptr, hold, stream, ret);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int readlist_input(Execute ptr, addr file, addr *ret)
{
	addr list;

	list = Nil;
	Return(readlist_unwind_protect_(ptr, file, &list));
	if (list == Unbound)
		return fmte_("Invalid file ~S.", file, NULL);

	return Result(ret, list);
}

