#include "compile_load.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_typedef.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "hashtable.h"
#include "hold.h"
#include "load_code.h"
#include "stream.h"
#include "stream_function.h"
#include "typedef.h"

static int eval_compile_load_toplevel_(Execute ptr, addr stream, int *ret)
{
	enum FaslCode type;
	addr code;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (;;) {
		Return(faslread_type_(stream, &type));
		if (type == FaslCode_break) {
			*ret = 0;
			break;
		}
		if (type == FaslCode_eof) {
			*ret = 1;
			break;
		}
		Return(unread_byte_stream_(stream, (byte)type));

		Return(faslread_value_(ptr, stream, &code));
		localhold_set(hold, 0, code);
		CheckType(code, LISPTYPE_CODE);
		Return(runcode_control_(ptr, code));
	}
	localhold_end(hold);

	return 0;
}

static int eval_compile_load_loop_(Execute ptr, addr stream)
{
	int check;

	check = 0;
	for (;;) {
		Return(eval_compile_load_toplevel_(ptr, stream, &check));
		if (check)
			break;
	}

	return 0;
}

static int eval_compile_load_call_(Execute ptr, addr stream)
{
	int check;

	/* header */
	Return(faslread_header_(stream, &check));
	if (check)
		return fmte_("Invalid fasl header.", NULL);

	/* fasl body */
	Return(eval_compile_load_loop_(ptr, stream));

	/* footer */
	Return(faslread_footer_(stream, &check));
	if (check)
		return fmte_("Invalid fasl footer.", NULL);

	return 0;
}

int eval_compile_load_(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	fasl_load_time_value(ptr);
	(void)eval_compile_load_call_(ptr, stream);
	return pop_control_(ptr, control);
}

