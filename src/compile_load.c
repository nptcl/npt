#include "compile_load.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_typedef.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "hold.h"
#include "stream.h"
#include "stream_function.h"
#include "typedef.h"

static int eval_compile_load_loop(Execute ptr, addr stream)
{
	enum FaslCode type;
	addr code;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (;;) {
		Return(faslread_type_(stream, &type));
		if (type == FaslCode_end)
			break;
		Return(unread_byte_stream_(stream, (byte)type));

		Return(faslread_value(ptr, stream, &code));
		localhold_set(hold, 0, code);
		CheckType(code, LISPTYPE_CODE);
		Return(runcode_control_(ptr, code));
	}
	localhold_end(hold);

	return 0;
}

_g int eval_compile_load(Execute ptr, addr stream)
{
	int check;

	/* header */
	Return(faslread_header_(stream, &check));
	if (check)
		return fmte_("Invalid fasl header.", NULL);

	/* fasl body */
	Return(eval_compile_load_loop(ptr, stream));

	/* footer */
	Return(faslread_footer_(stream, &check));
	if (check)
		return fmte_("Invalid fasl footer.", NULL);

	return 0;
}

