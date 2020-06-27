#include "compile_read.h"
#include "compile_stream.h"
#include "compile_typedef.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "stream.h"
#include "typedef.h"

static int eval_compile_load_loop(Execute ptr, addr stream)
{
	enum FaslCode type;
	addr code;

	for (;;) {
		faslread_type(stream, &type);
		if (type == FaslCode_end)
			break;
		unread_byte_stream(stream, (byte)type);

		Return(faslread_value(ptr, stream, &code));
		CheckType(code, LISPTYPE_CODE);
		Return(runcode_control(ptr, code));
	}

	return 0;
}

_g int eval_compile_load(Execute ptr, addr stream)
{
	/* header */
	if (faslread_header(stream)) {
		fmte("Invalid fasl header.", NULL);
		return 0;
	}

	/* fasl body */
	Return(eval_compile_load_loop(ptr, stream));

	/* footer */
	if (faslread_footer(stream)) {
		fmte("Invalid fasl footer.", NULL);
		return 0;
	}

	return 0;
}

