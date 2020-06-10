#include "compile_read.h"
#include "condition.h"
#include "control_execute.h"
#include "execute.h"
#include "typedef.h"

_g int eval_compile_load_loop(Execute ptr, addr stream)
{
	addr code;

	Return(faslread_code(ptr, stream, &code));
	Return(runcode_control(ptr, code));

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
