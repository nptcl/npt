#include "env_code.h"
#include "format.h"
#include "function.h"

/*
 *  disassemble
 */
static void disassemble_code(addr stream, addr pos)
{
	addr code;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetFunction(pos, &code);
	if (code == Nil)
		return;
	CheckType(code, LISPTYPE_CODE);

}

_g int disassemble_common(Execute ptr, addr stream, addr pos)
{
	CheckType(stream, LISPTYPE_STREAM);
	CheckType(pos, LISPTYPE_FUNCTION);
	if (compiled_function_p(pos)) {
		Return1(format_stream(ptr, stream, "COMPILED-FUNCTION.~%", NULL));
	}
	else {
		Return1(format_stream(ptr, stream, "INTERPRETED-FUNCTION.~%", NULL));
		disassemble_code(stream, pos);
	}

	return 0;
}

