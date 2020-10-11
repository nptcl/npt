#include "constant.h"
#include "execute.h"
#include "execute_values.h"
#include "function.h"
#include "pointer.h"
#include "typedef.h"

/*
 *  constantly
 */
static int function_constantly_nil(Execute ptr, addr ignore)
{
	setresult_control(ptr, Nil);
	return 0;
}

static int function_constantly_t(Execute ptr, addr ignore)
{
	setresult_control(ptr, T);
	return 0;
}

_g void build_function(void)
{
	addr pos;

	/* nil */
	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_constantly_nil);
	SetConst(FUNCTION_NIL, pos);
	/* t */
	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_constantly_t);
	SetConst(FUNCTION_T, pos);
}

_g void init_function(void)
{
	SetPointerCall(defun, dynamic, constantly_nil);
	SetPointerCall(defun, dynamic, constantly_t);
}

