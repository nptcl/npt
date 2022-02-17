#include "condition.h"
#include "cons.h"
#include "execute_values.h"
#include "define.h"
#include "control_object.h"
#include "terme_function.h"
#include "terme_values.h"

#ifdef LISP_TERME_WINDOWS
#include "windows_values.h"
#endif

static int terme_values_input_(Execute ptr, addr args)
{
	addr x, y;

	Return(terme_call_input_(args, &x, &y));
	setvalues_control(ptr, x, y, NULL);
	return 0;
}

static int terme_values_output_(Execute ptr, addr args)
{
	Return(terme_call_output_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_move_(Execute ptr, addr args)
{
	Return(terme_call_move_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_clear_(Execute ptr, addr args)
{
	Return(terme_call_clear_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_delete_(Execute ptr, addr args)
{
	Return(terme_call_delete_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_font_(Execute ptr, addr args)
{
	Return(terme_call_font_(ptr, args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_size_(Execute ptr, addr args)
{
	addr x, y;

	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(terme_call_size_(&x, &y));
	setvalues_control(ptr, x, y, NULL);

	return 0;
}

static int terme_values_scroll_(Execute ptr, addr args)
{
	Return(terme_call_scroll_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_begin_(Execute ptr, addr args)
{
	Return(terme_call_begin_(args, &args));
	setresult_control(ptr, args);
	return 0;
}

static int terme_values_end_(Execute ptr, addr args)
{
	addr pos;

	Return_getcons(args, &pos, &args);
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);
	Return(terme_call_end_(pos));
	setresult_control(ptr, Nil);

	return 0;
}

static int terme_values_enable_(Execute ptr, addr args)
{
	int check;

	/* arguments */
	if (args != Nil)
		return fmte_("Invalid arguments, ~S.", args, NULL);

	/* enable-p */
	check = terme_call_enable_p();
	setbool_control(ptr, check);

	return 0;
}

static int terme_values_signal_(Execute ptr, addr args)
{
	Return(terme_call_signal_(args));
	setresult_control(ptr, Nil);
	return 0;
}

static int terme_values_operator_(Execute ptr, addr var, addr args, int *ret)
{
	addr check;

	*ret = 1;

	/* input */
	GetConst(SYSTEM_TERME_INPUT, &check);
	if (var == check)
		return terme_values_input_(ptr, args);

	/* output */
	GetConst(SYSTEM_TERME_OUTPUT, &check);
	if (var == check)
		return terme_values_output_(ptr, args);

	/* move */
	GetConst(SYSTEM_TERME_MOVE, &check);
	if (var == check)
		return terme_values_move_(ptr, args);

	/* clear */
	GetConst(SYSTEM_TERME_CLEAR, &check);
	if (var == check)
		return terme_values_clear_(ptr, args);

	/* delete */
	GetConst(SYSTEM_TERME_DELETE, &check);
	if (var == check)
		return terme_values_delete_(ptr, args);

	/* font */
	GetConst(SYSTEM_TERME_FONT, &check);
	if (var == check)
		return terme_values_font_(ptr, args);

	/* size */
	GetConst(SYSTEM_TERME_SIZE, &check);
	if (var == check)
		return terme_values_size_(ptr, args);

	/* scroll */
	GetConst(SYSTEM_TERME_SCROLL, &check);
	if (var == check)
		return terme_values_scroll_(ptr, args);

	/* begin */
	GetConst(SYSTEM_TERME_BEGIN, &check);
	if (var == check)
		return terme_values_begin_(ptr, args);

	/* end */
	GetConst(SYSTEM_TERME_END, &check);
	if (var == check)
		return terme_values_end_(ptr, args);

	/* enable */
	GetConst(SYSTEM_TERME_ENABLE, &check);
	if (var == check)
		return terme_values_enable_(ptr, args);

	/* signal */
	GetConst(SYSTEM_TERME_SIGNAL, &check);
	if (var == check)
		return terme_values_signal_(ptr, args);

	/* error */
	return Result(ret, 0);
}

int terme_values_(Execute ptr, addr var, addr args)
{
	int check;

#ifdef LISP_TERME_WINDOWS
	Return(windows_values_(ptr, var, args, &check));
	if (check)
		return 0;
#endif

	Return(terme_values_operator_(ptr, var, args, &check));
	if (check)
		return 0;

	return fmte_("Invalid keyword ~S.", var, NULL);
}

