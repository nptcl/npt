#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "define.h"
#include "paper.h"
#include "terme_escape.h"
#include "terme_function.h"
#include "typedef.h"

#if defined(LISP_TERME_UNIX)
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#elif defined(LISP_TERME_WINDOWS)
#include <windows.h>
#endif

/*
 *  enable
 */
#if defined(LISP_TERME_UNIX)
int terme_call_enable_p(void)
{
	return 1;
}
#else
int terme_call_enable_p(void)
{
	return 0;
}
#endif


/*
 *  input
 */
#if defined(LISP_TERME_UNIX)
int terme_call_input_(addr args, addr *ret1, addr *ret2)
{
	*ret1 = *ret2 = Nil;
	return 0;
}
#else
int terme_call_input_(addr args, addr *ret1, addr *ret2)
{
	*ret1 = *ret2 = Nil;
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  move
 */
#if defined(LISP_TERME_UNIX)
static int terme_call_move_relative_(fixnum x, fixnum y)
{
	int value, check;

	check = 0;
	if (x) {
		value = (int)x;
		if (value < 0)
			check = terme_cursor_left(-value);
		else
			check = terme_cursor_right(value);
	}
	if (y) {
		value = (int)y;
		if (value < 0)
			check = terme_cursor_up(-value);
		else
			check = terme_cursor_down(value);
	}
	if (check)
		return fmte_("terme_cursor error.", NULL);

	return 0;
}

static int terme_call_fixnum_(addr pos, fixnum *ret)
{
	if (pos == Nil)
		return Result(ret, 0);
	else
		return getfixnum_signed_(pos, ret);
}

int terme_call_move_(addr args)
{
	addr pos;
	fixnum x, y;

	/* x */
	Return_getcons(args, &pos, &args);
	Return(terme_call_fixnum_(pos, &x));
	/* y */
	Return_getcons(args, &pos, &args);
	Return(terme_call_fixnum_(pos, &y));
	/* move */
	return terme_call_move_relative_(x, y);
}
#else
int terme_call_move_(addr args)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  clear
 */
#if defined(LISP_TERME_UNIX)
int terme_call_clear_(void)
{
	if (terme_cursor_delete_page())
		return fmte_("terme_cursor_delete_page error.", NULL);

	return 0;
}
#else
int terme_call_clear_(void)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  begin
 */
#if defined(LISP_TERME_UNIX)
int terme_call_begin_(addr *ret)
{
	addr pos;
	size_t size;
	struct termios v;

	/* backup */
	if (tcgetattr(STDIN_FILENO, &v)) {
		*ret = Nil;
		return fmte_("tcgetattr error.", NULL);
	}
	paper_body_heap(&pos, sizeoft(v));
	paper_set_memory(pos, 0, sizeoft(v), (const void *)&v, &size);
	Check(size != sizeoft(v), "size error");

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &v)) {
		*ret = Nil;
		return fmte_("tcsetattr error.", NULL);
	}

	return Result(ret, pos);
}
#else
int terme_call_begin_(addr *ret)
{
	*ret = Nil;
	return fmte_("TERME is not enabled.", NULL);
}
#endif


/*
 *  end
 */
#if defined(LISP_TERME_UNIX)
int terme_call_end_(addr pos)
{
	addr type;
	size_t size;
	struct termios v;

	if (! paperp(pos)) {
		GetConst(SYSTEM_PAPER, &type);
		return call_type_error_va_(NULL, pos, type,
				"Object ~S must be a PAPER type.", pos, NULL);
	}

	paper_get_memory(pos, 0, sizeoft(v), (void *)&v, &size);
	Check(size != sizeoft(v), "size error");
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &v))
		return fmte_("tcsetattr error.", NULL);

	return 0;
}
#else
int terme_call_end_(addr pos)
{
	return fmte_("TERME is not enabled.", NULL);
}
#endif

