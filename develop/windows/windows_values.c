#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "execute_values.h"
#include "strtype.h"
#include "typedef.h"
#include "windows_window.h"
#include "windows_values.h"

static int windows_values_single_(addr cons, addr *ret)
{
	addr check;

	Return_getcons(cons, ret, &check);
	if (check != Nil)
		return fmte_("Too many arguments, ~S.", cons, NULL);

	return 0;
}

static int windows_values_show_(Execute ptr, addr var)
{
	int check;

	/* nil */
	if (var == Nil)
		goto mode_default;

	/* :default */
	Return(windows_values_single_(var, &var));
	Return(string_designer_equalp_char_(var, "default", &check));
	if (check)
		goto mode_default;

	/* :show */
	Return(windows_values_single_(var, &var));
	Return(string_designer_equalp_char_(var, "show", &check));
	if (check)
		goto mode_show;

	/* :hide */
	Return(windows_values_single_(var, &var));
	Return(string_designer_equalp_char_(var, "hide", &check));
	if (check)
		goto mode_hide;

	/* error */
	return fmte_("Invalid show mode, ~S.", var, NULL);

mode_default:
	if (windows_window_show_default())
		return fmte_("show-window default error.", NULL);
	setresult_control(ptr, T);
	return 0;

mode_show:
	if (windows_window_show_show())
		return fmte_("show-window show error.", NULL);
	setresult_control(ptr, T);
	return 0;

mode_hide:
	if (windows_window_show_hide())
		return fmte_("show-window hide error.", NULL);
	setresult_control(ptr, T);
	return 0;
}

static int windows_values_window_x_(Execute ptr, addr var)
{
	fixnum value;

	Return(windows_values_single_(var, &var));
	Return(getfixnum_unsigned_(var, &value));
	/* TODO */

	return 0;
}

static int windows_values_window_y_(Execute ptr, addr var)
{
	fixnum value;

	Return(windows_values_single_(var, &var));
	Return(getfixnum_unsigned_(var, &value));
	/* TODO */

	return 0;
}

int windows_values_(Execute ptr, addr var, addr args, int *ret)
{
	/* :show */
	Return(string_designer_equalp_char_(var, "show", ret));
	if (*ret)
		return windows_values_show_(ptr, args);

	/* :window-x */
	Return(string_designer_equalp_char_(var, "window-x", ret));
	if (*ret)
		return windows_values_window_x_(ptr, args);

	/* :window-y */
	Return(string_designer_equalp_char_(var, "window-y", ret));
	if (*ret)
		return windows_values_window_y_(ptr, args);

	/* error */
	return Result(ret, 0);
}

