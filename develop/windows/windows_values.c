#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "execute_values.h"
#include "strtype.h"
#include "typedef.h"
#include "windows_arch.h"
#include "windows_values.h"
#include "windows_window.h"

static int windows_values_single_(addr cons, addr *ret)
{
	addr check;

	Return_getcons(cons, ret, &check);
	if (check != Nil)
		return fmte_("Too many arguments, ~S.", cons, NULL);

	return 0;
}

static int windows_values_show_(Execute ptr, addr args)
{
	int check;

	/* nil */
	if (args == Nil)
		goto mode_default;

	/* :default */
	Return(windows_values_single_(args, &args));
	Return(string_designer_equalp_char_(args, "default", &check));
	if (check)
		goto mode_default;

	/* :show */
	Return(string_designer_equalp_char_(args, "show", &check));
	if (check)
		goto mode_show;

	/* :hide */
	Return(string_designer_equalp_char_(args, "hide", &check));
	if (check)
		goto mode_hide;

	/* error */
	return fmte_("Invalid show mode, ~S.", args, NULL);

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

static int windows_values_size_(Execute ptr, addr args)
{
	int s;
	unsigned x, y;
	addr v1, v2;

	/* empty */
	getwidth_windows(&x, &y);
	if (args == Nil)
		goto finish;

	Return_getcons(args, &v1, &args);
	Return(windows_values_single_(args, &v2));
	if (v1 == Nil && v2 == Nil)
		return 0;
	if (v1 != Nil) {
		Return(getint_unsigned_(v1, &s));
		x = (unsigned)s;
	}
	if (v2 != Nil) {
		Return(getint_unsigned_(v2, &s));
		y = (unsigned)s;
	}
	if (windows_window_size_update(x, y))
		return fmte_("windows_window_size_update error", NULL);

finish:
	fixnum_heap(&v1, (fixnum)x);
	fixnum_heap(&v2, (fixnum)y);
	setvalues_control(ptr, v1, v2, NULL);
	return 0;
}

int windows_values_(Execute ptr, addr var, addr args, int *ret)
{
	int check;

	/* windows */
	Return(string_designer_equalp_char_(var, "windows", &check));
	if (! check)
		return Result(ret, 0);
	Return_getcons(args, &var, &args);
	*ret = 1;

	/* :show */
	Return(string_designer_equalp_char_(var, "show", &check));
	if (check)
		return windows_values_show_(ptr, args);

	/* :size */
	Return(string_designer_equalp_char_(var, "size", &check));
	if (check)
		return windows_values_size_(ptr, args);

	/* error */
	return Result(ret, 0);
}

