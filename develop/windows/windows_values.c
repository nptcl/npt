#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "execute_values.h"
#include "integer.h"
#include "strtype.h"
#include "terme_escape.h"
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
	Return(string_designator_equalp_char_(args, "default", &check));
	if (check)
		goto mode_default;

	/* :show */
	Return(string_designator_equalp_char_(args, "show", &check));
	if (check)
		goto mode_show;

	/* :hide */
	Return(string_designator_equalp_char_(args, "hide", &check));
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
	unsigned x0, y0, x, y;
	addr v1, v2;

	/* empty */
	getwidth_windows(&x0, &y0);
	x = x0;
	y = y0;
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
	if (x == x0 && y == y0)
		goto finish;
	if (windows_window_size_update(x, y))
		return fmte_("windows_window_size_update error", NULL);

finish:
	fixnum_heap(&v1, (fixnum)x);
	fixnum_heap(&v2, (fixnum)y);
	setvalues_control(ptr, v1, v2, NULL);
	return 0;
}

static int windows_values_font_(Execute ptr, addr args)
{
	/* (terme 'windows 'font name &optional size) */
	int value;
	addr list, name, size;

	Return_getcons(args, &name, &args);
	if (args == Nil) {
		size = Nil;
		value = -1;
	}
	else {
		Return_getcons(args, &size, &list);
		if (list != Nil)
			return fmte_("Invalid arguments, ~S.", list, NULL);
		Return(getint_unsigned_(size, &value));
	}
	Return(windows_window_setfont_(name, value));
	setresult_control(ptr, Nil);
	return 0;
}

static int windows_values_color_integer_(addr x, int forep)
{
	int v;

	Return(getint_signed_(x, &v));
	if (v < 0 || 0xFF < v)
		return fmte_("Value ~S must be a integer between 0 and 0xFF.", x, NULL);

	return windows_window_color_integer_((unsigned)v, forep);
}

static int windows_values_color_rgb_(addr x, addr y, addr z, int forep)
{
	int r, g, b;

	Return(getint_signed_(x, &r));
	if (r < 0 || 0xFF < r)
		return fmte_("Value ~S must be a integer between 0 and 0xFF.", x, NULL);
	Return(getint_signed_(y, &g));
	if (g < 0 || 0xFF < g)
		return fmte_("Value ~S must be a integer between 0 and 0xFF.", y, NULL);
	Return(getint_signed_(z, &b));
	if (b < 0 || 0xFF < b)
		return fmte_("Value ~S must be a integer between 0 and 0xFF.", b, NULL);

	return windows_window_color_rgb_(
		(unsigned)r,
		(unsigned)g,
		(unsigned)b,
		forep);
}

static int windows_values_color_symbol_(Execute ptr, addr x, int forep)
{
	int value, bright;

	/* color */
	Return(terme_color_symbol_(ptr, x, &value, &bright));
	if (0 <= value) {
		if (bright)
			value += 8;
		return windows_window_color_integer_(value, forep);
	}

	/* not */
	Return(terme_color_not_symbol_(ptr, x, &value, &bright));
	if (0 <= value) {
		if (! bright)
			value += 8;
		return windows_window_color_integer_(value, forep);
	}

	/* dark */
	Return(terme_color_dark_(x, &value));
	if (0 <= value)
		return windows_window_color_integer_(value, forep);

	/* bright */
	Return(terme_color_bright_(x, &value));
	if (0 <= value)
		return windows_window_color_integer_(value + 8, forep);

	/* error */
	return fmte_("Invalid argument, ~S.", x, NULL);
}

static int windows_values_color_(Execute ptr, addr args, int forep)
{
	/* (terme 'windows 'color1 x &optional y z) */
	addr x, y, z;

	x = y = z = Nil;
	Return_getcons(args, &x, &args);
	if (args != Nil) {
		Return_getcons(args, &y, &args);
		if (args != Nil) {
			Return_getcons(args, &z, &args);
			if (args != Nil)
				return fmte_("Invalid arguments, ~S.", args, NULL);
		}
	}

	/* integer */
	if (y == Nil && z == Nil) {
		if (integerp(x))
			return windows_values_color_integer_(x, forep);
		else
			return windows_values_color_symbol_(ptr, x, forep);
	}
	if (integerp(x) && integerp(y) && integerp(z))
		return windows_values_color_rgb_(x, y, z, forep);

	return fmte_("Invalid arguments, ~S, ~S, ~S.", x, y, z, NULL);
}

static int windows_values_color1_(Execute ptr, addr args)
{
	return windows_values_color_(ptr, args, 1);
}

static int windows_values_color2_(Execute ptr, addr args)
{
	return windows_values_color_(ptr, args, 0);
}

int windows_values_(Execute ptr, addr var, addr args, int *ret)
{
	int check;

	/* windows */
	Return(string_designator_equalp_char_(var, "windows", &check));
	if (! check)
		return Result(ret, 0);
	Return_getcons(args, &var, &args);
	*ret = 1;

	/* :show */
	Return(string_designator_equalp_char_(var, "show", &check));
	if (check)
		return windows_values_show_(ptr, args);

	/* :size */
	Return(string_designator_equalp_char_(var, "size", &check));
	if (check)
		return windows_values_size_(ptr, args);

	/* :font */
	Return(string_designator_equalp_char_(var, "font", &check));
	if (check)
		return windows_values_font_(ptr, args);

	/* :color1 */
	Return(string_designator_equalp_char_(var, "color1", &check));
	if (check)
		return windows_values_color1_(ptr, args);

	/* :color2 */
	Return(string_designator_equalp_char_(var, "color2", &check));
	if (check)
		return windows_values_color2_(ptr, args);

	/* error */
	return Result(ret, 0);
}

