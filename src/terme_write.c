#include "terme_display.h"
#include "terme_error.h"
#include "terme_escape.h"
#include "terme_output.h"
#include "terme_write.h"
#include "typedef.h"

int terme_write_flush_(void)
{
	if (terme_finish_output())
		return terme_fmte_("terme_finish_output error.", NULL);

	return 0;
}

int terme_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode)
{
	/* display */
	Return(terme_display_write_char_(ptr, c, width, mode));

	/* terminal */
	if (terme_write_char(c, width))
		return terme_fmte_("terme_write_char error.", NULL);

	return 0;
}

int terme_write_terpri_(Execute ptr)
{
	/* display */
	Return(terme_display_terpri_(ptr));

	/* terminal */
	if (terme_terpri())
		return terme_fmte_("terme_tepri error.", NULL);

	return 0;
}

int terme_write_delete_line_right_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_line_right_(ptr));

	/* terminal */
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);

	return 0;
}

int terme_write_left_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_left_(ptr, n));

	/* terminal */
	if (terme_cursor_left(n))
		return terme_fmte_("terme_cursor_left error.", NULL);

	return 0;
}

int terme_write_right_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_right_(ptr, n));

	/* terminal */
	if (terme_cursor_right(n))
		return terme_fmte_("terme_cursor_right error.", NULL);

	return 0;
}

int terme_write_up_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_up_(ptr, n));

	/* terminal */
	if (terme_cursor_up(n))
		return terme_fmte_("terme_cursor_up error.", NULL);

	return 0;
}

int terme_write_down_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_down_(ptr, n));

	/* terminal */
	if (terme_cursor_down(n))
		return terme_fmte_("terme_cursor_down error.", NULL);

	return 0;
}

int terme_write_first_up_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_first_up_(ptr, n));

	/* terminal */
	if (terme_cursor_first_up(n))
		return terme_fmte_("terme_cursor_first_up error.", NULL);

	return 0;
}

int terme_write_first_down_(Execute ptr, int n)
{
	/* display */
	Return(terme_display_first_down_(ptr, n));

	/* terminal */
	if (terme_cursor_first_down(n))
		return terme_fmte_("terme_cursor_first_down error.", NULL);

	return 0;
}

int terme_write_delete_line_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_line_(ptr));

	/* terminal */
	if (terme_cursor_delete_line())
		return terme_fmte_("terme_cursor_delete_line error.", NULL);

	return 0;
}

int terme_write_delete_page_(Execute ptr)
{
	/* display */
	Return(terme_display_delete_page_(ptr));

	/* terminal */
	if (terme_cursor_delete_page())
		return terme_fmte_("terme_cursor_delete_page error.", NULL);

	return 0;
}

