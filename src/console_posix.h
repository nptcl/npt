#include <sys/ioctl.h>
#include <unistd.h>

static int getwidth_console_check(size_t *ret)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	if (ws.ws_row <= 0)
		return 1;
	*ret = (size_t)ws.ws_col;

	return 0;
}

_g size_t getwidth_console(void)
{
	size_t ret;

	if (getwidth_console_check(&ret))
		return LISP_CONSOLE_DEFAULT_WIDTH;
	else
		return ret;
}

