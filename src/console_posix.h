#include <sys/ioctl.h>
#include <unistd.h>

_g int getwidth_console(size_t *ret)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	if (ws.ws_row <= 0)
		return 1;
	*ret = (size_t)ws.ws_col;

	return 0;
}

