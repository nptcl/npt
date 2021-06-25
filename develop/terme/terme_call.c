#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include "terme_call.h"
#include "terme_input.h"
#include "terme_output.h"
#include "typedef.h"

static struct termios terme_textmode_termios;
static struct termios terme_switch_termios;
static int terme_switch_textmode_p;
static int terme_x;
static int terme_y;

/*
 *  terme-init
 */
static int terme_init_get(struct termios *ret)
{
	if (ret == NULL)
		return 0;
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_init_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_init_termios(void)
{
	int fd;
	struct termios v;

	fd = STDIN_FILENO;

	/* backup */
	if (terme_init_get(&v)) {
		fprintf(stderr, "tcgetattr value error\n");
		return 1;
	}
	terme_textmode_termios = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	if (terme_init_set(&v)) {
		fprintf(stderr, "tcsetattr error.\n");
		return 1;
	}
	terme_switch_termios = v;

	return 0;
}

static int terme_getsize(void)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	terme_x = (int)ws.ws_col;
	terme_y = (int)ws.ws_row;

	return 0;
}

int terme_init(void)
{
	/* terminal size */
	if (terme_getsize())
		return 1;

	/* termios */
	if (terme_init_termios())
		return 1;

	/* switch */
	terme_switch_textmode_p = 1;

	/* output */
	terme_input_init();
	terme_output_init();

	return 0;
}

int terme_free(void)
{
	return terme_init_set(&terme_textmode_termios);
}


/*
 *  terme-switch
 */
int terme_switch_textmode(int *ret)
{
	if (terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_init_get(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 1;
	return terme_init_set(&terme_textmode_termios);
}

int terme_switch_rawmode(int *ret)
{
	if (! terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_init_set(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 0;
	memset(&terme_switch_termios, '\0', sizeof(terme_switch_termios));
	return 0;
}

