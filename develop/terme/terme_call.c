#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/ioctl.h>
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
static void terme_init_handler(int sig)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) {
		terme_x = 0;
		terme_y = 0;
	}
	else {
		terme_x = (int)ws.ws_col;
		terme_y = (int)ws.ws_row;
	}
}

void terme_init(void)
{
	struct sigaction act;

	act.sa_handler = terme_init_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags= SA_RESTART;
	if (sigaction(SIGWINCH, &act, NULL)) {
		Abort("sigaction error.");
	}
}


/*
 *  terme-begin
 */
static int terme_begin_get(struct termios *ret)
{
	if (ret == NULL)
		return 0;
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_begin_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_begin_termios(void)
{
	struct termios v;

	/* backup */
	if (terme_begin_get(&v)) {
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

int terme_begin(void)
{
	/* terminal size */
	if (terme_getsize())
		return 1;

	/* termios */
	if (terme_begin_termios())
		return 1;

	/* switch */
	terme_switch_textmode_p = 1;

	/* output */
	terme_input_init();
	terme_output_init();

	return 0;
}

int terme_end(void)
{
	return terme_begin_set(&terme_textmode_termios);
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
	if (terme_begin_get(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 1;
	return terme_begin_set(&terme_textmode_termios);
}

int terme_switch_rawmode(int *ret)
{
	if (! terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_begin_set(&terme_switch_termios)) {
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

void terme_screen_x(int *ret)
{
	*ret = terme_x;
}

