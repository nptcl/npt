#include "arch.h"
#include "terme_arch.h"
#include "typedef.h"

#if defined(LISP_TERME_UNIX)
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#elif defined(LISP_TERME_WINDOWS)
#include "windows_terme.h"
#endif

static unsigned terme_arch_x;
static unsigned terme_arch_y;
static int terme_arch_textmode_p;

/*
 *  terme-init
 */
#if defined(LISP_TERME_UNIX)
static void terme_init_handler(int sig)
{
	if (getwidth_arch(&terme_arch_x, &terme_arch_y)) {
		terme_arch_x = 0;
		terme_arch_y = 0;
	}
}

int terme_arch_init(void)
{
	struct sigaction act;

	act.sa_handler = terme_init_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_RESTART;
	if (sigaction(SIGWINCH, &act, NULL))
		return 0;

	return 0;
}
#elif defined(LISP_TERME_WINDOWS)
int terme_arch_init(void)
{
	return terme_windows_init();
}
#else
int terme_arch_init(void)
{
	terme_arch_x = 0;
	terme_arch_y = 0;
	return 0;
}
#endif


/*
 *  window size
 */
#if defined(LISP_TERME_UNIX)
int terme_arch_size_update(void)
{
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
}
#elif defined(LISP_TERME_WINDOWS)
int terme_arch_size_update(void)
{
	return getwidth_arch(&terme_arch_x, &terme_arch_y);
}
#else
int terme_arch_size_update(void)
{
	terme_arch_x = 0;
	terme_arch_y = 0;
	return 0;
}
#endif

void terme_arch_size_get(unsigned *ret_x, unsigned *ret_y)
{
	if (ret_x)
		*ret_x = terme_arch_x;
	if (ret_y)
		*ret_y = terme_arch_y;
}


/*
 *  terme-begin
 */
#if defined(LISP_TERME_UNIX)
static struct termios terme_arch_textmode_v;
static struct termios terme_arch_switch_v;

static int terme_arch_get(struct termios *ret)
{
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_arch_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_arch_termios(void)
{
	struct termios v;

	/* backup */
	if (terme_arch_get(&v)) {
		fprintf(stderr, "tcgetattr value error\n");
		return 1;
	}
	terme_arch_textmode_v = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	terme_arch_switch_v = v;

	return 0;
}

int terme_arch_begin(void)
{
	/* terminal size */
	if (terme_arch_size_update())
		return 1;

	/* termios */
	if (terme_arch_termios())
		return 1;

	/* switch */
	terme_arch_textmode_p = 1;

	return 0;
}

int terme_arch_end(void)
{
	return terme_arch_set(&terme_arch_textmode_v);
}

#elif defined(LISP_TERME_WINDOWS)
int terme_arch_begin(void)
{
	terme_arch_textmode_p = 1;
	return terme_windows_begin();
}

int terme_arch_end(void)
{
	return terme_windows_end();
}

#else
int terme_arch_begin(void)
{
	terme_arch_textmode_p = 1;
	return 0;
}

int terme_arch_end(void)
{
	return 0;
}
#endif


/*
 *  terme-switch
 */
#if defined(LISP_TERME_UNIX)
int terme_arch_textmode(int *ret)
{
	if (terme_arch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_arch_get(&terme_arch_switch_v)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_arch_textmode_p = 1;
	return terme_arch_set(&terme_arch_textmode_v);
}

int terme_arch_rawmode(int *ret)
{
	if (! terme_arch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_arch_set(&terme_arch_switch_v)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_arch_textmode_p = 0;
	memset(&terme_arch_switch_v, '\0', sizeof(terme_arch_switch_v));
	return 0;
}

#elif defined(LISP_TERME_WINDOWS)
int terme_arch_textmode(int *ret)
{
	if (terme_arch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_windows_textmode()) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_arch_textmode_p = 1;
	return 0;
}

int terme_arch_rawmode(int *ret)
{
	if (! terme_arch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_windows_rawmode()) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_arch_textmode_p = 0;
	return 0;
}

#else
int terme_arch_textmode(int *ret)
{
	terme_arch_textmode_p = 1;
	if (ret)
		*ret = 0;
	return 0;
}

int terme_arch_rawmode(int *ret)
{
	terme_arch_textmode_p = 0;
	if (ret)
		*ret = 0;
	return 0;
}
#endif


/*
 *  terme-build
 */
#ifdef LISP_TERME_WINDOWS
void terme_arch_build(void)
{
	terme_windows_build();
}

#else
void terme_arch_build(void)
{
}
#endif


/*
 *  input / output
 */
#if defined(LISP_TERME_UNIX)
int terme_arch_select(int *ret)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = 0;
	tm.tv_usec = 0;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti < 0) {
		*ret = 0;
		return 1; /* error */
	}
	if (reti == 0) {
		/* empty */
		*ret = 0;
		return 0;
	}
	else {
		/* can read */
		*ret = 1;
		return 0;
	}
}

int terme_arch_wait(void)
{
	int fd, reti;
	fd_set fdset;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	reti = select(fd + 1, &fdset, NULL, NULL, NULL);
	return reti < 0;
}

int terme_arch_read(void *data, size_t size, size_t *ret)
{
	ssize_t check;

	check = read(STDIN_FILENO, data, size);
	if (check < 0)
		return -1;
	*ret = (size_t)check;

	return 0;
}

int terme_arch_write(const void *data, size_t size, size_t *ret)
{
	ssize_t check;

	check = write(STDOUT_FILENO, data, size);
	if (check < 0)
		return -1;
	*ret = (size_t)check;

	return 0;
}

#elif defined(LISP_TERME_WINDOWS)
int terme_arch_select(int *ret)
{
	return terme_windows_select(ret);
}

int terme_arch_wait(void)
{
	return terme_windows_wait();
}

int terme_arch_read(void *data, size_t size, size_t *ret)
{
	return terme_windows_read(data, size, ret);
}

int terme_arch_write(const void *data, size_t size, size_t *ret)
{
	return terme_windows_write(data, size, ret);
}

#else
int terme_arch_select(int *ret)
{
	*ret = 0;
	return 0;
}

int terme_arch_wait(void)
{
	return 0;
}

int terme_arch_read(void *data, size_t size, size_t *ret)
{
	*ret = 0;
	return 1;
}

int terme_arch_write(const void *data, size_t size, size_t *ret)
{
	*ret = 0;
	return 1;
}
#endif


/*
 *  Ctrl + Z
 */
#ifdef LISP_TERME_UNIX
int terme_arch_terminal_stop_(void)
{
	return kill(getpid(), SIGTSTP);
}

#else

int terme_arch_terminal_stop_(void)
{
	return 0;
}
#endif

