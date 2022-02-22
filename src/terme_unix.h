#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/types.h>
#include <sys/time.h>
#include <termios.h>
#include <unistd.h>

#define TERME_UNIX_ESCAPE		9990

/*
 *  terme-init
 */
static int terme_unix_escape_p;
static struct timeval terme_unix_escape_timeval;
static struct termios terme_unix_default_v;

static int terme_unix_get(struct termios *ret)
{
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_unix_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_unix_init(void)
{
	if (terme_unix_get(&terme_unix_default_v)) {
		Debug("terme_unix_get error.");
		return 1;
	}
	terme_unix_escape_p = 0;
	cleartype(terme_unix_escape_timeval);

	return 0;
}


/*
 *  terme-signal
 */
static void terme_unix_handler(int sig)
{
	terme_arch_signal_value = 1;
}

static int terme_unix_signal(void)
{
	struct sigaction act;

	act.sa_handler = terme_unix_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	if (sigaction(SIGWINCH, &act, NULL))
		return 1;

	return 0;
}


/*
 *  terme-begin
 */
static int terme_unix_textmode_init;
static struct termios terme_unix_textmode_v;
static struct termios terme_unix_switch_v;

static int terme_unix_termios(void)
{
	struct termios v;

	/* backup */
	terme_unix_textmode_init = 0;
	if (terme_unix_get(&v)) {
		Debug("terme_unix_get error.");
		return 1;
	}
	terme_unix_textmode_init = 1;
	terme_unix_textmode_v = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	terme_unix_switch_v = v;

	return 0;
}

static int terme_unix_begin(void)
{
	/* terminal size */
	if (terme_arch_size_update())
		return 1;

	/* sigaction */
	if (terme_unix_signal())
		return 1;

	/* termios */
	if (terme_unix_termios())
		return 1;

	return 0;
}

static int terme_unix_end(void)
{
	if (terme_unix_textmode_init)
		return terme_unix_set(&terme_unix_textmode_v);
	return 0;
}


/*
 *  terme-switch
 */
static int terme_unix_textmode(void)
{
	struct termios v;

	if (terme_unix_get(&v))
		return 1;
	if (terme_unix_set(&terme_unix_textmode_v))
		return 1;
	terme_unix_switch_v = v;

	return 0;
}

static int terme_unix_rawmode(void)
{
	if (terme_unix_set(&terme_unix_switch_v))
		return 1;
	cleartype(terme_unix_switch_v);

	return 0;
}


/*
 *  input / output
 */
static int terme_unix_select_value(int *ret, long tv_sec, long tv_usec)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = tv_sec;
	tm.tv_usec = tv_usec;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti == 0) {
		/* timeout */
		*ret = 0;
		return 0;
	}
	if (0 < reti) {
		/* can read */
		*ret = 1;
		return 0;
	}
	if (errno == EINTR) {
		/* signal */
		*ret = 0;
		return -1;
	}
	else {
		/* error */
		*ret = 0;
		return 1;
	}
}

static int terme_unix_select(int *ret)
{
	return terme_unix_select_value(ret, 0, 0);
}

static int terme_unix_wait_integer(int *ret, int value)
{
	if (value <= 0)
		value = 0;
	return terme_unix_select_value(ret, (long)value, 0);
}

static int terme_unix_wait_float(int *ret, double value)
{
	long lx, ly;
	double x, y;

	if (value <= 0.0)
		return terme_unix_select_value(ret, 0, 0);

	/* usec = (expt 10 -6) sec */
	x = trunc(value);
	if (((double)LONG_MAX) < x) {
		*ret = 0;
		return 1;
	}
	value -= x;
	value *= 1.0e6;
	y = trunc(value);
	lx = (long)x;
	ly = (long)y;
	return terme_unix_select_value(ret, lx, ly);
}

static int terme_unix_read(void *data, size_t size, size_t *ret)
{
	ssize_t check;

	check = read(STDIN_FILENO, data, size);
	if (0 <= check) {
		*ret = (size_t)check;
		return 0;
	}
	if (errno == EINTR) {
		*ret = 0;
		return -1;
	}
	else {
		*ret = 0;
		return 1;
	}
}

static int terme_unix_write(const void *data, size_t size, size_t *ret)
{
	ssize_t check;

retry:
	check = write(STDOUT_FILENO, data, size);
	if (check < 0) {
		if (errno == EINTR)
			goto retry;
		return -1;
	}
	*ret = (size_t)check;

	return 0;
}


/*
 *  Ctrl + C
 */
static int terme_unix_terminal_sigint_(void)
{
	return kill(getpid(), SIGINT);
}


/*
 *  Ctrl + Z
 */
static int terme_unix_terminal_stop_(void)
{
	return kill(getpid(), SIGTSTP);
}


/*
 *  escape
 */
static int terme_unix_escape_begin(void)
{
	int check;

	if (terme_unix_escape_p)
		return 0;
	check = gettimeofday(&terme_unix_escape_timeval, NULL);
	if (check)
		return 1;
	terme_unix_escape_p = 1;

	return 0;
}
static int terme_unix_escape_end(int *ret)
{
	int check;
	struct timeval *a, *b, now, diff;

	if (terme_unix_escape_p == 0)
		goto error;
	a = &now;
	b = &terme_unix_escape_timeval;
	check = gettimeofday(a, NULL);
	if (check)
		goto error;

	/* minus */
	if (timercmp(a, b, <))
		goto normal;

	/* diff */
	timersub(a, b, &diff);
	if (diff.tv_sec != 0)
		goto normal;
	if (diff.tv_usec < TERME_UNIX_ESCAPE)
		goto normal;

	/* escape */
	terme_unix_escape_p = 0;
	*ret = 1;
	return 0;

normal:
	terme_unix_escape_p = 0;
	*ret = 0;
	return 0;

error:
	terme_unix_escape_p = 0;
	*ret = 0;
	return 1;
}


/*
 *  call begin-default
 */
static int terme_unix_begin_update_(addr *ret, void (*call)(struct termios *))
{
	addr pos;
	size_t size;
	struct termios v;

	/* flush */
	if (terme_finish_output()) {
		*ret = Nil;
		return fmte_("terme_finish_output error.", NULL);
	}

	/* backup */
	if (terme_unix_get(&v)) {
		*ret = Nil;
		return fmte_("terme_unix_get error.", NULL);
	}
	paper_body_heap(&pos, sizeoft(v));
	paper_set_memory(pos, 0, sizeoft(v), (const void *)&v, &size);
	Check(size != sizeoft(v), "size error");

	/* set terminal */
	(*call)(&v);
	if (terme_unix_set(&v)) {
		*ret = Nil;
		return fmte_("terme_unix_set error.", NULL);
	}

	return Result(ret, pos);
}

static void terme_unix_begin_default_call(struct termios *ptr)
{
	*ptr = terme_unix_default_v;
}

static int terme_unix_begin_default_(addr *ret)
{
	return terme_unix_begin_update_(ret, terme_unix_begin_default_call);
}


/*
 *  call begin-rawmode
 */
static void terme_unix_begin_rawmode_call(struct termios *ptr)
{
	ptr->c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	ptr->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	ptr->c_oflag &= ~OPOST;
	ptr->c_cflag &= ~(CSIZE | PARENB);
	ptr->c_cflag |= CS8;
	ptr->c_cc[VMIN] = 1;
	ptr->c_cc[VTIME] = 0;
}

static int terme_unix_begin_rawmode_(addr *ret)
{
	return terme_unix_begin_update_(ret, terme_unix_begin_rawmode_call);
}


/*
 *  call begin-restore
 */
static int terme_unix_restore_(addr pos)
{
	size_t size;
	struct termios v;

	paper_get_memory(pos, 0, sizeoft(v), (void *)&v, &size);
	Check(size != sizeoft(v), "size error");
	if (terme_unix_set(&v))
		return fmte_("terme_unix_set error.", NULL);

	return 0;
}

