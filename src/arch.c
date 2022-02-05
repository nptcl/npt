#include <stdio.h>
#include <stdlib.h>
#include "arch.h"
#include "build.h"
#include "c99.h"
#include "condition.h"
#include "define.h"

/*
 *  readforce arch
 */
#ifdef LISP_UNIX
#include <unistd.h>

/* Unix read */
int read_unix(int file, void *pos, size_t size, size_t *ret)
{
	ssize_t check;

	check = read(file, pos, size);
	if (check < 0) {
		*ret = 0;
		return check;
	}
	if (check == 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)check;

	return 0;
}

int readforce_unix(int file, void *ptr, size_t size, size_t *ret)
{
	ssize_t check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_unix(file, (void *)pos, diff, &rsize);
		/* Error */
		if (check < 0) {
			*ret = 0;
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) {
				*ret = 0;
				return check;
			}
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}
#endif

#ifdef LISP_WINDOWS
/* Windows ReadFile */
int read_windows(HANDLE file, void *pos, size_t size, size_t *ret)
{
	BOOL check;
	DWORD dsize;

	Check(0xFFFFFFFFULL < size, "size error");
	check = ReadFile(file, (LPVOID)pos, (DWORD)size, &dsize, NULL);
	if (check == 0) {
		*ret = 0;
		return -1;
	}
	if (dsize == 0) {
		*ret = 0;
		return 1;
	}
	*ret = (size_t)dsize;

	return 0;
}

int readforce_windows(HANDLE file, void *ptr, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		if (0xFFFFFFFFULL < diff)
			diff = 0xFFFFFFFFULL;
		check = read_windows(file, (void *)pos, diff, &rsize);
		/* Error */
		if (check < 0) {
			*ret = 0;
			return check;
		}
		/* EOF */
		if (check) {
			if (count == 0) {
				*ret = 0;
				return check;
			}
			break;
		}
		/* Next */
		pos += rsize;
	}
	*ret = count;

	return 0;
}
#endif


/*
 *  safe
 */
int multisafe_size(size_t left, size_t right, size_t *result)
{
	size_t temp;

	if (left == 0 || right == 0) {
		*result = 0;
		return 0;
	}
	temp = left * right;
	if (temp / right < left) {
		*result = 0;
		return 1;
	}
	*result = temp;

	return 0;
}

int plussafe_size(size_t a, size_t b, size_t *result)
{
	if (a > SIZE_MAX - b)
		return 1;
	*result = a + b;

	return 0;
}


/*
 *  arch
 */
#ifdef LISP_TERME_WINDOWS
#include "windows_arch.h"
void exit_arch(int code)
{
	exit_windows(code);
}

void stdout_arch(const char *msg)
{
	stdout_windows(msg);
}

void stderr_arch(const char *msg)
{
	stderr_windows(msg);
}

#else

void exit_arch(int code)
{
	exit(1);
}

void stdout_arch(const char *msg)
{
	(void)printf("%s", msg);
}

void stderr_arch(const char *msg)
{
	(void)fprintf(stderr, "%s", msg);
}
#endif

#if defined(LISP_TERME_WINDOWS)
#include "windows_arch.h"

int getwidth_arch(unsigned *rx, unsigned *ry)
{
	unsigned x, y;

	if (getwidth_windows(&x, &y))
		return 1;
	if (rx)
		*rx = x;
	if (ry)
		*ry = y;

	return 0;
}

#elif defined(LISP_UNIX)
#include <sys/ioctl.h>
#include <unistd.h>

int getwidth_arch(unsigned *rx, unsigned *ry)
{
	unsigned x, y;
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	x = (unsigned)ws.ws_col;
	y = (unsigned)ws.ws_row;
	if (x <= 2)
		x = 2;
	if (y <= 1)
		y = 1;
	if (rx)
		*rx = x;
	if (ry)
		*ry = y;

#if 0
	/* for debug */
	if (rx)
		*rx = 10;
	if (ry)
		*ry = 5;
#endif

	return 0;
}

#else
int getwidth_arch(unsigned *rx, unsigned *ry)
{
	if (rx)
		*rx = 0;
	if (ry)
		*ry = 0;

	return 1;
}
#endif

