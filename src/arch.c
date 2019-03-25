#include <stdio.h>
#include "arch.h"
#include "build.h"
#include "c99.h"
#include "condition.h"
#include "define.h"


/*
 *  localtime
 */
#ifdef LISP_THREAD_WINDOWS
void nowtime_string(char *ptr, size_t size)
{
	SYSTEMTIME st;
	GetLocalTime(&st);
	snprintf(ptr, size, "%04d/%02d/%02d-%2d:%02d:%02d",
			st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond);
}
#else
#include <time.h>
void nowtime_string(char *ptr, size_t size)
{
	char data[32];
	time_t now;
	struct tm *str;
	size_t ret;

	/* size */
	if (size == 0) return;
	size--;

	/* time */
	now = time(NULL);
	if (now == (time_t)-1)
		fmte("time error", NULL);
	str = localtime(&now);
	ret = strftime(data, 32, "%Y/%m/%d-%H:%M:%S", str);
	size = ret < size? ret: size;
	memcpy(ptr, data, size);
	ptr[size] = 0;
}
#endif


/*
 *  readforce arch
 */
#ifdef LISP_POSIX
#include <unistd.h>

/* Posix read */
int read_posix(int file, void *pos, size_t size, size_t *ret)
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

int readforce_posix(int file, void *ptr, size_t size, size_t *ret)
{
	ssize_t check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_posix(file, (void *)pos, diff, &rsize);
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

/* ANSI-C fread */
int read_clang(FILE *file, void *pos, size_t size, size_t *ret)
{
	size_t check;

	check = fread(pos, 1, size, file);
	if (ferror(file)) {
		*ret = 0;
		return -1;
	}
	if (check == 0 && feof(file)) {
		*ret = 0;
		return 1;
	}
	*ret = check;

	return 0;
}

int readforce_clang(FILE *file, void *ptr, size_t size, size_t *ret)
{
	int check;
	size_t count, rsize, diff;
	unsigned char *pos;

	pos = (unsigned char *)ptr;
	for (count = 0; count < size; count += rsize) {
		diff = size - count;
		check = read_clang(file, (void *)pos, diff, &rsize);
		/* Error */
		if (check < 0) {
			Debug("readcall_arch error");
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

