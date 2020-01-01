#include <stdio.h>
#include "arch.h"
#include "build.h"
#include "c99.h"
#include "condition.h"
#include "define.h"

/*
 *  readforce arch
 */
#ifdef LISP_POSIX
#include <unistd.h>

/* Posix read */
_g int read_posix(int file, void *pos, size_t size, size_t *ret)
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

_g int readforce_posix(int file, void *ptr, size_t size, size_t *ret)
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
_g int read_windows(HANDLE file, void *pos, size_t size, size_t *ret)
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

_g int readforce_windows(HANDLE file, void *ptr, size_t size, size_t *ret)
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
_g int read_clang(FILE *file, void *pos, size_t size, size_t *ret)
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

_g int readforce_clang(FILE *file, void *ptr, size_t size, size_t *ret)
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
_g int multisafe_size(size_t left, size_t right, size_t *result)
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

_g int plussafe_size(size_t a, size_t b, size_t *result)
{
	if (a > SIZE_MAX - b)
		return 1;
	*result = a + b;

	return 0;
}

