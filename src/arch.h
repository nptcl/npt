#ifndef __ARCH_HEADER__
#define __ARCH_HEADER__

#include <stdio.h>
#include <stddef.h>
#include "typedef.h"

/* localtime */
_g void nowtime_string(char *ptr, size_t size);

/* readforce */
#ifdef LISP_POSIX
_g int read_posix(int file, void *pos, size_t size, size_t *ret);
_g int readforce_posix(int file, void *pos, size_t size, size_t *ret);
#endif
#ifdef LISP_WINDOWS
#include <windows.h>
_g int read_windows(HANDLE file, void *pos, size_t size, size_t *ret);
_g int readforce_windows(HANDLE file, void *pos, size_t size, size_t *ret);
#endif
_g int read_clang(FILE *file, void *pos, size_t size, size_t *ret);
_g int readforce_clang(FILE *file, void *pos, size_t size, size_t *ret);

/* safe */
_g int multisafe_size(size_t left, size_t right, size_t *result);
_g int plussafe_size(size_t a, size_t b, size_t *result);

#endif

