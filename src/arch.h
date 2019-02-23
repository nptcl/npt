#ifndef __ARCH_HEADER__
#define __ARCH_HEADER__

#include <stdio.h>
#include <stddef.h>
#include "typedef.h"

/* localtime */
void nowtime_string(char *ptr, size_t size);

/* readforce */
#ifdef LISP_POSIX
int read_posix(int file, void *pos, size_t size, size_t *ret);
int readforce_posix(int file, void *pos, size_t size, size_t *ret);
#endif
#ifdef LISP_WINDOWS
#include <windows.h>
int read_windows(HANDLE file, void *pos, size_t size, size_t *ret);
int readforce_windows(HANDLE file, void *pos, size_t size, size_t *ret);
#endif
int read_clang(FILE *file, void *pos, size_t size, size_t *ret);
int readforce_clang(FILE *file, void *pos, size_t size, size_t *ret);

#endif

