#ifndef __ARCH_HEADER__
#define __ARCH_HEADER__

#include <stdio.h>
#include <stddef.h>
#include "typedef.h"

#define read_posix _n(read_posix)
#define readforce_posix _n(readforce_posix)
#define read_windows _n(read_windows)
#define readforce_windows _n(readforce_windows)
#define read_clang _n(read_clang)
#define readforce_clang _n(readforce_clang)
#define multisafe_size _n(multisafe_size)
#define plussafe_size _n(plussafe_size)

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

/* safe */
int multisafe_size(size_t left, size_t right, size_t *result);
int plussafe_size(size_t a, size_t b, size_t *result);

#endif

