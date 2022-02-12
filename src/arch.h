#ifndef __ARCH_HEADER__
#define __ARCH_HEADER__

#include <stdio.h>
#include <stddef.h>
#include "typedef.h"

#define read_unix _n(read_unix)
#define readf_unix _n(readf_unix)
#define read_windows _n(read_windows)
#define readf_windows _n(readf_windows)
#define multisafe_size _n(multisafe_size)
#define plussafe_size _n(plussafe_size)

#define exit_arch _n(exit_arch)
#define stdout_arch _n(stdout_arch)
#define stderr_arch _n(stderr_arch)
#define getwidth_arch _n(getwidth_arch)

/* read force */
#ifdef LISP_UNIX
int read_unix(int file, void *pos, size_t size, size_t *ret);
int readf_unix(int file, void *pos, size_t size, size_t *ret);
#endif
#ifdef LISP_WINDOWS
#include <windows.h>
int read_windows(HANDLE file, void *pos, size_t size, size_t *ret);
int readf_windows(HANDLE file, void *pos, size_t size, size_t *ret);
#endif

/* safe */
int multisafe_size(size_t left, size_t right, size_t *result);
int plussafe_size(size_t a, size_t b, size_t *result);

/* arch */
void exit_arch(int code);
void stdout_arch(const char *msg);
void stderr_arch(const char *msg);
int getwidth_arch(unsigned *rx, unsigned *ry);

#endif

