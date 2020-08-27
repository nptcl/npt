#ifndef __EXTERN_INIT_HEADER__
#define __EXTERN_INIT_HEADER__

#include <stdio.h>
#include "define.h"

#ifdef LISP_DEBUG
#define DefaultHeapSize	  (64UL * 1024UL * 1024UL);
#define DefaultLocalSize  (16UL * 1024UL * 1024UL);
#else
#define DefaultHeapSize	  (1024UL * 1024UL * 1024UL);
#define DefaultLocalSize  (256UL * 1024UL * 1024UL);
#endif

_g FILE *lisperror_stream(void);
_g int lisperror_noeol(const char *fmt, ...);
_g int lisperror_va(const char *fmt, va_list args);
_g int lisperror(const char *fmt, ...);

#endif

