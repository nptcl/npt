#ifndef __PROCESS_ED_HEADER__
#define __PROCESS_ED_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#if defined(LISP_POSIX)
#define LISP_ED_PROCESS_DEFAULT  "vi"
#elif defined(LISP_WINDOWS)
#define LISP_ED_PROCESS_DEFAULT  "notepad.exe"
#else
#define LISP_ED_PROCESS_DEFAULT  "ed"
#endif

#define ed_process_ _n(ed_process_)
int ed_process_(Execute ptr, addr file);

#endif

