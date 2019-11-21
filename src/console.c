#include "define.h"
#include "console.h"

#if defined(LISP_POSIX)
#include "console_posix.h"
#else
#include "console_ansi.h"
#endif

