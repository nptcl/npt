#include "files.h"

#if defined(LISP_POSIX)
#include "files_posix.h"
#elif defined(LISP_WINDOWS)
#include "files_windows.h"
#else
#include "files_ansi.h"
#endif

