#include "files.h"

#if defined(LISP_UNIX)
#include "files_unix.h"
#elif defined(LISP_WINDOWS)
#include "files_windows.h"
#else
#include "files_ansi.h"
#endif

