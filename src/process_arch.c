#include "execute.h"
#include "process_arch.h"
#include "typedef.h"

#if defined(LISP_POSIX)
#include "process_unix.h"
#elif defined(LISP_WINDOWS)
#include "process_windows.h"
#else
#include "condition.h"
_g int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	return fmte_("This implementation does not support RUN-PROGRAM.", NULL);
}
#endif

