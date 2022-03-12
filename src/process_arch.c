#include "execute.h"
#include "process_arch.h"
#include "typedef.h"

#if defined(LISP_UNIX) && defined(LISP_DYNAMIC_LINK)
#include "process_unix.h"
#elif defined(LISP_WINDOWS)
#include "process_windows.h"
#else
#include "condition.h"
int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	return fmte_("This implementation does not support RUN-PROGRAM.", NULL);
}

int dlfile_check_arch_(addr pos, addr *ret, int *openp)
{
	*ret = 0;
	*openp = 0;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	return fmte_("This implementation does not support DLCALL.", NULL);
}
#endif

