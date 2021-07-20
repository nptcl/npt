#include "condition.h"
#include "define.h"
#include "control_object.h"
#include "terme_values.h"

#ifdef LISP_TERME_WINDOWS
#include "windows_values.h"
#endif

int terme_values_(Execute ptr, addr var, addr args)
{
#ifdef LISP_TERME_WINDOWS
	int check;

	Return(windows_values_(ptr, var, args, &check));
	if (check)
		return 0;
#endif
	return fmte_("Invalid keyword ~S.", var, NULL);
}

