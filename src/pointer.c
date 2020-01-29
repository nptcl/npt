#include <string.h>
#include "build.h"
#include "pointer.h"

_g void clear_pointer(void)
{
#ifdef LISP_DEBUG
	int p;

	memset(pointer_table, 0, sizeoft(pointer_table));
	for (p = 0; p < SizePointer; p++) {
		SetPointer(p, error, NULL);
	}
#endif
}

