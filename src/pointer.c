#include <string.h>
#include "build.h"
#include "pointer.h"

struct callbind_struct pointer_table[p_size];

void clear_pointer(void)
{
#ifdef LISP_DEBUG
	int p;

	memset(pointer_table, 0, sizeoft(pointer_table));
	for (p = 0; p < p_size; p++) {
		SetPointer(p, error, NULL);
	}
#endif
}

