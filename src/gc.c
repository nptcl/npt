#include "execute.h"
#include "execute_values.h"
#include "gc.h"
#include "gc_execute.h"
#include "heap.h"
#include "heap_memory.h"
#include "typedef.h"

#define GCEXEC_DEFAULT			32
static void gcexec_default(void)
{
	if ((heap_gc_count % GCEXEC_DEFAULT) == 0)
		gcexec_full();
	else
		gcexec_partial();
}

_g void gcexec(enum GcMode mode)
{
	switch (mode) {
		case GcMode_Default:
			gcexec_default();
			break;

		case GcMode_Partial:
			gcexec_partial();
			break;

		case GcMode_Full:
			gcexec_full();
			break;

		case GcMode_Off:
		default:
			return;
	}
	heap_gc_count++;
}

_g void gcsync(Execute ptr, enum GcMode mode)
{
	clear_values_execute(ptr);
	gcstart_execute(ptr);
	if (ptr->index == 0) {
		gcexec(mode);
		gcend_execute();
	}
	else {
		gcwait_execute(ptr);
	}
	lisp_gcsync = GcMode_Off;
}

