#include "gc_check.h"
#include "info.h"
#include "heap.h"
#include "heap_memory.h"

/* reference */
static int heap_check_object(addr pos)
{
	if (! valid_header(pos)) {
		info("HEAP-CHECK: ERROR");
		info("HEAP-CHECK: Invalid type %02X.", GetType(pos));
		return 1;
	}

	return 0;
}

static int heap_check_reference_heap(void)
{
	struct heap_addr *str;

	str = (struct heap_addr *)heap_tail;
	for (; LessPointer(str, heap_range); str++) {
		if (heap_check_object(str->pos)) {
			info("HEAP-CHECK: heap object error");
			return 1;
		}
	}

	return 0;
}

static int heap_check_reference_local(Execute ptr)
{
	addr *array;
	struct localcell *cell;
	size_t i, count;

	for (cell = ptr->local->cell; cell; cell = cell->next) {
		array = cell->point;
		count = cell->count;
		for (i = 0; i < count; i++) {
			if (heap_check_object(array[i])) {
				info("HEAP-CHECK: local error");
				return 1;
			}
		}
	}

	return 0;
}

static int heap_check_reference(void)
{
	if (heap_check_reference_heap())
		return 1;
	if (foreach_check_execute(heap_check_reference_local))
		return 1;

	return 0;
}

/* space */
static size_t heap_check_size(addr pos)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPSYSTEM_SPACE1:
			GetSizeSpace1(pos, &size);
			break;

		case LISPSYSTEM_SPACE:
			GetSizeSpace(pos, &size);
			break;

		case LISPSYSTEM_RESERVED:
			GetSizeReserved(pos, &size);
			break;

		default:
			size = getobjectlength(pos);
			break;
	}

	return size;
}


static int heap_check_space(void)
{
	addr pos, next;

	for (pos = heap_root; pos ==  heap_front; pos = next) {
		if (heap_front < pos) {
			info("HEAP-CHECK: heap_front error");
			return 1;
		}
		if (heap_check_object(pos)) {
			return 1;
		}
		next = pos + heap_check_size(pos);
	}

	return 0;
}

_g void heap_check(void)
{
	info("HEAP-CHECK: start.");
	info("HEAP-CHECK: check-reference.");
	if (heap_check_reference()) {
		info("HEAP-CHECK: REFERENCE ERROR.");
		Abort("heap-check error");
		return;
	}
	info("HEAP-CHECK: check-space.");
	if (heap_check_space()) {
		info("HEAP-CHECK: SPACE ERROR.");
		Abort("heap-check error");
		return;
	}
	info("HEAP-CHECK: end.");
}

