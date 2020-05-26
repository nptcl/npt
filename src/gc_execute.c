#include "execute.h"
#include "gc.h"
#include "gc_check.h"
#include "heap.h"
#include "heap_memory.h"
#include "memory.h"
#include "stream.h"
#include "typedef.h"

#define IsObjectValue(x)    ((x) < LISPSYSTEM_SPACE)
#define IsObject(x)         (IsObjectValue(GetType(x)))

/***********************************************************************
 *  Full Garbage Collection
 ***********************************************************************/
/*
 *  checkallobject
 */
static void checkallobject_heap(void)
{
	addr pos;
	struct heap_addr *str;

	str = (struct heap_addr *)heap_tail;
	for (; LessPointer(str, heap_range); str++) {
		pos = str->pos;
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		if (GetStatusGc(pos))
			infoprint(pos);
	}
}

static void checkallarray_loop(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		if (GetStatusGc(pos))
			infoprint(pos);
	}
}

static void checkallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		checkallarray_loop(cell->point, cell->count);
}

_g void checkallobject_debug(void)
{
	checkallobject_heap();
	foreach_execute(checkallobject_local);
}


/*
 *  walkthrough
 */
#define resetrecursive_loop(type) { \
	LenArray##type(pos, &size); \
	for (i = 0; i < size; i++) { \
		GetArray##type(pos, i, &value); \
		resetrecursive(value); \
	} \
}

static void resetrecursive(addr pos)
{
	size_t i, size;
	addr value;

	if (pos == Unbound)
		return;
	Check(pos == NULL, "null error");
	Check(pos[0] == 0xAA, "memory 0xAA error");
	Check(! IsObject(pos), "type error");
	if (GetStatusGc(pos))
		return;

	SetStatusValue(pos, LISPSTATUS_GC, 1);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			resetrecursive_loop(A2);
			break;

		case LISPSIZE_ARRAY4:
			resetrecursive_loop(A4);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			resetrecursive_loop(A8);
			break;
#endif

		case LISPSIZE_SMALLSIZE:
			resetrecursive_loop(SS);
			break;

		case LISPSIZE_ARRAYBODY:
			resetrecursive_loop(AB);
			break;

		default:
			break;
	}
}

static void walkthrough_heap(void)
{
	size_t i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		resetrecursive(lisp_root[i]);
}

static void resetrecursive_local(addr *array, size_t count)
{
	size_t i;

	for (i = 0; i < count; i++)
		resetrecursive(array[i]);
}

static void walkthrough_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		resetrecursive_local(cell->point, cell->count);
}

static void walkthrough(void)
{
	walkthrough_heap();
	foreach_execute(walkthrough_local);
}


/*
 *  freegcobject
 */
static void freegcobject_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_STREAM:
			if (file_stream_p(pos) && open_stream_p(pos))
				close_stream(pos);
			break;

		default:
			break;
	}
}


/*
 *  replacespace
 */
static void replacespace(void)
{
	addr pos;
	struct heap_addr *root, *str;
	size_t size;

	root = (struct heap_addr *)heap_tail;
	for (str = root; LessPointer(str, heap_range); str++) {
		pos = str->pos;
		if (GetStatusGc(pos)) {
			SetStatusValue(pos, LISPSTATUS_GC, 0);
		}
		else {
			freegcobject_type(pos);
			size = getobjectlength(pos);
			makespace_heap(pos, size);
			if (root != str)
				memcpy(str, root, sizeoft(struct heap_addr));
			root++;
			/* statistics */
			Check(heap_object < size, "heap_object error");
			Check(heap_count == 0, "heap_count error");
			heap_object -= size;
			heap_count--;
		}
	}
	heap_tail = (addr)root;
}


/*
 *  setallobject
 */
static void setallarray(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		SetStatusValue(pos, LISPSTATUS_GC, 0);
	}
}

static void setallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		setallarray(cell->point, cell->count);
}

static void setallobject(void)
{
	foreach_execute(setallobject_local);
}


/*
 *  moveheappos
 */
static int getmemorylength_break(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPSYSTEM_SPACE1:
			GetSizeSpace1(pos, ret);
			break;

		case LISPSYSTEM_SPACE:
			GetSizeSpace(pos, ret);
			break;

		case LISPSYSTEM_RESERVED:
			GetSizeReserved(pos, ret);
			return 1;

		default:
			*ret = getobjectlength(pos);
			return 1;
	}

	return 0;
}

static void moveheappos(void)
{
	size_t size;

	heap_pos = heap_root;
	while (heap_pos < heap_front) {
		if (! getmemorylength_break(heap_pos, &size))
			break;
		heap_pos += size;
	}
}


/*
 *  full
 */
_g void gcexec_full(void)
{
#ifdef LISP_DEBUG
	checkallobject_debug();
#endif
	walkthrough();
	replacespace();
	setallobject();
	moveheappos();
	heap_gc_full++;
}


/***********************************************************************
 *  Partial Garbage Collection
 ***********************************************************************/
/*
 *  partial
 */
_g void gcexec_partial(void)
{
	gcexec_full();
	return;

	heap_gc_partial++;
}

