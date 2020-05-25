#include "execute.h"
#include "gc.h"
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
static void checkallarray(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		if (GetStatusGc(pos)) {
			infoprint(pos);
		}
	}
}

static void checkallobject_heap(struct heapinfo *info)
{
	struct heapcell *cell;

	for (cell = info->root; cell; cell = cell->next)
		checkallarray(cell->point, cell->count);
}

static void checkallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		checkallarray(cell->point, cell->count);
}

_g void checkallobject_debug(void)
{
	foreach_heap(checkallobject_heap);
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

	if (pos == Unbound) return;
	Check(pos == NULL, "null error");
	Check(pos[0] == 0xAA, "memory 0xAA error");
	Check(! IsObject(pos), "type error");
	if (GetStatusGc(pos)) return;

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
			if (open_stream_p(pos))
				close_stream(pos);
			break;

		default:
			break;
	}
}


/*
 *  replacespace
 */
static void replacememory_small(struct heapinfo *str)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = str->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				SetStatusValue(pos, LISPSTATUS_GC, 0);
			}
			else {
				freegcobject_type(pos);
				size = getobjectlength(pos);
				makereserved(pos, size);
				/* statistics */
				Check(heap_object < size, "heap_object error");
				Check(heap_count == 0, "heap_count error");
				heap_object -= size;
				heap_count--;
			}
		}
	}
}

static void replacememory_large(struct heapinfo *str)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = str->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				SetStatusValue(pos, LISPSTATUS_GC, 0);
			}
			else {
				freegcobject_type(pos);
				size = getobjectlength(pos);
				makespace(pos, size);
				array[i] = Unbound;
				/* statistics */
				Check(heap_object < size, "heap_object error");
				Check(heap_count == 0, "heap_count error");
				heap_object -= size;
				heap_count--;
			}
		}
	}
}

#define nextcheck(left, xindex, xarray) { \
	if (HeapCount <= xindex) { \
		left = left->next; \
		xindex = 0; \
		xarray = left->point; \
	} \
}
static struct heapcell *replacetable(struct heapinfo *info, int (*call)(addr))
{
	struct heapcell *left, *right;
	size_t xindex, yindex, ycount;
	addr *xarray, *yarray, pos, swap;
	int check;

	/* first left */
	left = info->root;
	if (left == NULL) return NULL;
	check = 0;
	xindex = 0;
	xarray = left->point;

	/* loop */
	for (right = left; right; right = right->next) {
		ycount = right->count;
		yarray = right->point;
		for (yindex = 0; yindex < ycount; yindex++) {
			pos = yarray[yindex];
			if (check == 0) {
				if (call(pos)) {
					check = 1;
				}
				else {
					nextcheck(left, xindex, xarray);
					xindex++;
				}
			}
			else if (! call(pos)) {
				nextcheck(left, xindex, xarray);
				/* swap */
				swap = xarray[xindex];
				xarray[xindex] = pos;
				yarray[yindex] = swap;
				xindex++;
			}
		}
	}

	/* update */
	if (check == 0) return NULL;
	right = left->next;
	left->next = NULL;
	left->count = xindex;
	info->front = left;

	return right;
}

static int replacetable_small(addr pos)
{
	return GetType(pos) == LISPSYSTEM_RESERVED;
}

static int replacetable_large(addr pos)
{
	return pos == Unbound;
}

static void deletetable_small(struct heapcell *left)
{
	struct heapcell *right;
	addr pos, *array;
	size_t index, size;

	for (; left; left = right) {
		right = left->next;
		array = left->point;
		for (index = 0; index < HeapCount; index++) {
			pos = array[index];
			Check(GetType(pos) != LISPSYSTEM_RESERVED, "type error");
			GetSizeReserved(pos, &size);
			makespace(pos, size);
#ifdef LISP_DEBUG
			array[index] = 0;
#endif
		}
		left->count = 0;
		left->next = NULL;
	}
}

static void deletetable_large(struct heapcell *left)
{
	struct heapcell *right;
#ifdef LISP_DEBUG
	addr pos, *array;
	size_t index;
#endif

	for (; left; left = right) {
		right = left->next;
#ifdef LISP_DEBUG
		array = left->point;
		for (index = 0; index < HeapCount; index++) {
			pos = array[index];
			Check(pos != Unbound, "type error");
			array[index] = 0;
		}
#endif
		left->count = 0;
		left->next = NULL;
	}
}

static void replacespace_heap(struct heapinfo *info)
{
	struct heapcell *cell;

	if (IsClassSmall(info->type)) {
		replacememory_small(info);
		cell = replacetable(info, replacetable_small);
		deletetable_small(cell);
	}
	else {
		replacememory_large(info);
		cell = replacetable(info, replacetable_large);
		deletetable_large(cell);
	}

	cell = info->root;
	if (cell && cell->next == NULL && cell->count == 0)
		info->root = info->front = NULL;
}

static void replacespace(void)
{
	foreach_heap(replacespace_heap);
	cellupdate_heap();
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
static int sizeobject(addr pos, size_t *ret)
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
		if (! sizeobject(heap_pos, &size))
			break;
		heap_pos += size;
	}
}


/*
 *  full
 */
_g void gcexec_full(void)
{
	/*checkallobject_debug();*/
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
 *  gc_chain_delete
 */
static void gc_chain_delete_object(addr pos)
{
	byte *p;
	addr value;
	size_t size, i;

	if (GetStatusDynamic(pos))
		return;
	if (GetStatusGc(pos))
		return;
	if (GetChain(pos))
		return;
	SetStatusValue(pos, LISPSTATUS_GC, 1);
	if (! IsArray(pos))
		return;

	/* recursive */
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &value);
		if (value != Unbound) {
			p = PtrChain(value);
			if (*p != 0xFF)
				(*p)--;
			gc_chain_delete_object(value);
		}
	}
}

static void gc_chain_delete_cell(addr *array, size_t count)
{
	addr pos;
	size_t i;

	for (i = 0; i < count; i++) {
		pos = array[i];
		Check((unsigned)LISPTYPE_SIZE <= (unsigned)pos[0], "type size error");
		Check(! IsObject(pos), "type error");
		gc_chain_delete_object(pos);
	}
}

static void gc_chain_delete_heap(struct heapinfo *info)
{
	struct heapcell *cell;

	for (cell = info->root; cell; cell = cell->next)
		gc_chain_delete_cell(cell->point, cell->count);
}

static void gc_chain_delete(void)
{
	foreach_heap(gc_chain_delete_heap);
}


/*
 *  gc_chain_space
 */
static void gc_chain_space_small(struct heapinfo *str)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = str->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				size = getobjectlength(pos);
				makereserved(pos, size);
				/* statistics */
				Check(heap_object < size, "heap_object error");
				Check(heap_count == 0, "heap_count error");
				heap_object -= size;
				heap_count--;
			}
		}
	}
}

static void gc_chain_space_large(struct heapinfo *str)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = str->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				size = getobjectlength(pos);
				makespace(pos, size);
				array[i] = Unbound;
				/* statistics */
				Check(heap_object < size, "heap_object error");
				Check(heap_count == 0, "heap_count error");
				heap_object -= size;
				heap_count--;
			}
		}
	}
}

static void gc_chain_space_heap(struct heapinfo *info)
{
	struct heapcell *cell;

	if (IsClassSmall(info->type)) {
		gc_chain_space_small(info);
		cell = replacetable(info, replacetable_small);
		deletetable_small(cell);
	}
	else {
		gc_chain_space_large(info);
		cell = replacetable(info, replacetable_large);
		deletetable_large(cell);
	}

	cell = info->root;
	if (cell && cell->next == NULL && cell->count == 0)
		info->root = info->front = NULL;
}

static void gc_chain_space(void)
{
	foreach_heap(gc_chain_space_heap);
	cellupdate_heap();
}


/*
 *  partial
 */
_g void gcexec_partial(void)
{
	gcexec_full();
	return;
	/* TODO */
	gc_chain_delete();
	gc_chain_space();
	moveheappos();
	heap_gc_partial++;
}

