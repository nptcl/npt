/*
 *  Garbage Collection
 */
#include "heap.h"
#include "info.h"
#include "gc.h"
#include "memory.h"
#include "stream.h"

#define IsObjectValue(x)    ((x) < LISPSYSTEM_SPACE)
#define IsObject(x)         (IsObjectValue(GetType(x)))

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
		SetStatusValue(pos, LISPSTATUS_GC, 1);
	}
}

static void setallobject_heap(struct heapinfo *info)
{
	struct heapcell *cell;

	for (cell = info->root; cell; cell = cell->next)
		setallarray(cell->point, cell->count);
}

static void setallobject_local(Execute ptr)
{
	struct localcell *cell;

	for (cell = ptr->local->cell; cell; cell = cell->next)
		setallarray(cell->point, cell->count);
}

static void setallobject(void)
{
	foreach_heap(setallobject_heap);
	foreach_execute(setallobject_local);
}


/*
 *  walkthrough
 */
static void resetrecursive(addr pos)
{
	size_t i, size;
	addr array;

	if (pos == Unbound) return;
	Check(! IsObject(pos), "type error");
	if (! GetStatusGc(pos)) return;

	SetStatusValue(pos, LISPSTATUS_GC, 0);
	if (IsArray(pos)) {
		lenarray(pos, &size);
		for (i = 0; i < size; i++) {
			getarray(pos, i, &array);
			resetrecursive(array);
		}
	}
}

static void walkthrough_heap(void)
{
	size_t i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		resetrecursive(lisp_root[i]);
}

static void walkthrough_local(Execute ptr)
{
	addr pos;
	pos = ptr->control;
	if (pos) resetrecursive(pos);
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
			close_stream(pos);
			break;

		default:
			break;
	}
}

static void freegcobject_heap(struct heapinfo *info)
{
	struct heapcell *cell;
	size_t i, count;
	addr pos, *array;

	for (cell = info->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				freegcobject_type(pos);
			}
		}
	}
}

static void freegcobject(void)
{
	foreach_heap(freegcobject_heap);
}


/*
 *  replacespace
 */
static void replacememory_small(struct heapinfo *info)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = info->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				size = getobjectlength(pos);
				makereserved(pos, size);
			}
		}
	}
}

static void replacememory_large(struct heapinfo *info)
{
	struct heapcell *cell;
	size_t i, count, size;
	addr pos, *array;

	for (cell = info->root; cell; cell = cell->next) {
		count = cell->count;
		array = cell->point;
		for (i = 0; i < count; i++) {
			pos = array[i];
			Check(! IsObject(pos), "type error");
			if (GetStatusGc(pos)) {
				size = getobjectlength(pos);
				makespace(pos, size);
				array[i] = Unbound;
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
 *  mergespace
 */
int sizeobject(addr pos, size_t *ret)
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

static void mergespace(void)
{
	addr pos, next, first;
	size_t size;
	int check, poscheck;

	first = NULL;
	check = poscheck = 0;
	for (pos = heap_root; pos < heap_front; pos = next) {
		check = sizeobject(pos, &size);
		next = pos + size;
		if (check) {
			/* object or reserved */
			if (first && (first != pos))
				makespace(first, (size_t)(pos - first));
			first = next;
		}
		else if (poscheck == 0) {
			/* space */
			heap_pos = pos;
			poscheck = 1;
		}
	}
	if (first && check == 0) {
		/* space */
		heap_front = first;
	}
	if (first == NULL) {
		/* no object */
		heap_front = heap_root;
	}
	if (poscheck == 0) {
		/* no object, no space */
		heap_pos = heap_front;
	}
}


/*
 *  interface
 */
void gcexec(void)
{
	setallobject();
	walkthrough();
	freegcobject();
	replacespace();
	mergespace();
}

void gcsync(Execute ptr)
{
	gcstart_execute(ptr);
	if (ptr->index == 0) {
		gcexec();
		gcend_execute();
	}
	else {
		gcwait_execute(ptr);
	}
}


/*
 *  heap-check
 */

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

static int heap_check_reference_direct(struct heapinfo *ptr)
{
	addr *array;
	struct heapcell *cell;
	size_t i, count;

	for (cell = ptr->root; cell; cell = cell->next) {
		array = cell->point;
		count = cell->count;
		for (i = 0; i < HeapCount; i++) {
			if (i < count) {
				if (heap_check_object(array[i])) {
					info("HEAP-CHECK: heap object error");
					return 1;
				}
			}
			else {
				if (array[i] != Unbound) {
					info("HEAP-CHECK: heap unbound error");
					return 1;
				}
			}
		}
	}

	return 0;
}

static int heap_check_reference_table(struct heapinfo *ptr)
{
	addr *array;
	struct heapcell *cell;
	size_t i, count;

	for (cell = ptr->root; cell; cell = cell->next) {
		array = cell->point;
		count = cell->count;
		for (i = 0; i < HeapCount; i++) {
			if (i < count) {
				if (heap_check_object(array[i])) {
					info("HEAP-CHECK: heap object error");
					return 1;
				}
			}
			else {
				if (GetType(array[i]) != LISPSYSTEM_RESERVED) {
					info("HEAP-CHECK: heap reserved error");
					return 1;
				}
			}
		}
	}

	return 0;
}

static int heap_check_reference_heap(struct heapinfo *ptr)
{
	if (ptr->direct)
		return heap_check_reference_direct(ptr);
	else
		return heap_check_reference_table(ptr);
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
	if (foreach_check_heap(heap_check_reference_heap)) return 1;
	if (foreach_check_execute(heap_check_reference_local)) return 1;
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

void heap_check(void)
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

