#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "buffering.h"
#include "heap.h"
#include "typedef.h"

/*
 *  bufcell
 */
struct bufcell_struct {
	size_t size, index;
	byte data[1];
};

#define StructBufcell_Low(x)  ((struct bufcell_struct *)PtrBodyAB(x))
#define GetNextBufcell_Low(x, y)  GetArrayAB((x), 0, (y))
#define SetNextBufcell_Low(x, y)  SetArrayAB((x), 0, (y))

static struct bufcell_struct *struct_bufcell(addr pos)
{
	CheckType(pos, LISPSYSTEM_BUFCELL);
	return StructBufcell_Low(pos);
}

static void getnext_bufcell(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_BUFCELL);
	GetNextBufcell_Low(pos, ret);
}

static void setnext_bufcell(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_BUFCELL);
	SetNextBufcell_Low(pos, value);
}

static void bufcell_heap(addr *ret, size_t size)
{
	addr pos;
	struct bufcell_struct *str;
	size_t alloc;

	/* size */
	alloc = size + (size_t)(((struct bufcell_struct *)NULL)->data);

	/* heap */
	heap_arraybody(&pos, LISPSYSTEM_BUFCELL, 1, alloc);
	str = struct_bufcell(pos);
	str->size = size;
	str->index = 0;
	*ret = pos;
}

static void push_bufcell(addr pos, byte c)
{
	struct bufcell_struct *str;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	str = struct_bufcell(pos);
	Check(str->size <= str->index, "index error");
	str->data[str->index] = c;
	str->index++;
}

static void position_bufcell(addr pos, size_t size, addr *ret)
{
	struct bufcell_struct *str;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	str = struct_bufcell(pos);
	if (str->index <= size) {
		setnext_bufcell(pos, Nil);
		str->index = size;
		*ret = pos;
		return;
	}

	size -= str->index;
	getnext_bufcell(pos, &pos);
	position_bufcell(pos, size, ret);
}


/*
 *  buffering
 */
struct buffering_struct {
	size_t size, index;
};

#ifdef LISP_DEBUG
#define BUFFERING_SIZE		1024
#else
#define BUFFERING_SIZE		8
#endif

#define StructBuffering_Low(x)  ((struct buffering_struct *)PtrBodySS(x))
#define GetRootBuffering_Low(x, y)  GetArraySS((x), 0, (y))
#define SetRootBuffering_Low(x, y)  SetArraySS((x), 0, (y))
#define GetTailBuffering_Low(x, y)  GetArraySS((x), 1, (y))
#define SetTailBuffering_Low(x, y)  SetArraySS((x), 1, (y))

static struct buffering_struct *struct_buffering(addr pos)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	return StructBuffering_Low(pos);
}

static void getroot_buffering(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	GetRootBuffering_Low(pos, ret);
}

static void setroot_buffering(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	SetRootBuffering_Low(pos, value);
}

static void gettail_buffering(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	GetTailBuffering_Low(pos, ret);
}

static void settail_buffering(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_BUFFERING);
	SetTailBuffering_Low(pos, value);
}

_g int bufferingp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_BUFFERING;
}

_g void buffering_heap(addr *ret, size_t size)
{
	addr pos;
	struct buffering_struct *str;

	if (size == 0)
		size = BUFFERING_SIZE;
	heap_smallsize(&pos, LISPSYSTEM_BUFFERING, 2, sizeoft(struct buffering_struct));
	str = struct_buffering(pos);
	str->size = size;
	str->index = 0;
	*ret = pos;
}

_g void clear_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = 0;
	setroot_buffering(pos, Nil);
	settail_buffering(pos, Nil);
}

static void new_buffering(addr pos)
{
	struct buffering_struct *str;
	addr cell, tail;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	bufcell_heap(&cell, str->size);
	gettail_buffering(pos, &tail);
	if (tail == Nil)
		setroot_buffering(pos, cell);
	else
		setnext_bufcell(tail, cell);
	settail_buffering(pos, cell);
}

_g void push_buffering(addr pos, byte c)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (str->index % str->size == 0)
		new_buffering(pos);
	gettail_buffering(pos, &pos);
	push_bufcell(pos, c);
	str->index++;
}

_g void position_buffering(addr pos, size_t value, int *ret)
{
	struct buffering_struct *str;
	addr x;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (str->index < value) {
		*ret = 1;  /* error */
		return;
	}
	if (str->index == value) {
		*ret = 0;  /* current position */
		return;
	}
	if (value == 0) {
		str->index = 0;  /* all clear */
		setroot_buffering(pos, Nil);
		settail_buffering(pos, Nil);
		return;
	}

	/* set position */
	getroot_buffering(pos, &x);
	position_bufcell(x, value, &x);
	settail_buffering(pos, x);
	*ret = 0; /* ok */
}

_g void get_length_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->index;
}

static int write_buffering_(addr array, addr pos, size_t bias, size_t size)
{
	byte c, *data;
	struct bufcell_struct *cell;
	size_t i;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	cell = struct_bufcell(pos);
	data = cell->data;
	for (i = 0; i < size; i++) {
		c = data[i];
		Return(array_set_unsigned8_(array, bias + i, c));
	}

	return 0;
}

static int make_array_buffering_(addr *ret, size_t size)
{
	addr pos;
	struct array_struct *str;

	Return(array_heap_(&pos, 1, size));
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	Return(array_build_(pos));

	return Result(ret, pos);
}

_g int get_buffering_heap_(addr pos, addr *ret)
{
	struct buffering_struct *str;
	struct bufcell_struct *cell;
	addr array;
	size_t i;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	Return(make_array_buffering_(&array, str->index));

	getroot_buffering(pos, &pos);
	for (i = 0; pos != Nil; i += cell->index) {
		CheckType(pos, LISPSYSTEM_BUFCELL);
		cell = struct_bufcell(pos);
		Return(write_buffering_(array, pos, i, cell->index));
		getnext_bufcell(pos, &pos);
	}

	return Result(ret, array);
}

