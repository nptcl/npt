#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "condition.h"
#include "buffering.h"
#include "heap.h"
#include "integer.h"
#include "sequence.h"
#include "typedef.h"

/*
 *  bufcell
 */
#define PtrBufCell(x) ((byte *)PtrBodyB2(x))
static void bufcell_heap(addr *ret, size_t cell)
{
	addr pos;

	Check(0xFFFF < cell, "cell error");
	heap_body2(&pos, LISPSYSTEM_BUFCELL, cell);
	memset(PtrBufCell(pos), '\0', cell);
	*ret = pos;
}

static void get_bufcell(addr pos, size_t m, byte *ret)
{
	byte *data;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	data = PtrBufCell(pos);
	*ret = data[m];
}

static void set_bufcell(addr pos, size_t m, byte c)
{
	byte *data;

	CheckType(pos, LISPSYSTEM_BUFCELL);
	data = PtrBufCell(pos);
	data[m] = c;
}


/*
 *  buffering
 */
struct buffering_struct {
	size_t cell, width, size, index, init_width;
};

#ifdef LISP_DEBUG
#define BUFFERING_CELL		64
#define BUFFERING_WIDTH		4
#else
#define BUFFERING_CELL		4096
#define BUFFERING_WIDTH		8
#endif

#define StructBuffering_Low(x)  ((struct buffering_struct *)PtrBodySS(x))
#define GetRootBuffering_Low(x, y)  GetArraySS((x), 0, (y))
#define SetRootBuffering_Low(x, y)  SetArraySS((x), 0, (y))

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

_g int bufferingp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_BUFFERING;
}

static void buffering_clear_root(addr pos)
{
	addr root;
	struct buffering_struct *str;

	str = struct_buffering(pos);
	vector_heap(&root, str->width);
	setroot_buffering(pos, root);
}

_g void buffering_heap(addr *ret, size_t cell, size_t width)
{
	addr pos;
	struct buffering_struct *str;

	if (cell == 0)
		cell = BUFFERING_CELL;
	if (width == 0)
		width = BUFFERING_WIDTH;
	heap_smallsize(&pos, LISPSYSTEM_BUFFERING, 1, sizeoft(struct buffering_struct));
	str = struct_buffering(pos);
	str->cell = cell;
	str->width = width;
	str->init_width = width;
	str->index = 0;
	str->size = 0;
	buffering_clear_root(pos);
	*ret = pos;
}

_g void clear_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->width = str->init_width;
	str->index = 0;
	str->size = 0;
	buffering_clear_root(pos);
}

_g void getcell_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->cell;
}

_g void getwidth_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->width;
}

static int realloc_buffering(addr pos, size_t n)
{
	struct buffering_struct *str;
	addr dst, src, value;
	size_t width, x, i;

	str = struct_buffering(pos);
	width = str->width;
	if (n < width)
		return 0;

	/* realloc width */
	while (width <= n) {
		x = width;
		width <<= 1ULL;
		if (width <= x)
			return 1; /* overflow */
	}
	str->width = width;

	/* realloc */
	vector_heap(&dst, width);
	getroot_buffering(pos, &src);
	lenarray(src, &width);
	for (i = 0; i < width; i++) {
		getarray(src, i, &value);
		setarray(dst, i, value);
	}
	setroot_buffering(pos, dst);

	return 0;
}

static void new_buffering(addr root, size_t n, size_t cell, addr *ret)
{
	addr pos;
	bufcell_heap(&pos, cell);
	setarray(root, n, pos);
	*ret = pos;
}

_g int putc_buffering(addr pos, byte c)
{
	struct buffering_struct *str;
	addr root, page;
	size_t cell, m, n;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	cell = str->cell;
	n = str->index / cell;
	m = str->index % cell;

	/* page */
	if (realloc_buffering(pos, n))
		return 1;
	getroot_buffering(pos, &root);
	getarray(root, n, &page);
	if (page == Nil)
		new_buffering(root, n, cell, &page);

	/* write */
	set_bufcell(page, m, c);
	str->index++;
	if (str->size < str->index)
		str->size = str->index;

	return 0;
}

static int get_buffering(addr pos, size_t index, byte *ret)
{
	struct buffering_struct *str;
	size_t m, n;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (str->size <= index)
		return 1; /* EOF */

	n = index / str->cell;
	m = index % str->cell;
	getroot_buffering(pos, &pos);
	getarray(pos, n, &pos);
	if (pos == Nil)
		*ret = 0;
	else
		get_bufcell(pos, m, ret);

	return 0;
}

_g int getc_buffering(addr pos, byte *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	if (get_buffering(pos, str->index, ret))
		return 1;

	str->index++;
	if (str->size < str->index)
		str->size = str->index;

	return 0;
}

_g void position_get_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->index;
}

_g void position_set_buffering(addr pos, size_t value)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = value;
}

_g void position_start_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = 0;
}

_g void position_end_buffering(addr pos)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	str->index = str->size;
}

_g void length_buffering(addr pos, size_t *ret)
{
	struct buffering_struct *str;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	*ret = str->size;
}


/*
 *  make vector
 */
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

_g int make_vector_buffering_heap_(addr pos, addr *ret)
{
	byte c;
	struct buffering_struct *str;
	addr array;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_BUFFERING);
	str = struct_buffering(pos);
	size = str->size;
	Return(make_array_buffering_(&array, size));

	for (i = 0; i < size; i++) {
		if (get_buffering(pos, i, &c))
			return fmte_("end-of-file error.", NULL);
		Return(array_set_unsigned8_(array, i, c));
	}

	return Result(ret, array);
}

_g int read_buffering_(addr pos, addr vector)
{
	byte c;
	addr value;
	size_t size, i;

	Return(length_sequence_(vector, 1, &size));
	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, vector, i, &value));
		if (GetByte_integer(value, &c)) {
			return fmte_("The value ~S "
					"must be a (unsigned-byte 8) type.", value, NULL);
		}
		if (putc_buffering(pos, c))
			return fmte_("Too large file size.", NULL);
	}

	return 0;
}

