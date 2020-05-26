#include "code_object.h"
#include "file_memory.h"
#include "heap_memory.h"
#include "memory.h"
#include "stream.h"

/*
 *  core
 */
#define IfWriteCheck(fm,p,s,m) IfDebug(writecheck_filememory((fm),(p),(s)),(m))
#define IfWriteAddr(fm,p,m) IfDebug(writeaddr_filememory((fm),(p)),(m))
#define IfWritePtr(fm,p,m) IfDebug(writeptr_filememory((fm),(p)),(m))
#define IfWriteSize(fm,p,m) IfDebug(writesize_filememory((fm),(p)),(m))
#define IfReadCheck(fm,p,s,m) IfDebug(readcheck_filememory((fm),(p),(s)),(m))
#define IfReadAddr(fm,p,m) IfDebug(readaddr_filememory((fm),(p)),(m))
#define IfReadPtr(fm,p,m) IfDebug(readptr_filememory((fm),(p)),(m))
#define IfReadSize(fm,p,m) IfDebug(readsize_filememory((fm),(p)),(m))

/* save/load array2 */
static int writearray(struct filememory *fm, const addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfWriteAddr(fm, array[i], "writeaddr error.");
	}

	return 0;
}
static int readarray(struct filememory *fm, addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfReadAddr(fm, array + i, "readaddr error.");
	}

	return 0;
}

static int save_object_array2(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array2(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array4 */
static int save_object_array4(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error.");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array4(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array8 */
#ifdef LISP_ARCH_64BIT
static int save_object_array8(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 16UL, "writecheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array8(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}
#else
static int save_object_array8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_array8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load smallsize */
static int save_object_smallsize(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_smallsize(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load arraybody */
static int save_object_arraybody(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_arraybody(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load body2 */
static int save_object_body2(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfWriteCheck(fm, pos + 8UL, size, "writecheck error");

	return 0;
}
static int load_object_body2(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfReadCheck(fm, pos + 8UL, size, "readcheck error");

	return 0;
}


/* save/load body4 */
static int save_object_body4(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfWriteCheck(fm, pos + 8UL, 8UL + size, "writecheck error.");

	return 0;
}
static int load_object_body4(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfReadCheck(fm, pos + 8UL, 8UL + size, "readcheck error");

	return 0;
}


/* save/load body8 */
#ifdef LISP_ARCH_64BIT
static int save_object_body8(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB8(pos);
	IfWriteCheck(fm, pos + 8UL, 16UL + size, "writecheck error");

	return 0;
}
static int load_object_body8(struct filememory *fm, addr pos)
{
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	size = (size_t)GetLenBodyB8(pos);
	IfReadCheck(fm, pos + 24UL, size, "readcheck error");

	return 0;
}
#else
static int save_object_body8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_body8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load object */
typedef int (*save_object_call)(struct filememory *, addr);
typedef int (*load_object_call)(struct filememory *, addr);
static save_object_call Heap_SaveObject[LISPSIZE_SIZE];
static load_object_call Heap_LoadObject[LISPSIZE_SIZE];

static int save_object(struct filememory *fm, addr pos, size_t *ret)
{
	unsigned index;

	*ret = getobjectlength(pos);
	IfWriteCheck(fm, pos, 8UL, "writecheck error: save_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	return (Heap_SaveObject[index])(fm, pos);
}
static int load_object(struct filememory *fm, addr pos, size_t *ret)
{
	unsigned index;

	IfReadCheck(fm, pos + 1UL, 7UL, "readcheck error: load_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	if ((Heap_LoadObject[index])(fm, pos)) {
		Debug("Heap_LoadObject error");
		return 1;
	}
	*ret = getobjectlength(pos);

	return 0;
}


/* save/load space1 */
static int save_space1(struct filememory *fm, addr pos, size_t *ret)
{
	GetSizeSpace1(pos, ret);
	IfWriteCheck(fm, pos, 2UL, "writecheck error.");
	return 0;
}
static int load_space1(struct filememory *fm, addr pos, size_t *ret)
{
	IfDebug(getc_filememory(fm, pos + 1), "readcheck error.");
	GetSizeSpace1(pos, ret);
	return 0;
}


/* save/load space */
static int save_space(struct filememory *fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeSpace(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueSpace(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_space(struct filememory *fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeSpace(pos, ret);
	return 0;
}


/* save/load reserved */
static int save_reserved(struct filememory *fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeReserved(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueReserved(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_reserved(struct filememory *fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeReserved(pos, ret);
	return 0;
}


/* save-dump-stream */
static int save_object_stream(struct filememory *fm, addr pos, size_t *ret)
{
	if (save_stream(pos)) {
		Debug("save_stream error");
		return 1;
	}
	if (save_object(fm, pos, ret)) {
		Debug("save_object error");
		return 1;
	}

	return 0;
}


/* save/load dump */
static int save_dump(struct filememory *fm)
{
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		switch (GetType(pos)) {
			case LISPSYSTEM_SPACE1:
				IfDebug(save_space1(fm, pos, &size), "save_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(save_space(fm, pos, &size), "save_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(save_reserved(fm, pos, &size), "save_reserved error.");
				break;

			case LISPTYPE_STREAM:
				IfDebug(save_object_stream(fm, pos, &size), "save_object_stream error.");
				break;

			default:
				IfDebug(save_object(fm, pos, &size), "save_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(putc_filememory(fm, (byte)LISPSYSTEM_END), "putc error.");

	return 0;
}
static int load_dump(struct filememory *fm)
{
	byte c;
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		IfDebug(getc_filememory(fm, pos), "getc error.");
		switch (pos[0]) {
			case LISPSYSTEM_SPACE1:
				IfDebug(load_space1(fm, pos, &size), "load_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(load_space(fm, pos, &size), "load_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(load_reserved(fm, pos, &size), "load_reserved error.");
				break;

			case LISPTYPE_CODE:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				setpointer_code(pos);
				break;

			case LISPSYSTEM_CODE:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				setpointer_code_call(pos);
				break;

			default:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(getc_filememory(fm, &c), "getc error.");
	IfDebug(c != LISPSYSTEM_END, "end error.");

	return 0;
}


/* save/load info */
static int save_info(struct filememory *fm)
{
	struct heap_addr *str;

	str = (struct heap_addr *)heap_range;
	str--;
	while (LessEqualPointer(heap_tail, str)) {
		IfWriteAddr(fm, str->pos, "writeaddr error: heap_addr.");
	}
	IfWriteCheck(fm, NULL, sizeoft(void *), "writecheck: null.");

	return 0;
}

static int load_info(struct filememory *fm)
{
	addr pos;
	struct heap_addr *str;

	for (;;) {
		IfReadAddr(fm, &pos, "readaddr error: heap_addr.");
		if (pos == NULL)
			break;
		/* alloctail */
		str = alloctail();
		str->pos = pos;
	}

	return 0;
}


/* save/load data */
static int save_data(struct filememory *fm)
{
	IfWriteSize(fm, heap_object, "writeptr error: heap_object");
	IfWriteSize(fm, heap_count, "writeptr error: heap_count");
	IfWriteSize(fm, heap_gc_count, "writeptr error: heap_gc_count");
	IfWriteSize(fm, heap_gc_partial, "writeptr error: heap_gc_partial");
	IfWriteSize(fm, heap_gc_full, "writeptr error: heap_gc_full");
	IfWritePtr(fm, heap_front, "writeptr error: heap_front");
	IfWritePtr(fm, heap_pos, "writeptr error: heap_pos");
	IfWritePtr(fm, heap_tail, "writeptr error: heap_tail");
	IfWritePtr(fm, heap_range, "writeptr error: heap_range");
	if (save_dump(fm)) {
		Debug("save_dump error");
		return 1;
	}

	return 0;
}
static int load_data(struct filememory *fm)
{
	IfReadSize(fm, &heap_object, "readptr error: heap_object");
	IfReadSize(fm, &heap_count, "readptr error: heap_count");
	IfReadSize(fm, &heap_gc_count, "readptr error: heap_gc_count");
	IfReadSize(fm, &heap_gc_partial, "readptr error: heap_gc_partial");
	IfReadSize(fm, &heap_gc_full, "readptr error: heap_gc_full");
	IfReadPtr(fm, (void **)&heap_front, "readptr error: heap_front");
	IfReadPtr(fm, (void **)&heap_pos, "readptr error: heap_pos");
	IfReadPtr(fm, (void **)&heap_tail, "readptr error: heap_tail");
	IfReadPtr(fm, (void **)&heap_range, "readptr error: heap_range");
	if (load_dump(fm)) {
		Debug("load_dump error");
		return 1;
	}

	return 0;
}


/* save/load info */
_g int save_heap(struct filememory *fm)
{
	if (save_data(fm)) {
		Debug("save_data error.");
		return 1;
	}
	if (save_info(fm)) {
		Debug("save_info error.");
		return 1;
	}

	return 0;
}
_g int load_heap(struct filememory *fm)
{
	if (load_data(fm)) {
		Debug("load_data error.");
		return 1;
	}
	if (load_info(fm)) {
		Debug("load_info error.");
		return 1;
	}

	return 0;
}


/*
 *  initialize
 */
_g void init_heap_core(void)
{
	/* save-object */
	Heap_SaveObject[LISPSIZE_ARRAY2] = save_object_array2;
	Heap_SaveObject[LISPSIZE_ARRAY4] = save_object_array4;
	Heap_SaveObject[LISPSIZE_ARRAY8] = save_object_array8;
	Heap_SaveObject[LISPSIZE_SMALLSIZE] = save_object_smallsize;
	Heap_SaveObject[LISPSIZE_ARRAYBODY] = save_object_arraybody;
	Heap_SaveObject[LISPSIZE_BODY2] = save_object_body2;
	Heap_SaveObject[LISPSIZE_BODY4] = save_object_body4;
	Heap_SaveObject[LISPSIZE_BODY8] = save_object_body8;
	/* load-object */
	Heap_LoadObject[LISPSIZE_ARRAY2] = load_object_array2;
	Heap_LoadObject[LISPSIZE_ARRAY4] = load_object_array4;
	Heap_LoadObject[LISPSIZE_ARRAY8] = load_object_array8;
	Heap_LoadObject[LISPSIZE_SMALLSIZE] = load_object_smallsize;
	Heap_LoadObject[LISPSIZE_ARRAYBODY] = load_object_arraybody;
	Heap_LoadObject[LISPSIZE_BODY2] = load_object_body2;
	Heap_LoadObject[LISPSIZE_BODY4] = load_object_body4;
	Heap_LoadObject[LISPSIZE_BODY8] = load_object_body8;
}

