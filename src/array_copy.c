#include "array.h"
#include "array_copy.h"
#include "array_object.h"
#include "bit.h"

/*
 *  arary_copy
 */
static void array_struct_copy(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	addr temp;

	/* array */
	GetArrayInfo(array, ARRAY_INFO_TYPE, &temp);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, temp);
	GetArrayInfo(array, ARRAY_INFO_DISPLACED, &temp);
	SetArrayInfo(pos, ARRAY_INFO_DISPLACED, temp);

	/* body */
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	*str1 = *str2;
}

_g void array_size_copy(LocalRoot local, addr pos, addr array)
{
	addr temp;
	size_t size;

	GetArrayInfo(array, ARRAY_INFO_DIMENSION, &temp);
	if (temp != Nil) {
		size = ArrayInfoStruct(array)->dimension;
		arraysize_copy_alloc(local, &temp, temp, size);
	}
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
}

static void copy_array_general(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one, temp;
	size_t i;

	arraygen_alloc(local, &one, size);
	for (i = 0; i < size; i++) {
		arraygen_get(pos, i, &temp);
		arraygen_set(one, i, temp);
	}
	*ret = one;
}

static void copy_array_specialized(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	void *data1;
	const void *data2;

	arrayspec_alloc(local, &one, size);
	data1 = (void *)arrayspec_ptr(one);
	data2 = (const void *)arrayspec_ptr(pos);
	memcpy(data1, data2, size);
}

static void array_memory_copy(LocalRoot local, addr pos, addr array)
{
	addr mem;
	struct array_struct *str;

	GetArrayInfo(array, ARRAY_INFO_MEMORY, &mem);
	switch (GetType(mem)) {
		case LISPTYPE_BITVECTOR:
			bitmemory_copy_alloc(local, &mem, mem);
			break;

		case LISPSYSTEM_ARRAY_GENERAL:
			str = ArrayInfoStruct(array);
			copy_array_general(local, &mem, mem, str->size);
			break;

		case LISPSYSTEM_ARRAY_SPECIALIZED:
			str = ArrayInfoStruct(array);
			copy_array_specialized(local, &mem, mem, str->size * str->element);
			break;

		default:
			break;
	}
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, mem);
}

_g void array_copy_alloc(LocalRoot local, addr *ret, addr array)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	array_struct_copy(pos, array);
	/* dimension */
	array_size_copy(local, pos, array);
	/* allocate */
	array_memory_copy(local, pos, array);
	/* result */
	*ret = pos;
}

_g void array_copy_local(LocalRoot local, addr *ret, addr array)
{
	Check(local == NULL, "local error");
	array_copy_alloc(local, ret, array);
}

_g void array_copy_heap(addr *ret, addr array)
{
	array_copy_alloc(NULL, ret, array);
}

