#include "arch.h"
#include "array.h"
#include "condition.h"
#include "type_table.h"
#include "type_upgraded.h"

/*
 *  accessor
 */
void arraygen_set_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_set_Low(pos, index, value);
}

void arraygen_get_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_get_Low(pos, index, ret);
}

void arraygen_len_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	arraygen_len_Low(pos, ret);
}

size_t arraygen_lenr_debug(addr pos)
{
	return arraygen_lenr_Low(pos);
}

void arrayspec_pos_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	arrayspec_pos_Low(pos, ret);
}

addr arrayspec_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	return arrayspec_ptr_Low(pos);
}

size_t *arraysize_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	return arraysize_ptr_Low(pos);
}

struct array_struct *arrayinfo_struct_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return arrayinfo_struct_Low(pos);
}

addr refarrayinfo_debug(addr pos, size_t index)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return RefArrayInfo_Low(pos, index);
}

void getarrayinfo_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo_Low(pos, index, ret);
}

void setarrayinfo_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayInfo_Low(pos, index, value);
}

void lenarrayinfo_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	LenArrayInfo_Low(pos, ret);
}

size_t lenarrayinfor_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return LenArrayInfor_Low(pos);
}


/*
 *  memory allocate
 */
void arraygen_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraygen_alloc_Low(local, ret, LISPSYSTEM_ARRAY_GENERAL, size);
}
void arraygen_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arraygen_alloc(local, ret, size);
}
void arraygen_heap(addr *ret, size_t size)
{
	arraygen_alloc(NULL, ret, size);
}

void arrayspec_alloc(LocalRoot local, addr *ret, size_t size)
{
	arrayspec_alloc_Low(local, ret, LISPSYSTEM_ARRAY_SPECIALIZED, size);
}
void arrayspec_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arrayspec_alloc(local, ret, size);
}
void arrayspec_heap(addr *ret, size_t size)
{
	arrayspec_alloc(NULL, ret, size);
}

void arrayinfo_alloc(LocalRoot local, addr *ret)
{
	arrayinfo_alloc_Low(local, ret, LISPTYPE_ARRAY,
			ARRAY_INFO_SIZE, sizeoft(struct array_struct));
}
void arrayinfo_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	arrayinfo_alloc(local, ret);
}
void arrayinfo_heap(addr *ret)
{
	arrayinfo_alloc(NULL, ret);
}

static void arraysize1_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraysize1_alloc_Low(local, ret, LISPSYSTEM_ARRAY_DIMENSION, size);
}
void arraysize_alloc(LocalRoot local, addr *ret, size_t index)
{
	if (multisafe_size(IdxSize, index, &index))
		fmte("Index overflow.", NULL);
	arraysize1_alloc(local, ret, index);
}
void arraysize_local(LocalRoot local, addr *ret, size_t index)
{
	CheckLocal(local);
	arraysize_alloc(local, ret, index);
}
void arraysize_heap(addr *ret, size_t index)
{
	arraysize_alloc(NULL, ret, index);
}

void arraysize_copy_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	size_t *data1;
	const size_t *data2;

	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	arraysize_alloc(local, &one, size);
	data1 = arraysize_ptr(one);
	data2 = arraysize_ptr(pos);
	memcpy(data1, data2, IdxSize * size);
	*ret = one;
}
void arraysize_copy_local(LocalRoot local, addr *ret, addr pos, size_t size)
{
	CheckLocal(local);
	arraysize_copy_alloc(local, ret, pos, size);
}
void arraysize_copy_heap(addr *ret, addr pos, size_t size)
{
	arraysize_copy_alloc(NULL, ret, pos, size);
}

void array_empty_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct array_struct *str;

	arrayinfo_alloc(local, &pos);
	str = ArrayInfoStruct(pos);
	clearpoint(str);
	str->type = ARRAY_TYPE_EMPTY;
	*ret = pos;
}
void array_empty_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	array_empty_alloc(local, ret);
}
void array_empty_heap(addr *ret)
{
	array_empty_alloc(NULL, ret);
}

void array_alloc(LocalRoot local, addr *ret, size_t index, size_t size)
{
	addr pos, temp;
	struct array_struct *str;

	/* object */
	array_empty_alloc(local, &pos);
	str = ArrayInfoStruct(pos);

	/* dimension */
	if (2 <= index) {
		arraysize_alloc(local, &temp, index);
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
	}
	str->dimension = index;
	str->size = str->front = str->refer = size;

	/* type */
	str->type = ARRAY_TYPE_T;
	GetTypeTable(&temp, T);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, temp);

	/* result */
	*ret = pos;
}
void array_local(LocalRoot local, addr *ret, size_t index, size_t size)
{
	CheckLocal(local);
	array_alloc(local, ret, index, size);
}
void array_heap(addr *ret, size_t index, size_t size)
{
	array_alloc(NULL, ret, index, size);
}

static void array_va_stdarg(LocalRoot local, addr *ret, va_list args)
{
	addr pos, dimension;
	size_t size, i, index, allcount, *data;
	va_list dest;

	/* index */
	va_copy(dest, args);
	allcount = 1;
	for (index = 0; ; index++) {
		size = (size_t)va_arg(dest, unsigned);
		if (size == 0) break;
		if (multisafe_size(allcount, size, &allcount))
			fmte("size overflow.", NULL);
	}

	/* make */
	array_alloc(local, &pos, index, allcount);
	if (2 <= index) {
		GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &dimension);
		data = arraysize_ptr(dimension);
		for (i = 0; i < index; i++)
			data[i] = (size_t)va_arg(args, unsigned);
	}

	/* result */
	*ret = pos;
}
void array_va_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	array_va_stdarg(local, ret, args);
	va_end(args);
}
void array_va_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	CheckLocal(local);
	va_start(args, ret);
	array_va_stdarg(local, ret, args);
	va_end(args);
}
void array_va_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	array_va_stdarg(NULL, ret, args);
	va_end(args);
}


/*
 *  type check
 */
int arrayp(addr pos)
{
	return GetType(pos) == LISPTYPE_ARRAY;
}

int array_simple_p(addr pos)
{
	return arrayp(pos) && ArrayInfoStruct(pos)->simple;
}

int array_vector_p(addr pos)
{
	return arrayp(pos) && ArrayInfoStruct(pos)->dimension == 1;
}

int array_size_vector_p(addr pos, size_t size)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	if (str->dimension != 1)
		return 0;

	return ArrayInfoStruct(pos)->size == size;
}

int array_general_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type == ARRAY_TYPE_T;
}

int array_specialized_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type != ARRAY_TYPE_T;
}

int array_simple_vector_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	return str->simple && str->dimension == 1;
}


/*
 *  memory access
 */
const size_t *array_ptrsize(addr pos)
{
	struct array_struct *str;

	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	switch (str->dimension) {
		case 0:
			return NULL;

		case 1:
			return (const size_t *)&(str->size);

		default:
			GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &pos);
			return arraysize_ptr(pos);
	}
}

void *array_ptrwrite(addr pos, size_t index)
{
	enum ARRAY_TYPE type;
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(pos);
	type = str->type;
	size = str->size;
	if (type == ARRAY_TYPE_EMPTY)
		fmte("The array has no memory yet.", NULL);
	if (type == ARRAY_TYPE_T || type == ARRAY_TYPE_BIT)
		fmte("The object is not specialized array.", NULL);
	if (size <= index)
		fmte("Index is too large.", NULL);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);

	return (void *)(arrayspec_ptr(pos) + (index * str->element));
}

const void *array_ptrread(addr pos, size_t index)
{
	return (const void *)array_ptrwrite(pos, index);
}

