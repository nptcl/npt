#include "arch.h"
#include "array.h"
#include "condition.h"
#include "integer.h"
#include "type_table.h"
#include "type_upgraded.h"

/*
 *  accessor
 */
_g void arraygen_set_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_set_Low(pos, index, value);
}

_g void arraygen_get_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(arraygen_lenr_Low(pos) <= index, "size error");
	arraygen_get_Low(pos, index, ret);
}

_g void arraygen_len_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	arraygen_len_Low(pos, ret);
}

_g size_t arraygen_lenr_debug(addr pos)
{
	return arraygen_lenr_Low(pos);
}

_g void arrayspec_pos_debug(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	arrayspec_pos_Low(pos, ret);
}

_g addr arrayspec_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	return arrayspec_ptr_Low(pos);
}

_g size_t *arraysize_ptr_debug(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	return arraysize_ptr_Low(pos);
}

_g struct array_struct *arrayinfo_struct_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return arrayinfo_struct_Low(pos);
}

_g void getarrayinfo_debug(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo_Low(pos, index, ret);
}

_g void setarrayinfo_debug(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayInfo_Low(pos, index, value);
}

_g void lenarrayinfo_debug(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	LenArrayInfo_Low(pos, ret);
}

_g size_t lenarrayinfor_debug(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return LenArrayInfor_Low(pos);
}


/*
 *  memory allocate
 */
_g void arraygen_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraygen_alloc_Low(local, ret, LISPSYSTEM_ARRAY_GENERAL, size);
}
_g void arraygen_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arraygen_alloc(local, ret, size);
}
_g void arraygen_heap(addr *ret, size_t size)
{
	arraygen_alloc(NULL, ret, size);
}

_g void arrayspec_alloc(LocalRoot local, addr *ret, size_t size)
{
	arrayspec_alloc_Low(local, ret, LISPSYSTEM_ARRAY_SPECIALIZED, size);
}
_g void arrayspec_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	arrayspec_alloc(local, ret, size);
}
_g void arrayspec_heap(addr *ret, size_t size)
{
	arrayspec_alloc(NULL, ret, size);
}

_g void arrayinfo_alloc(LocalRoot local, addr *ret)
{
	arrayinfo_alloc_Low(local, ret, LISPTYPE_ARRAY,
			ARRAY_INDEX_SIZE, sizeoft(struct array_struct));
}
_g void arrayinfo_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	arrayinfo_alloc(local, ret);
}
_g void arrayinfo_heap(addr *ret)
{
	arrayinfo_alloc(NULL, ret);
}

static void arraysize1_alloc(LocalRoot local, addr *ret, size_t size)
{
	arraysize1_alloc_Low(local, ret, LISPSYSTEM_ARRAY_DIMENSION, size);
}
_g void arraysize_alloc(LocalRoot local, addr *ret, size_t index)
{
	if (multisafe_size(IdxSize, index, &index))
		fmte("Index overflow.", NULL);
	arraysize1_alloc(local, ret, index);
}
_g void arraysize_local(LocalRoot local, addr *ret, size_t index)
{
	CheckLocal(local);
	arraysize_alloc(local, ret, index);
}
_g void arraysize_heap(addr *ret, size_t index)
{
	arraysize_alloc(NULL, ret, index);
}

_g void arraysize_copy_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
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
_g void arraysize_copy_local(LocalRoot local, addr *ret, addr pos, size_t size)
{
	CheckLocal(local);
	arraysize_copy_alloc(local, ret, pos, size);
}
_g void arraysize_copy_heap(addr *ret, addr pos, size_t size)
{
	arraysize_copy_alloc(NULL, ret, pos, size);
}

_g void array_empty_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct array_struct *str;

	arrayinfo_alloc(local, &pos);
	str = ArrayInfoStruct(pos);
	clearpoint(str);
	str->type = ARRAY_TYPE_EMPTY;
	*ret = pos;
}
_g void array_empty_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	array_empty_alloc(local, ret);
}
_g void array_empty_heap(addr *ret)
{
	array_empty_alloc(NULL, ret);
}

_g void array_alloc(LocalRoot local, addr *ret, size_t index, size_t size)
{
	addr pos, temp;
	struct array_struct *str;

	/* object */
	array_empty_alloc(local, &pos);
	str = ArrayInfoStruct(pos);

	/* dimension */
	if (2 <= index) {
		arraysize_alloc(local, &temp, index);
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	}
	str->dimension = index;
	str->size = str->front = size;

	/* type */
	str->type = ARRAY_TYPE_T;
	GetTypeTable(&temp, T);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, temp);

	/* result */
	*ret = pos;
}
_g void array_local(LocalRoot local, addr *ret, size_t index, size_t size)
{
	CheckLocal(local);
	array_alloc(local, ret, index, size);
}
_g void array_heap(addr *ret, size_t index, size_t size)
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
		GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &dimension);
		data = arraysize_ptr(dimension);
		for (i = 0; i < index; i++)
			data[i] = (size_t)va_arg(args, unsigned);
	}

	/* result */
	*ret = pos;
}
_g void array_va_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	array_va_stdarg(local, ret, args);
	va_end(args);
}
_g void array_va_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	CheckLocal(local);
	va_start(args, ret);
	array_va_stdarg(local, ret, args);
	va_end(args);
}
_g void array_va_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	array_va_stdarg(NULL, ret, args);
	va_end(args);
}


/*
 *  type check
 */
_g int array_system_general_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARRAY_GENERAL;
}

_g int array_system_specialized_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED;
}

_g int array_system_p(addr pos)
{
	enum LISPTYPE type = GetType(pos);
	return type == LISPSYSTEM_ARRAY_GENERAL
		|| type == LISPSYSTEM_ARRAY_SPECIALIZED;
}

_g int arrayp(addr pos)
{
	return GetType(pos) == LISPTYPE_ARRAY;
}

_g int array_simple_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	return str->simple;
}

_g int array_vector_p(addr pos)
{
	return arrayp(pos) && ArrayInfoStruct(pos)->dimension == 1;
}

_g int array_size_vector_p(addr pos, size_t size)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	if (str->dimension != 1)
		return 0;

	return ArrayInfoStruct(pos)->size == size;
}

_g int array_general_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type == ARRAY_TYPE_T;
}

_g int array_specialized_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type != ARRAY_TYPE_T;
}

_g int array_simple_vector_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);
	return str->simple && str->dimension == 1 && str->type == ARRAY_TYPE_T;
}

_g int array_adjustable_p(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->adjustable;
}

_g int array_fillpointer_p(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->fillpointer;
}

_g size_t array_dimension_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->dimension;
}

_g size_t array_total_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->size;
}

_g size_t array_fill_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->front;
}

_g enum ARRAY_TYPE array_type(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->type;
}

_g unsigned array_type_size(addr pos)
{
	Check(! arrayp(pos), "type error");
	return ArrayInfoStruct(pos)->bytesize;
}


/*
 *  memory access
 */
_g const size_t *array_ptrsize(addr pos)
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
			GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &pos);
			return arraysize_ptr(pos);
	}
}

_g void *array_ptrwrite(addr pos, size_t index)
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
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);

	return (void *)(arrayspec_ptr(pos) + (index * str->element));
}

_g const void *array_ptrread(addr pos, size_t index)
{
	return (const void *)array_ptrwrite(pos, index);
}


/*
 *  fill-pointer
 */
_g int array_fill_pointer(addr array, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) return 1;
	*ret = intsizeh(str->front);

	return 0;
}

_g int array_setf_fill_pointer(addr array, addr value)
{
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) return 1;
	if (GetIndex_integer(value, &size))
		fmte("Invalid fill-pointer value ~S.", value, NULL);
	if (str->size <= size) {
		fmte("Fill-pointer value ~A must be less than array size ~A.",
				value, intsizeh(str->size), NULL);
	}
	str->front = size;

	return 0;
}

_g int array_fill_pointer_start(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) {
		return 1;
	}
	else {
		str->front = 0;
		return 0;
	}
}

_g int array_fill_pointer_end(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	else
		return 0;
}

_g int array_fill_pointer_set(addr array, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	if (str->size <= size)
		return 1;
	str->front = size;

	return 0;
}

