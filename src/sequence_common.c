#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "integer.h"
#include "sequence_common.h"
#include "strvect.h"
#include "type_memory.h"
#include "type_upgraded.h"
#include "typedef.h"

int make_specialized_sequence_(addr *ret, enum ARRAY_TYPE type, int bs, size_t size)
{
	switch (type) {
		case ARRAY_TYPE_T:
			vector_heap(ret, size);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_unsafe(NULL, ret, size);
			break;

		case ARRAY_TYPE_CHARACTER:
			strvect_heap(ret, size);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return vector_signed_uninit_(ret, size, type, bs);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_uninit_(ret, size, type);

		default:
			*ret = Nil;
			return fmte_("Invalid array type.", NULL);
	}

	return 0;
}

int array_upgraded_merge_sequence_(addr *ret, addr type, size_t size)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
	return make_specialized_sequence_(ret, upgraded, upsize, size);
}

static int make_vector_array_sequence_(addr *ret, addr pos, size_t size)
{
	struct array_struct *info = ArrayInfoStruct(pos);
	return make_specialized_sequence_(ret, info->type, info->bytesize, size);
}

int make_vector_size_sequence_(addr *ret, addr pos, size_t size)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			vector_heap(ret, size);
			break;

		case LISPTYPE_STRING:
			strvect_heap(ret, size);
			break;

		case LISPTYPE_ARRAY:
			return make_vector_array_sequence_(ret, pos, size);

		case LISPTYPE_BITVECTOR:
			bitmemory_unsafe(NULL, ret, size);
			break;

		default:
			return TypeError_(pos, SEQUENCE);
	}

	return 0;
}

int setcount_sequence_(struct count_struct *str, addr count)
{
	int check;
	size_t limit;

	if (count == Nil) {
		str->count = Nil;
		str->limit = 0;
		return 0;
	}
	if (! integerp(count)) {
		return fmte_(":COUNT argument ~S must be an integer type.", count, NULL);
	}
	Return(minusp_integer_(count, &check));
	if (check) {
		fixnum_heap(&count, 0);
		limit = 0;
	}
	else if (GetIndex_integer(count, &limit)) {
		return fmte_(":COUNT argument ~S is too large.", count, NULL);
	}
	str->count = count;
	str->limit = limit;

	return 0;
}

