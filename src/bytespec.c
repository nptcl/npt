#include "bignum_data.h"
#include "bytespec.h"
#include "condition.h"
#include "integer.h"
#include "object.h"

_g void bytespec_alloc(LocalRoot local, addr *ret, size_t size, size_t posi)
{
	addr pos;
	struct bytespec_struct *ptr;

	alloc_body2(local, &pos, LISPTYPE_BYTESPEC, sizeoft(struct bytespec_struct));
	ptr = ByteSpecStruct(pos);
	ptr->size = size;
	ptr->position = posi;
	*ret = pos;
}

_g void bytespec_local(LocalRoot local, addr *ret, size_t size, size_t posi)
{
	CheckLocal(local);
	bytespec_alloc(local, ret, size, posi);
}

_g void bytespec_heap(addr *ret, size_t size, size_t posi)
{
	bytespec_alloc(NULL, ret, size, posi);
}


/*
 *  byte
 */
_g int byte_common_(addr size, addr posi, addr *ret)
{
	size_t ssize, spos;

	if (GetIndex_integer(size, &ssize))
		return fmte_("Byte spec SIZE ~S is too large.", size, NULL);
	if (GetIndex_integer(posi, &spos))
		return fmte_("Byte spec POSITION ~S is too large.", posi, NULL);
	bytespec_heap(ret, ssize, spos);

	return 0;
}

_g void byte_size_common(addr pos, addr *ret)
{
	struct bytespec_struct *ptr;

	CheckType(pos, LISPTYPE_BYTESPEC);
	ptr = ByteSpecStruct(pos);
	make_index_integer_heap(ret, ptr->size);
}

_g void byte_position_common(addr pos, addr *ret)
{
	struct bytespec_struct *ptr;

	CheckType(pos, LISPTYPE_BYTESPEC);
	ptr = ByteSpecStruct(pos);
	make_index_integer_heap(ret, ptr->position);
}


/*
 *  bytespec-mask
 */
_g void bytespec_mask_init(struct bytespec_mask *ptr, addr pos)
{
	struct bytespec_struct *spec;
	size_t size, posi;

	CheckType(pos, LISPTYPE_BYTESPEC);
	spec = ByteSpecStruct(pos);
	size = spec->size;
	posi = spec->position;
	ptr->size = size;
	ptr->position = posi;
	ptr->posend = size + posi;
	ptr->index = 0;
	ptr->start = posi / BIGNUM_FULLBIT;
	ptr->end = ptr->posend / BIGNUM_FULLBIT;
}

static bigtype bytespec_mask_start(size_t pos)
{
	size_t index;

	index = pos % BIGNUM_FULLBIT;
	if (index == BIGNUM_FULLBIT - 1ULL)
		return ((bigtype)1ULL) << (BIGNUM_FULLBIT - 1ULL);
	else
		return ~((1ULL << index) - 1ULL);
}

static bigtype bytespec_mask_end(size_t pos)
{
	return ~(bytespec_mask_start(pos));
}

_g bigtype bytespec_mask_get(struct bytespec_mask *ptr)
{
	bigtype v1, v2;
	size_t index, start, end, size, pos;

	index = ptr->index++;
	start = ptr->start;
	end = ptr->end;
	size = ptr->size;
	pos = ptr->position;
	if (size == 0)
		return 0;
	/* start */
	if (index < start)
		v1 = 0;
	else if (start < index)
		v1 = BIGNUM_FULL;
	else
		v1 =  bytespec_mask_start(pos);
	/* end */
	if (index < end)
		v2 = BIGNUM_FULL;
	else if (end < index)
		v2 = 0;
	else
		v2 = bytespec_mask_end(pos + size);
	/* result */
	return v1 & v2;
}

_g size_t bytespec_mask_getsize(struct bytespec_mask *ptr)
{
	size_t bit = ptr->position + ptr->size;
	size_t size = bit / BIGNUM_FULLBIT;
	return (bit % BIGNUM_FULLBIT)? size+1ULL: size;
}

