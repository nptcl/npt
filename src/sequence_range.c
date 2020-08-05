#include "array_access.h"
#include "array_value.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "sequence.h"
#include "sequence_range.h"

/*
 *  save/load
 */
_g void save_sequence_range(struct sequence_range *ptr)
{
	ptr->save_pos = ptr->pos;
	ptr->save_list = ptr->list;
	ptr->save_prev = ptr->prev;
	ptr->save_size = ptr->size;
	ptr->save_end = ptr->end;
	ptr->save_index = ptr->index;
}

_g void load_sequence_range(struct sequence_range *ptr)
{
	ptr->pos = ptr->save_pos;
	ptr->list = ptr->save_list;
	ptr->prev = ptr->save_prev;
	ptr->size = ptr->save_size;
	ptr->end = ptr->save_end;
	ptr->index = ptr->save_index;
}


/*
 *  build
 */
_g int build_sequence_range_(struct sequence_range *ptr,
		addr pos, addr start, addr end)
{
	int listp;
	addr list, prev;
	size_t index1, index2, size;

	clearpoint(ptr);
	Return(listp_sequence_(pos, &listp));
	ptr->pos = list = pos;
	ptr->listp = listp;
	if (start == Nil || start == Unbound)
		start = fixnumh(0);
	if (end == Nil)
		end = Unbound;

	if (listp) {
		Return(list_start_end_sequence_(&list, &prev, start, end, &index1, &index2));
		ptr->list = list;
		ptr->prev = prev;
		ptr->start = index1;
		ptr->endp = (end != Unbound);
		if (ptr->endp) {
			ptr->end = index2;
			ptr->size = index2 - index1;
		}
		else {
			ptr->end = 0;
			ptr->size = 0;
		}
		ptr->index = index1;
	}
	else {
		Return(length_sequence_(pos, 1, &size));
		Return(size_start_end_sequence_(start, end, size, &index1, &index2, NULL));
		ptr->prev = Nil;
		ptr->start = index1;
		ptr->endp = 1;
		ptr->end = index2;
		ptr->size = index2 - index1;
		ptr->index = index1;
	}
	save_sequence_range(ptr);

	return 0;
}

static struct sequence_range *sequence_range_local(LocalRoot local)
{
	return (struct sequence_range *)lowlevel_local(local,
			sizeoft(struct sequence_range));
}

_g int make_sequence_range_(LocalRoot local,
		addr pos, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_(ptr, pos, start, end));

	return Result(ret, ptr);
}

static int start_end_sequence_range_(
		addr list, size_t index1, size_t index2, addr end, size_t *ret)
{
	size_t size;

	for (size = 0; ; size++, index1++) {
		if (end != Unbound || end != Unbound) {
			if (index2 <= index1)
				break;
			if (list == Nil) {
				*ret = 0;
				return fmte_(":END ~A "
						"must be less than equal to list length.", end, NULL);
			}
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list)) {
			*ret = 0;
			return fmte_("Don't accept the dotted list ~S.", list, NULL);
		}
		GetCdr(list, &list);
	}

	return Result(ret, size);
}

_g int build_sequence_range_endp_(struct sequence_range *ptr,
		addr list, addr start, addr end)
{
	Return(build_sequence_range_(ptr, list, start, end));
	if (! ptr->endp) {
		ptr->size = length_list_safe(ptr->list);
		ptr->end = ptr->start + ptr->size;
		ptr->endp = 1;
	}

	return 0;
}

_g int make_sequence_range_endp_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_endp_(ptr, list, start, end));

	return Result(ret, ptr);
}

_g int build_sequence_range_vector2_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end,
		addr *root, addr *tail)
{
	int check;
	addr pos, value;
	size_t index1, index2, size, i;

	/* vector */
	Return(listp_sequence_(list, &check));
	if (! check)
		return build_sequence_range_(ptr, list, start, end);

	/* list */
	Return(list_start_end_sequence_(&list, NULL, start, end, &index1, &index2));
	Return(start_end_sequence_range_(list, index1, index2, end, &size));
	if (root)
		*root = list;
	vector_local(local, &pos, size);
	for (i = 0; i < size; i++) {
		GetCons(list, &value, &list);
		setarray(pos, i, value);
	}
	if (tail)
		*tail = list;

	return build_sequence_range_(ptr, pos, fixnumh(0), Nil);
}

_g int build_sequence_range_vector_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end)
{
	return build_sequence_range_vector2_(local, ptr, list, start, end, NULL, NULL);
}

_g int make_sequence_range_vector_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret)
{
	struct sequence_range *ptr;

	ptr = sequence_range_local(local);
	Return(build_sequence_range_vector_(local, ptr, list, start, end));

	return Result(ret, ptr);
}

static int getlist_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	if (! ptr->endp) {
		if (ptr->list == Nil)
			return Result(ret, 1);
		else
			goto normal;
	}
	if (ptr->index < ptr->end) {
		if (ptr->list == Nil)
			goto error;
		else
			goto normal;
	}
	return Result(ret, 1);

normal:
	Return_getcar(ptr->list, value);
	return Result(ret, 0);

error:
	*ret = 0;
	return fmte_(":END ~A must be less than equal to list length.",
			intsizeh(ptr->end), NULL);
}


/*
 *  access
 */
_g int get_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	/* list */
	if (ptr->listp)
		return getlist_sequence_range_(ptr, value, ret);

	/* vector */
	if (ptr->end <= ptr->index)
		return Result(ret, 1);
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index, value));

	return Result(ret, 0);
}

_g int getnext_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	int check;

	if (ptr->listp) {
		Return(getlist_sequence_range_(ptr, value, &check));
		if (! check) {
			ptr->prev = ptr->list;
			getcons(ptr->list, value, &(ptr->list));
			ptr->index++;
		}
		return Result(ret, check);
	}
	else {
		if (ptr->end <= ptr->index)
			return Result(ret, 1);
		Return(getelt_sequence_(NULL, ptr->pos, ptr->index++, value));

		return Result(ret, 0);
	}
}

_g int next_sequence_range_(struct sequence_range *ptr, int *ret)
{
	addr temp;
	return getnext_sequence_range_(ptr, &temp, ret);
}

_g int endp_sequence_range(struct sequence_range *ptr)
{
	if (ptr->endp)
		return ptr->end <= ptr->index;
	else
		return ptr->list == Nil;
}

_g int set_sequence_range_(struct sequence_range *ptr, addr value)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		SetCar(ptr->list, value);
		return 0;
	}
	else {
		return setelt_sequence_(ptr->pos, ptr->index, value);
	}
}

_g int getinplace_sequence_range_(struct sequence_range *ptr, struct array_value *ret)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		ret->type = ARRAY_TYPE_T;
		GetCar(ptr->list, &(ret->value.object));
		return 0;
	}
	else {
		return getelt_inplace_sequence_(ptr->pos, ptr->index, ret);
	}
}

_g int setinplace_sequence_range_(LocalRoot local,
		struct sequence_range *ptr, const struct array_value *str)
{
	addr value;

	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		Return(arrayvalue_alloc_(local, &value, str));
		SetCar(ptr->list, value);
		return 0;
	}
	else {
		return setelt_inplace_sequence_(local, ptr->pos, ptr->index, str);
	}
}


/*
 *  reverse
 */
_g void reverse_sequence_range(struct sequence_range *ptr)
{
	Check(ptr->listp, "type error");
	ptr->index = ptr->end;
}

_g int endp_reverse_sequence_range(struct sequence_range *ptr)
{
	Check(ptr->listp, "type error");
	return ptr->index <= ptr->start;
}

_g int next_reverse_sequence_range_(struct sequence_range *ptr, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	ptr->index--;

	return Result(ret, 0);
}

_g int get_reverse_sequence_range_(struct sequence_range *ptr, addr *value, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index - 1, value));

	return Result(ret, 0);
}

_g int getnext_reverse_sequence_range_(
		struct sequence_range *ptr, addr *value, int *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr))
		return Result(ret, 1);
	ptr->index--;
	Return(getelt_sequence_(NULL, ptr->pos, ptr->index, value));

	return Result(ret, 0);
}

_g int set_reverse_sequence_range_(struct sequence_range *ptr, addr value)
{
	Check(endp_reverse_sequence_range(ptr), "endp error");
	Check(ptr->listp, "type error");
	return setelt_sequence_(ptr->pos, ptr->index - 1, value);
}


/*
 *  remove
 */
_g int remove_sequence_range_(struct sequence_range *ptr)
{
	Check(! ptr->listp, "type error");
	Check(ptr->list == Nil, "list error");

	if (ptr->endp) {
		if (ptr->end <= ptr->index)
			return fmte_(":end size error", NULL);
		ptr->end--;
		ptr->size--;
	}
	if (ptr->prev == Nil) {
		Return_getcdr(ptr->list, &(ptr->list));
		ptr->pos = ptr->list;
	}
	else {
		Return_getcdr(ptr->list, &(ptr->list));
		Return_setcdr(ptr->prev, ptr->list);
	}

	return 0;
}

