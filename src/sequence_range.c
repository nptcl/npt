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
_g void build_sequence_range(struct sequence_range *ptr,
		addr pos, addr start, addr end)
{
	unsigned listp;
	addr list, prev;
	size_t index1, index2, size;

	clearpoint(ptr);
	listp = listp_sequence(pos);
	ptr->pos = list = pos;
	ptr->listp = listp;
	if (start == Nil || start == Unbound)
		start = fixnumh(0);
	if (end == Nil)
		end = Unbound;

	if (listp) {
		list_start_end_sequence(&list, &prev, start, end, &index1, &index2);
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
		size = length_sequence(pos, 1);
		size_start_end_sequence(start, end, size, &index1, &index2);
		ptr->prev = Nil;
		ptr->start = index1;
		ptr->endp = 1;
		ptr->end = index2;
		ptr->size = index2 - index1;
		ptr->index = index1;
	}
	save_sequence_range(ptr);
}

static struct sequence_range *sequence_range_local(LocalRoot local)
{
	return (struct sequence_range *)lowlevel_local(local,
			sizeoft(struct sequence_range));
}

_g struct sequence_range *make_sequence_range(LocalRoot local,
		addr pos, addr start, addr end)
{
	struct sequence_range *ptr = sequence_range_local(local);
	build_sequence_range(ptr, pos, start, end);
	return ptr;
}

static size_t start_end_sequence_range(
		addr list, size_t index1, size_t index2, addr end)
{
	size_t size;

	for (size = 0; ; size++, index1++) {
		if (end != Unbound || end != Unbound) {
			if (index2 <= index1)
				break;
			if (list == Nil)
				fmte(":END ~A must be less than equal to list length.", end, NULL);
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list))
			fmte("Don't accept the dotted list ~S.", list, NULL);
		GetCdr(list, &list);
	}

	return size;
}

_g void build_sequence_range_endp(struct sequence_range *ptr,
		addr list, addr start, addr end)
{
	build_sequence_range(ptr, list, start, end);
	if (! ptr->endp) {
		ptr->size = length_list_safe(ptr->list);
		ptr->end = ptr->start + ptr->size;
		ptr->endp = 1;
	}
}

_g struct sequence_range *make_sequence_range_endp(LocalRoot local,
		addr list, addr start, addr end)
{
	struct sequence_range *ptr = sequence_range_local(local);
	build_sequence_range_endp(ptr, list, start, end);
	return ptr;
}

_g void build_sequence_range_vector2(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end,
		addr *root, addr *tail)
{
	addr pos, value;
	size_t index1, index2, size, i;

	/* vector */
	if (! listp_sequence(list)) {
		build_sequence_range(ptr, list, start, end);
		return;
	}

	/* list */
	list_start_end_sequence(&list, NULL, start, end, &index1, &index2);
	size = start_end_sequence_range(list, index1, index2, end);
	if (root) *root = list;
	vector_local(local, &pos, size);
	for (i = 0; i < size; i++) {
		GetCons(list, &value, &list);
		setarray(pos, i, value);
	}
	if (tail) *tail = list;

	build_sequence_range(ptr, pos, fixnumh(0), Nil);
}

_g void build_sequence_range_vector(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end)
{
	build_sequence_range_vector2(local, ptr, list, start, end, NULL, NULL);
}

_g struct sequence_range *make_sequence_range_vector(LocalRoot local,
		addr list, addr start, addr end)
{
	struct sequence_range *ptr = sequence_range_local(local);
	build_sequence_range_vector(local, ptr, list, start, end);
	return ptr;
}

static int getlist_sequence_range(struct sequence_range *ptr, addr *ret)
{
	if (! ptr->endp) {
		if (ptr->list == Nil)
			return 1;
		else
			goto normal;
	}
	if (ptr->index < ptr->end) {
		if (ptr->list == Nil)
			goto error;
		else
			goto normal;
	}
	return 1;

normal:
	getcar(ptr->list, ret);
	return 0;

error:
	fmte(":END ~A must be less than equal to list length.",
			intsizeh(ptr->end), NULL);
	return 1;
}


/*
 *  access
 */
_g int get_sequence_range(struct sequence_range *ptr, addr *ret)
{
	/* list */
	if (ptr->listp)
		return getlist_sequence_range(ptr, ret);

	/* vector */
	if (ptr->end <= ptr->index)
		return 1;
	getelt_sequence(NULL, ptr->pos, ptr->index, ret);

	return 0;
}

_g int getnext_sequence_range(struct sequence_range *ptr, addr *ret)
{
	int check;

	if (ptr->listp) {
		check = getlist_sequence_range(ptr, ret);
		if (! check) {
			ptr->prev = ptr->list;
			getcons(ptr->list, ret, &(ptr->list));
			ptr->index++;
		}
		return check;
	}
	else {
		if (ptr->end <= ptr->index)
			return 1;
		getelt_sequence(NULL, ptr->pos, ptr->index++, ret);

		return 0;
	}
}

_g int next_sequence_range(struct sequence_range *ptr)
{
	addr temp;
	return getnext_sequence_range(ptr, &temp);
}

_g int endp_sequence_range(struct sequence_range *ptr)
{
	if (ptr->endp)
		return ptr->end <= ptr->index;
	else
		return ptr->list == Nil;
}

_g void set_sequence_range(struct sequence_range *ptr, addr value)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp)
		SetCar(ptr->list, value);
	else
		setelt_sequence(ptr->pos, ptr->index, value);
}

_g void getinplace_sequence_range(struct sequence_range *ptr, struct array_value *ret)
{
	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		ret->type = ARRAY_TYPE_T;
		GetCar(ptr->list, &(ret->value.object));
	}
	else {
		getelt_inplace_sequence(ptr->pos, ptr->index, ret);
	}
}

_g void setinplace_sequence_range(LocalRoot local,
		struct sequence_range *ptr, const struct array_value *str)
{
	addr value;

	Check(endp_sequence_range(ptr), "endp error");
	if (ptr->listp) {
		array_value_alloc(local, &value, str);
		SetCar(ptr->list, value);
	}
	else {
		setelt_inplace_sequence(local, ptr->pos, ptr->index, str);
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

_g int next_reverse_sequence_range(struct sequence_range *ptr)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr)) return 1;
	ptr->index--;

	return 0;
}

_g int get_reverse_sequence_range(struct sequence_range *ptr, addr *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr)) return 1;
	getelt_sequence(NULL, ptr->pos, ptr->index - 1, ret);

	return 0;
}

_g int getnext_reverse_sequence_range(struct sequence_range *ptr, addr *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_sequence_range(ptr)) return 1;
	ptr->index--;
	getelt_sequence(NULL, ptr->pos, ptr->index, ret);

	return 0;
}

_g void set_reverse_sequence_range(struct sequence_range *ptr, addr value)
{
	Check(endp_reverse_sequence_range(ptr), "endp error");
	Check(ptr->listp, "type error");
	setelt_sequence(ptr->pos, ptr->index - 1, value);
}


/*
 *  remove
 */
_g void remove_sequence_range(struct sequence_range *ptr)
{
	Check(! ptr->listp, "type error");
	Check(ptr->list == Nil, "list error");

	if (ptr->endp) {
		if (ptr->end <= ptr->index)
			fmte(":end size error", NULL);
		ptr->end--;
		ptr->size--;
	}
	if (ptr->prev == Nil) {
		getcdr(ptr->list, &(ptr->list));
		ptr->pos = ptr->list;
	}
	else {
		getcdr(ptr->list, &(ptr->list));
		setcdr(ptr->prev, ptr->list);
	}
}

