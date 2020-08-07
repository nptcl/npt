#include "cons.h"
#include "cons_list.h"
#include "sequence.h"
#include "sequence_iterator.h"

/*
 *  sequence-iterator
 */
_g int make_sequence_iterator_local_(LocalRoot local,
		addr pos, int fill, struct sequence_iterator **ret)
{
	struct sequence_iterator *ptr;

	Check(local == NULL, "local error");
	ptr = (struct sequence_iterator *)lowlevel_local(local,
			sizeoft(struct sequence_iterator));
	ptr->pos = pos;
	ptr->root = pos;
	if (listp(pos)) {
		ptr->listp = 1;
		ptr->size = 0;
	}
	else {
		ptr->listp = 0;
		Return(length_sequence_(pos, fill, &(ptr->size)));
	}
	ptr->index = 0;

	return Result(ret, ptr);
}

_g int end_sequence_iterator(struct sequence_iterator *ptr)
{
	if (ptr->listp)
		return ptr->pos == Nil;
	else
		return ptr->size <= ptr->index;
}

_g int length_sequence_iterator_(struct sequence_iterator *ptr, size_t *ret)
{
	if (ptr->listp)
		return length_list_safe_(ptr->root, ret);
	else
		return Result(ret, ptr->size);
}

_g int object_sequence_iterator_(struct sequence_iterator *iter, addr *value, int *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return Result(ret, 0);
		Return_getcons(iter->pos, value, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return Result(ret, 0);
		Return(getelt_sequence_(NULL, iter->pos, iter->index, value));
	}
	iter->index++;

	return Result(ret, 1);
}

static int next_sequence_iterator_(struct sequence_iterator *iter, addr *value, int *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return Result(ret, 0);
		Return_getcons(iter->pos, value, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return Result(ret, 0);
	}
	iter->index++;

	return Result(ret, 1);
}

_g int set_sequence_iterator_(struct sequence_iterator *iter, addr value, int *ret)
{
	if (iter->listp) {
		Return_setcar(iter->pos, value);
		Return_getcdr(iter->pos, &(iter->pos));
		iter->index++;
		return Result(ret, iter->pos == Nil);
	}
	else {
		Return(setelt_sequence_(iter->pos, iter->index, value));
		iter->index++;
		return Result(ret, iter->size <= iter->index);
	}
}


/*
 *  sequence-group
 */
_g int make_sequence_group_local_(
		LocalRoot local, addr rest, int fill, struct sequence_group **ret)
{
	struct sequence_group *ptr;
	struct sequence_iterator **data;
	addr pos;
	size_t size, i;

	Return(length_list_safe_(rest, &size));
	ptr = (struct sequence_group *)lowlevel_local(local,
			sizeoft(struct sequence_group));
	data  = (struct sequence_iterator **)lowlevel_local(local,
			sizeoft(struct sequence_iterator *) * size);

	for (i = 0; i < size; i++) {
		GetCons(rest, &pos, &rest);
		Return(make_sequence_iterator_local_(local, pos, fill, &(data[i])));
	}
	ptr->data = data;
	ptr->size = size;
	ptr->callsize = 0;
	ptr->list = NULL;

	return Result(ret, ptr);
}

_g void list_sequence_group_local(LocalRoot local,
		addr *ret, struct sequence_group *group)
{
	addr root;
	size_t size, i;

	size = group->size;
	root = Nil;
	for (i = 0; i < size; i++)
		conscdr_local(local, &root, root);
	group->list = root;
	if (ret)
		*ret = root;
}

_g int set_sequence_group_(struct sequence_group *group, addr list, int *ret)
{
	int check;
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		Return(object_sequence_iterator_(data[i], &temp, &check));
		if (! check)
			return Result(ret, 0);
		Check(! consp(list), "list error");
		SetCar(list, temp);
		GetCdr(list, &list);
	}

	return Result(ret, 1);
}

_g void clear_sequence_group(struct sequence_group *group)
{
	struct sequence_iterator **data, *ptr;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		ptr = data[i];
		ptr->pos = ptr->root;
		ptr->index = 0;
	}
}

static int next_sequence_group_(struct sequence_group *group, int *ret)
{
	int check;
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		Return(next_sequence_iterator_(data[i], &temp, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

_g int count_sequence_group_(struct sequence_group *group, size_t *ret)
{
	int check;
	size_t size;

	for (size = 0; ; size++) {
		Return(next_sequence_group_(group, &check));
		if (! check)
			break;
	}
	group->callsize = size;
	if (ret)
		*ret = size;

	return 0;
}

