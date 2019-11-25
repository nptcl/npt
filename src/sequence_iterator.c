#include "cons.h"
#include "cons_list.h"
#include "sequence.h"
#include "sequence_iterator.h"

/*
 *  sequence-iterator
 */
_g struct sequence_iterator *make_sequence_iterator_local(
		LocalRoot local, addr pos, int fill)
{
	unsigned check;
	struct sequence_iterator *ptr;

	Check(local == NULL, "local error");
	ptr = (struct sequence_iterator *)lowlevel_local(local,
			sizeoft(struct sequence_iterator));
	ptr->pos = pos;
	ptr->root = pos;
	check = listp(pos);
	ptr->listp = check;
	ptr->size = check? 0: length_sequence(pos, fill);
	ptr->index = 0;

	return ptr;
}

_g int end_sequence_iterator(struct sequence_iterator *ptr)
{
	if (ptr->listp)
		return ptr->pos == Nil;
	else
		return ptr->size <= ptr->index;
}

_g void length_sequence_iterator(struct sequence_iterator *ptr, size_t *ret)
{
	if (ptr->listp)
		*ret = length_list_safe(ptr->root);
	else
		*ret = ptr->size;
}

_g int object_sequence_iterator(struct sequence_iterator *iter, addr *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return 0;
		getcons(iter->pos, ret, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return 0;
		getelt_sequence(NULL, iter->pos, iter->index, ret);
	}
	iter->index++;

	return 1;
}

static int next_sequence_iterator(struct sequence_iterator *iter, addr *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return 0;
		getcons(iter->pos, ret, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return 0;
	}
	iter->index++;

	return 1;
}

_g int set_sequence_iterator(struct sequence_iterator *iter, addr value)
{
	if (iter->listp) {
		setcar(iter->pos, value);
		getcdr(iter->pos, &(iter->pos));
		iter->index++;
		return iter->pos == Nil;
	}
	else {
		setelt_sequence(iter->pos, iter->index, value);
		iter->index++;
		return iter->size <= iter->index;
	}
}


/*
 *  sequence-group
 */
_g struct sequence_group *make_sequence_group_local(
		LocalRoot local, addr rest, int fill)
{
	struct sequence_group *ptr;
	struct sequence_iterator **data;
	addr pos;
	size_t size, i;

	size = length_list_safe(rest);
	ptr = (struct sequence_group *)lowlevel_local(local,
			sizeoft(struct sequence_group));
	data  = (struct sequence_iterator **)lowlevel_local(local,
			sizeoft(struct sequence_iterator *) * size);

	for (i = 0; i < size; i++) {
		GetCons(rest, &pos, &rest);
		data[i] = make_sequence_iterator_local(local, pos, fill);
	}
	ptr->data = data;
	ptr->size = size;
	ptr->callsize = 0;
	ptr->list = NULL;

	return ptr;
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
	if (ret) *ret = root;
}

_g int set_sequence_group(struct sequence_group *group, addr list)
{
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		if (! object_sequence_iterator(data[i], &temp))
			return 0;
		Check(! consp(list), "list error");
		SetCar(list, temp);
		GetCdr(list, &list);
	}

	return 1;
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

static int next_sequence_group(struct sequence_group *group)
{
	struct sequence_iterator **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		if (! next_sequence_iterator(data[i], &temp))
			return 0;
	}

	return 1;
}

_g void count_sequence_group(struct sequence_group *group, size_t *ret)
{
	size_t size;

	for (size = 0; next_sequence_group(group); size++)
		continue;
	group->callsize = size;
	if (ret) *ret = size;
}

