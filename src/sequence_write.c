#include "cons.h"
#include "cons_list.h"
#include "sequence.h"
#include "sequence_write.h"

/*
 *  sequence write
 */
_g void build_sequence_write_list(struct sequence_write *ptr)
{
	ptr->listp = 1;
	ptr->reverse = 0;
	ptr->pos = Nil;
	ptr->index = 0;
	ptr->size = 0;
}

_g void build_sequence_write_result(struct sequence_write *ptr, addr pos)
{
	ptr->listp = 0;
	ptr->reverse = 0;
	ptr->pos = pos;
	ptr->index = 0;
	ptr->size = 0;
}

_g void build_sequence_write(struct sequence_write *ptr, addr pos)
{
	size_t size;

	if (listp_sequence(pos)) {
		build_sequence_write_list(ptr);
	}
	else {
		size = length_sequence(pos, 1);
		ptr->listp = 0;
		ptr->reverse = 0;
		ptr->pos = pos;
		ptr->size = size;
		ptr->index = 0;
	}
}

static addr nreverse_sequence_write(struct sequence_write *ptr)
{
	nreverse_list_unsafe(&ptr->pos, ptr->pos);
	return ptr->pos;
}

_g addr result_sequence_write(struct sequence_write *ptr)
{
	if (ptr->listp)
		return nreverse_sequence_write(ptr);
	else
		return ptr->pos;
}

_g void push_sequence_write(struct sequence_write *ptr, addr pos)
{
	size_t index;
	if (ptr->listp) {
		cons_heap(&ptr->pos, pos, ptr->pos);
		return;
	}
	if (ptr->reverse) {
		ptr->revsize--;
		index = ptr->revbase + ptr->revsize;
		setelt_sequence(ptr->pos, index, pos);
		if (ptr->revsize == 0)
			ptr->reverse = 0;
	}
	else {
		setelt_sequence(ptr->pos, ptr->index, pos);
	}
	ptr->index++;
}

_g void before_sequence_write(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	size = range->start;
	pos = range->pos;
	if (range->listp) {
		for (i = 0; i < size; i++) {
			getcons(pos, &value, &pos);
			push_sequence_write(ptr, value);
		}
	}
	else {
		for (i = 0; i < size; i++) {
			getelt_sequence(NULL, pos, i, &value);
			push_sequence_write(ptr, value);
		}
	}
}

_g void after_sequence_write(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	if (range->listp) {
		pos = range->list;
		while (pos != Nil) {
			getcons(pos, &value, &pos);
			push_sequence_write(ptr, value);
		}
	}
	else {
		pos = range->pos;
		size = length_sequence(pos, 1);
		for (i = range->end; i < size; i++) {
			getelt_sequence(NULL, pos, i, &value);
			push_sequence_write(ptr, value);
		}
	}
}

_g void reverse_sequence_write(struct sequence_write *ptr, size_t size)
{
	Check(ptr->listp, "type error");
	if (size <= 1) return;
	ptr->reverse = 1;
	ptr->revsize = size;
	ptr->revbase = ptr->index;
}

