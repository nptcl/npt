#include "cons.h"
#include "cons_list.h"
#include "sequence.h"
#include "sequence_write.h"

/*
 *  sequence write
 */
void build_sequence_write_list(struct sequence_write *ptr)
{
	ptr->listp = 1;
	ptr->reverse = 0;
	ptr->pos = Nil;
	ptr->index = 0;
	ptr->size = 0;
}

void build_sequence_write_result(struct sequence_write *ptr, addr pos)
{
	ptr->listp = 0;
	ptr->reverse = 0;
	ptr->pos = pos;
	ptr->index = 0;
	ptr->size = 0;
}

int build_sequence_write_(struct sequence_write *ptr, addr pos)
{
	int check;
	size_t size;

	Return(listp_sequence_(pos, &check));
	if (check) {
		build_sequence_write_list(ptr);
	}
	else {
		Return(length_sequence_(pos, 1, &size));
		ptr->listp = 0;
		ptr->reverse = 0;
		ptr->pos = pos;
		ptr->size = size;
		ptr->index = 0;
	}

	return 0;
}

static addr nreverse_sequence_write(struct sequence_write *ptr)
{
	nreverse(&ptr->pos, ptr->pos);
	return ptr->pos;
}

addr result_sequence_write(struct sequence_write *ptr)
{
	if (ptr->listp)
		return nreverse_sequence_write(ptr);
	else
		return ptr->pos;
}

int push_sequence_write_(struct sequence_write *ptr, addr pos)
{
	size_t index;
	if (ptr->listp) {
		cons_heap(&ptr->pos, pos, ptr->pos);
		return 0;
	}
	if (ptr->reverse) {
		ptr->revsize--;
		index = ptr->revbase + ptr->revsize;
		Return(setelt_sequence_(ptr->pos, index, pos));
		if (ptr->revsize == 0)
			ptr->reverse = 0;
	}
	else {
		Return(setelt_sequence_(ptr->pos, ptr->index, pos));
	}
	ptr->index++;

	return 0;
}

int before_sequence_write_(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	size = range->start;
	pos = range->pos;
	if (range->listp) {
		for (i = 0; i < size; i++) {
			Return_getcons(pos, &value, &pos);
			Return(push_sequence_write_(ptr, value));
		}
	}
	else {
		for (i = 0; i < size; i++) {
			Return(getelt_sequence_(NULL, pos, i, &value));
			Return(push_sequence_write_(ptr, value));
		}
	}

	return 0;
}

int after_sequence_write_(struct sequence_write *ptr, struct sequence_range *range)
{
	addr pos, value;
	size_t size, i;

	if (range->listp) {
		pos = range->list;
		while (pos != Nil) {
			Return_getcons(pos, &value, &pos);
			Return(push_sequence_write_(ptr, value));
		}
	}
	else {
		pos = range->pos;
		Return(length_sequence_(pos, 1, &size));
		for (i = range->end; i < size; i++) {
			Return(getelt_sequence_(NULL, pos, i, &value));
			Return(push_sequence_write_(ptr, value));
		}
	}

	return 0;
}

void reverse_sequence_write(struct sequence_write *ptr, size_t size)
{
	Check(ptr->listp, "type error");
	if (size <= 1)
		return;
	ptr->reverse = 1;
	ptr->revsize = size;
	ptr->revbase = ptr->index;
}

