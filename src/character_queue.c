#include <string.h>
#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "package.h"
#include "strtype.h"
#include "strvect.h"

#define LISP_CHARQUEUESIZE           64

/*
 *  charqueue
 */
static void charbit_size(size_t max, size_t *rsize, size_t *rmax)
{
	size_t size;

	if (0xFFFF < max)
		goto maxsize;
	size = IdxSize + max*sizeoft(unicode);
	if (0xFFFF < size)
		goto maxsize;

	*rmax = max;
	*rsize = size;
	return;

maxsize:
	max = 0x3900;
	size = IdxSize + max*sizeoft(unicode);
	Check(0xFFFF < size, "size error");
	*rmax = max;
	*rsize = size;
}

static void charbit_alloc(LocalRoot local, addr *ret, size_t max, size_t *rmax)
{
	size_t size;

	charbit_size(max, &size, &max);
	if (rmax)
		*rmax = max;
	alloc_arraybody(local, ret, LISPSYSTEM_CHARBIT, 1, (byte16)size);
	SetCharBitSize(*ret, 0);
}

static void charbit_push(addr pos, unicode c)
{
	size_t index;

	GetCharBitSize(pos, &index);
	SetCharBitChar(pos, index, c);
	SetCharBitSize(pos, index + 1);
}

void charqueue_alloc(LocalRoot local, addr *ret, size_t max)
{
	addr pos, root;
	struct charqueue_struct *str;

	if (max == 0)
		max = LISP_CHARQUEUESIZE;
	alloc_smallsize(local, &pos, LISPSYSTEM_CHARQUEUE,
			2, sizeoft(struct charqueue_struct));
	str = StructCharQueue(pos);
	str->size = 0;

	charbit_alloc(local, &root, max, &max);
	str->max = max;

	SetCharQueueRoot(pos, root);
	SetCharQueueTail(pos, root);
	*ret = pos;
}
void charqueue_local(LocalRoot local, addr *ret, size_t max)
{
	Check(local == NULL, "local error");
	charqueue_alloc(local, ret, max);
}
void charqueue_heap(addr *ret, size_t max)
{
	charqueue_alloc(NULL, ret, max);
}

void getsize_charqueue(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, ret);
}

void getchar_charqueue(addr pos, size_t index, unicode *ret)
{
	addr root;
	size_t size, quot, rem;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, &size);
	if (size <= index) {
		*ret =  0;
		return;
	}

	GetCharQueueRoot(pos, &root);
	GetCharQueueMax(pos, &size);
	quot = index / size;
	rem = index % size;
	for (; quot; quot--)
		GetCharBitNext(root, &root);
	Check(root == Nil, "next error");
	GetCharBitChar(root, rem, ret);
}

int push_charqueue_alloc_(LocalRoot local, addr pos, unicode c)
{
	addr tail, next;
	size_t size, max;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	if (! isvalidunicode(c)) {
		fixnum_heap(&pos, (fixnum)c);
		return fmte_("Invalid unicode character, ~A.", pos, NULL);
	}

	GetCharQueueMax(pos, &max);
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	if (max <= size) {
		GetCharBitNext(tail, &next);
		if (next == Nil) {
			/* new bit */
			charbit_alloc(local, &next, max, NULL);
			SetCharBitNext(tail, next);
			SetCharQueueTail(pos, next);
		}
		else {
			/* reuse bit */
			SetCharBitSize(next, 0);
			SetCharQueueTail(pos, next);
		}
		tail = next;
	}

	/* push */
	charbit_push(tail, c);
	IncCharQueueSize(pos);

	return 0;
}
int push_charqueue_local_(LocalRoot local, addr pos, unicode c)
{
	Check(local == NULL, "local error");
	return push_charqueue_alloc_(local, pos, c);
}
int push_charqueue_heap_(addr pos, unicode c)
{
	return push_charqueue_alloc_(NULL, pos, c);
}

void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret)
{
	int readonly;
	addr string, next;
	size_t size, bit, index, i;
	unicode *right;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, &size);
	strvect_alloc(local, &string, size);

	readonly = GetStatusReadOnly(string);
	if (readonly)
		SetStatusValue(string, LISPSTATUS_READONLY, 0);

	GetCharQueueRoot(pos, &next);
	for (index = 0; index < size; ) {
		GetCharBitSize(next, &bit);
		right = (unicode *)PtrCharBitChar(next);
		for (i = 0; i < bit; i++)
			strvect_setc_unsafe(string, index++, right[i]);
		GetCharBitNext(next, &next);
	}

	if (readonly)
		SetStatusValue(string, LISPSTATUS_READONLY, 1);

	*ret = string;
}
void make_charqueue_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	make_charqueue_alloc(local, pos, ret);
}
void make_charqueue_heap(addr pos, addr *ret)
{
	make_charqueue_alloc(NULL, pos, ret);
}

void clear_charqueue(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	SetCharQueueSize(pos, 0);
	GetCharQueueRoot(pos, &root);
	SetCharQueueTail(pos, root);
	SetCharBitSize(root, 0);
}

void free_charqueue(addr pos)
{
	addr root;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	SetCharQueueSize(pos, 0);
	GetCharQueueRoot(pos, &root);
	SetCharQueueTail(pos, root);
	SetCharBitSize(root, 0);

	SetCharBitNext(root, Nil);
}

int position_charqueue(addr pos, size_t size)
{
	size_t check, max, now;
	addr root;

	/* size check */
	GetCharQueueSize(pos, &check);
	if (check < size)
		return 1;
	/* set size */
	GetCharQueueMax(pos, &max);
	GetCharQueueRoot(pos, &root);
	for (now = size; max < now; now -= max) {
		Check(root == Nil, "root error");
		GetCharBitNext(root, &root);
	}
	SetCharBitSize(root, now);
	SetCharQueueSize(pos, size);
	SetCharQueueTail(pos, root);
	GetCharBitNext(root, &root);
	if (root != Nil) {
		SetCharQueueSize(pos, 0);
	}

	return 0;
}

int pushstring_charqueue_alloc_(LocalRoot local, addr pos, addr push)
{
	unicode c;
	size_t i, size;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type charqueue error");
	Check(! stringp(push), "type string error");
	string_length(push, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(push, i, &c));
		Return(push_charqueue_alloc_(local, pos, c));
	}

	return 0;
}
int pushstring_charqueue_local_(LocalRoot local, addr pos, addr push)
{
	Check(local == NULL, "local error");
	return pushstring_charqueue_alloc_(local, pos, push);
}
int pushstring_charqueue_heap_(addr pos, addr push)
{
	return pushstring_charqueue_alloc_(NULL, pos, push);
}

int pushchar_charqueue_alloc_(LocalRoot local, addr pos, const char *str)
{
	const byte *ptr;
	unicode u;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	ptr = (const byte *)str;
	for (;;) {
		u = (unicode)*ptr;
		if (u == 0)
			break;
		ptr++;
		Return(push_charqueue_alloc_(local, pos, u));
	}

	return 0;
}
int pushchar_charqueue_local_(LocalRoot local, addr pos, const char *str)
{
	Check(local == NULL, "local error");
	return pushchar_charqueue_alloc_(local, pos, str);
}
int pushchar_charqueue_heap_(addr pos, const char *str)
{
	return pushchar_charqueue_alloc_(NULL, pos, str);
}

