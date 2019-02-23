#include <string.h>
#include "charqueue.h"
#include "constant.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "package.h"
#include "strtype.h"

/*
 *  charqueue
 */
static void charbit_alloc(LocalRoot local, addr *ret, size_t size)
{
	size = IdxSize + size*sizeoft(unicode);
	Check(0xFFFF < size, "size error");
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

addr charqueue_allocr(LocalRoot local, size_t max)
{
	addr pos, root;
	struct charqueue_struct *str;

	if (max == 0) max = CHARQUEUESIZE;
	Check(0xFFFF < max, "size error");
	alloc_smallsize(local, &pos, LISPSYSTEM_CHARQUEUE,
			2, sizeoft(struct charqueue_struct));
	str = StructCharQueue(pos);
	str->size = 0;
	str->max = max;

	charbit_alloc(local, &root, max);
	SetCharQueueRoot(pos, root);
	SetCharQueueTail(pos, root);

	return pos;
}
addr charqueue_heapr(size_t max)
{
	return charqueue_allocr(NULL, max);
}
addr charqueue_localr(LocalRoot local, size_t max)
{
	Check(local == NULL, "local error");
	return charqueue_allocr(local, max);
}
void charqueue_alloc(LocalRoot local, addr *ret, size_t max)
{
	*ret = charqueue_allocr(local, max);
}
void charqueue_heap(addr *ret, size_t max)
{
	*ret = charqueue_allocr(NULL, max);
}
void charqueue_local(LocalRoot local, addr *ret, size_t max)
{
	Check(local == NULL, "local error");
	*ret = charqueue_allocr(local, max);
}

size_t refsize_charqueue(addr pos)
{
	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	return RefCharQueueSize(pos);
}
void getsize_charqueue(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, ret);
}

unicode refchar_charqueue(addr pos, size_t index)
{
	addr root;
	size_t size, quot, rem;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueSize(pos, &size);
	if (size <= index) return 0;

	GetCharQueueRoot(pos, &root);
	GetCharQueueMax(pos, &size);
	quot = index / size;
	rem = index % size;
	for (; quot; quot--)
		GetCharBitNext(root, &root);
	Check(root == Nil, "next error");
	return RefCharBitChar(root, rem);
}
void getchar_charqueue(addr pos, size_t index, unicode *ret)
{
	*ret = refchar_charqueue(pos, index);
}

void push_charqueue_alloc(LocalRoot local, addr pos, unicode c)
{
	addr tail, next;
	size_t size, max;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	GetCharQueueMax(pos, &max);
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	if (max <= size) {
		GetCharBitNext(tail, &next);
		if (next == Nil) {
			/* new bit */
			charbit_alloc(local, &next, max);
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
}
void push_charqueue_heap(addr pos, unicode c)
{
	push_charqueue_alloc(NULL, pos, c);
}
void push_charqueue_local(LocalRoot local, addr pos, unicode c)
{
	Check(local == NULL, "local error");
	push_charqueue_alloc(local, pos, c);
}

addr make_charqueue_allocr(LocalRoot local, addr pos)
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
			strvect_setc(string, index++, right[i]);
		GetCharBitNext(next, &next);
	}

	if (readonly)
		SetStatusValue(string, LISPSTATUS_READONLY, 1);

	return string;
}
addr make_charqueue_heapr(addr pos)
{
	return make_charqueue_allocr(NULL, pos);
}
addr make_charqueue_localr(LocalRoot local, addr pos)
{
	Check(local == NULL, "local error");
	return make_charqueue_allocr(local, pos);
}
void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret)
{
	*ret = make_charqueue_allocr(local, pos);
}
void make_charqueue_heap(addr pos, addr *ret)
{
	make_charqueue_alloc(NULL, pos, ret);
}
void make_charqueue_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	make_charqueue_alloc(local, pos, ret);
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

void pushstring_charqueue_alloc(LocalRoot local, addr pos, addr push)
{
	const unicode *body;
	size_t i, size;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type charqueue error");
	Check(! stringp(push), "type string error");
	string_posbodylen(push, &body, &size);
	for (i = 0; i < size; i++)
		push_charqueue_alloc(local, pos, body[i]);
}
void pushstring_charqueue_heap(addr pos, addr push)
{
	pushstring_charqueue_alloc(NULL, pos, push);
}
void pushstring_charqueue_local(LocalRoot local, addr pos, addr push)
{
	Check(local == NULL, "local error");
	pushstring_charqueue_alloc(local, pos, push);
}

void pushchar_charqueue_alloc(LocalRoot local, addr pos, const char *str)
{
	const byte *ptr;
	unicode u;

	Check(GetType(pos) != LISPSYSTEM_CHARQUEUE, "type error");
	ptr = (const byte *)str;
	for (;;) {
		u = (unicode)*ptr;
		if (u == 0) break;
		ptr++;
		push_charqueue_alloc(local, pos, u);
	}
}
void pushchar_charqueue_heap(addr pos, const char *str)
{
	pushchar_charqueue_alloc(NULL, pos, str);
}
void pushchar_charqueue_local(LocalRoot local, addr pos, const char *str)
{
	Check(local == NULL, "local error");
	pushchar_charqueue_alloc(local, pos, str);
}

