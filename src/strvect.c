#include <string.h>
#include "character.h"
#include "condition.h"
#include "local.h"
#include "heap.h"
#include "memory.h"
#include "integer.h"
#include "sequence.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  buffer compare
 */
#define equal_code(p1, p2, s1, s2) { \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		if ((unicode)p1[i] != (unicode)p2[i]) { \
			return 0; \
		} \
	} \
	return 1; \
}

#define equalp_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a != b) return 0; \
	} \
	return 1; \
}

#define compare_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

#define comparep_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

static int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 != s2)
		return 0;
	return memcmp(p1, p2, s1 * sizeoft(unicode)) == 0;
}

static int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 < s2)
		return -1;
	if (s1 > s2)
		return 1;
	return memcmp(p1, p2, s1 * sizeoft(unicode));
}

static int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}

static int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equal_code(p1, p2, s1, s2);
}

static int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

static int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	compare_code(p1, p2, s1, s2);
}

static int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}


/*
 *  strvect
 */
void strvect_alloc(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	alloc_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_local(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Check(local == NULL, "local error");
	local_body(local, &pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}
void strvect_heap(addr *ret, size_t len)
{
	addr pos;

	heap_body(&pos, LISPTYPE_STRING, StringBodyLength(len));
	SetStringSize(pos, len);
	*ret = pos;
}

void strvect_copy_alloc(LocalRoot local, addr *ret, addr value)
{
	addr pos;
	unicode *dst;
	const unicode *src;
	size_t size;

	CheckType(value, LISPTYPE_STRING);
	/* source */
	GetStringSize(value, &size);
	GetStringUnicode(value, (const unicode **)&src);
	/* destination */
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&dst);
	/* copy */
	memcpy(dst, src, sizeoft(unicode) * size);
	/* result */
	*ret = pos;
}
void strvect_copy_local(LocalRoot local, addr *ret, addr value)
{
	CheckLocal(local);
	strvect_copy_alloc(local, ret, value);
}
void strvect_copy_heap(addr *ret, addr value)
{
	strvect_copy_alloc(NULL, ret, value);
}

int strvect_character_alloc_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_alloc_(local, ret, &c, 1);
}
int strvect_character_local_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	Check(local == NULL, "local error");
	GetCharacter(pos, &c);
	return strvect_sizeu_local_(local, ret, &c, 1);
}
int strvect_character_heap_(addr *ret, addr pos)
{
	unicode c;
	GetCharacter(pos, &c);
	return strvect_sizeu_heap_(ret, &c, 1);
}

void strvect_length(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type left error");
	GetStringSize(pos, ret);
}

void strvect_posbodylen(addr pos, const unicode **body, size_t *len)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type error");
	GetStringSize(pos, len);
	GetStringUnicode(pos, body);
}

enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u)
{
	if (type == CHARACTER_TYPE_EMPTY) {
		return character_type(u);
	}
	if (isStandardType(u)) {
		return type;
	}
	if (isBaseType(u)) {
		if (type == CHARACTER_TYPE_STANDARD)
			return CHARACTER_TYPE_BASE;
		return type;
	}
	if (isExtendedType(u)) {
		if (type != CHARACTER_TYPE_INVALID && type != CHARACTER_TYPE_EXTENDED)
			return CHARACTER_TYPE_EXTENDED;
		return type;
	}
	return CHARACTER_TYPE_INVALID;
}

int strvect_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	enum CHARACTER_TYPE type;
	const unicode *body;
	size_t i, size;

	strvect_posbodylen(pos, &body, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		type = unicode_character_type(type, body[i]);
		if (type == CHARACTER_TYPE_INVALID)
			return fmte_("Invalid character code.", NULL);
	}

	return Result(ret, type);
}

int strvectp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING;
}

int strvect_base_p_(addr pos, int *ret)
{
	enum CHARACTER_TYPE type;

	if (! strvectp(pos))
		return Result(ret, 0);
	Return(strvect_character_type_(pos, &type));
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return Result(ret, 1);

		default:
			return Result(ret, 0);
	}
}

int strvect_simple_p(addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	return 0;
}

void strvect_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	*ret = pos;
}
void strvect_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	strvect_char_alloc(local, ret, arg);
}
void strvect_char_heap(addr *ret, const char *arg)
{
	strvect_char_alloc(NULL, ret, arg);
}

addr stringh(const char *arg) /* for debug */
{
	addr pos;
	strvect_char_heap(&pos, arg);
	return pos;
}

int strvect_sizeu_alloc_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeoft(unicode) * size);
	return Result(ret, pos);
}
int strvect_sizeu_local_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strvect_sizeu_alloc_(local, ret, arg, size);
}
int strvect_sizeu_heap_(addr *ret, const unicode *arg, size_t size)
{
	return strvect_sizeu_alloc_(NULL, ret, arg, size);
}


/*
 *  strvect_equal
 */
int strvect_equal_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equal(body, right, size1, size2);
}

int strvect_equalp_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equalp(body, right, size1, size2);
}

int strvect_equal_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equal(body1, (const byte *)body2, size1, size2);
}

int strvect_equalp_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equalp(body1, (const byte *)body2, size1, size2);
}

int strvect_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equal_binary(left, body, size);
}

int strvect_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equalp_binary(left, body, size);
}

int strvect_character_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);

	return size == 1 && body[0] == RefCharacter(right);
}

int strvect_character_equalp(addr left, addr right)
{
	const unicode *body;
	unicode a, b;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);
	if (size != 1)
		return 0;
	a = body[0];
	GetCharacter(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}


/*
 *  strvect_compare
 */
int strvect_compare_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_compare(body, right, size1, size2);
}

int strvect_comparep_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_comparep(body, right, size1, size2);
}

int strvect_compare_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_compare(body1, (const byte *)body2, size1, size2);
}

int strvect_comparep_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_comparep(body1, (const byte *)body2, size1, size2);
}

int strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_compare_binary(left, body, size);
}

int strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_comparep_binary(left, body, size);
}

int strvect_designator_equal_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equal_char(left, right);
	if (strvectp(left))
		return strvect_equal_char(left, right);

	return 0;
}

int strvect_designator_equalp_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equalp_char(left, right);
	if (strvectp(left))
		return strvect_equalp_char(left, right);

	return 0;
}


/*
 *  getc/setc
 */
void strvect_getc(addr pos, size_t index, unicode *c)
{
	const unicode *body;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, &body);
	*c =  body[index];
}

void strvect_setc_unsafe(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;
}

int strvect_setc_(addr pos, size_t index, unicode c)
{
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif

	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;

	return 0;
}

int strvect_setall_(addr pos, unicode c)
{
	unicode *destroy;
	size_t size, i;

	strvect_length(pos, &size);
	if (size == 0)
		return 0;
	if (character_type(c) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = c;

	return 0;
}

int strvect_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	strvect_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	strvect_getc(pos, index, &c);
	character_alloc(local, ret, c);

	return 0;
}

int strvect_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (! consp(args)) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = 0;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = 0;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return strvect_get_(local, pos, index, ret);
}

int strvect_set_(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	if (! characterp(value))
		return fmte_("SETF arg ~S must be a character type.", value, NULL);
	strvect_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);

	return strvect_setc_(pos, index, RefCharacter(value));
}

int strvect_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return strvect_set_(pos, index, value);
}

int strvect_fill_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	unicode c;

	/* argument */
	if (! characterp(item))
		return fmte_("FILL tem ~S must be a character type.", item, NULL);
	GetCharacter(item, &c);
	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++)
		strvect_setc_unsafe(pos, index1, c);

	return 0;
}

int strvect_subseq_alloc_(LocalRoot local, addr *ret, addr pos, size_t x, size_t y)
{
	unicode *data1;
	const unicode *data2;
	addr root;
	size_t diff;

	Check(y < x, "index error");
	diff = y - x;
	strvect_alloc(local, &root, diff);
	GetStringUnicode(root, &data1);
	GetStringUnicode(pos, &data2);
	memcpy(data1, data2 + x, diff * sizeoft(unicode));

	return Result(ret, root);
}

int strvect_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2)
{
	return strvect_subseq_alloc_(NULL, ret, pos, index1, index2);
}

int strvect_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	strvect_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return strvect_subseq_index_(ret, pos, index1, index2);
}

int strvect_setget_(addr pos1, size_t index1, addr pos2, size_t index2)
{
	unicode value;

	strvect_getc(pos2, index2, &value);
	return strvect_setc_(pos1, index1, value);
}

int strvect_reverse_(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, x, y;

	strvect_length(pos, &size);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		strvect_getc(pos, x, &c);
		Return(strvect_setc_(one, y, c));
	}

	return Result(ret, one);
}

int strvect_nreverse_(addr *ret, addr pos)
{
	unicode a, b;
	size_t size, x, y;

	strvect_length(pos, &size);
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		strvect_getc(pos, x, &a);
		strvect_getc(pos, y, &b);
		Return(strvect_setc_(pos, x, b));
		Return(strvect_setc_(pos, y, a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/*
 *  make
 */
int strvect_char1_heap_(addr *ret, const char *arg, unicode c)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_heap(&pos, size + 1ULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	destroy[i] = c;
	return Result(ret, pos);
}

int strvect_size1_heap_(addr *ret, const char *arg, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t i;

	strvect_alloc(NULL, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	return Result(ret, pos);
}

