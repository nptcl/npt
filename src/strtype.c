#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "character.h"
#include "condition.h"
#include "type_table.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"

#define strvect_string_p(x) (GetType(x) == LISPTYPE_STRING)

/*
 *  string check
 */
int array_stringp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->dimension == 1 && str->type == ARRAY_TYPE_CHARACTER;
}

int strarrayp(addr pos)
{
	return arrayp(pos) && array_stringp(pos);
}

int stringp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING || strarrayp(pos);
}

int string_designer_p(addr pos)
{
	return stringp(pos) || symbolp(pos) || characterp(pos);
}

int string_base_p_(addr pos, int *ret)
{
	if (strvectp(pos))
		return strvect_base_p_(pos, ret);

	if (strarrayp(pos))
		return strarray_base_p_(pos, ret);

	return Result(ret, 0);
}

int string_simple_p(addr pos)
{
	if (strvectp(pos))
		return strvect_simple_p(pos);

	if (strarrayp(pos))
		return strarray_simple_p(pos);

	return 0;
}

int string_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	if (strvectp(pos))
		return strvect_character_type_(pos, ret);

	if (strarrayp(pos))
		return strarray_character_type_(pos, ret);

	*ret = CHARACTER_TYPE_INVALID;
	return fmte_("Invalid string object ~S.", pos, NULL);
}

int strarray_base_p_(addr pos, int *ret)
{
	enum CHARACTER_TYPE type;

	if (! strarrayp(pos))
		return Result(ret, 0);

	type = CHARACTER_TYPE_INVALID;
	Return(strarray_character_type_(pos, &type));
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return Result(ret, 1);

		default:
			return Result(ret, 0);
	}
}

int strarray_simple_p(addr pos)
{
	return strarrayp(pos) && array_simple_p(pos);
}

int strarray_character_type_(addr pos, enum CHARACTER_TYPE *ret)
{
	enum CHARACTER_TYPE type;
	unicode c;
	size_t i, size;

	strarray_length(pos, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(pos, i, &c));
		type = unicode_character_type(type, c);
		if (type == CHARACTER_TYPE_INVALID)
			return fmte_("Invalid character code.", NULL);
	}

	return Result(ret, type);
}


/*
 *  strarray
 */
int strarray_alloc_(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Return(array_alloc_(local, &pos, 1, len));
	Return(array_character_alloc_(local, pos));
	return Result(ret, pos);
}

int strarray_local_(LocalRoot local, addr *ret, size_t len)
{
	addr pos;

	Check(local == NULL, "local error");
	Return(array_local_(local, &pos, 1, len));
	Return(array_character_alloc_(local, pos));
	return Result(ret, pos);
}

int strarray_heap_(addr *ret, size_t len)
{
	addr pos;

	Return(array_heap_(&pos, 1, len));
	Return(array_character_alloc_(NULL, pos));
	return Result(ret, pos);
}

int strarray_char_alloc_(LocalRoot local, addr *ret, const char *arg)
{
	addr pos;
	size_t size, i;

	size = strlen(arg);
	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, (unicode)arg[i]));
	}
	return Result(ret, pos);
}
int strarray_char_local_(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	return strarray_char_alloc_(local, ret, arg);
}
int strarray_char_heap_(addr *ret, const char *arg)
{
	return strarray_char_alloc_(NULL, ret, arg);
}

int strarray_size1_alloc_(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	addr pos;
	size_t i;

	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, (unicode)arg[i]));
	}
	return Result(ret, pos);
}
int strarray_size1_local_(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_size1_alloc_(local, ret, arg, size);
}
int strarray_size1_heap_(addr *ret, const char *arg, size_t size)
{
	return strarray_size1_alloc_(NULL, ret, arg, size);
}

int strarray_sizeu_alloc_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	addr pos;
	size_t i;

	Return(strarray_alloc_(local, &pos, size));
	for (i = 0; i < size; i++) {
		Return(strarray_setc_(pos, i, arg[i]));
	}
	return Result(ret, pos);
}
int strarray_sizeu_local_(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_sizeu_alloc_(local, ret, arg, size);
}
int strarray_sizeu_heap_(addr *ret, const unicode *arg, size_t size)
{
	return strarray_sizeu_alloc_(NULL, ret, arg, size);
}

void strarray_length(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->front;
}

void strarray_length_buffer(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->size;
}

int strarray_getc_(addr pos, size_t index, unicode *u)
{
	Check(! array_stringp(pos), "string type error");
	return array_get_unicode_(pos, index, u);
}

int strarray_setc_(addr pos, size_t index, unicode u)
{
	Check(! array_stringp(pos), "string type error");
	if (character_type(u) == CHARACTER_TYPE_INVALID)
		return fmte_("Invalid character code.", NULL);
	return array_set_character_(pos, index, u);
}

int strarray_equal_binary_(addr left, const unicode *right, size_t size, int *ret)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_binary_(addr left, const unicode *right, size_t size, int *ret)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equal_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return Result(ret, 0);
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return Result(ret, 0);
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equal_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_equalp_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int strarray_character_equal_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return Result(ret, 0);
	Return(strarray_getc_(left, 0, &a));
	GetCharacter(right, &b);
	return Result(ret, a == b);
}

int strarray_character_equalp_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return Result(ret, 0);
	Return(strarray_getc_(left, 0, &a));
	GetCharacter(right, &b);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	return Result(ret, a == b);
}

int strarray_compare_binary_(addr left,
		const unicode *right, size_t size2, int *ret)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_binary_(addr left,
		const unicode *right, size_t size2, int *ret)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_compare_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_char_(addr left, const char *right, int *ret)
{
	const byte *body;
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_compare_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

int strarray_comparep_(addr left, addr right, int *ret)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return Result(ret, -1);
	if (size1 > size2)
		return Result(ret, 1);
	for (i = 0; i < size1; i++) {
		Return(strarray_getc_(left, i, &a));
		Return(strarray_getc_(right, i, &b));
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return Result(ret, -1);
		if (a > b)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}


/*
 *  string
 */
static int copy_strvect_strarray_(addr vector, addr array, size_t size)
{
	unicode c;
	size_t i;

	Check(! strvectp(vector), "type error");
	Check(! strarrayp(array), "type error");
	for (i = 0; i < size; i++) {
		Return(strarray_getc_(array, i, &c));
		Return(strvect_setc_(vector, i, c));
	}

	return 0;
}

int string_alloc_(LocalRoot local, addr *ret, addr pos)
{
	const unicode *body;
	addr vector;
	size_t size;

	if (strarrayp(pos)) {
		strarray_length(pos, &size);
		strvect_alloc(local, &vector, size);
		Return(copy_strvect_strarray_(vector, pos, size));
		return Result(ret, vector);
	}
	if (strvectp(pos)) {
		strvect_posbodylen(pos, &body, &size);
		return strvect_sizeu_alloc_(local, ret, body, size);
	}

	/* error */
	*ret = 0;
	return fmte_("type error.", NULL);
}
int string_local_(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return string_alloc_(local, ret, pos);
}

int string_heap_(addr *ret, addr pos)
{
	return string_alloc_(NULL, ret, pos);
}

int strvect_value_heap_(addr *ret, addr pos)
{
	addr dst;
	unicode c;
	size_t size, i;

	if (GetType(pos) == LISPTYPE_STRING)
		return Result(ret, pos);
	string_length(pos, &size);
	strvect_heap(&dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, c));
	}

	return Result(ret, dst);
}

void string_length(addr pos, size_t *ret)
{
	if (strvect_string_p(pos)) {
		strvect_length(pos, ret);
		return;
	}
	if (strarrayp(pos)) {
		strarray_length(pos, ret);
		return;
	}
	*ret = 0;
	Abort("type error.");
}

int string_getc_(addr pos, size_t index, unicode *u)
{
	if (strvect_string_p(pos)) {
		strvect_getc(pos, index, u);
		return 0;
	}
	if (strarrayp(pos))
		return strarray_getc_(pos, index, u);
	*u = 0;
	return fmte_("Argument ~S must be a string type.", pos, NULL);
}

int string_setc_(addr pos, size_t index, unicode u)
{
	if (strvect_string_p(pos))
		return strvect_setc_(pos, index, u);
	if (strarrayp(pos))
		return strarray_setc_(pos, index, u);

	return fmte_("Argument ~S must be a string type.", pos, NULL);
}

int string_equal_binary_(addr left, const unicode *right, size_t len, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equal_binary(left, right, len));
	if (strarrayp(left))
		return strarray_equal_binary_(left, right, len, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equalp_binary_(addr left, const unicode *right, size_t len, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equalp_binary(left, right, len));
	if (strarrayp(left))
		return strarray_equalp_binary_(left, right, len, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equal_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equal_char(left, right));
	if (strarrayp(left))
		return strarray_equal_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_equalp_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_equalp_char(left, right));
	if (strarrayp(left))
		return strarray_equalp_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int stringp_equal_char_(addr left, const char *right, int *ret)
{
	if (! stringp(left))
		return Result(ret, 0);
	else
		return string_equal_char_(left, right, ret);
}

int stringp_equalp_char_(addr left, const char *right, int *ret)
{
	if (! stringp(left))
		return Result(ret, 0);
	else
		return string_equalp_char_(left, right, ret);
}

int string_equalp_char_va_(addr pos, int *ret, ...)
{
	int check, value;
	va_list args;
	const char *str;

	va_start(args, ret);
	value = 0;
	for (;;) {
		str = va_arg(args, const char *);
		if (str == NULL)
			break;
		Return(string_equalp_char_(pos, str, &check));
		if (check) {
			value = 1;
			break;
		}
	}
	va_end(args);

	return Result(ret, value);
}

static int strarray_strvect_equal_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equal_binary_(left, body, size, ret);
}

int string_equal_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_equal(left, right));
		if (strarrayp(right))
			return strarray_strvect_equal_(right, left, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equal_(left, right, ret);
		if (strarrayp(right))
			return strarray_equal_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

static int strarray_strvect_equalp_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equalp_binary_(left, body, size, ret);
}

int string_equalp_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_equalp(left, right));
		if (strarrayp(right))
			return strarray_strvect_equalp_(right, left, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equalp_(left, right, ret);
		if (strarrayp(right))
			return strarray_equalp_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_character_equal_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_character_equal(left, right));
	if (strarrayp(left))
		return strarray_character_equal_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_character_equalp_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_character_equalp(left, right));
	if (strarrayp(left))
		return strarray_character_equalp_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_compare_binary_(addr left, const unicode *right, size_t size2, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_compare_binary(left, right, size2));
	if (strarrayp(left))
		return strarray_compare_binary_(left, right, size2, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_binary_(addr left, const unicode *right, size_t size2, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_comparep_binary(left, right, size2));
	if (strarrayp(left))
		return strarray_comparep_binary_(left, right, size2, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_compare_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_compare_char(left, right));
	if (strarrayp(left))
		return strarray_compare_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_char_(addr left, const char *right, int *ret)
{
	if (strvect_string_p(left))
		return Result(ret, strvect_comparep_char(left, right));
	if (strarrayp(left))
		return strarray_comparep_char_(left, right, ret);
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

static int strarray_strvect_compare_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_compare_binary_(left, body, size, ret);
}

static int strvect_strarray_compare_(addr left, addr right, int *ret)
{
	int check;
	Return(strarray_strvect_compare_(right, left, &check));
	return Result(ret, -check);
}

static int strarray_strvect_comparep_(addr left, addr right, int *ret)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_comparep_binary_(left, body, size, ret);
}

static int strvect_strarray_comparep_(addr left, addr right, int *ret)
{
	int check;
	Return(strarray_strvect_comparep_(right, left, &check));
	return Result(ret, -check);
}

int string_compare_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_compare(left, right));
		if (strarrayp(right))
			return strvect_strarray_compare_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_compare_(left, right, ret);
		if (strarrayp(right))
			return strarray_compare_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_comparep_(addr left, addr right, int *ret)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return Result(ret, strvect_comparep(left, right));
		if (strarrayp(right))
			return strvect_strarray_comparep_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_comparep_(left, right, ret);
		if (strarrayp(right))
			return strarray_comparep_(left, right, ret);
		*ret = 0;
		return fmte_("Argument ~S must be a string type.", right, NULL);
	}
	*ret = 0;
	return fmte_("Argument ~S must be a string type.", left, NULL);
}

int string_designer_equal_(addr left, addr right, int *ret)
{
	int check1, check2, check3, check4;

	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (symbolp(right))
		GetNameSymbol(right, &right);
	check1 = stringp(left);
	check2 = stringp(right);
	check3 = characterp(left);
	check4 = characterp(right);
	if (check1 && check2)
		return string_equal_(left, right, ret);
	if (check3 && check4)
		return Result(ret, character_equal(left, right));
	if (check1 && check4)
		return string_character_equal_(left, right, ret);
	if (check2 && check3)
		return string_character_equal_(right, left, ret);

	return Result(ret, 0);
}

int string_designer_equal_char_(addr left, const char *right, int *ret)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return Result(ret, character_equal_char(left, right));
	if (stringp(left))
		return string_equal_char_(left, right, ret);

	return Result(ret, 0);
}

int string_designer_equalp_(addr left, addr right, int *ret)
{
	int check1, check2, check3, check4;

	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (symbolp(right))
		GetNameSymbol(right, &right);
	check1 = stringp(left);
	check2 = stringp(right);
	check3 = characterp(left);
	check4 = characterp(right);
	if (check1 && check2)
		return string_equalp_(left, right, ret);
	if (check3 && check4)
		return Result(ret, character_equalp(left, right));
	if (check1 && check4)
		return string_character_equalp_(left, right, ret);
	if (check2 && check3)
		return string_character_equalp_(right, left, ret);

	return Result(ret, 0);
}

int string_designer_equalp_char_(addr left, const char *right, int *ret)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return Result(ret, character_equalp_char(left, right));
	if (stringp(left))
		return string_equalp_char_(left, right, ret);

	return Result(ret, 0);
}

int string_designer_alloc_(LocalRoot local, addr *value, addr pos, int *ret)
{
	addr type;

	if (stringp(pos)) {
		*value = pos;
		if (ret)
			*ret = 1;
		return 0;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, value);
		if (ret)
			*ret = 1;
		return 0;
	}
	if (characterp(pos)) {
		Return(strvect_character_alloc_(local, value, pos));
		if (ret)
			*ret = 1;
		return 0;
	}

	if (ret)
		*ret = 0;

	/* error */
	GetTypeTable(&type, StringDesigner);
	return call_type_error_va_(NULL, pos, type,
			"The object ~S is not string-designer.", pos, NULL);
}

int string_designer_local_(LocalRoot local, addr *value, addr pos, int *ret)
{
	Check(local == NULL, "local error");
	return string_designer_alloc_(local, value, pos, ret);
}

int string_designer_heap_(addr *value, addr pos, int *ret)
{
	return string_designer_alloc_(NULL, value, pos, ret);
}

int string_designer_string(addr *value, addr pos)
{
	if (stringp(pos)) {
		*value = pos;
		return 1;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, value);
		return 1;
	}

	return 0;
}


/*
 *  concatenate
 */
int string_concat_heap_(addr *ret, addr a, addr b)
{
	unicode u;
	addr c;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	Check(! stringp(b), "type error");
	string_length(a, &x);
	string_length(b, &y);
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x, u));
	}

	return Result(ret, c);
}

int string_concat_hyphen_heap_(addr *ret, addr a, addr b)
{
	unicode u;
	addr c;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	Check(! stringp(b), "type error");
	string_length(a, &x);
	string_length(b, &y);
	strvect_heap(&c, x + y + 1UL);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	Return(string_setc_(c, i, (unicode)'-'));
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x+1UL, u));
	}

	return Result(ret, c);
}

int string_concat_char1_heap_(addr *ret, const char *str, addr b)
{
	const byte *a;
	addr c;
	unicode u;
	size_t x, y, i;

	Check(! stringp(b), "type error");
	x = strlen(str);
	a = (const byte *)str;
	string_length(b, &y);
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++)
		Return(string_setc_(c, i, (unicode)a[i]));
	for (i = 0; i < y; i++) {
		Return(string_getc_(b, i, &u));
		Return(string_setc_(c, i+x, u));
	}

	return Result(ret, c);
}

int string_concat_char2_heap_(addr *ret, addr a, const char *str)
{
	const byte *b;
	addr c;
	unicode u;
	size_t x, y, i;

	Check(! stringp(a), "type error");
	string_length(a, &x);
	y = strlen(str);
	b = (const byte *)str;
	strvect_heap(&c, x + y);
	for (i = 0; i < x; i++) {
		Return(string_getc_(a, i, &u));
		Return(string_setc_(c, i, u));
	}
	for (i = 0; i < y; i++) {
		Return(string_setc_(c, i+x, (unicode)b[i]));
	}

	return Result(ret, c);
}


/*
 *  case
 */
int string_upper_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	Check(! stringp(pos), "type error");
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (isLowerCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int string_lower_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	Check(! stringp(pos), "type error");
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (isUpperCase(c))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int string_upper_alloc_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr dst;
	size_t size, i;

	string_length(pos, &size);
	strvect_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, toUpperUnicode(c)));
	}

	return Result(ret, dst);
}

int string_upper_local_(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	return string_upper_alloc_(local, pos, ret);
}

int string_upper_heap_(addr pos, addr *ret)
{
	return string_upper_alloc_(NULL, pos, ret);
}

int string_lower_alloc_(LocalRoot local, addr pos, addr *ret)
{
	unicode c;
	addr dst;
	size_t size, i;

	string_length(pos, &size);
	strvect_alloc(local, &dst, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		Return(strvect_setc_(dst, i, toLowerUnicode(c)));
	}

	return Result(ret, dst);
}

int string_lower_local_(LocalRoot local, addr pos, addr *ret)
{
	CheckLocal(local);
	return string_lower_alloc_(local, pos, ret);
}

int string_lower_heap_(addr pos, addr *ret)
{
	return string_lower_alloc_(NULL, pos, ret);
}


/*
 *  debug
 */
int string_equal_char_debug(addr left, const char *right)
{
	int check;
	check = 0;
	Error(string_equal_char_(left, right, &check));
	return check;
}

int string_equalp_char_debug(addr left, const char *right)
{
	int check;
	check = 0;
	Error(string_equalp_char_(left, right, &check));
	return check;
}

int string_equal_debug(addr left, addr right)
{
	int check;
	check = 0;
	Error(string_equal_(left, right, &check));
	return check;
}

