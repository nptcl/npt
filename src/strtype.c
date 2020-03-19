#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "character.h"
#include "condition.h"
#include "type_table.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

#define strvect_string_p(x) (GetType(x) == LISPTYPE_STRING)
#define GetArrayUnicode(x,v) (*(v) = (unicode *)array_ptrwrite((x), 0))

/*
 *  string check
 */
_g int array_stringp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->dimension == 1 && str->type == ARRAY_TYPE_CHARACTER;
}

_g int strarrayp(addr pos)
{
	if (GetType(pos) != LISPTYPE_ARRAY) return 0;
	return array_stringp(pos);
}

_g int stringp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING || strarrayp(pos);
}

_g int string_designer_p(addr pos)
{
	return stringp(pos) || symbolp(pos) || characterp(pos);
}

_g int string_base_p(addr pos)
{
	if (strvectp(pos))
		return strvect_base_p(pos);

	if (strarrayp(pos))
		return strarray_base_p(pos);

	return 0;
}

_g int string_simple_p(addr pos)
{
	if (strvectp(pos))
		return strvect_simple_p(pos);

	if (strarrayp(pos))
		return strarray_simple_p(pos);

	return 0;
}

_g int strarray_base_p(addr pos)
{
	enum CHARACTER_TYPE type;

	if (! strarrayp(pos))
		return 0;
	strarray_update_character_type(pos);
	GetCharacterType(pos, &type);
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return 1;

		default:
			return 0;
	}
}

_g int strarray_simple_p(addr pos)
{
	return strarrayp(pos) && array_simple_p(pos);
}

_g void strarray_update_character_type(addr pos)
{
	enum CHARACTER_TYPE type;
	unicode c;
	size_t i, size;

	strarray_length(pos, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		strarray_getc(pos, i, &c);
		type = unicode_character_type(type, c);
		if (type == CHARACTER_TYPE_INVALID)
			_fmte("Invalid character code.", NULL);
	}
	SetCharacterType(pos, type);
}


/*
 *  strarray
 */
_g addr strarray_allocr(LocalRoot local, size_t len)
{
	addr pos;

	array_alloc(local, &pos, 1, len);
	array_character_alloc(local, pos);
	SetCharacterType(pos, CHARACTER_TYPE_EMPTY);

	return pos;
}
_g addr strarray_localr(LocalRoot local, size_t len)
{
	Check(local == NULL, "local error");
	return strarray_allocr(local, len);
}
_g addr strarray_heapr(size_t len)
{
	return strarray_allocr(NULL, len);
}
_g void strarray_alloc(LocalRoot local, addr *ret, size_t len)
{
	*ret = strarray_allocr(local, len);
}
_g void strarray_local(LocalRoot local, addr *ret, size_t len)
{
	Check(local == NULL, "local error");
	*ret = strarray_allocr(local, len);
}
_g void strarray_heap(addr *ret, size_t len)
{
	*ret = strarray_allocr(NULL, len);
}

_g addr strarray_char_allocr(LocalRoot local, const char *arg)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strarray_alloc(local, &pos, size);
	GetArrayUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	strarray_update_character_type(pos);

	return pos;
}
_g addr strarray_char_localr(LocalRoot local, const char *arg)
{
	Check(local == NULL, "local error");
	return strarray_char_allocr(local, arg);
}
_g addr strarray_char_heapr(const char *arg)
{
	return strarray_char_allocr(NULL, arg);
}
_g void strarray_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	*ret = strarray_char_allocr(local, arg);
}
_g void strarray_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	*ret = strarray_char_allocr(local, arg);
}
_g void strarray_char_heap(addr *ret, const char *arg)
{
	*ret = strarray_char_allocr(NULL, arg);
}

_g addr strarray_size1_allocr(LocalRoot local, const char *arg, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t i;

	strarray_alloc(local, &pos, size);
	GetArrayUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	strarray_update_character_type(pos);

	return pos;
}
_g addr strarray_size1_localr(LocalRoot local, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_size1_allocr(local, arg, size);
}
_g addr strarray_size1_heapr(const char *arg, size_t size)
{
	return strarray_size1_allocr(NULL, arg, size);
}
_g void strarray_size1_alloc(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	*ret = strarray_size1_allocr(local, arg, size);
}
_g void strarray_size1_local(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strarray_size1_allocr(local, arg, size);
}
_g void strarray_size1_heap(addr *ret, const char *arg, size_t size)
{
	*ret = strarray_size1_allocr(NULL, arg, size);
}

_g addr strarray_sizeu_allocr(LocalRoot local, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strarray_alloc(local, &pos, size);
	GetArrayUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeof(unicode) * size);
	strarray_update_character_type(pos);

	return pos;
}
_g addr strarray_sizeu_localr(LocalRoot local, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_sizeu_allocr(local, arg, size);
}
_g addr strarray_sizeu_heapr(const unicode *arg, size_t size)
{
	return strarray_sizeu_allocr(NULL, arg, size);
}
_g void strarray_sizeu_alloc(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	*ret = strarray_sizeu_allocr(local, arg, size);
}
_g void strarray_sizeu_local(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strarray_sizeu_allocr(local, arg, size);
}
_g void strarray_sizeu_heap(addr *ret, const unicode *arg, size_t size)
{
	*ret = strarray_sizeu_allocr(NULL, arg, size);
}

_g void strarray_length(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->front;
}

_g void strarray_length_buffer(addr pos, size_t *ret)
{
	Check(! array_stringp(pos), "string type error");
	*ret = ArrayInfoStruct(pos)->size;
}

_g unicode strarray_refc(addr pos, size_t index)
{
	unicode u;

	Check(! array_stringp(pos), "string type error");
	array_get_unicode(pos, index, &u);
	return u;
}

_g void strarray_getc(addr pos, size_t index, unicode *u)
{
	*u = strarray_refc(pos, index);
}

_g void strarray_setc(addr pos, size_t index, unicode u)
{
	enum CHARACTER_TYPE type;

	Check(! array_stringp(pos), "string type error");
	type = unicode_character_type(RefCharacterType(pos), u);
	if (type == CHARACTER_TYPE_INVALID)
		_fmte("Invalid character code.", NULL);
	SetCharacterType(pos, type);
	*(unicode *)array_ptrwrite(pos, index) = u;
}

_g int strarray_equal_binary(addr left, const unicode *right, size_t size)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return 0;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		b = right[i];
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_equalp_binary(addr left, const unicode *right, size_t size)
{
	unicode a, b;
	size_t check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &check);
	if (size != check)
		return 0;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_equal_char(addr left, const char *right)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return 0;
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		b = (unicode)body[i];
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_equalp_char(addr left, const char *right)
{
	const byte *body;
	unicode a, b;
	size_t size, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	if (size != strlen(right))
		return 0;
	body = (const byte *)right;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_equal(addr left, addr right)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return 0;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		strarray_getc(right, i, &b);
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_equalp(addr left, addr right)
{
	unicode a, b;
	size_t size, check, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size);
	strarray_length(right, &check);
	if (size != check)
		return 0;
	for (i = 0; i < size; i++) {
		strarray_getc(left, i, &a);
		strarray_getc(right, i, &b);
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b)
			return 0;
	}

	return 1;
}

_g int strarray_character_equal(addr left, addr right)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return 0;
	strarray_getc(left, 0, &a);
	GetCharacter(right, &b);
	return a == b;
}

_g int strarray_character_equalp(addr left, addr right)
{
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_length(left, &size);
	if (size != 1)
		return 0;
	strarray_getc(left, 0, &a);
	GetCharacter(right, &b);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	return a == b;
}

_g int strarray_compare_binary(addr left, const unicode *right, size_t size2)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		b = right[i];
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

_g int strarray_comparep_binary(addr left, const unicode *right, size_t size2)
{
	unicode a, b;
	size_t size1, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		b = right[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

_g int strarray_compare_char(addr left, const char *right)
{
	const byte *body; 
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		b = (unicode)body[i];
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

_g int strarray_comparep_char(addr left, const char *right)
{
	const byte *body; 
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	size2 = strlen(right);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	body = (const byte *)right;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		b = (unicode)body[i];
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

_g int strarray_compare(addr left, addr right)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		strarray_getc(right, i, &b);
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}

_g int strarray_comparep(addr left, addr right)
{
	unicode a, b;
	size_t size1, size2, i;

	Check(! strarrayp(left), "type error");
	strarray_length(left, &size1);
	strarray_length(right, &size2);
	if (size1 < size2)
		return -1;
	if (size1 > size2)
		return 1;
	for (i = 0; i < size1; i++) {
		strarray_getc(left, i, &a);
		strarray_getc(right, i, &b);
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a < b)
			return -1;
		if (a > b)
			return 1;
	}

	return 0;
}


/*
 *  string
 */
static void copy_strvect_strarray(addr vector, addr array, size_t size)
{
	unicode c;
	size_t i;

	Check(! strvectp(vector), "type error");
	Check(! strarrayp(array), "type error");
	for (i = 0; i < size; i++) {
		strarray_getc(array, i, &c);
		strvect_setc(vector, i, c);
	}
}

_g addr string_allocr(LocalRoot local, addr pos)
{
	const unicode *body;
	addr vector;
	size_t size;

	if (strarrayp(pos)) {
		strarray_length(pos, &size);
		strvect_alloc(local, &vector, size);
		copy_strvect_strarray(vector, pos, size);
		return vector;
	}
	if (strvectp(pos)) {
		strvect_posbodylen(pos, &body, &size);
		return strvect_sizeu_allocr(local, body, size);
	}

	/* error */
	Abort("type error.");
	return NULL;
}

_g addr string_localr(LocalRoot local, addr pos)
{
	Check(local == NULL, "local error");
	return string_allocr(local, pos);
}

_g addr string_heapr(addr pos)
{
	return string_allocr(NULL, pos);
}

_g void string_alloc(LocalRoot local, addr *ret, addr pos)
{
	*ret = string_allocr(local, pos);
}

_g void string_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	*ret = string_allocr(local, pos);
}

_g void string_heap(addr *ret, addr pos)
{
	*ret = string_allocr(NULL, pos);
}

_g void string_length(addr pos, size_t *ret)
{
	if (strvect_string_p(pos)) {
		strvect_length(pos, ret);
		return;
	}
	if (strarrayp(pos)) {
		strarray_length(pos, ret);
		return;
	}
	_fmte("Argument ~S must be a string type.", pos, NULL);
}

_g unicode string_refc(addr pos, size_t index)
{
	if (strvect_string_p(pos))
		return strvect_refc(pos, index);
	if (strarrayp(pos))
		return strarray_refc(pos, index);
	_fmte("Argument ~S must be a string type.", pos, NULL);
	return 0;
}

_g void string_getc(addr pos, size_t index, unicode *u)
{
	*u = string_refc(pos, index);
}

_g void string_setc(addr pos, size_t index, unicode u)
{
	if (strvect_string_p(pos)) {
		strvect_setc(pos, index, u);
		return;
	}
	if (strarrayp(pos)) {
		strarray_setc(pos, index, u);
		return;
	}
	_fmte("Argument ~S must be a string type.", pos, NULL);
}

_g int string_equal_binary(addr left, const unicode *right, size_t len)
{
	if (strvect_string_p(left))
		return strvect_equal_binary(left, right, len);
	if (strarrayp(left))
		return strarray_equal_binary(left, right, len);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_equalp_binary(addr left, const unicode *right, size_t len)
{
	if (strvect_string_p(left))
		return strvect_equalp_binary(left, right, len);
	if (strarrayp(left))
		return strarray_equalp_binary(left, right, len);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_equal_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_equal_char(left, right);
	if (strarrayp(left))
		return strarray_equal_char(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_equalp_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_equalp_char(left, right);
	if (strarrayp(left))
		return strarray_equalp_char(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

static int strarray_strvect_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equal_binary(left, body, size);
}

_g int string_equal(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_equal(left, right);
		if (strarrayp(right))
			return strarray_strvect_equal(right, left);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equal(left, right);
		if (strarrayp(right))
			return strarray_equal(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

static int strarray_strvect_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type right error");
	Check(! strvect_string_p(right), "type left error");
	strvect_posbodylen(right, &body, &size);

	return strarray_equalp_binary(left, body, size);
}

_g int string_equalp(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_equalp(left, right);
		if (strarrayp(right))
			return strarray_strvect_equalp(right, left);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_equalp(left, right);
		if (strarrayp(right))
			return strarray_equalp(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_character_equal(addr left, addr right)
{
	if (strvect_string_p(left))
		return strvect_character_equal(left, right);
	else if (strarrayp(left))
		return strarray_character_equal(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_character_equalp(addr left, addr right)
{
	if (strvect_string_p(left))
		return strvect_character_equalp(left, right);
	else if (strarrayp(left))
		return strarray_character_equalp(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_compare_binary(addr left, const unicode *right, size_t size2)
{
	if (strvect_string_p(left))
		return strvect_compare_binary(left, right, size2);
	if (strarrayp(left))
		return strarray_compare_binary(left, right, size2);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_comparep_binary(addr left, const unicode *right, size_t size2)
{
	if (strvect_string_p(left))
		return strvect_comparep_binary(left, right, size2);
	if (strarrayp(left))
		return strarray_comparep_binary(left, right, size2);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_compare_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_compare_char(left, right);
	if (strarrayp(left))
		return strarray_compare_char(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_comparep_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_comparep_char(left, right);
	if (strarrayp(left))
		return strarray_comparep_char(left, right);
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

static int strarray_strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_compare_binary(left, body, size);
}

static int strvect_strarray_compare(addr left, addr right)
{
	return -strarray_strvect_compare(right, left);
}

static int strarray_strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_comparep_binary(left, body, size);
}

static int strvect_strarray_comparep(addr left, addr right)
{
	return -strarray_strvect_comparep(right, left);
}

_g int string_compare(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_compare(left, right);
		if (strarrayp(right))
			return strvect_strarray_compare(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_compare(left, right);
		if (strarrayp(right))
			return strarray_compare(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_comparep(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_comparep(left, right);
		if (strarrayp(right))
			return strvect_strarray_comparep(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_comparep(left, right);
		if (strarrayp(right))
			return strarray_comparep(left, right);
		_fmte("Argument ~S must be a string type.", right, NULL);
		return 0;
	}
	_fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

_g int string_designer_equal(addr left, addr right)
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
		return string_equal(left, right);
	if (check3 && check4)
		return character_equal(left, right);
	if (check1 && check4)
		return string_character_equal(left, right);
	if (check2 && check3)
		return string_character_equal(right, left);

	return 0;
}

static int character_equal_char(addr left, const char *right)
{
	unicode a, b;

	Check(! characterp(left), "type error");
	b = right[0];
	if (b == 0 || right[1] != 0)
		return 0;
	GetCharacter(left, &a);
	return a == b;
}

_g int string_designer_equal_char(addr left, const char *right)
{
	if (symbolp(left))
		GetNameSymbol(left, &left);
	if (characterp(left))
		return character_equal_char(left, right);
	if (stringp(left))
		return string_equal_char(left, right);
	return 0;
}

_g int string_designer_alloc(LocalRoot local, addr *ret, addr pos)
{
	if (stringp(pos)) {
		*ret = pos;
		return 1;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, ret);
		return 1;
	}
	if (characterp(pos)) {
		strvect_character_alloc(local, ret, pos);
		return 1;
	}

	return 0;
}

_g int string_designer_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return string_designer_alloc(local, ret, pos);
}

_g int string_designer_heap(addr *ret, addr pos)
{
	return string_designer_alloc(NULL, ret, pos);
}

_g int string_designer_string(addr *ret, addr pos)
{
	if (stringp(pos)) {
		*ret = pos;
		return 1;
	}
	if (symbolp(pos)) {
		GetNameSymbol(pos, ret);
		return 1;
	}

	return 0;
}


/*
 *  concatenate
 */
_g void string_concat_heap(addr *ret, addr a, addr b)
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
		string_getc(a, i, &u);
		string_setc(c, i, u);
	}
	for (i = 0; i < y; i++) {
		string_getc(b, i, &u);
		string_setc(c, i+x, u);
	}
	*ret = c;
}

_g void string_concat_hyphen_heap(addr *ret, addr a, addr b)
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
		string_getc(a, i, &u);
		string_setc(c, i, u);
	}
	string_setc(c, i, (unicode)'-');
	for (i = 0; i < y; i++) {
		string_getc(b, i, &u);
		string_setc(c, i+x+1UL, u);
	}
	*ret = c;
}

_g void string_concat_char1_heap(addr *ret, const char *str, addr b)
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
		string_setc(c, i, (unicode)a[i]);
	for (i = 0; i < y; i++) {
		string_getc(b, i, &u);
		string_setc(c, i+x, u);
	}
	*ret = c;
}

_g void string_concat_char2_heap(addr *ret, addr a, const char *str)
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
		string_getc(a, i, &u);
		string_setc(c, i, u);
	}
	for (i = 0; i < y; i++)
		string_setc(c, i+x, (unicode)b[i]);
	*ret = c;
}

