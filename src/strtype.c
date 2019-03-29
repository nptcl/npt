#include "array.h"
#include "array_object.h"
#include "character.h"
#include "condition.h"
#include "type_table.h"
#include "strtype.h"
#include "symbol.h"
#include "unicode.h"

#define strvect_string_p(x) (GetType(x) == LISPTYPE_STRING)
#define GetArrayUnicode(x,v) (*(v) = (unicode *)array_ptrwrite((x), 0))

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
	if (GetType(pos) != LISPTYPE_ARRAY) return 0;
	return array_stringp(pos);
}

int stringp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING || strarrayp(pos);
}

int string_designer_p(addr pos)
{
	return stringp(pos) || symbolp(pos) || characterp(pos);
}

int string_base_p(addr pos)
{
	if (strvectp(pos))
		return strvect_base_p(pos);
	
	if (strarrayp(pos))
		return strarray_base_p(pos);
	
	return 0;
}

int string_simple_p(addr pos)
{
	if (strvectp(pos))
		return strvect_simple_p(pos);
	
	if (strarrayp(pos))
		return strarray_simple_p(pos);
	
	return 0;
}

int strarray_base_p(addr pos)
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

int strarray_simple_p(addr pos)
{
	return strarrayp(pos) && array_simple_p(pos);
}

void strarray_update_character_type(addr pos)
{
	enum CHARACTER_TYPE type;
	const unicode *body;
	size_t i, size;

	strarray_posbodylen(pos, &body, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		type = unicode_character_type(type, body[i]);
		if (type == CHARACTER_TYPE_INVALID)
			fmte("Invalid character code.", NULL);
	}
	SetCharacterType(pos, type);
}


/*
 *  strarray
 */
addr strarray_allocr(LocalRoot local, size_t len)
{
	addr pos;

	array_alloc(local, &pos, 1, len);
	array_character_alloc(local, pos);
	SetCharacterType(pos, CHARACTER_TYPE_EMPTY);

	return pos;
}
addr strarray_localr(LocalRoot local, size_t len)
{
	Check(local == NULL, "local error");
	return strarray_allocr(local, len);
}
addr strarray_heapr(size_t len)
{
	return strarray_allocr(NULL, len);
}
void strarray_alloc(LocalRoot local, addr *ret, size_t len)
{
	*ret = strarray_allocr(local, len);
}
void strarray_local(LocalRoot local, addr *ret, size_t len)
{
	Check(local == NULL, "local error");
	*ret = strarray_allocr(local, len);
}
void strarray_heap(addr *ret, size_t len)
{
	*ret = strarray_allocr(NULL, len);
}

addr strarray_char_allocr(LocalRoot local, const char *arg)
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
addr strarray_char_localr(LocalRoot local, const char *arg)
{
	Check(local == NULL, "local error");
	return strarray_char_allocr(local, arg);
}
addr strarray_char_heapr(const char *arg)
{
	return strarray_char_allocr(NULL, arg);
}
void strarray_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	*ret = strarray_char_allocr(local, arg);
}
void strarray_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	*ret = strarray_char_allocr(local, arg);
}
void strarray_char_heap(addr *ret, const char *arg)
{
	*ret = strarray_char_allocr(NULL, arg);
}

addr strarray_size1_allocr(LocalRoot local, const char *arg, size_t size)
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
addr strarray_size1_localr(LocalRoot local, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_size1_allocr(local, arg, size);
}
addr strarray_size1_heapr(const char *arg, size_t size)
{
	return strarray_size1_allocr(NULL, arg, size);
}
void strarray_size1_alloc(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	*ret = strarray_size1_allocr(local, arg, size);
}
void strarray_size1_local(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strarray_size1_allocr(local, arg, size);
}
void strarray_size1_heap(addr *ret, const char *arg, size_t size)
{
	*ret = strarray_size1_allocr(NULL, arg, size);
}

addr strarray_sizeu_allocr(LocalRoot local, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strarray_alloc(local, &pos, size);
	GetArrayUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeof(unicode) * size);
	strarray_update_character_type(pos);

	return pos;
}
addr strarray_sizeu_localr(LocalRoot local, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strarray_sizeu_allocr(local, arg, size);
}
addr strarray_sizeu_heapr(const unicode *arg, size_t size)
{
	return strarray_sizeu_allocr(NULL, arg, size);
}
void strarray_sizeu_alloc(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	*ret = strarray_sizeu_allocr(local, arg, size);
}
void strarray_sizeu_local(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strarray_sizeu_allocr(local, arg, size);
}
void strarray_sizeu_heap(addr *ret, const unicode *arg, size_t size)
{
	*ret = strarray_sizeu_allocr(NULL, arg, size);
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

void strarray_posbodylen(addr pos, const unicode **body, size_t *len)
{
	Check(! array_stringp(pos), "string type error");
	*body = (const unicode *)array_ptrread(pos, 0);
	*len = ArrayInfoStruct(pos)->front;
}

unicode strarray_refc(addr pos, size_t index)
{
	Check(! array_stringp(pos), "string type error");
	return *(const unicode *)array_ptrread(pos, index);
}

void strarray_getc(addr pos, size_t index, unicode *u)
{
	*u = strarray_refc(pos, index);
}

void strarray_setc(addr pos, size_t index, unicode u)
{
	enum CHARACTER_TYPE type;

	Check(! array_stringp(pos), "string type error");
	type = unicode_character_type(RefCharacterType(pos), u);
	if (type == CHARACTER_TYPE_INVALID)
		fmte("Invalid character code.", NULL);
	SetCharacterType(pos, type);
	*(unicode *)array_ptrwrite(pos, index) = u;
}

int strarray_equal_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body, &size1);

	return memu_equal(body, right, size1, size2);
}

int strarray_equalp_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body, &size1);

	return memu_equalp(body, right, size1, size2);
}

int strarray_equal_char(addr left, const char *right)
{
	const unicode *body1;
	size_t size1, size2;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body1, &size1);
	size2 = strlen(right);

	return memu1_equal(body1, (const byte *)right, size1, size2);
}

int strarray_equalp_char(addr left, const char *right)
{
	const unicode *body1;
	size_t size1, size2;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body1, &size1);
	size2 = strlen(right);

	return memu1_equalp(body1, (const byte *)right, size1, size2);
}

int strarray_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strarray_equal_binary(left, body, size);
}

int strarray_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strarray_equalp_binary(left, body, size);
}

int strarray_character_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return size == 1 && body[0] == RefCharacter(right);
}

int strarray_character_equalp(addr left, addr right)
{
	const unicode *body;
	unicode a, b;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! characterp(right), "type right error");
	strarray_posbodylen(right, &body, &size);
	if (size != 1) return 0;
	a = body[0];
	GetCharacter(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}

int strarray_compare_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body, &size1);

	return memu_compare(body, right, size1, size2);
}

int strarray_comparep_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body, &size1);

	return memu_comparep(body, right, size1, size2);
}

int strarray_compare_char(addr left, const char *right)
{
	const unicode *body1;
	size_t size1, size2;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body1, &size1);
	size2 = strlen(right);

	return memu1_compare(body1, (const byte *)right, size1, size2);
}

int strarray_comparep_char(addr left, const char *right)
{
	const unicode *body1;
	size_t size1, size2;

	Check(! strarrayp(left), "type error");
	strarray_posbodylen(left, &body1, &size1);
	size2 = strlen(right);

	return memu1_comparep(body1, (const byte *)right, size1, size2);
}

int strarray_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strarrayp(right), "type rigth error");
	strarray_posbodylen(right, &body, &size);

	return strarray_compare_binary(left, body, size);
}

int strarray_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strarrayp(right), "type rigth error");
	strarray_posbodylen(right, &body, &size);

	return strarray_comparep_binary(left, body, size);
}


/*
 *  string
 */
addr string_allocr(LocalRoot local, addr pos)
{
	const unicode *body;
	size_t size;

	if (strarrayp(pos)) {
		strarray_posbodylen(pos, &body, &size);
	}
	else if (GetType(pos) == LISPTYPE_STRING) {
		strvect_posbodylen(pos, &body, &size);
	}
	else {
		Abort("type error.");
		return NULL;
	}
	return strvect_sizeu_allocr(local, body, size);
}

addr string_localr(LocalRoot local, addr pos)
{
	Check(local == NULL, "local error");
	return string_allocr(local, pos);
}

addr string_heapr(addr pos)
{
	return string_allocr(NULL, pos);
}

void string_alloc(LocalRoot local, addr *ret, addr pos)
{
	*ret = string_allocr(local, pos);
}

void string_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	*ret = string_allocr(local, pos);
}

void string_heap(addr *ret, addr pos)
{
	*ret = string_allocr(NULL, pos);
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
	fmte("Argument ~S must be a string type.", pos, NULL);
}

void string_posbodylen(addr pos, const unicode **body, size_t *len)
{
	if (strvect_string_p(pos)) {
		strvect_posbodylen(pos, body, len);
		return;
	}
	if (strarrayp(pos)) {
		strarray_posbodylen(pos, body, len);
		return;
	}
	fmte("Argument ~S must be a string type.", pos, NULL);
}

unicode string_refc(addr pos, size_t index)
{
	if (strvect_string_p(pos))
		return strvect_refc(pos, index);
	if (strarrayp(pos))
		return strarray_refc(pos, index);
	fmte("Argument ~S must be a string type.", pos, NULL);
	return 0;
}

void string_getc(addr pos, size_t index, unicode *u)
{
	*u = string_refc(pos, index);
}

void string_setc(addr pos, size_t index, unicode u)
{
	if (strvect_string_p(pos)) {
		strvect_setc(pos, index, u);
		return;
	}
	if (strarrayp(pos)) {
		strarray_setc(pos, index, u);
		return;
	}
	fmte("Argument ~S must be a string type.", pos, NULL);
}

int string_equal_binary(addr left, const unicode *right, size_t len)
{
	if (strvect_string_p(left))
		return strvect_equal_binary(left, right, len);
	if (strarrayp(left))
		return strarray_equal_binary(left, right, len);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_equalp_binary(addr left, const unicode *right, size_t len)
{
	if (strvect_string_p(left))
		return strvect_equalp_binary(left, right, len);
	if (strarrayp(left))
		return strarray_equalp_binary(left, right, len);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_equal_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_equal_char(left, right);
	if (strarrayp(left))
		return strarray_equal_char(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_equalp_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_equalp_char(left, right);
	if (strarrayp(left))
		return strarray_equalp_char(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

static int strvect_strarray_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strvect_string_p(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strvect_equal_binary(left, body, size);
}

int string_equal(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_equal(left, right);
		if (strarrayp(right))
			return strvect_strarray_equal(left, right);
		fmte("Argument ~S must be a string type.", left, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strvect_strarray_equal(right, left);
		if (strarrayp(right))
			return strarray_equal(left, right);
		fmte("Argument ~S must be a string type.", right, NULL);
	}
	return 0;
}

static int strvect_strarray_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strvect_string_p(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strvect_equalp_binary(left, body, size);
}

int string_equalp(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_equalp(left, right);
		if (strarrayp(right))
			return strvect_strarray_equalp(left, right);
		fmte("Argument ~S must be a string type.", left, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strvect_strarray_equalp(right, left);
		if (strarrayp(right))
			return strarray_equalp(left, right);
		fmte("Argument ~S must be a string type.", right, NULL);
	}
	return 0;
}

int string_character_equal(addr left, addr right)
{
	if (strvect_string_p(left))
		return strvect_character_equal(left, right);
	else if (strarrayp(left))
		return strarray_character_equal(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_character_equalp(addr left, addr right)
{
	if (strvect_string_p(left))
		return strvect_character_equalp(left, right);
	else if (strarrayp(left))
		return strarray_character_equalp(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_compare_binary(addr left, const unicode *right, size_t size2)
{
	if (strvect_string_p(left))
		return strvect_compare_binary(left, right, size2);
	if (strarrayp(left))
		return strarray_compare_binary(left, right, size2);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_comparep_binary(addr left, const unicode *right, size_t size2)
{
	if (strvect_string_p(left))
		return strvect_comparep_binary(left, right, size2);
	if (strarrayp(left))
		return strarray_comparep_binary(left, right, size2);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_compare_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_compare_char(left, right);
	if (strarrayp(left))
		return strarray_compare_char(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int string_comparep_char(addr left, const char *right)
{
	if (strvect_string_p(left))
		return strvect_comparep_char(left, right);
	if (strarrayp(left))
		return strarray_comparep_char(left, right);
	fmte("Argument ~S must be a string type.", left, NULL);
	return 0;
}

int strvect_strarray_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strvect_string_p(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strvect_compare_binary(left, body, size);
}

int strarray_strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_compare_binary(left, body, size);
}

int string_compare(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_compare(left, right);
		if (strarrayp(right))
			return strvect_strarray_compare(left, right);
		fmte("Argument ~S must be a string type.", left, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_compare(left, right);
		if (strarrayp(right))
			return strarray_compare(left, right);
		fmte("Argument ~S must be a string type.", right, NULL);
	}
	return 0;
}

int strvect_strarray_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strvect_string_p(left), "type left error");
	Check(! strarrayp(right), "type right error");
	strarray_posbodylen(right, &body, &size);

	return strvect_comparep_binary(left, body, size);
}

int strarray_strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(! strarrayp(left), "type left error");
	Check(! strvect_string_p(right), "type right error");
	strvect_posbodylen(right, &body, &size);

	return strarray_comparep_binary(left, body, size);
}

int string_comparep(addr left, addr right)
{
	if (strvect_string_p(left)) {
		if (strvect_string_p(right))
			return strvect_comparep(left, right);
		if (strarrayp(right))
			return strvect_strarray_comparep(left, right);
		fmte("Argument ~S must be a string type.", left, NULL);
	}
	else if (strarrayp(left)) {
		if (strvect_string_p(right))
			return strarray_strvect_comparep(left, right);
		if (strarrayp(right))
			return strarray_comparep(left, right);
		fmte("Argument ~S must be a string type.", right, NULL);
	}
	return 0;
}

int string_designer_equal(addr left, addr right)
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

int string_designer_alloc(LocalRoot local, addr *ret, addr pos)
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

int string_designer_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	return string_designer_alloc(local, ret, pos);
}

int string_designer_heap(addr *ret, addr pos)
{
	return string_designer_alloc(NULL, ret, pos);
}

int string_designer_string(addr *ret, addr pos)
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

