/*
 *  main-string
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "character.h"
#include "encode.h"
#include "extern_init.h"
#include "main_string.h"
#include "typedef.h"


/*
 *  getsize_stringu
 */
static int strsize_stringu(const unicode *str, const unicode **endp, size_t *ret)
{
	unicode c;
	size_t value;

	/* trim space */
	for (;;) {
		c = *str;
		if (! isSpaceUnicode(c)) break;
		str++;
	}
	if (! isDigitCase(c)) return 1;

	/* parse-integer */
	value = 0;
	for (;;) {
		c = *str;
		if (! isDigitCase(c)) break;
		if (value) {
			if ((SIZE_MAX / 10) < value) {
				*ret = SIZE_MAX;
				return 2;
			}
			value *= 10;
		}
		if (value > (SIZE_MAX - c)) {
			*ret = SIZE_MAX;
			return 2;
		}
		value += c - '0';
		str++;
	}
	if (endp) *endp = str;
	*ret = value;

	return 0;
}

static int getunit_stringu(const unicode *str, int *type)
{
	unicode c, check;

	c = *(str++);
	if (c == 0) {
		*type = 0;
		return 0;
	}
	c = toUpperUnicode(c);
	if (c == 'K' || c == 'M' || c == 'G' || c == 'T' || c == 'P' || c == 'E') {
		for (;;) {
			check = *str;
			if (! isSpaceUnicode(check)) break;
			str++;
		}
		if (check != 0) return 1;
	}
	else {
		return 1;
	}
	*type = (int)c;

	return 0;
}

static int unitloop(size_t *value, int loop)
{
	int i;
	size_t v;

	v = *value;
	for (i = 0; i < loop; i++) {
		if ((SIZE_MAX / 1024UL) < v) {
			return 1;
		}
		v *= 1024UL;
	}
	*value = v;

	return 0;
}

_g int getsize_stringu(lispstringu str, size_t *ret)
{
	int check;
	const unicode *next;
	size_t size;

	check = strsize_stringu(str->ptr, &next, &size);
	if (check == 2) {
		lisperror("Number is too large.");
		return 1;
	}
	if (check) {
		lisperror("Invalid memory argument.");
		return 1;
	}
	if (size == 0) {
		lisperror("Memory size must not be a zero.");
		return 1;
	}
	if (getunit_stringu(next, &check)) {
		lisperror("Invalid unit string.");
		return 1;
	}
	switch (check) {
		case 'K': if (unitloop(&size, 1)) goto error; break;
		case 'M': if (unitloop(&size, 2)) goto error; break;
		case 'G': if (unitloop(&size, 3)) goto error; break;
		case 'T': if (unitloop(&size, 4)) goto error; break;
		case 'P': if (unitloop(&size, 5)) goto error; break;
		case 'E': if (unitloop(&size, 6)) goto error; break;
		default: break;
	}
	*ret = size;
	return 0;

error:
	lisperror("Number is too large.");
	return 1;
}


/*
 *  lispstringu
 */
static lispstringu make_stringu(size_t size)
{
	lispstringu ptr;
	unicode *data;

	ptr = (lispstringu)malloc(sizeoft(struct lispstringu_struct));
	if (ptr == NULL)
		return NULL;
	data = (unicode *)malloc(sizeoft(unicode) * size);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->ptr = data;
	ptr->size = size;

	return ptr;
}

_g lispstringu char_stringu(const char *str)
{
	lispstringu ptr;
	unicode *a;
	const byte *b;
	size_t size, i;

	size = strlen(str);
	b = (const byte *)str;
	ptr = make_stringu(size + 1UL);
	a = ptr->ptr;
	for (i = 0; i < size; i++)
		a[i] = (unicode)b[i];
	a[i] = 0;

	return ptr;
}

_g lispstringu copy_stringu(lispstringu ptr)
{
	lispstringu copy;

	copy = make_stringu(ptr->size);
	if (copy == NULL)
		return NULL;
	memcpy(copy->ptr, ptr->ptr, sizeoft(unicode) * ptr->size);

	return copy;
}

_g lispstringu concatchar_stringu(lispstringu a, const char *b)
{
	unicode *pa;
	const byte *pb;
	lispstringu copy;
	size_t i, sizea, sizeb;

	sizea = a->size;
	if (sizea == 0)
		return NULL;
	sizeb = strlen(b);
	copy = make_stringu(sizea + sizeb);
	if (copy == NULL)
		return NULL;
	sizea--;
	pa = copy->ptr;
	pb = (const byte *)b;
	memcpy(pa, a->ptr, sizeoft(unicode) * sizea);
	for (i = 0; i < sizeb; i++)
		pa[sizea + i] = (unicode)pb[i];
	pa[sizea + i] = 0;

	return copy;
}

_g void output_stringu(lispstringu ptr, FILE *file)
{
	unicode *data, u;
	size_t i, size;

	size = ptr->size;
	data = ptr->ptr;
	for (i = 0; i < size; i++) {
		u = data[i];
		fprintf(file, "%c", (u < 0x80 && isgraph((int)u))? (int)u: '.');
	}
}

_g void free_stringu(lispstringu ptr)
{
	if (ptr) {
		free(ptr->ptr);
		ptr->ptr = NULL;
		free(ptr);
	}
}

_g int equal_stringu(lispstringu a, lispstringu b)
{
	return (a->size == b->size) && (memcmp(a->ptr, b->ptr, a->size) == 0);
}

_g int equalchar_stringu(lispstringu a, const char *b)
{
	const unicode *c;
	const byte *d;
	size_t i, size;

	size = strlen(b) + 1UL;
	if (a->size != size) return 0;
	c = a->ptr;
	d = (const byte *)b;
	for (i = 0; i < size; i++) {
		if (c[i] != (unicode)d[i]) return 0;
	}

	return 1;
}


/*
 *  lisparrayu
 */
static lispstringu *stringu_call(int argc, const void *const *argv,
		int (*len)(const void *, size_t *),
		int (*make)(unicode *, const void *))
{
	int i, k;
	const void *str;
	lispstringu x, *r;
	size_t size;

	r = (lispstringu *)malloc(sizeoft(lispstringu) * argc);
	if (r == NULL)
		return NULL;
	for (i = 0; i < argc; i++) {
		str = argv[i];
		if ((*len)(str, &size))
			goto error;
		x = make_stringu(size + 1UL);
		if (x == NULL)
			goto error;
		if ((*make)(x->ptr, str)) {
			free_stringu(x);
			goto error;
		}
		x->ptr[size] = 0;
		r[i] = x;
	}
	return r;

error:
	for (k = 0; k < i; i++) {
		free_stringu(r[k]);
		goto error;
	}
	free(r);
	return NULL;
}

static int stringu_utf8_strlen(const void *ptr, size_t *ret)
{
	return UTF8_null_strlen((const byte *)ptr, ret);
}
static int stringu_utf8_make(unicode *data, const void *ptr)
{
	return UTF8_null_makeunicode(data, (const byte *)ptr);
}
static lispstringu *stringu_utf8(int argc, const void *const *argv)
{
	return stringu_call(argc, argv, stringu_utf8_strlen, stringu_utf8_make);
}

static int stringu_utf16_strlen(const void *ptr, size_t *ret)
{
	return UTF16_null_strlen((const byte16 *)ptr, ret);
}
static int stringu_utf16_make(unicode *data, const void *ptr)
{
	return UTF16_null_makeunicode(data, (const byte16 *)ptr);
}
static lispstringu *stringu_utf16(int argc, const void *const *argv)
{
	return stringu_call(argc, argv, stringu_utf16_strlen, stringu_utf16_make);
}

static lisparrayu arrayu_argv_call(int argc, const void *const *argv,
		lispstringu *(*call)(int, const void *const *))
{
	lisparrayu ptr;
	lispstringu *data;

	ptr = (lisparrayu)malloc(sizeoft(struct lisparrayu_struct));
	if (ptr == NULL)
		return NULL;
	data = (*call)(argc, argv);
	if (data == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->ptr = data;
	ptr->size = (size_t)argc;

	return ptr;
}
_g lisparrayu arrayu_argv_utf8(int argc, const byte *const *argv)
{
	return arrayu_argv_call(argc, (const void *const *)argv, stringu_utf8);
}
_g lisparrayu arrayu_argv_utf16(int argc, const byte16 *const *argv)
{
	return arrayu_argv_call(argc, (const void *const *)argv, stringu_utf16);
}

_g void free_arrayu(lisparrayu ptr)
{
	lispstringu *v;
	size_t size, i;

	if (ptr) {
		v = ptr->ptr;
		size = ptr->size;
		for (i = 0; i < size; i++) {
			free_stringu(v[i]);
			v[i] = NULL;
		}
		free(v);
		ptr->ptr = NULL;
		free(ptr);
	}
}


/*
 *  envirnment - main
 */
static lisptableu make_tableu(size_t size)
{
	struct lispkeyvalueu *table;
	lisptableu ptr;

	ptr = (lisptableu)malloc(sizeoft(struct lisptableu_struct));
	if (ptr == NULL)
		return NULL;
	table = (struct lispkeyvalueu *)calloc(size, sizeoft(struct lispkeyvalueu));
	if (table == NULL) {
		free(ptr);
		return NULL;
	}
	ptr->table = table;
	ptr->size = size;

	return ptr;
}

_g void free_tableu(lisptableu ptr)
{
	struct lispkeyvalueu *table;
	size_t size, i;

	if (ptr) {
		table = ptr->table;
		size = ptr->size;
		for (i = 0; i < size; i++) {
			free_stringu(table[i].key);
			free_stringu(table[i].value);
		}
		free(table);
		ptr->table = NULL;
		free(ptr);
	}
}

static int findchar_stringu(lispstringu str, unicode v, size_t *ret)
{
	unicode *u;
	size_t size, i;

	u = str->ptr;
	size = str->size;
	for (i = 0; i < size; i++) {
		if (u[i] == v) {
			*ret = i;
			return 1;
		}
	}

	return 0;
}

static int split_keyvalue_main(lispstringu str, lispstringu *key, lispstringu *value)
{
	lispstringu k, v;
	size_t a, b, s;

	s = str->size;
	if (s == 0) {
		k = make_stringu(1);
		if (k == NULL)
			return 1;
		v = make_stringu(1);
		if (v == NULL) {
			free_stringu(k);
			return 1;
		}
		a = b = 0;
		goto result;
	}
	s--;
	if (findchar_stringu(str, '=', &a))
		b = s - a - 1UL;
	else {
		a = s - 1UL;
		b = 0;
	}
	k = make_stringu(a + 1UL);
	if (k == NULL)
		return 1;
	v = make_stringu(b + 1UL);
	if (v == NULL) {
		free_stringu(k);
		return 1;
	}
	memcpy(k->ptr, str->ptr, a * sizeoft(unicode));
	memcpy(v->ptr, str->ptr+a+1, b * sizeoft(unicode));

result:
	k->ptr[a] = 0;
	v->ptr[b] = 0;
	*key = k;
	*value = v;

	return 0;
}

_g lisptableu tableu_env_main(const byte *const *env)
{
	int i, size;
	lisparrayu a;
	lispstringu *pa, k, v;
	lisptableu b;
	struct lispkeyvalueu *pb;

	for (size = 0; env[size]; size++)
		continue;
	a = arrayu_argv_utf8(size, env);
	if (a == NULL)
		return NULL;
	b = make_tableu((size_t)size);
	if (b == NULL) {
		free_arrayu(a);
		return NULL;
	}
	pa = a->ptr;
	pb = b->table;
	for (i = 0; i < size; i++) {
		if (split_keyvalue_main(pa[i], &k, &v))
			goto error;
		pb[i].key = k;
		pb[i].value = v;
	}
	return b;

error:
	free_arrayu(a);
	free_tableu(b);
	return NULL;
}


/*
 *  envirnment - windows
 */
static lispstringu *stringu_windows(int argc, const void *const *env)
{
	int i, k;
	lispstringu x, *r;
	const byte16 *str;
	size_t size;

	str = (const byte16 *)env;
	r = (lispstringu *)malloc(sizeoft(lispstringu) * argc);
	if (r == NULL)
		return NULL;
	for (i = 0; i < argc; i++) {
		if (UTF16_null_strlen(str, &size))
			goto error;
		x = make_stringu(size + 1UL);
		if (x == NULL)
			goto error;
		if (UTF16_null_makeunicode(x->ptr, str)) {
			free_stringu(x);
			goto error;
		}
		x->ptr[size] = 0;
		r[i] = x;
		while (*str) str++;
		str++;
	}
	return r;

error:
	for (k = 0; k < i; i++) {
		free_stringu(r[k]);
		goto error;
	}
	free(r);
	return NULL;
}

_g lisptableu tableu_env_windows(const byte16 *env)
{
	int i, size;
	const byte16 *p;
	lisparrayu a;
	lispstringu *pa, k, v;
	lisptableu b;
	struct lispkeyvalueu *pb;

	/* length */
	size = 0;
	for (p = env; *p; p++) {
		while (*p) p++;
		size++;
	}

	/* lisparrayu */
	a = arrayu_argv_call(size, (const void *const *)env, stringu_windows);
	if (a == NULL)
		return NULL;
	b = make_tableu((size_t)size);
	if (b == NULL) {
		free_arrayu(a);
		return NULL;
	}
	pa = a->ptr;
	pb = b->table;
	for (i = 0; i < size; i++) {
		if (split_keyvalue_main(pa[i], &k, &v))
			goto error;
		pb[i].key = k;
		pb[i].value = v;
	}
	return b;

error:
	free_arrayu(a);
	free_tableu(b);
	return NULL;
}

_g lispstringu findchar_tableu(lisptableu env, const char *key)
{
	struct lispkeyvalueu *ptr;
	size_t size, i;

	ptr = env->table;
	size = env->size;
	for (i = 0; i < size; i++) {
		if (equalchar_stringu(ptr[i].key, key))
			return ptr[i].value;
	}

	return NULL;
}

