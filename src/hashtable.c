#include "build.h"
#include "character.h"
#include "clos_cache.h"
#include "condition.h"
#include "equal.h"
#include "hashtable.h"
#include "memory.h"
#include "local.h"
#include "object.h"
#include "typedef.h"
#include "strtype.h"
#include "sxhash.h"

#ifdef LISP_ARCH_32BIT
#define heap_hashtable      heap_array4
#define local_hashtable     local_array4
#define alloc_hashtable     alloc_array4
#else
#define heap_hashtable      heap_array8
#define local_hashtable     local_array8
#define alloc_hashtable     alloc_array8
#endif
#define SetTableHash(x,y)   SetArraySS((x), HASHTABLE_INDEX_ARRAY, (y))


/*
 *  hashtable
 *    HASHTABLE_INDEX_ARRAY             : vector4 / vector8
 */
struct StructHashtable {
	unsigned resize_float_p : 1;
	unsigned expand_p : 1;
	enum HASHTABLE_TEST test;
	size_t count, size, limit;
	size_t resize_integer;
	double_float resize_float;
	double_float threshold;
};

#define PtrHashtable(x) PtrBodySSa(x, HASHTABLE_INDEX_SIZE)
#define PtrStructHashtable(x) ((struct StructHashtable *)PtrHashtable(x))
#define GetTestHashtable(x) ((int)PtrStructHashtable(x)->test)


/*
 *  frontend
 */
static void setlimit(addr pos)
{
	struct StructHashtable *ptr;

	ptr = PtrStructHashtable(pos);
	if (! ptr->expand_p) {
		ptr->limit = 0;
		return;
	}
	if (ptr->resize_float_p) {
		ptr->limit = (size_t)(ptr->size * ptr->threshold);
	}
	else {
		ptr->limit = ptr->size + ptr->resize_integer;
		if (ptr->limit < ptr->size)
			ptr->limit = SIZE_MAX;
	}
}

void hashtable_full_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	addr pos, array;
	struct StructHashtable *ptr;

	/* argument check */
	Check(test != HASHTABLE_TEST_EQ &&
			test != HASHTABLE_TEST_EQL &&
			test != HASHTABLE_TEST_EQUAL &&
			test != HASHTABLE_TEST_EQUALP &&
			test != HASHTABLE_TEST_CACHE, "test error");
	if (size < HASHTABLE_SIZE_DEFAULT)
		size = HASHTABLE_SIZE_DEFAULT;
	Check(resize < 1.0, "rehash_size error");
	Check(threshold < 0.0 || 1.0 < threshold, "rehash_threshold error");

	/* allocate */
	alloc_smallsize(local, &pos, LISPTYPE_HASHTABLE, HASHTABLE_INDEX_SIZE,
			sizeoft(struct StructHashtable));
	alloc_hashtable(local, &array, LISPTYPE_VECTOR, size);
	ptr = PtrStructHashtable(pos);
	clearpoint(ptr);
	SetTableHash(pos, array);
	ptr->test = test;
	ptr->count = 0;
	ptr->size = size;
	ptr->threshold = threshold;
	setrehash_float_hashtable(pos, resize);
	*ret = pos;
}

void hashtable_full_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	Check(local == NULL, "hashtable_local error");
	hashtable_full_alloc(local, ret, test, size, resize, threshold);
}

void hashtable_full_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	hashtable_full_alloc(NULL, ret, test, size, resize, threshold);
}

void hashtable_integer_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	addr pos, array;
	struct StructHashtable *ptr;

	/* argument check */
	Check(test != HASHTABLE_TEST_EQ &&
			test != HASHTABLE_TEST_EQL &&
			test != HASHTABLE_TEST_EQUAL &&
			test != HASHTABLE_TEST_EQUALP &&
			test != HASHTABLE_TEST_CACHE, "test error");
	if (size < HASHTABLE_SIZE_DEFAULT)
		size = HASHTABLE_SIZE_DEFAULT;
	Check(resize < 1, "rehash_size error");
	Check(threshold < 0.0 || 1.0 < threshold, "rehash_threshold error");

	/* allocate */
	alloc_smallsize(local, &pos, LISPTYPE_HASHTABLE, HASHTABLE_INDEX_SIZE,
			sizeoft(struct StructHashtable));
	alloc_hashtable(local, &array, LISPTYPE_VECTOR, size);
	ptr = PtrStructHashtable(pos);
	clearpoint(ptr);
	SetTableHash(pos, array);
	ptr->test = test;
	ptr->count = 0;
	ptr->size = size;
	ptr->threshold = threshold;
	setrehash_integer_hashtable(pos, resize);
	*ret = pos;
}

void hashtable_integer_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	Check(local == NULL, "local error");
	hashtable_integer_alloc(local, ret, test, size, resize, threshold);
}

void hashtable_integer_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	hashtable_integer_alloc(NULL, ret, test, size, resize, threshold);
}

/* default */
void hashtable_heap(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_local(LocalRoot local, addr *ret)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_alloc(LocalRoot local, addr *ret)
{
	if (local)
		hashtable_local(local, ret);
	else
		hashtable_heap(ret);
}

void hashtable_size_heap(addr *ret, size_t size)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_size_local(LocalRoot local, addr *ret, size_t size)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

void hashtable_size_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		hashtable_size_local(local, ret, size);
	else
		hashtable_size_heap(ret, size);
}

void clear_hashtable_local(addr pos)
{
	addr temp;
	size_t i, size;

	GetTableHash(pos, &temp);
	LenArrayHash(temp, &size);
	for (i = 0; i < size; i++)
		SetArrayHash(temp, i, Nil);
	PtrStructHashtable(pos)->count = 0;
}

void clear_hashtable_heap(addr pos)
{
	addr temp;
	size_t size;
	struct StructHashtable *ptr;

	size = HASHTABLE_SIZE_DEFAULT;
	ptr = PtrStructHashtable(pos);
	/* array */
	heap_hashtable(&temp, LISPTYPE_VECTOR, (size_t)size);
	SetTableHash(pos, temp);
	/* struct */
	ptr->count = 0;
	ptr->size = size;
	setlimit(pos);
}

void clear_hashtable(addr pos)
{
	if (GetStatusDynamic(pos))
		clear_hashtable_heap(pos);
	else
		clear_hashtable_local(pos);
}

int hashtablep(addr pos)
{
	return GetType(pos) == LISPTYPE_HASHTABLE;
}

void gettest_hashtable(addr pos, enum HASHTABLE_TEST *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->test;
}

void settest_hashtable(addr pos, enum HASHTABLE_TEST value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(GetStatusReadOnly(pos), "readonly error");
	PtrStructHashtable(pos)->test = value;
}

void gettest_symbol_hashtable(addr pos, addr *ret)
{
	enum HASHTABLE_TEST test;

	gettest_hashtable(pos, &test);
	switch (test) {
		case HASHTABLE_TEST_EQ:
			GetConst(COMMON_EQ, ret);
			break;

		case HASHTABLE_TEST_EQL:
			GetConst(COMMON_EQL, ret);
			break;

		case HASHTABLE_TEST_EQUAL:
			GetConst(COMMON_EQUAL, ret);
			break;

		case HASHTABLE_TEST_EQUALP:
			GetConst(COMMON_EQUALP, ret);
			break;

		case HASHTABLE_TEST_CACHE:
			GetConst(SYSTEM_CACHE, ret);
			break;

		default:
			GetConst(COMMON_ERROR, ret);
			break;
	}
}

void getcount_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->count;
}

void inccount_hashtable(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	PtrStructHashtable(pos)->count += value;
}

void getsize_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->size;
}

void setrehash_float_hashtable(addr pos, double_float value)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(value < 1.0, "rehash_size error");
	ptr = PtrStructHashtable(pos);
	ptr->resize_float_p = 1;
	ptr->resize_float = value;
	ptr->resize_integer = 0;
	ptr->expand_p = (value != 1.0);
	setlimit(pos);
}

int getrehash_float_hashtable(addr pos, double_float *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (! ptr->resize_float_p) return 0;
	*ret = PtrStructHashtable(pos)->resize_float;

	return 1;
}

void setrehash_integer_hashtable(addr pos, size_t value)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	ptr->resize_float_p = 0;
	ptr->resize_float = 0;
	ptr->resize_integer = value;
	ptr->expand_p = (value != 0);
	setlimit(pos);
}

int getrehash_integer_hashtable(addr pos, size_t *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (ptr->resize_float_p) return 0;
	*ret = PtrStructHashtable(pos)->resize_integer;

	return 1;
}

void getrehash_threshold_hashtable(addr pos, double_float *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->threshold;
}


/*
 *  hashindex
 */
static void hashindex_eq(addr key, size_t size, size_t *ret)
{
	*ret = sxhash_pointer(key) % size;
}

static void hashindex_eql(addr key, size_t size, size_t *ret)
{
	switch (GetType(key)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
			*ret = sxhash_equal(key) % size;
			break;

		default:
			hashindex_eq(key, size, ret);
			break;
	}
}

static void hashindex_equal(addr key, size_t size, size_t *ret)
{
	switch (GetType(key)) {
		case LISPTYPE_CONS:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			*ret = sxhash_equal(key) % size;
			break;

		default:
			hashindex_eql(key, size, ret);
			break;
	}
}

/* ARGSUSED0 */
static void hashindex_equalp(addr key, size_t size, size_t *ret)
{
	switch (GetType(key)) {
		case LISPTYPE_CHARACTER:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			*ret = sxhash_equalp(key) % size;
			break;

		default:
			hashindex_equal(key, size, ret);
			break;
	}
}

/* ARGSUSED0 */
typedef void (*hashindextype)(addr, size_t, size_t *);
static const hashindextype hashindex_switch[] = {
	hashindex_eq,
	hashindex_eql,
	hashindex_equal,
	hashindex_equalp,
	hashindex_cache,
};

static void gethashindex(addr pos, hashindextype *ret)
{
	*ret = hashindex_switch[GetTestHashtable(pos)];
}

static void hashindex(addr pos, addr key, size_t *ret)
{
	hashindextype index;

	gethashindex(pos, &index);
	index(key, PtrStructHashtable(pos)->size, ret);
}


/*
 *  gethashequal
 */
/* ARGSUSED0 */
typedef int (*hashequaltype)(addr, addr);
static const hashequaltype hashequal_switch[] = {
	eq_function,
	eql_function,
	equal_function,
	equalp_function,
	cache_equal_function
};

static void gethashequal(addr pos, hashequaltype *ret)
{
	*ret = hashequal_switch[GetTestHashtable(pos)];
}


/*
 *  rehash
 */
static void insert_rehash(LocalRoot local,
		addr array, size_t index, addr key, addr value)
{
	addr cons, one, root;

	/* one */
	cons_alloc(local, &one, key, value);
	/* cons */
	GetArrayHash(array, index, &root);
	cons_alloc(local, &cons, one, root);
	/* setarray */
	SetArrayHash(array, index, cons);
}

static void resize_rehash(LocalRoot local, addr pos, size_t resize)
{
	addr prev, next, left, right, key, value;
	size_t index, len, make;
	hashindextype calcindex;

	GetTableHash(pos, &prev);
	alloc_hashtable(local, &next, LISPTYPE_VECTOR, resize);

	gethashindex(pos, &calcindex);
	LenArrayHash(prev, &len);
	for (index = 0; index < len; index++) {
		GetArrayHash(prev, index, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &value);
			calcindex(key, resize, &make);
			insert_rehash(local, next, make, key, value);
		}
	}
	SetTableHash(pos, next);
}

void force_resize_hashtable(addr pos, size_t size)
{
	LocalRoot local;

	if (size == 0) size = 1;
	local = GetStatusDynamic(pos)? Local_Thread: NULL;
	resize_rehash(local, pos, size);
	PtrStructHashtable(pos)->size = size;
	setlimit(pos);
}

static void rehash_execute(LocalRoot local, addr pos)
{
	size_t size, newsize;
	struct StructHashtable *ptr;

	/* get parameter */
	ptr = PtrStructHashtable(pos);
	if (! ptr->expand_p) return;
	if (ptr->count < ptr->limit) return;

	size = ptr->size;
	if (ptr->resize_float_p) {
		newsize = (size_t)(size * ptr->resize_float);
		if (size == newsize) newsize++;
	}
	else {
		newsize = size + ptr->resize_integer;
	}
	if (newsize < size)
		newsize = SIZE_MAX;

	/* resize array */
	resize_rehash(local, pos, newsize);
	ptr->size = newsize;
	setlimit(pos);
}


/*
 *  intern
 */
static int findroot(hashequaltype equal, addr right, addr key, addr *ret)
{
	addr left, leftkey;

	/* found=0, notfound=1 */
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetCar(left, &leftkey);
		if (equal(leftkey, key)) {
			*ret = left;
			return 0;
		}
	}

	return 1;
}

static void appendroot(LocalRoot local,
		addr array, size_t index, addr root, addr key, addr *ret)
{
	addr left, next;

	/* array[index] -> ((key . nil) . next)
	 * ret -> (key . nil) */
	conscar_alloc(local, &left, key);
	cons_alloc(local, &next, left, root);
	SetArrayHash(array, index, next);
	*ret = left;
}

static int static_intern_hashtable(LocalRoot local, addr pos, addr key, addr *ret)
{
	size_t index;
	addr array, root;
	hashequaltype equal;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(GetStatusReadOnly(pos), "readonly error");

	rehash_execute(local, pos);
	hashindex(pos, key, &index);
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal);
	if (findroot(equal, root, key, ret)) {
		appendroot(local, array, index, root, key, ret);
		PtrStructHashtable(pos)->count++;
		return 1; /* notfound, create */
	}

	return 0; /* find */
}

int intern_hashheap(addr pos, addr key, addr *ret)
{
	Check(GetStatusDynamic(pos), "dynamic error");
	return static_intern_hashtable(NULL, pos, key, ret);
}

int intern_hashlocal(LocalRoot local, addr pos, addr key, addr *ret)
{
	Check(local == NULL, "local error");
	Check(! GetStatusDynamic(pos), "dynamic error");
	return static_intern_hashtable(local, pos, key, ret);
}

int intern_hashalloc(LocalRoot local, addr pos, addr key, addr *ret)
{
	/* heap check */
	Check(! GetStatusDynamic(pos) && local != NULL, "local error");
	/* local check */
	Check(  GetStatusDynamic(pos) && local == NULL, "local error");
	return static_intern_hashtable(local, pos, key, ret);
}

int intern_hashtable(LocalRoot local, addr pos, addr key, addr *ret)
{
	if (GetStatusDynamic(pos)) {
		if (local == NULL)
			fmte("The dynamic hashtable must use a localroot.", NULL);
		return static_intern_hashtable(local, pos, key, ret);
	}
	else {
		return static_intern_hashtable(NULL, pos, key, ret);
	}
}

void findcons_hashtable(addr pos, addr key, addr *ret)
{
	size_t index;
	addr array, root, left, right, check;
	hashequaltype equal;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	hashindex(pos, key, &index);
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (equal(check, key)) {
			*ret = left;
			return;
		}
	}
	*ret = Nil;
}

int findvalue_hashtable(addr pos, addr key, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	findcons_hashtable(pos, key, &cons);

	if (cons != Nil) {
		GetCdr(cons, ret);
		return 0;
	}
	else {
		*ret = Nil;
		return 1;
	}
}


/*
 *  delete
 */
int delete_hashtable(addr pos, addr key)
{
	size_t index;
	addr array, root, left, right, leftkey, prev;
	hashequaltype equal;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(GetStatusReadOnly(pos), "readonly error");

	hashindex(pos, key, &index);
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal);
	for (prev = Nil; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &leftkey);
		if (equal(leftkey, key)) {
			if (prev == Nil) {
				SetArrayHash(array, index, right);
			}
			else {
				SetCdr(prev, right);
			}
			PtrStructHashtable(pos)->count--;
			return 0;
		}
		prev = root;
	}

	return 1;
}


/*
 *  map-function
 */
void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr cons, cell, result;
	size_t size, index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	size = PtrStructHashtable(pos)->size;
	GetTableHash(pos, &pos);

	result = Nil;
	for (index = 0; index < size; index++) {
		GetArrayHash(pos, index, &cons);
		while (cons != Nil) {
			GetCons(cons, &cell, &cons);
			GetCar(cell, &cell);
			cons_alloc(local, &result, cell, result);
		}
	}

	*ret = result;
}

void allkeys_hashtable_heap(addr pos, addr *ret)
{
	allkeys_hashtable_alloc(NULL, pos, ret);
}

void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	allkeys_hashtable_alloc(local, pos, ret);
}

/* equalp */
static int equalp_allelement(addr left, addr right, int (*call)(addr, addr))
{
	addr cons, key, value1, value2;
	size_t size, index;

	size = PtrStructHashtable(left)->size;
	GetTableHash(left, &left);
	for (index = 0; index < size; index++) {
		GetArrayHash(left, index, &cons);
		while (cons != Nil) {
			GetCons(cons, &key, &cons);
			GetCons(key, &key, &value1);
			if (findvalue_hashtable(right, key, &value2)) return 0;
			if (! call(value1, value2)) return 0;
		}
	}

	return 1;
}

int equalcall_hashtable(addr left, addr right, int (*call)(addr, addr))
{
	struct StructHashtable *str1, *str2;

	str1 = PtrStructHashtable(left);
	str2 = PtrStructHashtable(right);
	if (str1->count != str2->count) return 0;
	if (str1->test != str2->test) return 0;
	if (! equalp_allelement(left, right, call)) return 0;

	return 1;
}

int equalp_hashtable(addr left, addr right)
{
	return equalcall_hashtable(left, right, equalp_function);
}

int equalrt_hashtable(addr left, addr right)
{
	return equalcall_hashtable(left, right, equalrt_function);
}

/* clang */
void findcons_char_hashtable(addr pos, const char *key, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, const char *);
	size_t index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			value = sxhash_char_equal(key);
			equal = string_equal_char;
			break;

		case HASHTABLE_TEST_EQUALP:
			value = sxhash_char_equalp(key);
			equal = string_equalp_char;
			break;

		default:
			*ret = Nil;
			return;
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (stringp(check) && equal(check, key)) {
			*ret = left;
			return;
		}
	}
	*ret = Nil;
}

int findvalue_char_hashtable(addr pos, const char *key, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	findcons_char_hashtable(pos, key, &cons);

	if (cons != Nil) {
		GetCdr(cons, ret);
		return 0;
	}
	else {
		*ret = Nil;
		return 1;
	}
}

void findcons_unicode_hashtable(addr pos, unicode key, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode);
	size_t index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQL:
		case HASHTABLE_TEST_EQUAL:
			value = sxhash_unicode_equal(key);
			equal = character_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			value = sxhash_unicode_equalp(key);
			equal = character_equalp_unicode;
			break;

		default:
			*ret = Nil;
			return;
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (GetType(check) == LISPTYPE_CHARACTER && equal(check, key)) {
			*ret = left;
			return;
		}
	}
	*ret = Nil;
}

int findvalue_unicode_hashtable(addr pos, unicode key, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	findcons_unicode_hashtable(pos, key, &cons);

	if (cons != Nil) {
		GetCdr(cons, ret);
		return 0;
	}
	else {
		*ret = Nil;
		return 1;
	}
}

void findcons_character2_hashtable(addr pos, unicode a, unicode b, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode, unicode);
	size_t index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			value = sxhash_character2_equal(a, b);
			equal = character2_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			value = sxhash_character2_equalp(a, b);
			equal = character2_equalp_unicode;
			break;

		default:
			*ret = Nil;
			return;
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (GetType(check) == LISPSYSTEM_CHARACTER2 && equal(check, a, b)) {
			*ret = left;
			return;
		}
	}
	*ret = Nil;
}

int findvalue_character2_hashtable(addr pos, unicode a, unicode b, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	findcons_character2_hashtable(pos, a, b, &cons);

	if (cons != Nil) {
		GetCdr(cons, ret);
		return 0;
	}
	else {
		*ret = Nil;
		return 1;
	}
}


/*
 *  iterator
 */
struct StructHashIterator {
	int finish;
	size_t index;
};

enum HashIterator {
	HashIterator_Hash,
	HashIterator_Array,
	HashIterator_List,
	HashIterator_Size
};

#define PtrHashIterator(x) PtrBodySSa(x, HashIterator_Size)
#define PtrStructHashIterator(x) ((struct StructHashIterator *)PtrHashIterator(x))
#define GetIndexHashIterator(x) (PtrStructHashtable(x)->index)
#define RetHashIterator RetArraySS
#define GetHashIterator GetArraySS
#define SetHashIterator SetArraySS

void hash_iterator_alloc(LocalRoot local, addr *ret, addr table)
{
	addr pos, array;
	struct StructHashIterator *ptr;

	CheckType(table, LISPTYPE_HASHTABLE);
	alloc_smallsize(local, &pos,
			LISPSYSTEM_HASHITERATOR,
			HashIterator_Size,
			sizeoft(struct StructHashIterator));
	ptr = PtrStructHashIterator(pos);
	clearpoint(ptr);
	ptr->finish = 0;
	ptr->index = 0;
	GetTableHash(table, &array);
	SetHashIterator(pos, HashIterator_Hash, table);
	SetHashIterator(pos, HashIterator_Array, array);
	*ret = pos;
}

void hash_iterator_local(LocalRoot local, addr *ret, addr table)
{
	Check(local == NULL, "local error");
	hash_iterator_alloc(local, ret, table);
}

void hash_iterator_heap(addr *ret, addr table)
{
	hash_iterator_alloc(NULL, ret, table);
}

void set_hash_iterator(addr pos, addr table)
{
	addr array;
	struct StructHashIterator *ptr;

	CheckType(pos, LISPSYSTEM_HASHITERATOR);
	CheckType(table, LISPTYPE_HASHTABLE);
	ptr = PtrStructHashIterator(pos);
	ptr->finish = 0;
	ptr->index = 0;
	GetTableHash(table, &array);
	SetHashIterator(pos, HashIterator_Hash, table);
	SetHashIterator(pos, HashIterator_Array, array);
}

/* 0:finish, 1:find */
int next_hash_iterator(addr pos, addr *key, addr *value)
{
	addr list, array, cons;
	struct StructHashIterator *ptr;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_HASHITERATOR);
	/* Iterator is already closed */
	ptr = PtrStructHashIterator(pos);
	if (ptr->finish) return 0;
	GetHashIterator(pos, HashIterator_List, &list);
	GetHashIterator(pos, HashIterator_Array, &array);
	/* First search */
	if (list == Nil) {
		goto search;
	}
	/* After first */
	GetCdr(list, &list);
	if (list == Nil) {
		ptr->index++;
		goto search;
	}
	goto find;

search:
	LenArrayHash(array, &size);
	for (i = ptr->index; i < size; i++) {
		GetArrayHash(array, i, &list);
		if (list != Nil) {
			ptr->index = i;
			goto find;
		}
	}
	ptr->finish = 1;
	return 0;

find:
	GetCar(list, &cons);
	GetCons(cons, key, value);
	SetHashIterator(pos, HashIterator_List, list);
	return 1;
}

