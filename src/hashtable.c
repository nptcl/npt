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

_g void hashtable_full_alloc(LocalRoot local, addr *ret,
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

_g void hashtable_full_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	Check(local == NULL, "hashtable_local error");
	hashtable_full_alloc(local, ret, test, size, resize, threshold);
}

_g void hashtable_full_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float resize, double_float threshold)
{
	hashtable_full_alloc(NULL, ret, test, size, resize, threshold);
}

_g void hashtable_integer_alloc(LocalRoot local, addr *ret,
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

_g void hashtable_integer_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	Check(local == NULL, "local error");
	hashtable_integer_alloc(local, ret, test, size, resize, threshold);
}

_g void hashtable_integer_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t resize, double_float threshold)
{
	hashtable_integer_alloc(NULL, ret, test, size, resize, threshold);
}

/* default */
_g void hashtable_heap(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

_g void hashtable_local(LocalRoot local, addr *ret)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			HASHTABLE_SIZE_DEFAULT,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

_g void hashtable_alloc(LocalRoot local, addr *ret)
{
	if (local)
		hashtable_local(local, ret);
	else
		hashtable_heap(ret);
}

_g void hashtable_size_heap(addr *ret, size_t size)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

_g void hashtable_size_local(LocalRoot local, addr *ret, size_t size)
{
	hashtable_full_local(local, ret,
			HASHTABLE_TEST_DEFAULT,
			size,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

_g void hashtable_size_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		hashtable_size_local(local, ret, size);
	else
		hashtable_size_heap(ret, size);
}

_g void clear_hashtable_local(addr pos)
{
	addr temp;
	size_t i, size;

	GetTableHash(pos, &temp);
	LenArrayHash(temp, &size);
	for (i = 0; i < size; i++)
		SetArrayHash(temp, i, Nil);
	PtrStructHashtable(pos)->count = 0;
}

_g void clear_hashtable_heap(addr pos)
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

_g void clear_hashtable(addr pos)
{
	if (GetStatusDynamic(pos))
		clear_hashtable_heap(pos);
	else
		clear_hashtable_local(pos);
}

_g int hashtablep(addr pos)
{
	return GetType(pos) == LISPTYPE_HASHTABLE;
}

_g void gettest_hashtable(addr pos, enum HASHTABLE_TEST *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->test;
}

_g void settest_hashtable(addr pos, enum HASHTABLE_TEST value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Check(GetStatusReadOnly(pos), "readonly error");
	PtrStructHashtable(pos)->test = value;
}

_g void gettest_symbol_hashtable(addr pos, addr *ret)
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

_g void getcount_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->count;
}

_g void inccount_hashtable(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	PtrStructHashtable(pos)->count += value;
}

_g void getsize_hashtable(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->size;
}

_g void setrehash_float_hashtable(addr pos, double_float value)
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

_g int getrehash_float_hashtable(addr pos, double_float *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (! ptr->resize_float_p)
		return 0;
	*ret = PtrStructHashtable(pos)->resize_float;

	return 1;
}

_g void setrehash_integer_hashtable(addr pos, size_t value)
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

_g int getrehash_integer_hashtable(addr pos, size_t *ret)
{
	struct StructHashtable *ptr;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	ptr = PtrStructHashtable(pos);
	if (ptr->resize_float_p)
		return 0;
	*ret = PtrStructHashtable(pos)->resize_integer;

	return 1;
}

_g void getrehash_threshold_hashtable(addr pos, double_float *ret)
{
	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	*ret = PtrStructHashtable(pos)->threshold;
}


/*
 *  hashindex
 */
static int hashindex_eq_(addr key, size_t size, size_t *ret)
{
	fixnum value;
	Return(sxhash_eq_(key, &value));
	return Result(ret, (size_t)(value % size));
}

static int hashindex_eql_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
			Return(sxhash_equal_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_eq_(key, size, ret);
	}
}

static int hashindex_equal_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_CONS:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			Return(sxhash_equal_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_eql_(key, size, ret);
	}
}

static int hashindex_equalp_(addr key, size_t size, size_t *ret)
{
	fixnum value;

	switch (GetType(key)) {
		case LISPTYPE_CHARACTER:
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPSYSTEM_CHARACTER2:
			Return(sxhash_equalp_(key, &value));
			return Result(ret, (size_t)(value % size));

		default:
			return hashindex_equal_(key, size, ret);
	}
}

typedef int (*hashindextype)(addr, size_t, size_t *);
static const hashindextype hashindex_switch[] = {
	hashindex_eq_,
	hashindex_eql_,
	hashindex_equal_,
	hashindex_equalp_,
	hashindex_cache_,
};

static void gethashindex(addr pos, hashindextype *ret)
{
	*ret = hashindex_switch[GetTestHashtable(pos)];
}

static int call_hashindex_(addr pos, addr key, size_t *ret)
{
	hashindextype call_;
	gethashindex(pos, &call_);
	return (*call_)(key, PtrStructHashtable(pos)->size, ret);
}


/*
 *  gethashequal
 */
typedef int (*hashequaltype)(addr, addr, int *);
static const hashequaltype hashequal_switch[] = {
	eq_function_,
	eql_function_,
	equal_function_,
	equalp_function_,
	cache_equal_function_
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

static int resize_rehash_(LocalRoot local, addr pos, size_t resize)
{
	addr prev, next, left, right, key, value;
	size_t index, len, make;
	hashindextype call_;

	GetTableHash(pos, &prev);
	alloc_hashtable(local, &next, LISPTYPE_VECTOR, resize);

	gethashindex(pos, &call_);
	LenArrayHash(prev, &len);
	for (index = 0; index < len; index++) {
		GetArrayHash(prev, index, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &value);
			Return((*call_)(key, resize, &make));
			insert_rehash(local, next, make, key, value);
		}
	}
	SetTableHash(pos, next);

	return 0;
}

_g int force_resize_hashtable_(addr pos, size_t size)
{
	LocalRoot local;

	if (size == 0)
		size = 1;
	local = GetStatusDynamic(pos)? Local_Thread: NULL;
	Return(resize_rehash_(local, pos, size));
	PtrStructHashtable(pos)->size = size;
	setlimit(pos);

	return 0;
}

static int rehash_execute_(LocalRoot local, addr pos)
{
	size_t size, newsize;
	struct StructHashtable *ptr;

	/* get parameter */
	ptr = PtrStructHashtable(pos);
	if (! ptr->expand_p)
		return 0;
	if (ptr->count < ptr->limit)
		return 0;

	size = ptr->size;
	if (ptr->resize_float_p) {
		newsize = (size_t)(size * ptr->resize_float);
		if (size == newsize)
			newsize++;
	}
	else {
		newsize = size + ptr->resize_integer;
	}
	if (newsize < size)
		newsize = SIZE_MAX;

	/* resize array */
	Return(resize_rehash_(local, pos, newsize));
	ptr->size = newsize;
	setlimit(pos);

	return 0;
}


/*
 *  intern
 */
static int findroot_hashtable_(hashequaltype equal_,
		addr right, addr key, addr *value, int *ret)
{
	int check;
	addr left, leftkey;

	/* found=1, notfound=0 */
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetCar(left, &leftkey);
		Return((*equal_)(leftkey, key, &check));
		if (check) {
			*value = left;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static void appendroot_hashtable(LocalRoot local,
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

static int intern_hashtable_alloc_(LocalRoot local,
		addr pos, addr key, addr *ret, int *existp)
{
	int check;
	size_t index;
	addr array, root;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");

	Return(rehash_execute_(local, pos));
	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	Return(findroot_hashtable_(equal_, root, key, ret, &check));
	if (! check) {
		appendroot_hashtable(local, array, index, root, key, ret);
		PtrStructHashtable(pos)->count++;
		return Result(existp, 0); /* notfound, create */
	}

	return Result(existp, 1); /* find */
}

_g int internp_hashheap_(addr pos, addr key, addr *ret, int *existp)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusDynamic(pos), "dynamic error");
	return intern_hashtable_alloc_(NULL, pos, key, ret, existp);
}

_g int intern_hashheap_(addr pos, addr key, addr *ret)
{
	int check;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusDynamic(pos), "dynamic error");
	return intern_hashtable_alloc_(NULL, pos, key, ret, &check);
}


/*
 *  find
 */
_g int findcons_hashtable_(addr pos, addr key, addr *ret)
{
	int check;
	size_t index;
	addr array, root, left, right, value;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &value);
		Return((*equal_)(value, key, &check));
		if (check)
			return Result(ret, left);
	}

	return Result(ret, Nil);
}

_g int find_hashtable_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return Result(ret, Unbound);
	GetCdr(pos, ret);
	return 0;
}

_g int findnil_hashtable_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return Result(ret, Nil);
	GetCdr(pos, ret);
	return 0;
}

/* for debug */
_g int findnil_hashtable_debug(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_HASHTABLE);
	Error(findcons_hashtable_(pos, key, &pos));
	if (pos == Nil)
		return 0;
	GetCdr(pos, ret);
	return 1;
}


/*
 *  delete
 */
_g int delete_hashtable_(addr pos, addr key, int *ret)
{
	int check;
	size_t index;
	addr array, root, left, right, leftkey, prev;
	hashequaltype equal_;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");

	Return(call_hashindex_(pos, key, &index));
	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);

	gethashequal(pos, &equal_);
	for (prev = Nil; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &leftkey);
		Return((*equal_)(leftkey, key, &check));
		if (check) {
			if (prev == Nil) {
				SetArrayHash(array, index, right);
			}
			else {
				SetCdr(prev, right);
			}
			PtrStructHashtable(pos)->count--;
			return Result(ret, 0);
		}
		prev = root;
	}

	return Result(ret, 1);
}


/*
 *  map-function
 */
_g void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr cons, cell, result;
	size_t size, index;

	CheckType(pos, LISPTYPE_HASHTABLE);
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

_g void allkeys_hashtable_heap(addr pos, addr *ret)
{
	allkeys_hashtable_alloc(NULL, pos, ret);
}

_g void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	allkeys_hashtable_alloc(local, pos, ret);
}

/* equalp */
static int equalp_allelement_(addr left, addr right, int *ret,
		int (*call_)(addr, addr, int *))
{
	int check;
	addr list, key, value1, value2;
	size_t size, index;

	size = PtrStructHashtable(left)->size;
	GetTableHash(left, &left);
	for (index = 0; index < size; index++) {
		GetArrayHash(left, index, &list);
		while (list != Nil) {
			GetCons(list, &key, &list);
			GetCons(key, &key, &value1);
			Return(find_hashtable_(right, key, &value2));
			if (value2 == Unbound)
				return Result(ret, 0);
			Return((*call_)(value1, value2, &check));
			if (! check)
				return Result(ret, 0);
		}
	}

	return Result(ret, 1);
}

static int equalcall_hashtable_(addr left, addr right, int *ret,
		int (*call_)(addr, addr, int *))
{
	struct StructHashtable *str1, *str2;

	str1 = PtrStructHashtable(left);
	str2 = PtrStructHashtable(right);
	if (str1->count != str2->count)
		return Result(ret, 0);
	if (str1->test != str2->test)
		return Result(ret, 0);

	return equalp_allelement_(left, right, ret, call_);
}

_g int equalp_hashtable_(addr left, addr right, int *ret)
{
	return equalcall_hashtable_(left, right, ret, equalp_function_);
}

_g int equalrt_hashtable_(addr left, addr right, int *ret)
{
	return equalcall_hashtable_(left, right, ret, equalrt_function_);
}


/* clang */
_g int findcons_char_hashtable_(addr pos, const char *key, addr *ret)
{
	int check;
	fixnum value;
	addr array, root, left, right, str;
	int (*equal_)(addr, const char *, int *);
	size_t index;

	CheckType(pos, LISPTYPE_HASHTABLE);
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_char_equal_(key, &value));
			equal_ = string_equal_char_;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_char_equalp_(key, &value));
			equal_ = string_equalp_char_;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &str);
		if (! stringp(str))
			continue;
		Return((*equal_)(str, key, &check));
		if (! check)
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

_g int find_char_hashtable_(addr pos, const char *key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_char_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

_g int findnil_char_hashtable_(addr pos, const char *key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_char_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
}

_g int findcons_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode);
	size_t index;

	CheckType(pos, LISPTYPE_HASHTABLE);
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQL:
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_unicode_equal_(key, &value));
			equal = character_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_unicode_equalp_(key, &value));
			equal = character_equalp_unicode;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (! characterp(check))
			continue;
		if (! (*equal)(check, key))
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

_g int find_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_unicode_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

_g int findnil_unicode_hashtable_(addr pos, unicode key, addr *ret)
{
	addr cons;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(findcons_unicode_hashtable_(pos, key, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
}

_g int findcons_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	fixnum value;
	addr array, root, left, right, check;
	int (*equal)(addr, unicode, unicode);
	size_t index;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	switch (GetTestHashtable(pos)) {
		case HASHTABLE_TEST_EQUAL:
			Return(sxhash_character2_equal_(a, b, &value));
			equal = character2_equal_unicode;
			break;

		case HASHTABLE_TEST_EQUALP:
			Return(sxhash_character2_equalp_(a, b, &value));
			equal = character2_equalp_unicode;
			break;

		default:
			return Result(ret, Nil);
	}
	index = value % PtrStructHashtable(pos)->size;

	GetTableHash(pos, &array);
	GetArrayHash(array, index, &root);
	for (; root != Nil; root = right) {
		GetCons(root, &left, &right);
		GetCar(left, &check);
		if (GetType(check) != LISPSYSTEM_CHARACTER2)
			continue;
		if (! (*equal)(check, a, b))
			continue;

		return Result(ret, left);
	}

	return Result(ret, Nil);
}

_g int find_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Return(findcons_character2_hashtable_(pos, a, b, &cons));
	if (cons == Nil)
		return Result(ret, Unbound);
	GetCdr(cons, ret);
	return 0;
}

_g int findnil_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret)
{
	addr cons;

	Check(GetType(pos) != LISPTYPE_HASHTABLE, "type hashtable error");
	Return(findcons_character2_hashtable_(pos, a, b, &cons));
	if (cons == Nil)
		return Result(ret, Nil);
	GetCdr(cons, ret);
	return 0;
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

_g void hash_iterator_alloc(LocalRoot local, addr *ret, addr table)
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

_g void hash_iterator_local(LocalRoot local, addr *ret, addr table)
{
	Check(local == NULL, "local error");
	hash_iterator_alloc(local, ret, table);
}

_g void hash_iterator_heap(addr *ret, addr table)
{
	hash_iterator_alloc(NULL, ret, table);
}

_g void set_hash_iterator(addr pos, addr table)
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
_g int next_hash_iterator(addr pos, addr *key, addr *value)
{
	addr list, array, cons;
	struct StructHashIterator *ptr;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_HASHITERATOR);
	/* Iterator is already closed */
	ptr = PtrStructHashIterator(pos);
	if (ptr->finish)
		return 0;
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

