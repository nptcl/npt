#ifndef __HASHTABLE_HEADER__
#define __HASHTABLE_HEADER__

#include "build.h"
#include "heap.h"
#include "local.h"
#include "typedef.h"

enum HASHTABLE_INDEX {
	HASHTABLE_INDEX_ARRAY = 0,
	HASHTABLE_INDEX_PADDING,
	HASHTABLE_INDEX_SIZE
};

enum HASHTABLE_TEST {
	HASHTABLE_TEST_EQ = 0,
	HASHTABLE_TEST_EQL,
	HASHTABLE_TEST_EQUAL,
	HASHTABLE_TEST_EQUALP,
	HASHTABLE_TEST_CACHE, /* for generic function */
	HASHTABLE_TEST_SIZE
};

struct StructHashtable {
	unsigned resize_float_p : 1;
	unsigned expand_p : 1;
	enum HASHTABLE_TEST test;
	size_t count, size, limit;
	size_t resize_integer;
	double_float resize_float;
	double_float threshold;
};

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

#define PtrHashtable(x) PtrBodySSa(x, HASHTABLE_INDEX_SIZE)
#define PtrStructHashtable(x) ((struct StructHashtable *)PtrHashtable(x))
#define GetTestHashtable(x) ((int)PtrStructHashtable(x)->test)

#ifdef LISP_ARCH_32BIT
#define GetArrayHash		GetArrayA4
#define SetArrayHash		SetArrayA4
#define LenArrayHash		LenArrayA4
#else
#define GetArrayHash		GetArrayA8
#define SetArrayHash		SetArrayA8
#define LenArrayHash		LenArrayA8
#endif
#define GetTableHash(x,y)	GetArraySS((x), HASHTABLE_INDEX_ARRAY, (y))

#define HASHTABLE_TEST_DEFAULT				HASHTABLE_TEST_EQ
#define HASHTABLE_SIZE_DEFAULT				4
#define HASHTABLE_REHASH_SIZE_DEFAULT		1.5
#define HASHTABLE_REHASH_THRESHOLD_DEFAULT	1.0

_g void hashtable_full_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);
_g void hashtable_full_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);
_g void hashtable_full_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);

_g void hashtable_integer_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);
_g void hashtable_integer_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);
_g void hashtable_integer_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);

_g void hashtable_heap(addr *ret);
_g void hashtable_local(LocalRoot local, addr *ret);
_g void hashtable_alloc(LocalRoot local, addr *ret);
_g void hashtable_size_heap(addr *ret, size_t size);
_g void hashtable_size_local(LocalRoot local, addr *ret, size_t size);
_g void hashtable_size_alloc(LocalRoot local, addr *ret, size_t size);
_g void clear_hashtable_heap(addr pos);
_g void clear_hashtable_local(addr pos);
_g void clear_hashtable(addr pos);
_g int hashtablep(addr pos);

_g void gettest_hashtable(addr pos, enum HASHTABLE_TEST *ret);
_g void settest_hashtable(addr pos, enum HASHTABLE_TEST value);
_g void gettest_symbol_hashtable(addr pos, addr *ret);
_g void getcount_hashtable(addr pos, size_t *ret);
_g void inccount_hashtable(addr pos, size_t value);
_g void getsize_hashtable(addr pos, size_t *ret);
_g void setrehash_float_hashtable(addr pos, double_float value);
_g int getrehash_float_hashtable(addr pos, double_float *ret);
_g void setrehash_integer_hashtable(addr pos, size_t value);
_g int getrehash_integer_hashtable(addr pos, size_t *ret);
_g void getrehash_threshold_hashtable(addr pos, double_float *ret);
/* delete=0, notfound=1 */
_g int delete_hashtable(addr pos, addr key);
/* map-function */
_g void allkeys_hashtable_heap(addr pos, addr *ret);
_g void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret);
_g void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret);

_g void force_resize_hashtable(addr pos, size_t size);
/* found=1, notfound=0 */
_g int intern_hashheap(addr pos, addr key, addr *ret);
_g int intern_hashlocal(LocalRoot root, addr pos, addr key, addr *ret);
_g int intern_hashalloc(LocalRoot root, addr pos, addr key, addr *ret);
_g int intern_hashtable(LocalRoot root, addr pos, addr key, addr *ret);
/* notfound=nil */
_g void findcons_hashtable(addr pos, addr key, addr *ret);
_g void findcons_char_hashtable(addr pos, const char *key, addr *ret);
_g void findcons_unicode_hashtable(addr pos, unicode key, addr *ret);
_g void findcons_character2_hashtable(addr pos, unicode a, unicode b, addr *ret);
/* found=1, notfound=0 */
_g int findvalue_hashtable(addr pos, addr key, addr *ret);
_g int findvalue_char_hashtable(addr pos, const char *key, addr *ret);
_g int findvalue_unicode_hashtable(addr pos, unicode key, addr *ret);
_g int findvalue_character2_hashtable(addr pos, unicode a, unicode b, addr *ret);
/* equalp */
_g int equalp_hashtable(addr left, addr right);
_g int equalrt_hashtable(addr left, addr right);

/* iterator */
_g void hash_iterator_alloc(LocalRoot local, addr *ret, addr table);
_g void hash_iterator_local(LocalRoot local, addr *ret, addr table);
_g void hash_iterator_heap(addr *ret, addr table);
_g void set_hash_iterator(addr pos, addr table);
/* 0:finish, 1:find */
_g int next_hash_iterator(addr pos, addr *key, addr *value);

#endif

