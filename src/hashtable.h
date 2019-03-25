#ifndef __HASHTABLE_HEADER__
#define __HASHTABLE_HEADER__

#include "build.h"
#include "heap.h"
#include "local.h"
#include "typedef.h"

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

void hashtable_full_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);
void hashtable_full_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);
void hashtable_full_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		double_float rehash_size, double_float rehash_threshold);

void hashtable_integer_heap(addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);
void hashtable_integer_local(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);
void hashtable_integer_alloc(LocalRoot local, addr *ret,
		enum HASHTABLE_TEST test, size_t size,
		size_t rehash_size, double_float rehash_threshold);

void hashtable_heap(addr *ret);
void hashtable_local(LocalRoot local, addr *ret);
void hashtable_alloc(LocalRoot local, addr *ret);
void hashtable_size_heap(addr *ret, size_t size);
void hashtable_size_local(LocalRoot local, addr *ret, size_t size);
void hashtable_size_alloc(LocalRoot local, addr *ret, size_t size);
void clear_hashtable_heap(addr pos);
void clear_hashtable_local(addr pos);
void clear_hashtable(addr pos);
int hashtablep(addr pos);

void gettest_hashtable(addr pos, enum HASHTABLE_TEST *ret);
void settest_hashtable(addr pos, enum HASHTABLE_TEST value);
void gettest_symbol_hashtable(addr pos, addr *ret);
void getcount_hashtable(addr pos, size_t *ret);
void inccount_hashtable(addr pos, size_t value);
void getsize_hashtable(addr pos, size_t *ret);
void setrehash_float_hashtable(addr pos, double_float value);
int getrehash_float_hashtable(addr pos, double_float *ret);
void setrehash_integer_hashtable(addr pos, size_t value);
int getrehash_integer_hashtable(addr pos, size_t *ret);
void getrehash_threshold_hashtable(addr pos, double_float *ret);
/* delete=0, notfound=1 */
int delete_hashtable(addr pos, addr key);
/* map-function */
void allkeys_hashtable_heap(addr pos, addr *ret);
void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret);
void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret);

void force_resize_hashtable(addr pos, size_t size);
/* found=0, notfound=1 */
int intern_hashheap(addr pos, addr key, addr *ret);
int intern_hashlocal(LocalRoot root, addr pos, addr key, addr *ret);
int intern_hashalloc(LocalRoot root, addr pos, addr key, addr *ret);
int intern_hashtable(LocalRoot root, addr pos, addr key, addr *ret);
/* notfound=nil */
void findcons_hashtable(addr pos, addr key, addr *ret);
void findcons_char_hashtable(addr pos, const char *key, addr *ret);
void findcons_unicode_hashtable(addr pos, unicode key, addr *ret);
void findcons_character2_hashtable(addr pos, unicode a, unicode b, addr *ret);
/* found=0, notfound=1 */
int findvalue_hashtable(addr pos, addr key, addr *ret);
int findvalue_char_hashtable(addr pos, const char *key, addr *ret);
int findvalue_unicode_hashtable(addr pos, unicode key, addr *ret);
int findvalue_character2_hashtable(addr pos, unicode a, unicode b, addr *ret);
/* equalp */
int equalp_hashtable(addr left, addr right);
int equalrt_hashtable(addr left, addr right);

/* iterator */
void hash_iterator_alloc(LocalRoot local, addr *ret, addr table);
void hash_iterator_local(LocalRoot local, addr *ret, addr table);
void hash_iterator_heap(addr *ret, addr table);
void set_hash_iterator(addr pos, addr table);
/* 0:finish, 1:find */
int next_hash_iterator(addr pos, addr *key, addr *value);

#endif

