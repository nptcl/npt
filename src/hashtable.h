#ifndef __HASHTABLE_HEADER__
#define __HASHTABLE_HEADER__

#include "build.h"
#include "heap.h"
#include "local.h"
#include "typedef.h"

#define hashtable_full_heap _n(hashtable_full_heap)
#define hashtable_full_local _n(hashtable_full_local)
#define hashtable_full_alloc _n(hashtable_full_alloc)
#define hashtable_integer_heap _n(hashtable_integer_heap)
#define hashtable_integer_local _n(hashtable_integer_local)
#define hashtable_integer_alloc _n(hashtable_integer_alloc)
#define hashtable_heap _n(hashtable_heap)
#define hashtable_local _n(hashtable_local)
#define hashtable_alloc _n(hashtable_alloc)
#define hashtable_size_heap _n(hashtable_size_heap)
#define hashtable_size_local _n(hashtable_size_local)
#define hashtable_size_alloc _n(hashtable_size_alloc)
#define clear_hashtable_heap _n(clear_hashtable_heap)
#define clear_hashtable_local _n(clear_hashtable_local)
#define clear_hashtable _n(clear_hashtable)
#define hashtablep _n(hashtablep)
#define gettest_hashtable _n(gettest_hashtable)
#define settest_hashtable _n(settest_hashtable)
#define gettest_symbol_hashtable _n(gettest_symbol_hashtable)
#define getcount_hashtable _n(getcount_hashtable)
#define inccount_hashtable _n(inccount_hashtable)
#define getsize_hashtable _n(getsize_hashtable)
#define setrehash_float_hashtable _n(setrehash_float_hashtable)
#define getrehash_float_hashtable _n(getrehash_float_hashtable)
#define setrehash_integer_hashtable _n(setrehash_integer_hashtable)
#define getrehash_integer_hashtable _n(getrehash_integer_hashtable)
#define getrehash_threshold_hashtable _n(getrehash_threshold_hashtable)
#define force_resize_hashtable_ _n(force_resize_hashtable_)
#define internp_hashheap_ _n(internp_hashheap_)
#define intern_hashheap_ _n(intern_hashheap_)
#define findcons_hashtable_ _n(findcons_hashtable_)
#define findcons_char_hashtable_ _n(findcons_char_hashtable_)
#define findcons_unicode_hashtable_ _n(findcons_unicode_hashtable_)
#define findcons_character2_hashtable_ _n(findcons_character2_hashtable_)
#define find_hashtable_ _n(find_hashtable_)
#define find_char_hashtable_ _n(find_char_hashtable_)
#define find_unicode_hashtable_ _n(find_unicode_hashtable_)
#define find_character2_hashtable_ _n(find_character2_hashtable_)
#define findnil_hashtable_ _n(findnil_hashtable_)
#define findnil_char_hashtable_ _n(findnil_char_hashtable_)
#define findnil_unicode_hashtable_ _n(findnil_unicode_hashtable_)
#define findnil_character2_hashtable_ _n(findnil_character2_hashtable_)
#define findnil_hashtable_debug _n(findnil_hashtable_debug)
#define delete_hashtable_ _n(delete_hashtable_)
#define allkeys_hashtable_heap _n(allkeys_hashtable_heap)
#define allkeys_hashtable_local _n(allkeys_hashtable_local)
#define allkeys_hashtable_alloc _n(allkeys_hashtable_alloc)
#define equalp_hashtable_ _n(equalp_hashtable_)
#define equalrt_hashtable_ _n(equalrt_hashtable_)
#define hash_iterator_alloc _n(hash_iterator_alloc)
#define hash_iterator_local _n(hash_iterator_local)
#define hash_iterator_heap _n(hash_iterator_heap)
#define set_hash_iterator _n(set_hash_iterator)
#define next_hash_iterator _n(next_hash_iterator)

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

/* intern */
int force_resize_hashtable_(addr pos, size_t size);
int internp_hashheap_(addr pos, addr key, addr *ret, int *existp);
int intern_hashheap_(addr pos, addr key, addr *ret);

/* notfound = nil */
int findcons_hashtable_(addr pos, addr key, addr *ret);
int findcons_char_hashtable_(addr pos, const char *key, addr *ret);
int findcons_unicode_hashtable_(addr pos, unicode key, addr *ret);
int findcons_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret);

/* notfound = unbound */
int find_hashtable_(addr pos, addr key, addr *ret);
int find_char_hashtable_(addr pos, const char *key, addr *ret);
int find_unicode_hashtable_(addr pos, unicode key, addr *ret);
int find_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret);

/* notfound = nil */
int findnil_hashtable_(addr pos, addr key, addr *ret);
int findnil_char_hashtable_(addr pos, const char *key, addr *ret);
int findnil_unicode_hashtable_(addr pos, unicode key, addr *ret);
int findnil_character2_hashtable_(addr pos, unicode a, unicode b, addr *ret);

/* debug */
int findnil_hashtable_debug(addr pos, addr key, addr *ret);

/* delete=0, notfound=1 */
int delete_hashtable_(addr pos, addr key, int *ret);

/* map-function */
void allkeys_hashtable_heap(addr pos, addr *ret);
void allkeys_hashtable_local(LocalRoot local, addr pos, addr *ret);
void allkeys_hashtable_alloc(LocalRoot local, addr pos, addr *ret);

/* equalp */
int equalp_hashtable_(addr left, addr right, int *ret);
int equalrt_hashtable_(addr left, addr right, int *ret);

/* iterator */
void hash_iterator_alloc(LocalRoot local, addr *ret, addr table);
void hash_iterator_local(LocalRoot local, addr *ret, addr table);
void hash_iterator_heap(addr *ret, addr table);
void set_hash_iterator(addr pos, addr table);
/* 0:finish, 1:find */
int next_hash_iterator(addr pos, addr *key, addr *value);

#endif

