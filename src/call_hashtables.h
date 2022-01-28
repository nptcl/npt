#ifndef __CALL_HASHTABLES_HEADER__
#define __CALL_HASHTABLES_HEADER__

#include "local.h"
#include "execute.h"
#include "typedef.h"

#define make_hash_table_common_ _n(make_hash_table_common_)
#define hash_table_count_common _n(hash_table_count_common)
#define hash_table_rehash_size_common_ _n(hash_table_rehash_size_common_)
#define hash_table_rehash_threshold_common _n(hash_table_rehash_threshold_common)
#define hash_table_size_common _n(hash_table_size_common)
#define hash_table_test_common _n(hash_table_test_common)
#define gethash_common_ _n(gethash_common_)
#define setf_gethash_common_ _n(setf_gethash_common_)
#define remhash_common_ _n(remhash_common_)
#define maphash_common_ _n(maphash_common_)
#define with_hash_table_iterator_common_ _n(with_hash_table_iterator_common_)
#define sxhash_common_ _n(sxhash_common_)

int make_hash_table_common_(addr rest, addr *ret);
void hash_table_count_common(addr var, addr *ret);
int hash_table_rehash_size_common_(addr var, addr *ret);
void hash_table_rehash_threshold_common(addr var, addr *ret);
void hash_table_size_common(addr var, addr *ret);
void hash_table_test_common(addr var, addr *ret);
int gethash_common_(addr key, addr table, addr value, addr *ret, addr *check);
int setf_gethash_common_(LocalRoot local, addr value, addr key, addr table);
int remhash_common_(addr key, addr table, addr *ret);
int maphash_common_(Execute ptr, addr call, addr table);
int with_hash_table_iterator_common_(Execute ptr, addr form, addr env, addr *ret);
int sxhash_common_(addr var, addr *ret);

#endif

