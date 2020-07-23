#ifndef __CALL_HASHTABLES_HEADER__
#define __CALL_HASHTABLES_HEADER__

#include "local.h"
#include "execute.h"
#include "typedef.h"

_g int make_hash_table_common(addr rest, addr *ret);
_g void hash_table_count_common(addr var, addr *ret);
_g int hash_table_rehash_size_common(addr var, addr *ret);
_g void hash_table_rehash_threshold_common(addr var, addr *ret);
_g void hash_table_size_common(addr var, addr *ret);
_g void hash_table_test_common(addr var, addr *ret);
_g int gethash_common_(addr key, addr table, addr value, addr *ret, addr *check);
_g int setf_gethash_common_(LocalRoot local, addr value, addr key, addr table);
_g int remhash_common_(addr key, addr table, addr *ret);
_g int maphash_common(Execute ptr, addr call, addr table);
_g int with_hash_table_iterator_common(Execute ptr, addr form, addr env, addr *ret);
_g int sxhash_common_(addr var, addr *ret);

#endif

