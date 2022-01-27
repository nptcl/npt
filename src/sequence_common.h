#ifndef __SEQUENCE_COMMON_HEADER__
#define __SEQUENCE_COMMON_HEADER__

#include "sequence_count.h"
#include "typedef.h"

#define make_specialized_sequence_ _n(make_specialized_sequence_)
#define array_upgraded_merge_sequence_ _n(array_upgraded_merge_sequence_)
#define make_vector_size_sequence_ _n(make_vector_size_sequence_)
#define setcount_sequence_ _n(setcount_sequence_)

int make_specialized_sequence_(addr *ret, enum ARRAY_TYPE type, int bs, size_t size);
int array_upgraded_merge_sequence_(addr *ret, addr type, size_t size);
int make_vector_size_sequence_(addr *ret, addr pos, size_t size);
int setcount_sequence_(struct count_struct *str, addr count);

#endif

