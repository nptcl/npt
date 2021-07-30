#ifndef __TERME_DATA_HEADER__
#define __TERME_DATA_HEADER__

#include "typedef.h"

#define terme_data_build _n(terme_data_build)
#define terme_data_clear_ _n(terme_data_clear_)
#define terme_data_insert_ _n(terme_data_insert_)
#define terme_data_next_ _n(terme_data_next_)
#define terme_data_push_ _n(terme_data_push_)
#define terme_data_make_ _n(terme_data_make_)
#define terme_data_copy_ _n(terme_data_copy_)
#define terme_data_size_ _n(terme_data_size_)

#define terme_data_get_value _n(terme_data_get_value)
#define terme_data_get_character _n(terme_data_get_character)

#define terme_data_left_ _n(terme_data_left_)
#define terme_data_right_ _n(terme_data_right_)
#define terme_data_first_ _n(terme_data_first_)
#define terme_data_last_ _n(terme_data_last_)
#define terme_data_delete_ _n(terme_data_delete_)
#define terme_data_backspace_ _n(terme_data_backspace_)
#define terme_data_rmleft_ _n(terme_data_rmleft_)
#define terme_data_rmright_ _n(terme_data_rmright_)

void terme_data_build(addr *ret);
int terme_data_clear_(Execute ptr);
int terme_data_insert_(Execute ptr, unicode c, unsigned *rwidth, int *ret);
int terme_data_next_(Execute ptr);
int terme_data_push_(Execute ptr, unicode c, unsigned *rwidth, int *ret);
int terme_data_make_(Execute ptr, addr *ret, int eol);
int terme_data_copy_(Execute ptr, addr value);
int terme_data_size_(Execute ptr, unsigned *ret);

void terme_data_get_value(addr pos, unsigned *rnow, unsigned *rsize);
int terme_data_get_character(addr pos, unsigned i, unicode *retc, unsigned *retw);

int terme_data_left_(Execute ptr, unsigned *ret);
int terme_data_right_(Execute ptr, unsigned *ret);
int terme_data_first_(Execute ptr);
int terme_data_last_(Execute ptr);
int terme_data_delete_(Execute ptr, int *ret);
int terme_data_backspace_(Execute ptr, unsigned *ret);
int terme_data_rmleft_(Execute ptr, int *ret);
int terme_data_rmright_(Execute ptr, int *ret);

#endif

