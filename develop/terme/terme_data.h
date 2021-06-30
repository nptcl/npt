#ifndef __TERME_DATA_HEADER__
#define __TERME_DATA_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_data_init_ _n(terme_data_init_)
#define terme_data_push_ _n(terme_data_push_)
#define terme_data_get_ _n(terme_data_get_)
#define terme_data_get_width_ _n(terme_data_get_width_)
#define terme_data_size_ _n(terme_data_size_)
#define terme_data_size_width_ _n(terme_data_size_width_)
#define terme_data_allwidth_ _n(terme_data_allwidth_)
#define terme_data_delete_ _n(terme_data_delete_)
#define terme_data_delete_left_ _n(terme_data_delete_left_)
#define terme_data_delete_right_ _n(terme_data_delete_right_)
#define terme_data_make_ _n(terme_data_make_)
#define terme_history_save_ _n(terme_history_save_)
#define terme_history_update_ _n(terme_history_update_)

int terme_data_init_(Execute ptr);
int terme_data_push_(Execute ptr, int index, unicode c, int *ret);
int terme_data_get_(Execute ptr, int index, unicode *value, int *ret);
int terme_data_get_width_(Execute ptr, int index, int *ret);
int terme_data_size_(Execute ptr, int *ret);
int terme_data_size_width_(Execute ptr, int *size, int *width);
int terme_data_allwidth_(Execute ptr, int *ret);
int terme_data_delete_(Execute ptr, int index, int *ret);
int terme_data_delete_left_(Execute ptr, int index, int *ret);
int terme_data_delete_right_(Execute ptr, int index, int *ret);
int terme_data_make_(Execute ptr, addr *ret);
int terme_history_save_(Execute ptr);
int terme_history_update_(Execute ptr, int index, int *ret);

#endif
