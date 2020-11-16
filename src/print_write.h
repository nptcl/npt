#ifndef __PRINT_TYPE_HEADER__
#define __PRINT_TYPE_HEADER__

#include "define.h"
#include "print.h"
#include "typedef.h"

#define push_write_object _n(push_write_object)
#define getdepth_print_write _n(getdepth_print_write)
#define setdepth_print_write _n(setdepth_print_write)
#define write_check_all_clear _n(write_check_all_clear)
#define write_check_call_ _n(write_check_call_)
#define pprint_pop_circle_ _n(pprint_pop_circle_)
#define pprint_check_circle_ _n(pprint_check_circle_)
#define write_default_print_ _n(write_default_print_)
#define write_print _n(write_print)
#define princ_print _n(princ_print)
#define prin1_print _n(prin1_print)
#define print_print _n(print_print)
#define pprint_print _n(pprint_print)
#define write_string_heap _n(write_string_heap)
#define write_string_local _n(write_string_local)
#define princ_string_heap _n(princ_string_heap)
#define princ_string_local _n(princ_string_local)
#define prin1_string_heap _n(prin1_string_heap)
#define prin1_string_local _n(prin1_string_local)
#define init_print_write _n(init_print_write)

void push_write_object(Execute ptr);
void getdepth_print_write(Execute ptr, size_t *ret);
void setdepth_print_write(Execute ptr, size_t value);
void write_check_all_clear(Execute ptr);
int write_check_call_(Execute ptr, addr pos);
int pprint_pop_circle_(Execute ptr, addr stream, addr pos, int *ret);
int pprint_check_circle_(Execute ptr, addr pos, addr *value, int *ret);
int write_default_print_(Execute ptr, addr stream, addr pos);
int write_print(Execute ptr, addr stream, addr pos);
int princ_print(Execute ptr, addr stream, addr pos);
int prin1_print(Execute ptr, addr stream, addr pos);
int print_print(Execute ptr, addr stream, addr pos);
int pprint_print(Execute ptr, addr stream, addr pos);
int write_string_heap(Execute ptr, addr *ret, addr pos);
int write_string_local(Execute ptr, addr *ret, addr pos);
int princ_string_heap(Execute ptr, addr *ret, addr pos);
int princ_string_local(Execute ptr, addr *ret, addr pos);
int prin1_string_heap(Execute ptr, addr *ret, addr pos);
int prin1_string_local(Execute ptr, addr *ret, addr pos);
void init_print_write(void);

#endif

