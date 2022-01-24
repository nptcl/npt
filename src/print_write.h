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
#define write_print_ _n(write_print_)
#define princ_print_ _n(princ_print_)
#define prin1_print_ _n(prin1_print_)
#define print_print_ _n(print_print_)
#define pprint_print_ _n(pprint_print_)
#define write_string_heap_ _n(write_string_heap_)
#define write_string_local_ _n(write_string_local_)
#define princ_string_heap_ _n(princ_string_heap_)
#define princ_string_local_ _n(princ_string_local_)
#define prin1_string_heap_ _n(prin1_string_heap_)
#define prin1_string_local_ _n(prin1_string_local_)
#define init_print_write _n(init_print_write)

void push_write_object(Execute ptr);
void getdepth_print_write(Execute ptr, size_t *ret);
void setdepth_print_write(Execute ptr, size_t value);
void write_check_all_clear(Execute ptr);
int write_check_call_(Execute ptr, addr pos);
int pprint_pop_circle_(Execute ptr, addr stream, addr pos, int *ret);
int pprint_check_circle_(Execute ptr, addr pos, addr *value, int *ret);
int write_default_print_(Execute ptr, addr stream, addr pos);
int write_print_(Execute ptr, addr stream, addr pos);
int princ_print_(Execute ptr, addr stream, addr pos);
int prin1_print_(Execute ptr, addr stream, addr pos);
int print_print_(Execute ptr, addr stream, addr pos);
int pprint_print_(Execute ptr, addr stream, addr pos);
int write_string_heap_(Execute ptr, addr *ret, addr pos);
int write_string_local_(Execute ptr, addr *ret, addr pos);
int princ_string_heap_(Execute ptr, addr *ret, addr pos);
int princ_string_local_(Execute ptr, addr *ret, addr pos);
int prin1_string_heap_(Execute ptr, addr *ret, addr pos);
int prin1_string_local_(Execute ptr, addr *ret, addr pos);
void init_print_write(void);

#endif

