#ifndef __SYSCODE_FUNCTION_HEADER__
#define __SYSCODE_FUNCTION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define hello_syscode_ _n(hello_syscode_)
#define infobit_syscode _n(infobit_syscode)
#define infoprint_syscode _n(infoprint_syscode)
#define gc_syscode _n(gc_syscode)
#define savecore_syscode_ _n(savecore_syscode_)
#define package_export_list_syscode_ _n(package_export_list_syscode_)
#define specialp_syscode _n(specialp_syscode)
#define array_general_p_syscode _n(array_general_p_syscode)
#define array_specialized_p_syscode _n(array_specialized_p_syscode)
#define simple_sort_syscode_ _n(simple_sort_syscode_)
#define bubble_sort_syscode_ _n(bubble_sort_syscode_)
#define quick_sort_syscode_ _n(quick_sort_syscode_)
#define merge_sort_syscode_ _n(merge_sort_syscode_)
#define exit_syscode_ _n(exit_syscode_)
#define closp_syscode _n(closp_syscode)
#define fixnump_syscode _n(fixnump_syscode)
#define bignump_syscode _n(bignump_syscode)
#define ratiop_syscode _n(ratiop_syscode)
#define short_float_p_syscode _n(short_float_p_syscode)
#define single_float_p_syscode _n(single_float_p_syscode)
#define double_float_p_syscode _n(double_float_p_syscode)
#define long_float_p_syscode _n(long_float_p_syscode)
#define callnamep_syscall _n(callnamep_syscall)
#define large_number_syscode_ _n(large_number_syscode_)
#define make_character_syscode_ _n(make_character_syscode_)
#define make_fixnum_syscode_ _n(make_fixnum_syscode_)
#define make_bignum_syscode_ _n(make_bignum_syscode_)
#define make_ratio_syscode_ _n(make_ratio_syscode_)
#define make_complex_code_ _n(make_complex_code_)
#define equal_random_state_syscode _n(equal_random_state_syscode)
#define subtypep_extend_syscode_ _n(subtypep_extend_syscode_)
#define subtypep_number_syscode_ _n(subtypep_number_syscode_)
#define eastasian_set_syscode_ _n(eastasian_set_syscode_)
#define eastasian_get_syscode_ _n(eastasian_get_syscode_)
#define eastasian_width_syscode_ _n(eastasian_width_syscode_)
#define run_program_syscode_ _n(run_program_syscode_)
#define make_callname_syscode_ _n(make_callname_syscode_)
#define remove_file_syscode_ _n(remove_file_syscode_)
#define remove_directory_syscode_ _n(remove_directory_syscode_)
#define declare_parse_syscode_ _n(declare_parse_syscode_)
#define parse_type_syscode_ _n(parse_type_syscode_)
#define upgraded_open_element_type_syscode_ _n(upgraded_open_element_type_syscode_)
#define make_memory_input_stream_syscode_ _n(make_memory_input_stream_syscode_)
#define make_memory_output_stream_syscode_ _n(make_memory_output_stream_syscode_)
#define make_memory_io_stream_syscode_ _n(make_memory_io_stream_syscode_)
#define with_input_from_memory_syscode_ _n(with_input_from_memory_syscode_)
#define with_output_to_memory_syscode_ _n(with_output_to_memory_syscode_)
#define get_output_stream_memory_syscode_ _n(get_output_stream_memory_syscode_)
#define byte_integer_syscode_ _n(byte_integer_syscode_)
#define memory_stream_p_syscode _n(memory_stream_p_syscode)
#define setf_memory_stream_p_syscode_ _n(setf_memory_stream_p_syscode_)
#define sysctl_syscode_ _n(sysctl_syscode_)
#define extension_syscode_ _n(extension_syscode_)
#define terme_syscode_ _n(terme_syscode_)
#define fpclassify_syscode _n(fpclassify_syscode)

int hello_syscode_(Execute ptr);
void infobit_syscode(addr rest, addr *ret);
void infoprint_syscode(addr rest, addr *ret);
void gc_syscode(addr rest);
int savecore_syscode_(Execute ptr, addr file);
int package_export_list_syscode_(addr var, addr *ret);
void specialp_syscode(addr var, addr *ret);
void array_general_p_syscode(addr var, addr *ret);
void array_specialized_p_syscode(addr var, addr *ret);
int simple_sort_syscode_(Execute ptr, addr pos, addr call, addr rest);
int bubble_sort_syscode_(Execute ptr, addr pos, addr call, addr rest);
int quick_sort_syscode_(Execute ptr, addr pos, addr call, addr rest);
int merge_sort_syscode_(Execute ptr, addr pos, addr call, addr rest);
int exit_syscode_(Execute ptr, addr code);
void closp_syscode(addr var, addr *ret);
void fixnump_syscode(addr var, addr *ret);
void bignump_syscode(addr var, addr *ret);
void ratiop_syscode(addr var, addr *ret);
void short_float_p_syscode(addr var, addr *ret);
void single_float_p_syscode(addr var, addr *ret);
void double_float_p_syscode(addr var, addr *ret);
void long_float_p_syscode(addr var, addr *ret);
void callnamep_syscall(addr var, addr *ret);
int large_number_syscode_(LocalRoot local, addr var, addr opt, addr *ret);
int make_character_syscode_(addr var, addr *ret);
int make_fixnum_syscode_(addr var, addr *ret);
int make_bignum_syscode_(addr var, addr *ret);
int make_ratio_syscode_(addr numer, addr denom, addr *ret);
int make_complex_code_(addr real, addr imag, addr *ret);
void equal_random_state_syscode(addr left, addr right, addr *ret);
int subtypep_extend_syscode_(Execute ptr,
		addr x, addr y, addr env, addr check, addr *ret);
int subtypep_number_syscode_(Execute ptr, addr x, addr *ret);
int eastasian_set_syscode_(addr var, addr value, addr errorp, addr *ret);
int eastasian_get_syscode_(addr var, addr *ret1, addr *ret2);
int eastasian_width_syscode_(addr pos, addr *ret1, addr *ret2);
int run_program_syscode_(Execute ptr, addr var, addr args, addr rest, addr *ret);
int make_callname_syscode_(addr var, addr *ret);
int remove_file_syscode_(Execute ptr, addr var, addr opt, addr *ret);
int remove_directory_syscode_(Execute ptr, addr var, addr opt, addr *ret);
int declare_parse_syscode_(addr form, addr *ret);
int parse_type_syscode_(Execute ptr, addr var, addr *ret);
int upgraded_open_element_type_syscode_(addr var, addr *ret);
int make_memory_input_stream_syscode_(addr var, addr rest, addr *ret);
int make_memory_output_stream_syscode_(addr rest, addr *ret);
int make_memory_io_stream_syscode_(addr rest, addr *ret);
int with_input_from_memory_syscode_(Execute ptr, addr form, addr *ret);
int with_output_to_memory_syscode_(Execute ptr, addr form, addr *ret);
int get_output_stream_memory_syscode_(addr var, addr *ret);
int byte_integer_syscode_(addr list, addr *ret);
void memory_stream_p_syscode(addr var, addr *ret);
int setf_memory_stream_p_syscode_(addr var, addr value);
int sysctl_syscode_(Execute ptr, addr var, addr args);
int extension_syscode_(Execute ptr, addr var);
int terme_syscode_(Execute ptr, addr var, addr args);
void fpclassify_syscode(addr var, addr *rtype, addr *rsign);

#endif

