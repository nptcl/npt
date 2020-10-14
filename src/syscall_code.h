#ifndef __SYSCALL_CODE_HEADER__
#define __SYSCALL_CODE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define hello_syscode _n(hello_syscode)
#define infobit_syscode _n(infobit_syscode)
#define infoprint_syscode _n(infoprint_syscode)
#define gc_syscode _n(gc_syscode)
#define savecore_syscode _n(savecore_syscode)
#define redirect_restart_syscode _n(redirect_restart_syscode)
#define defconstant_syscode _n(defconstant_syscode)
#define in_package_syscode_ _n(in_package_syscode_)
#define setplist_syscode _n(setplist_syscode)
#define remplist_syscode_ _n(remplist_syscode_)
#define make_hash_iterator_syscode _n(make_hash_iterator_syscode)
#define next_hash_iterator_syscode _n(next_hash_iterator_syscode)
#define make_package_iterator_syscode_ _n(make_package_iterator_syscode_)
#define next_package_iterator_syscode_ _n(next_package_iterator_syscode_)
#define defpackage_syscode _n(defpackage_syscode)
#define do_symbols_syscode _n(do_symbols_syscode)
#define do_external_symbols_syscode _n(do_external_symbols_syscode)
#define do_all_symbols_syscode_ _n(do_all_symbols_syscode_)
#define getdoc_variable_syscode _n(getdoc_variable_syscode)
#define setdoc_variable_syscode _n(setdoc_variable_syscode)
#define specialp_syscode _n(specialp_syscode)
#define ecase_error_syscode_ _n(ecase_error_syscode_)
#define etypecase_error_syscode_ _n(etypecase_error_syscode_)
#define define_setf_expander_syscode_ _n(define_setf_expander_syscode_)
#define defsetf_short_syscode _n(defsetf_short_syscode)
#define defsetf_long_syscode _n(defsetf_long_syscode)
#define array_general_p_syscode _n(array_general_p_syscode)
#define array_specialized_p_syscode _n(array_specialized_p_syscode)
#define simple_sort_syscode _n(simple_sort_syscode)
#define bubble_sort_syscode _n(bubble_sort_syscode)
#define quick_sort_syscode _n(quick_sort_syscode)
#define merge_sort_syscode _n(merge_sort_syscode)
#define exit_syscode_ _n(exit_syscode_)
#define end_input_stream_syscode _n(end_input_stream_syscode)
#define make_extend_output_stream_syscode _n(make_extend_output_stream_syscode)
#define prompt_for_syscode _n(prompt_for_syscode)
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
#define print_unreadable_call_syscode _n(print_unreadable_call_syscode)
#define write_default_syscode _n(write_default_syscode)
#define make_character_syscode _n(make_character_syscode)
#define make_fixnum_syscode _n(make_fixnum_syscode)
#define make_bignum_syscode _n(make_bignum_syscode)
#define make_ratio_syscode _n(make_ratio_syscode)
#define make_complex_code_ _n(make_complex_code_)
#define equal_random_state_syscode _n(equal_random_state_syscode)
#define symbol_deftype_syscode _n(symbol_deftype_syscode)
#define delete_deftype_syscode _n(delete_deftype_syscode)
#define subtypep_result_syscode _n(subtypep_result_syscode)
#define ensure_structure_syscode_ _n(ensure_structure_syscode_)
#define structure_constructor_syscode _n(structure_constructor_syscode)
#define loop_bind_syscode _n(loop_bind_syscode)
#define make_pprint_stream_syscode_ _n(make_pprint_stream_syscode_)
#define pprint_gensym_syscode _n(pprint_gensym_syscode)
#define pprint_exit_syscode _n(pprint_exit_syscode)
#define pprint_pop_syscode _n(pprint_pop_syscode)
#define pprint_check_syscode _n(pprint_check_syscode)
#define pprint_close_syscode _n(pprint_close_syscode)
#define pprint_pretty_syscode _n(pprint_pretty_syscode)
#define eastasian_set_syscode_ _n(eastasian_set_syscode_)
#define eastasian_get_syscode_ _n(eastasian_get_syscode_)
#define eastasian_width_syscode_ _n(eastasian_width_syscode_)
#define timeinfo_syscode_ _n(timeinfo_syscode_)
#define ed_function_syscode_ _n(ed_function_syscode_)
#define run_program_syscode_ _n(run_program_syscode_)
#define make_callname_syscode_ _n(make_callname_syscode_)
#define trace_add_syscode_ _n(trace_add_syscode_)
#define trace_del_syscode_ _n(trace_del_syscode_)
#define set_slots_syscode _n(set_slots_syscode)
#define remove_file_syscode _n(remove_file_syscode)
#define remove_directory_syscode _n(remove_directory_syscode)
#define declare_parse_syscode _n(declare_parse_syscode)
#define parse_type_syscode _n(parse_type_syscode)
#define type_object_syscode _n(type_object_syscode)
#define extension_syscode _n(extension_syscode)

_g int hello_syscode(Execute ptr);
_g void infobit_syscode(addr rest, addr *ret);
_g void infoprint_syscode(addr rest, addr *ret);
_g void gc_syscode(addr rest);
_g int savecore_syscode(Execute ptr, addr file);
_g int redirect_restart_syscode(Execute ptr, addr condition, addr list);
_g int defconstant_syscode(addr symbol, addr value, addr doc);
_g int in_package_syscode_(Execute ptr, addr name, addr *ret);
_g void setplist_syscode(addr key, addr value, addr list, addr *ret);
_g int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2);
_g void make_hash_iterator_syscode(addr pos, addr *ret);
_g void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3);
_g int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret);
_g int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4);
_g int defpackage_syscode(Execute ptr, addr var, addr rest, addr *ret);
_g int do_symbols_syscode(Execute ptr, addr call, addr package);
_g int do_external_symbols_syscode(Execute ptr, addr call, addr package);
_g int do_all_symbols_syscode_(Execute ptr, addr call);
_g void getdoc_variable_syscode(addr var, addr *ret);
_g void setdoc_variable_syscode(addr var, addr value);
_g void specialp_syscode(addr var, addr *ret);
_g int ecase_error_syscode_(Execute ptr, addr value, addr list);
_g int etypecase_error_syscode_(Execute ptr, addr value, addr list);
_g int define_setf_expander_syscode_(addr symbol, addr call);
_g int defsetf_short_syscode(Execute ptr,
		addr access, addr update, addr args, addr env,
		addr *r1, addr *r2, addr *r3, addr *r4, addr *r5);
_g int defsetf_long_syscode(Execute ptr, addr rest,
		addr *r1, addr *r2, addr *r3, addr *r4, addr *r5);
_g void array_general_p_syscode(addr var, addr *ret);
_g void array_specialized_p_syscode(addr var, addr *ret);
_g int simple_sort_syscode(Execute ptr, addr pos, addr call, addr rest);
_g int bubble_sort_syscode(Execute ptr, addr pos, addr call, addr rest);
_g int quick_sort_syscode(Execute ptr, addr pos, addr call, addr rest);
_g int merge_sort_syscode(Execute ptr, addr pos, addr call, addr rest);
_g int exit_syscode_(Execute ptr, addr code);
_g void end_input_stream_syscode(addr var, addr *ret);
_g void make_extend_output_stream_syscode(addr var, addr rest, addr *ret);
_g int prompt_for_syscode(Execute ptr, addr type, addr args, addr *ret);
_g void closp_syscode(addr var, addr *ret);
_g void fixnump_syscode(addr var, addr *ret);
_g void bignump_syscode(addr var, addr *ret);
_g void ratiop_syscode(addr var, addr *ret);
_g void short_float_p_syscode(addr var, addr *ret);
_g void single_float_p_syscode(addr var, addr *ret);
_g void double_float_p_syscode(addr var, addr *ret);
_g void long_float_p_syscode(addr var, addr *ret);
_g void callnamep_syscall(addr var, addr *ret);
_g int large_number_syscode_(LocalRoot local, addr var, addr opt, addr *ret);
_g int print_unreadable_call_syscode(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body);
_g int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret);
_g int make_character_syscode(addr var, addr *ret);
_g int make_fixnum_syscode(addr var, addr *ret);
_g int make_bignum_syscode(addr var, addr *ret);
_g int make_ratio_syscode(addr numer, addr denom, addr *ret);
_g int make_complex_code_(addr real, addr imag, addr *ret);
_g void equal_random_state_syscode(addr left, addr right, addr *ret);
_g void symbol_deftype_syscode(addr var, addr *ret);
_g void delete_deftype_syscode(addr var, addr *ret);
_g int subtypep_result_syscode(Execute ptr, addr left, addr right, addr *ret);
_g int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest);
_g int structure_constructor_syscode(Execute ptr, addr symbol, addr rest, addr *ret);
_g int loop_bind_syscode(Execute ptr, addr a, addr b, addr c, addr *ret);
_g int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix);
_g int pprint_gensym_syscode(addr stream, addr *ret);
_g int pprint_exit_syscode(Execute ptr, addr stream);
_g int pprint_pop_syscode(Execute ptr, addr stream, addr *ret);
_g int pprint_check_syscode(Execute ptr, addr stream);
_g int pprint_close_syscode(Execute ptr, addr stream);
_g int pprint_pretty_syscode(Execute ptr, addr stream, addr call);
_g int eastasian_set_syscode_(addr var, addr value, addr errorp, addr *ret);
_g int eastasian_get_syscode_(addr var, addr *ret1, addr *ret2);
_g int eastasian_width_syscode_(addr pos, addr *ret1, addr *ret2);
_g int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount);
_g int ed_function_syscode_(Execute ptr, addr file);
_g int run_program_syscode_(LocalRoot local, addr var, addr args, addr rest, addr *ret);
_g int make_callname_syscode_(addr var, addr *ret);
_g int trace_add_syscode_(Execute ptr, addr var, addr *ret);
_g int trace_del_syscode_(Execute ptr, addr var, addr *ret);
_g int set_slots_syscode(addr var, addr slots, addr values);
_g int remove_file_syscode(Execute ptr, addr var, addr opt, addr *ret);
_g int remove_directory_syscode(Execute ptr, addr var, addr opt, addr *ret);
_g int declare_parse_syscode(addr form, addr *ret);
_g int parse_type_syscode(Execute ptr, addr var, addr *ret);
_g int type_object_syscode(addr var, addr *ret);
_g int extension_syscode(Execute ptr, addr var);

#endif

