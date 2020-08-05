#ifndef __SYSCALL_CODE_HEADER__
#define __SYSCALL_CODE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int hello_syscode(Execute ptr);
_g void infobit_syscode(addr rest, addr *ret);
_g void infoprint_syscode(addr rest, addr *ret);
_g void gc_syscode(addr rest);
_g int savecore_syscode(Execute ptr, addr file);
_g int redirect_restart_syscode(Execute ptr, addr condition, addr list);
_g int defconstant_syscode(addr symbol, addr value, addr doc);
_g int in_package_syscode_(Execute ptr, addr name, addr *ret);
_g void setplist_syscode(addr key, addr value, addr list, addr *ret);
_g void remplist_syscode(addr key, addr list, addr *ret1, addr *ret2);
_g void make_hash_iterator_syscode(addr pos, addr *ret);
_g void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3);
_g int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret);
_g int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4);
_g int defpackage_syscode(Execute ptr, addr rest, addr *ret);
_g int do_symbols_syscode(Execute ptr, addr call, addr package);
_g int do_external_symbols_syscode(Execute ptr, addr call, addr package);
_g int do_all_symbols_syscode_(Execute ptr, addr call);
_g void getdoc_variable_syscode(addr var, addr *ret);
_g void setdoc_variable_syscode(addr var, addr value);
_g void specialp_syscode(addr var, addr *ret);
_g int ecase_error_syscode_(Execute ptr, addr value, addr list);
_g int etypecase_error_syscode_(Execute ptr, addr value, addr list);
_g void define_setf_expander_syscode(addr symbol, addr call);
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
_g int exit_syscode(addr code);
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
_g void timeinfo_syscode(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount);
_g int ed_function_syscode_(Execute ptr, addr file);
_g int run_program_syscode_(LocalRoot local, addr var, addr args, addr rest, addr *ret);
_g void make_callname_syscode(addr var, addr *ret);
_g int trace_add_syscode_(Execute ptr, addr var, addr *ret);
_g int trace_del_syscode_(Execute ptr, addr var, addr *ret);
_g int set_slots_syscode(addr var, addr slots, addr values);
_g int remove_file_syscode(Execute ptr, addr var, addr opt, addr *ret);
_g int remove_directory_syscode(Execute ptr, addr var, addr opt, addr *ret);
_g int declare_parse_syscode(addr form, addr *ret);
_g int parse_type_syscode(Execute ptr, addr var, addr *ret);
_g int type_object_syscode(addr var, addr *ret);

#endif

