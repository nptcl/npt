#ifndef __SYSCODE_COMMON_HEADER__
#define __SYSCODE_COMMON_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

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
#define ecase_error_syscode_ _n(ecase_error_syscode_)
#define etypecase_error_syscode_ _n(etypecase_error_syscode_)
#define define_setf_expander_syscode_ _n(define_setf_expander_syscode_)
#define end_input_stream_syscode _n(end_input_stream_syscode)
#define make_extend_output_stream_syscode _n(make_extend_output_stream_syscode)
#define prompt_for_syscode _n(prompt_for_syscode)
#define print_unreadable_call_syscode _n(print_unreadable_call_syscode)
#define write_default_syscode _n(write_default_syscode)
#define symbol_deftype_syscode _n(symbol_deftype_syscode)
#define delete_deftype_syscode _n(delete_deftype_syscode)
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
#define timeinfo_syscode_ _n(timeinfo_syscode_)
#define ed_function_syscode_ _n(ed_function_syscode_)
#define trace_add_syscode_ _n(trace_add_syscode_)
#define trace_del_syscode_ _n(trace_del_syscode_)
#define set_slots_syscode _n(set_slots_syscode)
#define intern_eql_specializer_syscode _n(intern_eql_specializer_syscode)

int redirect_restart_syscode(Execute ptr, addr condition, addr list);
int defconstant_syscode(addr symbol, addr value, addr doc);
int in_package_syscode_(Execute ptr, addr name, addr *ret);
void setplist_syscode(addr key, addr value, addr list, addr *ret);
int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2);
void make_hash_iterator_syscode(addr pos, addr *ret);
void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3);
int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret);
int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4);
int defpackage_syscode(Execute ptr, addr var, addr rest, addr *ret);
int do_symbols_syscode(Execute ptr, addr call, addr package);
int do_external_symbols_syscode(Execute ptr, addr call, addr package);
int do_all_symbols_syscode_(Execute ptr, addr call);
void getdoc_variable_syscode(addr var, addr *ret);
void setdoc_variable_syscode(addr var, addr value);
int ecase_error_syscode_(Execute ptr, addr value, addr list);
int etypecase_error_syscode_(Execute ptr, addr value, addr list);
int define_setf_expander_syscode_(addr symbol, addr call);
void end_input_stream_syscode(addr var, addr *ret);
void make_extend_output_stream_syscode(addr var, addr rest, addr *ret);
int prompt_for_syscode(Execute ptr, addr type, addr args, addr *ret);
int print_unreadable_call_syscode(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body);
int write_default_syscode(Execute ptr, addr stream, addr var, addr *ret);
void symbol_deftype_syscode(addr var, addr *ret);
void delete_deftype_syscode(addr var, addr *ret);
int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest);
int structure_constructor_syscode(Execute ptr, addr symbol, addr rest, addr *ret);
int loop_bind_syscode(Execute ptr, addr a, addr b, addr c, addr *ret);
int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix);
int pprint_gensym_syscode(addr stream, addr *ret);
int pprint_exit_syscode(Execute ptr, addr stream);
int pprint_pop_syscode(Execute ptr, addr stream, addr *ret);
int pprint_check_syscode(Execute ptr, addr stream);
int pprint_close_syscode(Execute ptr, addr stream);
int pprint_pretty_syscode(Execute ptr, addr stream, addr call);
int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount);
int ed_function_syscode_(Execute ptr, addr file);
int trace_add_syscode_(Execute ptr, addr var, addr *ret);
int trace_del_syscode_(Execute ptr, addr var, addr *ret);
int set_slots_syscode(addr var, addr slots, addr values);
int intern_eql_specializer_syscode(addr var, addr *ret);

#endif

