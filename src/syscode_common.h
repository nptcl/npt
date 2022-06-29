#ifndef __SYSCODE_COMMON_HEADER__
#define __SYSCODE_COMMON_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define defconstant_syscode_ _n(defconstant_syscode_)
#define in_package_syscode_ _n(in_package_syscode_)
#define setplist_syscode _n(setplist_syscode)
#define remplist_syscode_ _n(remplist_syscode_)
#define make_hash_iterator_syscode _n(make_hash_iterator_syscode)
#define next_hash_iterator_syscode _n(next_hash_iterator_syscode)
#define make_package_iterator_syscode_ _n(make_package_iterator_syscode_)
#define next_package_iterator_syscode_ _n(next_package_iterator_syscode_)
#define defpackage_syscode_ _n(defpackage_syscode_)
#define do_symbols_syscode_ _n(do_symbols_syscode_)
#define do_external_symbols_syscode_ _n(do_external_symbols_syscode_)
#define do_all_symbols_syscode_ _n(do_all_symbols_syscode_)
#define getdoc_variable_syscode _n(getdoc_variable_syscode)
#define setdoc_variable_syscode _n(setdoc_variable_syscode)
#define ecase_error_syscode_ _n(ecase_error_syscode_)
#define etypecase_error_syscode_ _n(etypecase_error_syscode_)
#define define_setf_expander_syscode_ _n(define_setf_expander_syscode_)
#define end_input_stream_syscode _n(end_input_stream_syscode)
#define make_extend_output_stream_syscode _n(make_extend_output_stream_syscode)
#define prompt_for_syscode_ _n(prompt_for_syscode_)
#define print_unreadable_call_syscode_ _n(print_unreadable_call_syscode_)
#define write_default_syscode_ _n(write_default_syscode_)
#define symbol_deftype_syscode _n(symbol_deftype_syscode)
#define delete_deftype_syscode _n(delete_deftype_syscode)
#define ensure_structure_syscode_ _n(ensure_structure_syscode_)
#define structure_constructor_syscode_ _n(structure_constructor_syscode_)
#define loop_bind_syscode_ _n(loop_bind_syscode_)
#define make_pprint_stream_syscode_ _n(make_pprint_stream_syscode_)
#define pprint_gensym_syscode_ _n(pprint_gensym_syscode_)
#define pprint_exit_syscode_ _n(pprint_exit_syscode_)
#define pprint_pop_syscode_ _n(pprint_pop_syscode_)
#define pprint_check_syscode_ _n(pprint_check_syscode_)
#define pprint_close_syscode_ _n(pprint_close_syscode_)
#define pprint_pretty_syscode_ _n(pprint_pretty_syscode_)
#define timeinfo_syscode_ _n(timeinfo_syscode_)
#define ed_function_syscode_ _n(ed_function_syscode_)
#define trace_add_syscode_ _n(trace_add_syscode_)
#define trace_del_syscode_ _n(trace_del_syscode_)
#define set_slots_syscode_ _n(set_slots_syscode_)
#define intern_eql_specializer_syscode_ _n(intern_eql_specializer_syscode_)
#define defgeneric_define_syscode_ _n(defgeneric_define_syscode_)
#define defgeneric_method_syscode_ _n(defgeneric_method_syscode_)
#define condition_restarts_push_syscode_ _n(condition_restarts_push_syscode_)
#define condition_restarts_pop_syscode_ _n(condition_restarts_pop_syscode_)
#define condition_restarts_make_syscode_ _n(condition_restarts_make_syscode_)
#define make_restart_syscode_ _n(make_restart_syscode_)

int defconstant_syscode_(addr symbol, addr value, addr doc);
int in_package_syscode_(Execute ptr, addr name, addr *ret);
void setplist_syscode(addr key, addr value, addr list, addr *ret);
int remplist_syscode_(addr key, addr list, addr *ret1, addr *ret2);
void make_hash_iterator_syscode(addr pos, addr *ret);
void next_hash_iterator_syscode(addr pos, addr *ret1, addr *ret2, addr *ret3);
int make_package_iterator_syscode_(addr pos, addr a, addr b, addr c, addr *ret);
int next_package_iterator_syscode_(Execute ptr, addr pos,
		addr *ret1, addr *ret2, addr *ret3, addr *ret4);
int defpackage_syscode_(Execute ptr, addr var, addr rest, addr *ret);
int do_symbols_syscode_(Execute ptr, addr call, addr package);
int do_external_symbols_syscode_(Execute ptr, addr call, addr package);
int do_all_symbols_syscode_(Execute ptr, addr call);
void getdoc_variable_syscode(addr var, addr *ret);
void setdoc_variable_syscode(addr var, addr value);
int ecase_error_syscode_(Execute ptr, addr value, addr list);
int etypecase_error_syscode_(Execute ptr, addr value, addr list);
int define_setf_expander_syscode_(addr symbol, addr call);
void end_input_stream_syscode(addr var, addr *ret);
void make_extend_output_stream_syscode(addr var, addr rest, addr *ret);
int prompt_for_syscode_(Execute ptr, addr type, addr args, addr *ret);
int print_unreadable_call_syscode_(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body);
int write_default_syscode_(Execute ptr, addr stream, addr var, addr *ret);
void symbol_deftype_syscode(addr var, addr *ret);
void delete_deftype_syscode(addr var, addr *ret);
int ensure_structure_syscode_(Execute ptr, addr name, addr slots, addr rest);
int structure_constructor_syscode_(Execute ptr, addr symbol, addr rest, addr *ret);
int loop_bind_syscode_(Execute ptr, addr a, addr b, addr c, addr *ret);
int make_pprint_stream_syscode_(Execute ptr, addr *ret,
		addr stream, addr object, addr prefix, addr perline, addr suffix);
int pprint_gensym_syscode_(addr stream, addr *ret);
int pprint_exit_syscode_(Execute ptr, addr stream);
int pprint_pop_syscode_(Execute ptr, addr stream, addr *ret);
int pprint_check_syscode_(Execute ptr, addr stream);
int pprint_close_syscode_(Execute ptr, addr stream);
int pprint_pretty_syscode_(Execute ptr, addr stream, addr call);
int timeinfo_syscode_(LocalRoot local,
		addr *rreal, addr *rrun, addr *rsize, addr *rcount);
int ed_function_syscode_(Execute ptr, addr file);
int trace_add_syscode_(Execute ptr, addr var, addr *ret);
int trace_del_syscode_(Execute ptr, addr var, addr *ret);
int set_slots_syscode_(addr var, addr slots, addr values);
int intern_eql_specializer_syscode_(Execute ptr, addr var, addr *ret);
int defgeneric_define_syscode_(Execute ptr, addr name, addr args, addr *ret);
int defgeneric_method_syscode_(addr inst, addr args);
int condition_restarts_push_syscode_(addr condition, addr restarts);
int condition_restarts_pop_syscode_(addr condition, addr restarts);
int condition_restarts_make_syscode_(Execute ptr, addr var, addr list, addr *ret);
int make_restart_syscode_(addr var, addr call, addr rest, addr *ret);

#endif

