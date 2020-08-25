#include <stdio.h>
#include "define.h"
#include "degrade.h"

#define LISP_DEGRADE_RTONLY
#undef LISP_DEGRADE_RTONLY

#ifdef LISP_DEGRADE
/*
 *  prototype declaration
 */
int test_c99(void);
int test_arch(void);
int test_alloc(void);
int test_memory(void);
int test_heap(void);
int test_heap_memory(void);
int test_local(void);
int test_execute(void);
int test_character(void);
int test_character_check(void);
int test_character_name(void);
int test_character_queue(void);
int test_strvect(void);
int test_array(void);
int test_array_access(void);
int test_array_make(void);
int test_strtype(void);
int test_object(void);
int test_symbol(void);
int test_callname(void);
int test_function(void);
int test_cons(void);
int test_cons_list(void);
int test_cons_plist(void);
int test_gc_execute(void);
int test_sxhash(void);
int test_hashtable(void);
int test_sequence(void);
int test_sort(void);
int test_pathname(void);

int test_code_object(void);
int test_control(void);
int test_control_callbind(void);
int test_file_memory(void);
int test_encode(void);
int test_file(void);
int test_stream(void);
int test_integer(void);
int test_print(void);
int test_format_parse(void);
int test_format_print(void);
int test_format_function(void);
int test_format_float(void);
int test_format(void);
int test_bignum_cons(void);
int test_bignum_data(void);
int test_bignum(void);
int test_radix(void);
int test_ratio(void);
int test_ratio_equal(void);
int test_ratio_plus(void);
int test_ratio_multi(void);
int test_number(void);
int test_token(void);
int test_reader(void);
int test_quote(void);
int test_package(void);
int test_lambda(void);
int test_clos(void);
int test_clos_class(void);
int test_clos_type(void);
int test_clos_cache(void);
int test_clos_combination(void);
int test_clos_generic(void);
int test_clos_method(void);
int test_type(void);
int test_type_copy(void);
int test_type_name(void);
int test_type_number(void);
int test_type_object(void);
int test_type_optimize(void);
int test_type_parse(void);
int test_type_range(void);
int test_type_subtypep(void);
int test_type_symbol(void);
int test_type_table(void);
int test_type_typep(void);
int test_type_upgraded(void);
int test_type_value(void);
int test_declare(void);
int test_parse(void);
int test_parse_function(void);
int test_parse_macro(void);
int test_parse_object(void);
int test_scope(void);
int test_code_queue(void);
int test_code_make(void);
int test_eval(void);
int test_eval_copy(void);
int test_eval_table(void);
int test_eval_stack(void);
int test_equal(void);
int test_condition(void);
int test_bit(void);
int test_extern_type(void);
int test_extern_sequence(void);
int test_extern_object(void);
int test_extern_control(void);
int test_optimize(void);
int loadrt(void);


/*
 *  degrade function
 */
void degrade_execute(void)
{
#ifdef LISP_DEGRADE_RTONLY
	DegradeCheck(loadrt);
#else
#if 0
#endif
	DegradeCheck(test_c99);
	DegradeCheck(test_arch);
	DegradeCheck(test_alloc);
	DegradeCheck(test_memory);
	DegradeCheck(test_local);
	DegradeCheck(test_heap_memory);
	DegradeCheck(test_heap);
	DegradeCheck(test_execute);
	DegradeCheck(test_object);
	DegradeCheck(test_symbol);
	DegradeCheck(test_callname);
	DegradeCheck(test_function);
	DegradeCheck(test_cons);
	DegradeCheck(test_cons_list);
	DegradeCheck(test_cons_plist);
	DegradeCheck(test_character);
	DegradeCheck(test_character_check);
	DegradeCheck(test_character_name);
	DegradeCheck(test_character_queue);
	DegradeCheck(test_strvect);
	DegradeCheck(test_array);
	DegradeCheck(test_array_make);
	DegradeCheck(test_array_access);
	DegradeCheck(test_strtype);
	DegradeCheck(test_gc_execute);
	DegradeCheck(test_sxhash);
	DegradeCheck(test_hashtable);
	DegradeCheck(test_sequence);
	DegradeCheck(test_sort);
	DegradeCheck(test_pathname);

	DegradeCheck(test_code_object);
	DegradeCheck(test_control);
	DegradeCheck(test_control_callbind);
	DegradeCheck(test_file_memory);
	DegradeCheck(test_encode);
	DegradeCheck(test_file);
	DegradeCheck(test_bignum_cons);
	DegradeCheck(test_bignum_data);
	DegradeCheck(test_bignum);
	DegradeCheck(test_radix);
	DegradeCheck(test_ratio);
	DegradeCheck(test_ratio_equal);
	DegradeCheck(test_ratio_plus);
	DegradeCheck(test_ratio_multi);
	DegradeCheck(test_number);
	DegradeCheck(test_token);
	DegradeCheck(test_reader);
	DegradeCheck(test_quote);
	DegradeCheck(test_package);
	DegradeCheck(test_lambda);
	DegradeCheck(test_clos);
	DegradeCheck(test_clos_class);
	DegradeCheck(test_clos_type);
	DegradeCheck(test_clos_cache);
	DegradeCheck(test_clos_generic);
	DegradeCheck(test_clos_combination);
	DegradeCheck(test_clos_method);
	DegradeCheck(test_type);
	DegradeCheck(test_type_table);
	DegradeCheck(test_type_symbol);
	DegradeCheck(test_type_parse);
	DegradeCheck(test_type_typep);
	DegradeCheck(test_type_name);
	DegradeCheck(test_type_value);
	DegradeCheck(test_type_object);
	DegradeCheck(test_type_copy);
	DegradeCheck(test_type_optimize);
	DegradeCheck(test_type_range);
	DegradeCheck(test_type_number);
	DegradeCheck(test_type_subtypep);
	DegradeCheck(test_type_upgraded);
	DegradeCheck(test_equal);
	DegradeCheck(test_declare);
	DegradeCheck(test_parse);
	DegradeCheck(test_parse_function);
	DegradeCheck(test_parse_macro);
	DegradeCheck(test_parse_object);
	DegradeCheck(test_scope);
	DegradeCheck(test_code_queue);
	DegradeCheck(test_code_make);
	DegradeCheck(test_eval);
	DegradeCheck(test_eval_copy);
	DegradeCheck(test_eval_table);
	DegradeCheck(test_eval_stack);
	DegradeCheck(test_integer);
	DegradeCheck(test_bit);
	DegradeCheck(test_format_parse);
	DegradeCheck(test_format_print);
	DegradeCheck(test_format_function);
	DegradeCheck(test_format_float);
	DegradeCheck(test_format);
	DegradeCheck(test_print);
	DegradeCheck(test_stream);
	DegradeCheck(test_condition);
	DegradeCheck(test_extern_type);
	DegradeCheck(test_extern_sequence);
	DegradeCheck(test_extern_object);
	DegradeCheck(test_extern_control);
	DegradeCheck(test_optimize);
	DegradeCheck(loadrt);
#if 0
#endif
#endif
}

#endif

