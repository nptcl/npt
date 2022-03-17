#ifndef __DOCUMENT_CALL_HEADER__
#define __DOCUMENT_CALL_HEADER__

#include "execute.h"
#include "typedef.h"

#define document_function_get_ _n(document_function_get_)
#define document_function_set_ _n(document_function_set_)
#define document_function_setf_get_ _n(document_function_setf_get_)
#define document_function_setf_set_ _n(document_function_setf_set_)
#define document_function_symbol_get_ _n(document_function_symbol_get_)
#define document_function_symbol_set_ _n(document_function_symbol_set_)
#define document_compiler_macro_symbol_get_ _n(document_compiler_macro_symbol_get_)
#define document_compiler_macro_symbol_set_ _n(document_compiler_macro_symbol_set_)
#define document_compiler_macro_setf_get_ _n(document_compiler_macro_setf_get_)
#define document_compiler_macro_setf_set_ _n(document_compiler_macro_setf_set_)
#define document_defsetf_symbol_get_ _n(document_defsetf_symbol_get_)
#define document_defsetf_symbol_set_ _n(document_defsetf_symbol_set_)
#define document_method_combination_get_ _n(document_method_combination_get_)
#define document_method_combination_set_ _n(document_method_combination_set_)
#define document_method_combination_symbol_get_ _n(document_method_combination_symbol_get_)
#define document_method_combination_symbol_set_ _n(document_method_combination_symbol_set_)
#define document_standard_method_get_ _n(document_standard_method_get_)
#define document_standard_method_set_ _n(document_standard_method_set_)
#define document_package_get_ _n(document_package_get_)
#define document_package_set_ _n(document_package_set_)
#define document_standard_class_get_ _n(document_standard_class_get_)
#define document_standard_class_set_ _n(document_standard_class_set_)
#define document_structure_class_get_ _n(document_structure_class_get_)
#define document_structure_class_set_ _n(document_structure_class_set_)
#define document_type_symbol_get_ _n(document_type_symbol_get_)
#define document_type_symbol_set_ _n(document_type_symbol_set_)
#define document_structure_symbol_get_ _n(document_structure_symbol_get_)
#define document_structure_symbol_set_ _n(document_structure_symbol_set_)
#define document_variable_symbol_get_ _n(document_variable_symbol_get_)
#define document_variable_symbol_set_ _n(document_variable_symbol_set_)

int document_function_get_(addr pos, addr *ret);
int document_function_set_(addr pos, addr value);
int document_function_setf_get_(addr pos, addr *ret);
int document_function_setf_set_(addr pos, addr value);
int document_function_symbol_get_(addr pos, addr *ret);
int document_function_symbol_set_(addr pos, addr value);
int document_compiler_macro_symbol_get_(addr pos, addr *ret);
int document_compiler_macro_symbol_set_(addr pos, addr value);
int document_compiler_macro_setf_get_(addr pos, addr *ret);
int document_compiler_macro_setf_set_(addr pos, addr value);
int document_defsetf_symbol_get_(addr pos, addr *ret);
int document_defsetf_symbol_set_(addr pos, addr value);
int document_method_combination_get_(addr pos, addr *ret);
int document_method_combination_set_(addr pos, addr value);
int document_method_combination_symbol_get_(addr pos, addr *ret);
int document_method_combination_symbol_set_(addr pos, addr value);
int document_standard_method_get_(addr pos, addr *ret);
int document_standard_method_set_(addr pos, addr value);
int document_package_get_(addr pos, addr *ret);
int document_package_set_(addr pos, addr value);
int document_standard_class_get_(addr pos, addr *ret);
int document_standard_class_set_(addr pos, addr value);
int document_structure_class_get_(addr pos, addr *ret);
int document_structure_class_set_(addr pos, addr value);
int document_type_symbol_get_(Execute ptr, addr pos, addr doc_type, addr *ret);
int document_type_symbol_set_(Execute ptr, addr pos, addr doc_type, addr value);
int document_structure_symbol_get_(addr pos, addr *ret);
int document_structure_symbol_set_(addr pos, addr value);
int document_variable_symbol_get_(addr pos, addr *ret);
int document_variable_symbol_set_(addr pos, addr value);

#endif

