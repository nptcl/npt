#ifndef __DOCUMENT_HEADER__
#define __DOCUMENT_HEADER__

#include "typedef.h"

void get_function_document(addr key, addr *ret);
void set_function_document(addr key, addr value);
void get_setf_document(addr key, addr *ret);
void set_setf_document(addr key, addr value);
void get_compiler_macro_document(addr key, addr *ret);
void set_compiler_macro_document(addr key, addr value);
void get_method_combination_document(addr key, addr *ret);
void set_method_combination_document(addr key, addr value);
void get_standard_method_document(addr key, addr *ret);
void set_standard_method_document(addr key, addr value);
void get_package_document(addr key, addr *ret);
void set_package_document(addr key, addr value);
void get_type_document(addr key, addr *ret);
void set_type_document(addr key, addr value);
void get_standard_class_document(addr key, addr *ret);
void set_standard_class_document(addr key, addr value);
void get_structure_class_document(addr key, addr *ret);
void set_structure_class_document(addr key, addr value);
void get_variable_document(addr key, addr *ret);
void set_variable_document(addr key, addr value);


#endif

