#ifndef __DOCUMENT_HEADER__
#define __DOCUMENT_HEADER__

#include "typedef.h"

_g void get_function_document(addr key, addr *ret);
_g void set_function_document(addr key, addr value);
_g void get_setf_document(addr key, addr *ret);
_g void set_setf_document(addr key, addr value);
_g void get_compiler_macro_document(addr key, addr *ret);
_g void set_compiler_macro_document(addr key, addr value);
_g void get_standard_method_document(addr key, addr *ret);
_g void set_standard_method_document(addr key, addr value);
_g void get_package_document(addr key, addr *ret);
_g void set_package_document(addr key, addr value);
_g void get_type_document(addr key, addr *ret);
_g void set_type_document(addr key, addr value);
_g void get_standard_class_document(addr key, addr *ret);
_g void set_standard_class_document(addr key, addr value);
_g void get_structure_class_document(addr key, addr *ret);
_g void set_structure_class_document(addr key, addr value);
_g void get_variable_document(addr key, addr *ret);
_g void set_variable_document(addr key, addr value);

#endif

