#include "clos_class.h"
#include "clos_combination.h"
#include "clos_method.h"
#include "condition.h"
#include "document.h"
#include "function.h"
#include "package.h"
#include "symbol.h"

void get_function_document(addr key, addr *ret)
{
	getdocumentation_function(key, ret);
}

void set_function_document(addr key, addr value)
{
	setdocumentation_function(key, value);
}

void get_setf_document(addr key, addr *ret)
{
	addr name;

	if (parse_callname(key, &name) == CALLNAME_SETF)
		fmte("The function ~S must be a setf form.", key, NULL);
	getdocumentation_function(name, ret);
}

void set_setf_document(addr key, addr value)
{
	addr name;

	if (parse_callname(key, &name) == CALLNAME_SETF)
		fmte("The function ~S must be a setf form.", key, NULL);
	setdocumentation_function(name, value);
}

void get_compiler_macro_document(addr key, addr *ret)
{
	fmte("TODO: documentation compiler-macro is not implemented.", NULL);
}

void set_compiler_macro_document(addr key, addr value)
{
	fmte("TODO: documentation compiler-macro is not implemented.", NULL);
}

void get_standard_method_document(addr key, addr *ret)
{
	methodget_document(key, ret);
}
void set_standard_method_document(addr key, addr value)
{
	methodset_document(key, value);
}

void get_package_document(addr key, addr *ret)
{
	getdocument_package(key, ret);
}

void set_package_document(addr key, addr value)
{
	setdocument_package(key, value);
}

void get_type_document(addr key, addr *ret)
{
	getdocument_type_symbol(key, ret);
}

void set_type_document(addr key, addr value)
{
	setdocument_type_symbol(key, value);
}

void get_standard_class_document(addr key, addr *ret)
{
	stdget_class_document(key, ret);
}

void set_standard_class_document(addr key, addr value)
{
	stdset_class_document(key, value);
}

void get_structure_class_document(addr key, addr *ret)
{
	fmte("TODO: documentation structure is not implemented.", NULL);
}

void set_structure_class_document(addr key, addr value)
{
	fmte("TODO: documentation structure is not implemented.", NULL);
}

void get_variable_document(addr key, addr *ret)
{
	getdocument_variable_symbol(key, ret);
}

void set_variable_document(addr key, addr value)
{
	setdocument_variable_symbol(key, value);
}

