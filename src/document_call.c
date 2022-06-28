#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "closget_class.h"
#include "closget_combination.h"
#include "closget_generic.h"
#include "closget_structure.h"
#include "condition.h"
#include "control_execute.h"
#include "document_call.h"
#include "document_search.h"
#include "function.h"
#include "package_object.h"
#include "structure.h"
#include "symbol.h"
#include "type_deftype.h"
#include "typedef.h"

/*
 *  function, t
 */
static int document_function_name_(addr pos, addr *ret)
{
	if (functionp(pos)) {
		GetNameFunction(pos, ret);
		return 0;
	}

	return stdget_generic_name_(pos, ret);
}

int document_function_get_(addr pos, addr *ret)
{
	addr value;

	Return(get_documentation_function_object_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(document_function_name_(pos, &pos));
	if (callnamep(pos))
		GetCallName(pos, &pos);
	if (pos == Nil)
		return Result(ret, Nil);

	return document_function_(pos, ret);
}

int document_function_set_(addr pos, addr value)
{
	return set_documentation_function_object_(pos, value);
}


/*
 *  (setf name), function
 */
int document_function_setf_get_(addr pos, addr *ret)
{
	addr value;

	Return(parse_callname_error_(&pos, pos));
	Return(getglobalcheck_callname_(pos, &value));
	Return(get_documentation_function_object_(value, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	GetCallName(pos, &pos);
	return document_function_(pos, ret);
}

int document_function_setf_set_(addr pos, addr value)
{
	Return(parse_callname_error_(&pos, pos));
	Return(getglobalcheck_callname_(pos, &pos));
	Return(set_documentation_function_object_(pos, value));

	return 0;
}


/*
 *  symbol, function
 */
static int document_fdefinition_symbol_get_(addr pos, addr *ret)
{
	/* function */
	GetFunctionSymbol(pos, ret);
	if (*ret != Unbound)
		return 0;

	/* macro-function */
	getmacro_symbol(pos, ret);
	if (*ret != Unbound)
		return 0;

	/* error */
	return call_undefined_function_(NULL, pos);
}

int document_function_symbol_get_(addr pos, addr *ret)
{
	addr value;

	Return(document_fdefinition_symbol_get_(pos, &value));
	Return(get_documentation_function_object_(value, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_function_(pos, ret);
}

int document_function_symbol_set_(addr pos, addr value)
{
	Return(getfunction_global_(pos, &pos));
	Return(set_documentation_function_object_(pos, value));

	return 0;
}


/*
 *  symbol, compiler-macro
 */
int document_compiler_macro_symbol_get_(addr pos, addr *ret)
{
	get_compiler_macro_symbol(pos, &pos);
	if (pos != Nil) {
		Return(get_documentation_function_object_(pos, &pos));
	}

	return Result(ret, pos);
}

int document_compiler_macro_symbol_set_(addr pos, addr value)
{
	addr macro;

	get_compiler_macro_symbol(pos, &macro);
	if (macro == Nil)
		return fmte_("There is no compiler-macro, ~S.", pos, NULL);
	Return(set_documentation_function_object_(macro, value));

	return 0;
}


/*
 *  (setf name), compiler-macro
 */
int document_compiler_macro_setf_get_(addr pos, addr *ret)
{
	Return(parse_callname_error_(&pos, pos));
	GetCallName(pos, &pos);
	get_setf_compiler_macro_symbol(pos, &pos);
	if (pos != Nil) {
		Return(get_documentation_function_object_(pos, &pos));
	}

	return Result(ret, pos);
}

int document_compiler_macro_setf_set_(addr pos, addr value)
{
	addr macro;

	Return(parse_callname_error_(&pos, pos));
	GetCallName(pos, &pos);
	get_setf_compiler_macro_symbol(pos, &macro);
	if (macro == Nil)
		return fmte_("There is no compiler-macro, ~S.", pos, NULL);
	Return(set_documentation_function_object_(macro, value));

	return 0;
}


/*
 *  symbol, setf
 */
int document_defsetf_symbol_get_(addr pos, addr *ret)
{
	addr setf;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(pos, &setf);
	/* setf-function */
	if (setf == Unbound) {
		Return(getsetf_global_(pos, &setf));
	}
	/* get documentation */
	Return(get_documentation_function_object_(setf, &setf));

	/* contents */
	if (setf != Nil)
		return Result(ret, setf);

	return document_function_(pos, ret);
}

int document_defsetf_symbol_set_(addr pos, addr value)
{
	addr setf;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(pos, &setf);
	/* setf-function */
	if (setf == Unbound) {
		Return(getsetf_global_(pos, &setf));
	}
	/* set documentation */
	Return(set_documentation_function_object_(setf, value));

	return 0;
}


/*
 *  method-combination
 */
int document_method_combination_get_(addr pos, addr *ret)
{
	return stdget_longcomb_document_(pos, ret);
}

int document_method_combination_set_(addr pos, addr value)
{
	return stdset_longcomb_document_(pos, value);
}


/*
 *  symbol, method-combination
 */
int document_method_combination_symbol_get_(addr pos, addr *ret)
{
	Return(clos_find_combination_(pos, &pos));
	Return(stdget_longcomb_document_(pos, ret));

	return 0;
}

int document_method_combination_symbol_set_(addr pos, addr value)
{
	Return(clos_find_combination_(pos, &pos));
	Return(stdset_longcomb_document_(pos, value));

	return 0;
}


/*
 *  standard-method
 */
int document_standard_method_get_(addr pos, addr *ret)
{
	return methodget_document_(pos, ret);
}

int document_standard_method_set_(addr pos, addr value)
{
	return methodset_document_(pos, value);
}


/*
 *  package
 */
int document_package_get_(addr pos, addr *ret)
{
	getdocument_package(pos, ret);
	return 0;
}

int document_package_set_(addr pos, addr value)
{
	setdocument_package(pos, value);
	return 0;
}


/*
 *  standard-class
 */
int document_standard_class_get_(addr pos, addr *ret)
{
	addr value;

	Return(stdget_class_document_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(stdget_class_name_(pos, &pos));
	return document_type_(pos, ret);
}

int document_standard_class_set_(addr pos, addr value)
{
	return stdset_class_document_(pos, value);
}


/*
 *  structure-class
 */
int document_structure_class_get_(addr pos, addr *ret)
{
	addr value;

	Return(stdget_structure_documentation_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	Return(stdget_structure_name_(pos, &pos));
	return document_type_(pos, ret);
}

int document_structure_class_set_(addr pos, addr value)
{
	return stdset_structure_documentation_(pos, value);
}


/*
 *  symbol, type
 */
int document_type_symbol_get_(Execute ptr, addr pos, addr doc_type, addr *ret)
{
	addr clos, type;

	/* clos */
	clos_find_class_nil(pos, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &type);
		Return(document_fdefinition_symbol_get_(type, &type));
		return funcall1_control_(ptr, ret, type, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(pos, &type);
	if (type == Nil) {
		*ret = Nil;
		return fmte_("The symbol ~S don't have a deftype function.", pos, NULL);
	}
	Return(get_documentation_function_object_(type, &type));

	/* contents */
	if (type != Nil)
		return Result(ret, type);

	return document_type_(pos, ret);
}

int document_type_symbol_set_(Execute ptr, addr pos, addr doc_type, addr value)
{
	addr clos, type;

	/* clos */
	clos_find_class_nil(pos, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &type);
		Return(getsetf_global_(type, &type));
		return funcall_control_(ptr, type, value, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(pos, &type);
	if (type == Nil)
		return fmte_("The symbol ~S don't have a deftype function.", pos, NULL);
	Return(set_documentation_function_object_(type, value));

	return 0;
}


/*
 *  symbol, structure
 */
int document_structure_symbol_get_(addr pos, addr *ret)
{
	addr value;

	Return(getdoc_structure_(pos, &value));
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_type_(pos, ret);
}

int document_structure_symbol_set_(addr pos, addr value)
{
	return setdoc_structure_(pos, value);
}


/*
 *  symbol, variable
 */
int document_variable_symbol_get_(addr pos, addr *ret)
{
	addr value;

	getdocument_variable_symbol(pos, &value);
	if (value != Nil)
		return Result(ret, value);

	/* contents */
	return document_variable_(pos, ret);
}

int document_variable_symbol_set_(addr pos, addr value)
{
	setdocument_variable_symbol(pos, value);
	return 0;
}

