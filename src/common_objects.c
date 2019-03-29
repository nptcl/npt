/*
 *  ANSI COMMON LISP: 7. Objects
 */
#include "common_header.h"
#include "clos_common.h"
#include "mop.h"
#include "package.h"

/*
 *  import
 */
#define ImportMopPackage(x) import_mop_package(CONSTANT_CLOSNAME_##x)

static void import_mop_package(constindex index)
{
	addr package, symbol;

	GetConst(PACKAGE_COMMON_LISP, &package);
	GetConstant(index, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	import_package(package, symbol);
	export_package(package, symbol);
}


/* defgeneric_function_keywords(); */
/* defun_ensure_generic_function(); */
/* defgeneric_allocate_instance(); */
/* defgeneric_reinitialize_instance(); */
/* defgeneric_shared_initialize(); */
/* defgeneric_update_instance_for_different_class(); */
/* defgeneric_update_instance_for_redefined_class(); */
/* defgeneric_change_class(); */


/* (defun slot-boundp (object symbol) ...) -> boolean */
static void function_slot_boundp(Execute ptr, addr pos, addr name)
{
	slot_boundp_common(pos, name, &pos);
	setresult_control(ptr, pos);
}

static void type_slot_boundp(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, Symbol);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_slot_boundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_BOUNDP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_slot_boundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_slot_boundp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* defun_exists_p(); */
/* defun_slot_makunbound(); */
/* defgeneric_slot_missing(); */
/* defgeneric_slot_unbound(); */
/* defun_slot_value(); */
/* defgeneric_method_qualifiers(); */
/* defgeneric_no_applicable_method() */
/* defgeneric_no_next_method(); */
/* defgeneric_remove_method(); */
/* defgeneric_make_instance(); */
/* defgeneric_make_instances_obsolete(); */
/* defgeneric_make_load_form(); */
/* defun_make_load_form_saving_slots(); */
/* defmacro_with_accessors(); */
/* defmacro_with_slots(); */


/* (defmacro defclass (name ({superclass}*) ({slot}*) &rest option) -> class
 *    name        (or symbol (not null))  ;; nonnil-symbol
 *    superclass  (or symbol (not null))  ;; nonnil-symbol
 *    slot        (or symbol cons)
 *    option      &rest cons
 *    class       class
 */
static void function_defclass(Execute ptr, addr form, addr env)
{
	if (defclass_common(ptr, form, env, &form)) return;
	setresult_control(ptr, form);
}

static void defmacro_defclass(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCLASS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_defclass);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* defmacro_defgeneric(); */
/* defmacro_defmethod(); */


/* (defun find-class (symbol &optional errorp env) ...) -> class
 *   symbol  symbol
 *   errorp  t  ;; boolean, default t
 *   env     (or environment null)  ;; default nil
 *   class   (or class null)
 */
static void function_find_class(Execute ptr, addr pos, addr errorp, addr env)
{
	if (errorp == Unbound) errorp = T;
	if (env == Unbound) env = Nil;
	find_class_common(pos, errorp != Nil, env, &pos);
	setresult_control(ptr, pos);
}

static void type_find_class(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Symbol);
	GetTypeTable(&values, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt2(&arg, arg, values, type);
	GetTypeValues(&values, ClassNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_CLASS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, function_find_class);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* defun_setf_find_class(); */
/* defgeneric_compute_applicable_methods(); */
/* defmacro_define_method_combination(); */
/* defgeneric_find_method(); */
/* defgeneric_add_method(); */
/* defgeneric_initialize_instance(); */


/* (defgeneric class-name ...); */
static void defgeneric_class_name(void)
{
	ImportMopPackage(CLASS_NAME);
}


/* (defgeneric (setf class-name) ...) */
static void defgeneric_setf_class_name(void)
{
	/* do-nothing */
}


/* defun_class_of(); */
/* defun_unbound_slot_instance(); */


/*
 *  intern
 */
static void intern_clos_objects(void)
{
	/* defgeneric_function_keywords(); */
	/* defun_ensure_generic_function(); */
	/* defgeneric_allocate_instance(); */
	/* defgeneric_reinitialize_instance(); */
	/* defgeneric_shared_initialize(); */
	/* defgeneric_update_instance_for_different_class(); */
	/* defgeneric_update_instance_for_redefined_class(); */
	/* defgeneric_change_class(); */
	defun_slot_boundp();
	/* defun_exists_p(); */
	/* defun_slot_makunbound(); */
	/* defgeneric_slot_missing(); */
	/* defgeneric_slot_unbound(); */
	/* defun_slot_value(); */
	/* defgeneric_method_qualifiers(); */
	/* defgeneric_no_applicable_method() */
	/* defgeneric_no_next_method(); */
	/* defgeneric_remove_method(); */
	/* defgeneric_make_instance(); */
	/* defgeneric_make_instances_obsolete(); */
	/* defgeneric_make_load_form(); */
	/* defun_make_load_form_saving_slots(); */
	/* defmacro_with_accessors(); */
	/* defmacro_with_slots(); */
	defmacro_defclass();
	/* defmacro_defgeneric(); */
	/* defmacro_defmethod(); */
	defun_find_class();
	/* defun_setf_find_class(); */
	/* defgeneric_compute_applicable_methods(); */
	/* defmacro_define_method_combination(); */
	/* defgeneric_find_method(); */
	/* defgeneric_add_method(); */
	/* defgeneric_initialize_instance(); */
	defgeneric_class_name();
	defgeneric_setf_class_name();
	/* defun_class_of(); */
	/* defun_unbound_slot_instance(); */
}

void intern_common_objects(void)
{
	/* metaobject protocol */
	intern_metaobject_protocol();
	/* common-lisp package */
	intern_clos_objects();
}

