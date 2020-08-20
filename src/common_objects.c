/*
 *  ANSI COMMON LISP: 7. Objects
 */
#include "call_objects.h"
#include "common_header.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_make.h"
#include "clos_method.h"
#include "clos_redefine.h"
#include "clos_type.h"
#include "cons.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "mop.h"
#include "mop_common.h"
#include "package.h"
#include "package_symbol.h"

/*
 *  import
 */
#define ImportMopPackage(x) Error(import_mop_package_(CONSTANT_COMMON_##x))

static int import_mop_package_(constindex index)
{
	addr package, symbol;

	GetConst(PACKAGE_COMMON_LISP, &package);
	GetConstant(index, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	Return(import_package_(package, symbol));
	Return(export_package_(package, symbol));

	return 0;
}


/* (defgeneric function-keywords (method)) */
static void defgeneric_function_keywords(void)
{
	ImportMopPackage(FUNCTION_KEYWORDS);
}


/* (defun ensure-generic-function (name &key
 *     argument-precedence-order
 *     declare
 *     documentation
 *     environment
 *     lambda-list
 *     generic-function-class
 *     method-class
 *     method-combination
 */
static int function_ensure_generic_function(Execute ptr, addr name, addr rest)
{
	Return(ensure_generic_function_common_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_ensure_generic_function(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8;

	KeyTypeTable(&key1, ARGUMENT_PRECEDENCE_ORDER, T);
	KeyTypeTable(&key2, DECLARE, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	KeyTypeTable(&key4, ENVIRONMENT, EnvironmentNull);
	KeyTypeTable(&key5, LAMBDA_LIST, T);
	KeyTypeTable(&key6, GENERIC_FUNCTION_CLASS, T);
	KeyTypeTable(&key7, METHOD_CLASS, T);
	KeyTypeTable(&key8, METHOD_COMBINATION, T);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, NULL);
	/* type */
	GetTypeTable(&args, T);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_generic_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENSURE_GENERIC_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_generic_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ensure_generic_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defgeneric allocate-instance ...)  */
static void defgeneric_allocate_instance(void)
{
	ImportMopPackage(ALLOCATE_INSTANCE);
}


/* (defgeneric reinitialize-instance ...) */
static void defgeneric_reinitialize_instance(void)
{
	ImportMopPackage(REINITIALIZE_INSTANCE);
}


/* (defgeneric shared-initialize ...) */
static void defgeneric_shared_initialize(void)
{
	ImportMopPackage(SHARED_INITIALIZE);
}


/* defgeneric_update_instance_for_different_class(); */
static void defgeneric_update_instance_for_different_class(void)
{
	ImportMopPackage(UPDATE_INSTANCE_FOR_DIFFERENT_CLASS);
}


/* defgeneric_update_instance_for_redefined_class(); */
static void defgeneric_update_instance_for_redefined_class(void)
{
	ImportMopPackage(UPDATE_INSTANCE_FOR_REDEFINED_CLASS);
}


/* (defgeneric change-class ...) */
static void defgeneric_change_class(void)
{
	ImportMopPackage(CHANGE_CLASS);
}


/* (defun slot-boundp (object symbol) ...) -> boolean */
static int function_slot_boundp(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* redefined */
	Return(clos_class_of_(pos, &clos));
	Return(clos_version_check_(ptr, pos, clos));

	/* call generic */
	GetConst(CLOSNAME_SLOT_BOUNDP_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_boundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_BOUNDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_boundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-exists-p (object symbol) ...) -> boolean */
static int function_slot_exists_p(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* redefined */
	Return(clos_class_of_(pos, &clos));
	Return(clos_version_check_(ptr, pos, clos));

	/* call generic */
	GetConst(CLOSNAME_SLOT_EXISTS_P_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_exists_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_EXISTS_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_exists_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-makunbound (instance symbol) ...) -> instance */
static int function_slot_makunbound(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* redefined */
	Return(clos_class_of_(pos, &clos));
	Return(clos_version_check_(ptr, pos, clos));

	/* call generic */
	GetConst(CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control(ptr, call, clos, pos, name, NULL);
}

static void defun_slot_makunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_MAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_makunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defgeneric slot-missing ...) */
static void defgeneric_slot_missing(void)
{
	ImportMopPackage(SLOT_MISSING);
}


/* (defgeneric slot-unbound ...) */
static void defgeneric_slot_unbound(void)
{
	ImportMopPackage(SLOT_UNBOUND);
}


/* (defun slot-value (object name) ...) -> t */
static int function_slot_value(Execute ptr, addr pos, addr name)
{
	addr call, clos;

	/* redefined */
	Return(clos_class_of_(pos, &clos));
	Return(clos_version_check_(ptr, pos, clos));

	/* call generic */
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control(ptr, call, clos, pos, name, NULL);
}

static void type_slot_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Clos);
	GetTypeTable(&values, Symbol);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_slot_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_slot_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_slot_value(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf slot-value) (value pos name) ...) -> value */
static int function_setf_slot_value(Execute ptr, addr value, addr pos, addr name)
{
	addr call, clos;

	/* redefined */
	Return(clos_class_of_(pos, &clos));
	Return(clos_version_check_(ptr, pos, clos));

	/* call generic */
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	Return(getsetf_global_(call, &call));
	return funcall_control(ptr, call, value, clos, pos, name, NULL);
}

static void type_setf_slot_value(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Clos);
	GetTypeTable(&type, Symbol);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_slot_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_VALUE, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_slot_value);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_slot_value(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defgeneric method-qualifiers ...) */
static void defgeneric_method_qualifiers_import(void)
{
	ImportMopPackage(METHOD_QUALIFIERS);
}


/* (defgeneric no-applicable-method ...) */
static void defgeneric_no_applicable_method(void)
{
	ImportMopPackage(NO_APPLICABLE_METHOD);
}


/* (defgeneric no-next-method ...) */
static void defgeneric_no_next_method(void)
{
	ImportMopPackage(NO_NEXT_METHOD);
}


/* (defgeneric remove-method ...) */
static void defgeneric_remove_method(void)
{
	ImportMopPackage(REMOVE_METHOD);
}


/* (defgeneric make-instance ...) */
static void defgeneric_make_instance(void)
{
	ImportMopPackage(MAKE_INSTANCE);
}


/* (defgeneric make-instances-obsolete ...) */
static void defgeneric_make_instances_obsolete(void)
{
	ImportMopPackage(MAKE_INSTANCES_OBSOLETE);
}


/* (defgeneric make-load-form ...) */
static void defgeneric_make_load_form_common(void)
{
	ImportMopPackage(MAKE_LOAD_FORM);
}


/* (defun make-load-form-saving-slots
 *     (object &key slot-names environment) ...)
 *     -> creation-form, initialization-form
 *   object               T
 *   slot-names           list
 *   environment          Environment
 *   creation-form        T
 *   initialization-form  T
 */
static int function_make_load_form_saving_slots(Execute ptr, addr var, addr rest)
{
	addr list, env;

	if (GetKeyArgs(rest, KEYWORD_SLOT_NAMES, &list))
		list = Unbound;
	if (GetKeyArgs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	Return(make_load_form_saving_slots_common(ptr, var, list, env, &var, &list));
	setvalues_control(ptr, var, list, NULL);

	return 0;
}

static void type_make_load_form_saving_slots(addr *ret)
{
	addr args, values;
	addr key, key1, key2;

	KeyTypeTable(&key1, SLOT_NAMES, List);
	KeyTypeTable(&key2, ENVIRONMENT, EnvironmentNull);
	list_heap(&key, key1, key2, NULL);
	/* type */
	GetTypeTable(&values, T);
	typeargs_var1key(&args, values, key);
	typevalues_values2(&values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_load_form_saving_slots(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_LOAD_FORM_SAVING_SLOTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_load_form_saving_slots);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_load_form_saving_slots(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-accessors ((entry*) instance declare* &body body) ...) -> t */
static int function_with_accessors(Execute ptr, addr form, addr env)
{
	Return(with_accessors_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_accessors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_ACCESSORS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_accessors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-slots ((entry*) instance declare* &body form) ...) -> t */
static int function_with_slots(Execute ptr, addr form, addr env)
{
	Return(with_slots_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_slots(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SLOTS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_slots);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defclass (name ({superclass}*) ({slot}*) &rest option) -> class
 *    name        (or symbol (not null))  ;; nonnil-symbol
 *    superclass  (or symbol (not null))  ;; nonnil-symbol
 *    slot        (or symbol cons)
 *    option      &rest cons
 *    class       class
 */
static int function_defclass(Execute ptr, addr form, addr env)
{
	Return(defclass_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defclass(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCLASS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defclass);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defgeneric (name lambda &rest args) ...) -> generic-function */
static int function_defgeneric(Execute ptr, addr form, addr env)
{
	Return(defgeneric_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defgeneric(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFGENERIC, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defgeneric);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defmethod
 *     (name qualifier* lambda declare* document* form*) -> method
 */
static int function_defmethod(Execute ptr, addr form, addr env)
{
	Return(defmethod_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defmethod(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFMETHOD, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defmethod);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun find-class (symbol &optional errorp env) ...) -> class
 *   symbol  symbol
 *   errorp  t  ;; boolean, default t
 *   env     (or environment null)  ;; default nil
 *   class   (or class null)
 */
static int function_find_class(Execute ptr, addr pos, addr errorp, addr env)
{
	if (errorp == Unbound)
		errorp = T;
	if (env == Unbound)
		env = Nil;
	Return(find_class_common_(pos, errorp != Nil, env, &pos));
	setresult_control(ptr, pos);

	return 0;
}

static void type_find_class(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt2(&args, args, values, type);
	GetTypeValues(&values, ClassNull);
	type_compiled_heap(args, values, ret);
}

static void defun_find_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_find_class);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf find-class) (class symbol &optional errorp env) ...) -> class */
static int function_setf_find_class(Execute ptr,
		addr clos, addr name, addr errorp, addr env)
{
	/* (declare (ignore errorp)) */
	if (env == Unbound)
		env = Nil;
	setf_find_class_common(clos, name, env);
	setresult_control(ptr, clos);

	return 0;
}

static void type_setf_find_class(addr *ret)
{
	addr args, values, type1, type2;

	GetTypeTable(&args, Class);
	GetTypeTable(&values, Symbol);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, EnvironmentNull);
	typeargs_var2opt2(&args, args, values, type1, type2);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_find_class(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_CLASS, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_setf_find_class);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_find_class(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defgeneric compute-applicable-methods ...) */
static void defgeneric_compute_applicable_methods(void)
{
	ImportMopPackage(COMPUTE_APPLICABLE_METHODS);
}


/* defmacro_define_method_combination(); */
static int function_define_method_combination(Execute ptr, addr form, addr env)
{
	Return(define_method_combination_common(ptr->local, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_method_combination(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_METHOD_COMBINATION, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_method_combination);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defgeneric find-method ...) */
static void defgeneric_find_method(void)
{
	ImportMopPackage(FIND_METHOD);
}


/* (defgeneric add-method ...) */
static void defgeneric_add_method(void)
{
	ImportMopPackage(ADD_METHOD);
}


/* (defgeneric initialize-instance ...) */
static void defgeneric_initialize_instance(void)
{
	ImportMopPackage(INITIALIZE_INSTANCE);
}


/* (defgeneric class-name ...) */
static void defgeneric_class_name_import(void)
{
	ImportMopPackage(CLASS_NAME);
}


/* (defgeneric (setf class-name) ...) */
static void defgeneric_setf_class_name_import(void)
{
	/* do-nothing */
}


/* (defun class-of (object) ...) -> class */
static int function_class_of(Execute ptr, addr pos)
{
	Return(clos_class_of_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_class_of(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_class_of(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLASS_OF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_class_of);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_class_of(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unbound-slot-instance ...) */
static int function_unbound_slot_instance(Execute ptr, addr var)
{
	Return(unbound_slot_instance_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_unbound_slot_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_unbound_slot_instance(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNBOUND_SLOT_INSTANCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_unbound_slot_instance);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unbound_slot_instance(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
static void init_clos_objects(void)
{
	SetPointerCall(defun, var1dynamic, ensure_generic_function);
	SetPointerCall(defun, var2, slot_boundp);
	SetPointerCall(defun, var2, slot_exists_p);
	SetPointerCall(defun, var2, slot_makunbound);
	SetPointerCall(defun, var2, slot_value);
	SetPointerCall(defun, var3, setf_slot_value);
	SetPointerCall(defun, var1dynamic, make_load_form_saving_slots);
	SetPointerCall(defmacro, macro, with_accessors);
	SetPointerCall(defmacro, macro, with_slots);
	SetPointerCall(defmacro, macro, defclass);
	SetPointerCall(defmacro, macro, defgeneric);
	SetPointerCall(defmacro, macro, defmethod);
	SetPointerCall(defun, var1opt2, find_class);
	SetPointerCall(defun, var2opt2, setf_find_class);
	SetPointerCall(defmacro, macro, define_method_combination);
	SetPointerCall(defun, var1, class_of);
	SetPointerCall(defun, var1, unbound_slot_instance);
}

static void build_clos_objects(void)
{
	defgeneric_function_keywords();
	defun_ensure_generic_function();
	defgeneric_allocate_instance();
	defgeneric_reinitialize_instance();
	defgeneric_shared_initialize();
	defgeneric_update_instance_for_different_class();
	defgeneric_update_instance_for_redefined_class();
	defgeneric_change_class();
	defun_slot_boundp();
	defun_slot_exists_p();
	defun_slot_makunbound();
	defgeneric_slot_missing();
	defgeneric_slot_unbound();
	defun_slot_value();
	defun_setf_slot_value();
	defgeneric_method_qualifiers_import();
	defgeneric_no_applicable_method();
	defgeneric_no_next_method();
	defgeneric_remove_method();
	defgeneric_make_instance();
	defgeneric_make_instances_obsolete();
	defgeneric_make_load_form_common();
	defun_make_load_form_saving_slots();
	defmacro_with_accessors();
	defmacro_with_slots();
	defmacro_defclass();
	defmacro_defgeneric();
	defmacro_defmethod();
	defun_find_class();
	defun_setf_find_class();
	defgeneric_compute_applicable_methods();
	defmacro_define_method_combination();
	defgeneric_find_method();
	defgeneric_add_method();
	defgeneric_initialize_instance();
	defgeneric_class_name_import();
	defgeneric_setf_class_name_import();
	defun_class_of();
	defun_unbound_slot_instance();
}

_g void init_common_objects(void)
{
	/* metaobject protocol */
	init_metaobject_protocol();
	/* common-lisp objects */
	init_clos_objects();
}

_g void build_common_objects(void)
{
	/* metaobject protocol */
	build_metaobject_protocol();
	/* common-lisp objects */
	build_clos_objects();
}

