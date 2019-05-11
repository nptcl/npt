/*
 *  ANSI COMMON LISP: 7. Objects
 */
#include "common_header.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_type.h"
#include "cons.h"
#include "mop_common.h"
#include "package.h"

/*
 *  import
 */
#define ImportMopPackage(x) import_mop_package(CONSTANT_COMMON_##x)

static void import_mop_package(constindex index)
{
	addr package, symbol;

	GetConst(PACKAGE_COMMON_LISP, &package);
	GetConstant(index, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	import_package(package, symbol);
	export_package(package, symbol);
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
static void function_ensure_generic_function(Execute ptr, addr name, addr rest)
{
	if (ensure_generic_function_common(ptr, name, rest, &name)) return;
	setresult_control(ptr, name);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_ensure_generic_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ensure_generic_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defgeneric allocate-instance ...)  */
void static defgeneric_allocate_instance(void)
{
	ImportMopPackage(ALLOCATE_INSTANCE);
}


/* (defgeneric reinitialize-instance ...) */
void static defgeneric_reinitialize_instance(void)
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
static void function_slot_boundp(Execute ptr, addr pos, addr name)
{
	addr call, clos;
	LocalRoot local;
	LocalStack stack;

	/* redefined */
	clos_class_of(pos, &clos);
	if (clos_version_check(ptr, pos, clos)) return;

	/* call generic */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_SLOT_BOUNDP_USING_CLASS, &call);
	getfunctioncheck_local(ptr, call, &call);
	list_local(local, &pos, clos, pos, name, NULL);
	if (apply_control(ptr, call, pos)) return;
	rollback_local(local, stack);
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
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-exists-p (object symbol) ...) -> boolean */
static void function_slot_exists_p(Execute ptr, addr pos, addr name)
{
	addr call, clos;
	LocalRoot local;
	LocalStack stack;

	/* redefined */
	clos_class_of(pos, &clos);
	if (clos_version_check(ptr, pos, clos)) return;

	/* call generic */
	local = ptr->local;
	GetConst(CLOSNAME_SLOT_EXISTS_P_USING_CLASS, &call);
	getfunctioncheck_local(ptr, call, &call);
	push_local(local, &stack);
	list_local(local, &pos, clos, pos, name, NULL);
	if (apply_control(ptr, call, pos)) return;
	rollback_local(local, stack);
}

static void defun_slot_exists_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_EXISTS_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_slot_exists_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SlotBoundp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun slot-makunbound (instance symbol) ...) -> instance */
static void function_slot_makunbound(Execute ptr, addr pos, addr name)
{
	addr call, clos;
	LocalRoot local;
	LocalStack stack;

	/* redefined */
	clos_class_of(pos, &clos);
	if (clos_version_check(ptr, pos, clos)) return;

	/* call generic */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, &call);
	getfunctioncheck_local(ptr, call, &call);
	list_local(local, &pos, clos, pos, name, NULL);
	if (apply_control(ptr, call, pos)) return;
	rollback_local(local, stack);
}

static void defun_slot_makunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_MAKUNBOUND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_slot_makunbound);
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
static void function_slot_value(Execute ptr, addr pos, addr name)
{
	addr call, clos;
	LocalRoot local;
	LocalStack stack;

	/* redefined */
	clos_class_of(pos, &clos);
	if (clos_version_check(ptr, pos, clos)) return;

	/* call generic */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	getfunctioncheck_local(ptr, call, &call);
	list_local(local, &pos, clos, pos, name, NULL);
	if (apply_control(ptr, call, pos)) return;
	rollback_local(local, stack);
}

static void type_slot_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_slot_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_slot_value(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf slot-value) (value pos name) ...) -> value */
static void function_setf_slot_value(Execute ptr, addr value, addr pos, addr name)
{
	addr call, clos;
	LocalRoot local;
	LocalStack stack;

	/* redefined */
	clos_class_of(pos, &clos);
	if (clos_version_check(ptr, pos, clos)) return;

	/* call generic */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &call);
	getsetfcheck_local(ptr, call, &call);
	list_local(local, &pos, value, clos, pos, name, NULL);
	if (apply_control(ptr, call, pos)) return;
	rollback_local(local, stack);
}

static void type_setf_slot_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var3(&args, args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_slot_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLOT_VALUE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, function_setf_slot_value);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_slot_value(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defgeneric method-qualifiers ...) */
static void defgeneric_method_qualifiers(void)
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


/* defgeneric_make_instance(); */
static void defgeneric_make_instance(void)
{
	ImportMopPackage(MAKE_INSTANCE);
}


/* defgeneric_make_instances_obsolete(); */
/* defgeneric_make_load_form(); */
/* defun_make_load_form_saving_slots(); */


/* (defmacro with-accessors ((entry*) instance declare* &body body) ...) -> t */
static void function_with_accessors(Execute ptr, addr form, addr env)
{
	with_accessors_common(ptr, form, env, &form);
	setresult_control(ptr, form);
}

static void defmacro_with_accessors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_ACCESSORS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_with_accessors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-slots ((entry*) instance declare* &body form) ...) -> t */
static void function_with_slots(Execute ptr, addr form, addr env)
{
	with_slots_common(ptr, form, env, &form);
	setresult_control(ptr, form);
}

static void defmacro_with_slots(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SLOTS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_with_slots);
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


/* (defmacro defgeneric (name lambda &rest args) ...) -> generic-function */
static void function_defgeneric(Execute ptr, addr form, addr env)
{
	defgeneric_common(form, env, &form);
	setresult_control(ptr, form);
}

static void defmacro_defgeneric(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFGENERIC, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_defgeneric);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defmethod
 *     (name qualifier* lambda declare* document* form*) -> method
 */
static void function_defmethod(Execute ptr, addr form, addr env)
{
	if (defmethod_common(ptr, form, env, &form)) return;
	setresult_control(ptr, form);
}

static void defmacro_defmethod(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFMETHOD, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_defmethod);
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
static void function_find_class(Execute ptr, addr pos, addr errorp, addr env)
{
	if (errorp == Unbound) errorp = T;
	if (env == Unbound) env = Nil;
	find_class_common(pos, errorp != Nil, env, &pos);
	setresult_control(ptr, pos);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, function_find_class);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf find-class) (class symbol &optional errorp env) ...) -> class */
static void function_setf_find_class(Execute ptr,
		addr clos, addr name, addr errorp, addr env)
{
	/* (declare (ignore errorp)) */
	if (env == Unbound) env = Nil;
	setf_find_class_common(clos, name, env);
	setresult_control(ptr, clos);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2opt2(pos, function_setf_find_class);
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
static void function_define_method_combination(Execute ptr, addr form, addr env)
{
	define_method_combination_common(ptr, form, env, &form);
	setresult_control(ptr, form);
}

static void defmacro_define_method_combination(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_METHOD_COMBINATION, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_define_method_combination);
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
static void defgeneric_class_name(void)
{
	ImportMopPackage(CLASS_NAME);
}


/* (defgeneric (setf class-name) ...) */
static void defgeneric_setf_class_name(void)
{
	/* do-nothing */
}


/* (defun class-of (object) ...) -> class */
static void function_class_of(Execute ptr, addr pos)
{
	clos_class_of(pos, &pos);
	setresult_control(ptr, pos);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_class_of);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_class_of(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unbound-slot-instance ...) */
static void function_unbound_slot_instance(Execute ptr, addr var)
{
	unbound_slot_instance(var, &var);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_unbound_slot_instance);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unbound_slot_instance(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
static void intern_clos_objects(void)
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
	defgeneric_method_qualifiers();
	defgeneric_no_applicable_method();
	defgeneric_no_next_method();
	defgeneric_remove_method();
	defgeneric_make_instance();
	/* defgeneric_make_instances_obsolete(); */
	/* defgeneric_make_load_form(); */
	/* defun_make_load_form_saving_slots(); */
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
	defgeneric_class_name();
	defgeneric_setf_class_name();
	defun_class_of();
	defun_unbound_slot_instance();
}

void intern_common_objects(void)
{
	/* metaobject protocol */
	intern_metaobject_protocol();
	/* common-lisp package */
	intern_clos_objects();
}

