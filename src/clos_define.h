#ifndef __CLOS_DEFINE_HEADER__
#define __CLOS_DEFINE_HEADER__

#define CLOS_TABLE_CLASS_SIZE            256
#define CLOS_TABLE_COMBINATION_SIZE      32
#define CLOS_TABLE_SPECIALIZER_SIZE      32

enum SLOT_INDEX {
	SLOT_INDEX_NAME,
	SLOT_INDEX_TYPE,
	SLOT_INDEX_INITARGS,
	SLOT_INDEX_INITFORM,
	SLOT_INDEX_INITFUNCTION,
	SLOT_INDEX_READERS,
	SLOT_INDEX_WRITERS,
	SLOT_INDEX_DOCUMENT,
	SLOT_INDEX_CLASS,
	SLOT_INDEX_SIZE
};

enum CLOS_INDEX {
	CLOS_INDEX_CLASS_OF,
	CLOS_INDEX_SLOT,
	CLOS_INDEX_VALUE,
	CLOS_INDEX_SIZE
};

enum Clos_class_Index {
	Clos_class_name,
	Clos_class_direct_slots,
	Clos_class_direct_subclasses,
	Clos_class_direct_superclasses,
	Clos_class_precedence_list,
	Clos_class_slots,
	Clos_class_finalized_p,
	Clos_class_prototype,
	Clos_class_direct_methods,
	Clos_class_default_initargs,
	Clos_class_direct_default_initargs,
	Clos_class_version,
	Clos_class_update_info,
	Clos_class_document,
	Clos_class_size
};

enum Clos_generic_Index {
	Clos_generic_name,
	Clos_generic_lambda_list,
	Clos_generic_methods,
	Clos_generic_method_class,
	Clos_generic_argument_precedence_order,
	Clos_generic_declarations,
	Clos_generic_method_combination,
	Clos_generic_combination_arguments,
	Clos_generic_eqlcheck,
	Clos_generic_cache,
	Clos_generic_call,
	Clos_generic_size
};

enum Clos_method_Index {
	Clos_method_function,
	Clos_method_generic_function,
	Clos_method_lambda_list,
	Clos_method_qualifiers,
	Clos_method_specializers,
	Clos_method_size
};

enum Clos_combination_Index {
	Clos_combination_name,
	Clos_combination_long_p,
	Clos_combination_document,
	Clos_combination_identity,
	Clos_combination_operator,
	Clos_combination_lambda_list,
	Clos_combination_qualifiers,
	Clos_combination_arguments,
	Clos_combination_generic,
	Clos_combination_form,
	Clos_combination_function,
	Clos_combination_size
};

enum Clos_specializer_Index {
	Clos_specializer_object,
	Clos_specializer_type,
	Clos_specializer_size
};

#endif

