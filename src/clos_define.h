#ifndef __CLOS_DEFINE_HEADER__
#define __CLOS_DEFINE_HEADER__

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
	SLOT_INDEX_READONLY,
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
	Clos_class_default_initargs,
	Clos_class_direct_default_initargs,
	Clos_class_version,
	Clos_class_document,
	Clos_class_redefined_class,
	Clos_class_size
};

enum Clos_generic_Index {
	Clos_generic_name,
	Clos_generic_methods,
	Clos_generic_lambda_list,
	Clos_generic_argument_precedence_order,
	Clos_generic_declarations,
	Clos_generic_method_class,
	Clos_generic_method_combination,

	Clos_generic_vector,
	Clos_generic_remove,
	Clos_generic_argument,
	Clos_generic_documentation,
	Clos_generic_eqlcheck,
	Clos_generic_cache,
	Clos_generic_call,
	Clos_generic_precedence_index,
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

enum Clos_shortcomb_Index {
	Clos_shortcomb_name,
	Clos_shortcomb_document,
	Clos_shortcomb_identity,
	Clos_shortcomb_operator,
	Clos_shortcomb_order,
	Clos_shortcomb_size
};

enum Clos_longcomb_Index {
	Clos_longcomb_name,
	Clos_longcomb_document,
	Clos_longcomb_lambda_list,
	Clos_longcomb_binding,
	Clos_longcomb_qualifiers,
	Clos_longcomb_arguments,
	Clos_longcomb_generic,
	Clos_longcomb_form,
	Clos_longcomb_function,
	Clos_longcomb_size
};

enum Clos_shortdef_Index {
	Clos_shortdef_name,
	Clos_shortdef_document,
	Clos_shortdef_identity,
	Clos_shortdef_operator,
	Clos_shortdef_size
};

enum Clos_longdef_Index {
	Clos_longdef_name,
	Clos_longdef_document,
	Clos_longdef_lambda_list,
	Clos_longdef_qualifiers,
	Clos_longdef_arguments,
	Clos_longdef_generic,
	Clos_longdef_form,
	Clos_longdef_size
};

enum Clos_specializer_Index {
	Clos_specializer_object,
	Clos_specializer_type,
	Clos_specializer_size
};

enum Clos_standard {
	Clos_standard_around,
	Clos_standard_before,
	Clos_standard_primary,
	Clos_standard_after,
	Clos_standard_size
};

enum Clos_short {
	Clos_short_around,
	Clos_short_primary,
	Clos_short_size
};

enum Clos_structure_Index {
	Clos_structure_name,
	Clos_structure_direct_slots,
	Clos_structure_slots,
	Clos_structure_documentation,
	Clos_structure_include,
	Clos_structure_precedence_list,
	Clos_structure_type,
	Clos_structure_vector,
	Clos_structure_named,
	Clos_structure_named_index,
	Clos_structure_value,
	Clos_structure_predicate,
	Clos_structure_access,
	Clos_structure_copier,
	Clos_structure_constructor,
	Clos_structure_print,
	Clos_structure_size
};

#endif

