#ifndef __CLOS_OBJECT_HEADER__
#define __CLOS_OBJECT_HEADER__

#include "execute.h"
#include "local.h"

enum SLOT_INDEX {
	SLOT_INDEX_NAME,
	SLOT_INDEX_ALLOCATION,
	SLOT_INDEX_LOCATION,
	SLOT_INDEX_TYPE,
	SLOT_INDEX_INITARGS,
	SLOT_INDEX_INITFORM,
	SLOT_INDEX_INITFUNCTION,
	SLOT_INDEX_READERS,
	SLOT_INDEX_WRITERS,
	SLOT_INDEX_DOCUMENTATION,
	SLOT_INDEX_SIZE
};

enum CLOS_INDEX {
	CLOS_INDEX_CLASS_OF,
	CLOS_INDEX_VERSION,
	CLOS_INDEX_TABLE,
	CLOS_INDEX_SLOTS,
	CLOS_INDEX_DATA,
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
	Clos_class_direct_shared,
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
	Clos_method_lambda_parse,
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

#define GetClos GetArrayA2
#define SetClos SetArrayA2
#define GetSlot GetArrayA2
#define SetSlot SetArrayA2

/* object */
addr find_class_nil(addr name);
addr find_class(addr name);
void setf_find_class(addr name, addr value);
void forget_all_classes(void);
void unsafe_remove_class(addr name);

addr find_generic_function_nil(addr name);
addr find_generic_function(addr name);
void setf_find_generic_function(addr name, addr value);

addr find_method_combination_nil(addr name);
addr find_method_combination(addr name);
void setf_find_method_combination(addr name, addr value);
void forget_all_method_combination(void);

addr find_eql_specializer_nil(addr name);
addr find_eql_specializer(addr name);
void setf_find_eql_specializer(addr name, addr value);
void forget_all_eql_specializer(void);

void slot_alloc(LocalRoot local, addr *ret);
void slot_local(LocalRoot local, addr *ret);
void slot_heap(addr *ret);
void slot_copy_alloc(LocalRoot local, addr *ret, addr slot);
void slot_copy_local(LocalRoot local, addr *ret, addr slot);
void slot_copy_heap(addr *ret, addr slot);

void clos_alloc(LocalRoot local, addr *ret, addr slots);
void clos_local(LocalRoot local, addr *ret, addr slots);
void clos_heap(addr *ret, addr slots);
void clos_copy_alloc(LocalRoot local, addr *ret, addr pos);
void clos_copy_local(LocalRoot local, addr *ret, addr pos);
void clos_copy_heap(addr *ret, addr pos);

int closp(addr clos);
void clos_swap(addr clos1, addr clos2);
void clos_destroy(addr clos);
void clos_class_of(addr clos, addr *ret);
void setf_clos_class_of(addr clos, addr value);
void clos_version(addr clos, addr *ret);
void setf_clos_version(addr clos, addr value);
void clos_version_value(addr clos, fixnum *ret);

void clos_elt_unbound(addr clos, size_t index, addr *ret);
int clos_elt_unbound_p(addr clos, size_t index);
int clos_elt_boundp(addr clos, size_t index);
void clos_elt(addr clos, size_t index, addr *ret);
void setf_clos_elt(addr clos, size_t index, addr value);

int clos_value_check(addr clos, addr name, addr *ret);
void clos_value_unbound(addr clos, addr name, addr *ret);
void clos_value(addr clos, addr name, addr *ret);
void setf_clos_value(addr clos, addr name, addr value);

void make_instance_restrict_alloc(LocalRoot local, addr clos, addr *ret);
void make_instance_restrict_local(LocalRoot local, addr clos, addr *ret);
void make_instance_restrict_heap(addr clos, addr *ret);

int clos_slot_exists_p(addr clos, addr name);
int clos_slot_boundp_nil(addr clos, addr name);
int clos_slot_boundp(addr clos, addr name);
int clos_slot_makunbound_nil(addr clos, addr name);
void clos_slot_makunbound(addr clos, addr name);

#ifdef LISP_DEBUG
void class_elt(addr clos, enum Clos_class_Index index, addr *ret);
void generic_elt(addr clos, enum Clos_generic_Index index, addr *ret);
void method_elt(addr clos, enum Clos_method_Index index, addr *ret);
void combination_elt(addr clos, enum Clos_combination_Index index, addr *ret);
void specializer_elt(addr clos, enum Clos_specializer_Index index, addr *ret);

void setf_class_elt(addr clos, enum Clos_class_Index index, addr value);
void setf_generic_elt(addr clos, enum Clos_generic_Index index, addr value);
void setf_method_elt(addr clos, enum Clos_method_Index index, addr value);
void setf_combination_elt(addr clos, enum Clos_combination_Index index, addr value);
void setf_specializer_elt(addr clos, enum Clos_specializer_Index index, addr value);

#else
#define class_elt clos_elt
#define generic_elt clos_elt
#define method_elt clos_elt
#define combination_elt clos_elt
#define specializer_elt clos_elt

#define setf_class_elt setf_clos_elt
#define setf_generic_elt setf_clos_elt
#define setf_method_elt setf_clos_elt
#define setf_combination_elt setf_clos_elt
#define setf_specializer_elt setf_clos_elt
#endif

#endif

