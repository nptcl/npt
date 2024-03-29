#ifndef __CALL_OBJECTS_HEADER__
#define __CALL_OBJECTS_HEADER__

#include "execute.h"
#include "typedef.h"

#define defclass_common_ _n(defclass_common_)
#define define_condition_common_ _n(define_condition_common_)
#define find_class_common_ _n(find_class_common_)
#define setf_find_class_common _n(setf_find_class_common)
#define with_accessors_common_ _n(with_accessors_common_)
#define with_slots_common_ _n(with_slots_common_)
#define defgeneric_common_ _n(defgeneric_common_)
#define defmethod_common_ _n(defmethod_common_)
#define define_method_combination_common_ _n(define_method_combination_common_)
#define make_load_form_saving_slots_common_ _n(make_load_form_saving_slots_common_)
#define set_slots_syscall_ _n(set_slots_syscall_)

int defclass_common_(Execute ptr, addr form, addr env, addr *ret);
int define_condition_common_(Execute ptr, addr form, addr env, addr *ret);
int find_class_common_(addr pos, int errorp, addr env, addr *ret);
void setf_find_class_common(addr pos, addr name, addr env);
int with_accessors_common_(Execute ptr, addr form, addr env, addr *ret);
int with_slots_common_(Execute ptr, addr form, addr env, addr *ret);

int defgeneric_common_(Execute ptr, addr form, addr env, addr *ret);
int defmethod_common_(Execute ptr, addr form, addr env, addr *ret);
int define_method_combination_common_(
		LocalRoot local, addr form, addr env, addr *ret);
int make_load_form_saving_slots_common_(Execute ptr,
		addr var, addr list, addr env, addr *ret1, addr *ret2);
int set_slots_syscall_(addr var, addr slots, addr values);

#endif

