#ifndef __LISP_EXTERN_INSTANCE_HEADER__
#define __LISP_EXTERN_INSTANCE_HEADER__

#include "typedef_basic.h"

/* find-class */
void lisp0_find_class(addr *ret, addr symbol);
int lisp0_find_class_(addr *ret, addr symbol);
int lisp0_find_class8_(addr *ret, const void *str);
int lisp0_find_class16_(addr *ret, const void *str);
int lisp0_find_class32_(addr *ret, const void *str);
void lisp_find_class(addr x, addr symbol);
int lisp_find_class_(addr x, addr symbol);
int lisp_find_class8_(addr x, const void *str);
int lisp_find_class16_(addr x, const void *str);
int lisp_find_class32_(addr x, const void *str);

/* make-instance */
int lisp0_instance_(addr *ret, addr clos, ...);
int lisp0_instance8_(addr *ret, const void *clos, ...);
int lisp0_instance16_(addr *ret, const void *clos, ...);
int lisp0_instance32_(addr *ret, const void *clos, ...);
int lisp_instance_(addr x, addr clos, ...);
int lisp_instance8_(addr x, const void *clos, ...);
int lisp_instance16_(addr x, const void *clos, ...);
int lisp_instance32_(addr x, const void *clos, ...);

/* slot-exists-p */
int lisp_slot_exists_(addr instance, addr symbol, int *ret);
int lisp_slot_exists8_(addr instance, const void *str, int *ret);
int lisp_slot_exists16_(addr instance, const void *str, int *ret);
int lisp_slot_exists32_(addr instance, const void *str, int *ret);

/* slot-boundp */
int lisp_slot_boundp_(addr instance, addr symbol, int *ret);
int lisp_slot_boundp8_(addr instance, const void *str, int *ret);
int lisp_slot_boundp16_(addr instance, const void *str, int *ret);
int lisp_slot_boundp32_(addr instance, const void *str, int *ret);

/* slot-makunbound */
int lisp_slot_makunbound_(addr instance, addr symbol);
int lisp_slot_makunbound8_(addr instance, const void *str);
int lisp_slot_makunbound16_(addr instance, const void *str);
int lisp_slot_makunbound32_(addr instance, const void *str);

/* slot-value */
int lisp0_slot_value_(addr *ret, addr instance, addr symbol);
int lisp0_slot_value8_(addr *ret, addr instance, const void *str);
int lisp0_slot_value16_(addr *ret, addr instance, const void *str);
int lisp0_slot_value32_(addr *ret, addr instance, const void *str);
int lisp_slot_value_(addr x, addr instance, addr symbol);
int lisp_slot_value8_(addr x, addr instance, const void *str);
int lisp_slot_value16_(addr x, addr instance, const void *str);
int lisp_slot_value32_(addr x, addr instance, const void *str);

/* setf slot-value */
int lisp_slot_setf_(addr instance, addr symbol, addr value);
int lisp_slot_setf8_(addr instance, const void *str, addr value);
int lisp_slot_setf16_(addr instance, const void *str, addr value);
int lisp_slot_setf32_(addr instance, const void *str, addr value);

#endif

