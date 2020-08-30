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
int lisp0_instance_(addr *ret, addr instance, ...);
int lisp0_instance8_(addr *ret, addr instance, ...);
int lisp0_instance16_(addr *ret, addr instance, ...);
int lisp0_instance32_(addr *ret, addr instance, ...);
int lisp_instance_(addr x, addr instance, ...);
int lisp_instance8_(addr x, addr instance, ...);
int lisp_instance16_(addr x, addr instance, ...);
int lisp_instance32_(addr x, addr instance, ...);

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
int lisp0_get_slot_(addr *ret, addr instance, addr symbol);
int lisp0_get_slot8_(addr *ret, addr instance, const void *str);
int lisp0_get_slot16_(addr *ret, addr instance, const void *str);
int lisp0_get_slot32_(addr *ret, addr instance, const void *str);
int lisp_get_slot_(addr x, addr instance, addr symbol);
int lisp_get_slot8_(addr x, addr instance, const void *str);
int lisp_get_slot16_(addr x, addr instance, const void *str);
int lisp_get_slot32_(addr x, addr instance, const void *str);

/* setf slot-value */
int lisp_set_slot_(addr instance, addr symbol, addr value);
int lisp_set_slot8_(addr instance, const void *str, addr value);
int lisp_set_slot16_(addr instance, const void *str, addr value);
int lisp_set_slot32_(addr instance, const void *str, addr value);

#endif

