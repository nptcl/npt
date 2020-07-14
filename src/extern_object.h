#ifndef __LISP_EXTERN_OBJECT_HEADER__
#define __LISP_EXTERN_OBJECT_HEADER__

#include "typedef_basic.h"

/* nil, t */
addr lisp_nil(void);
addr lisp_t(void);

/* type */
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_symbol_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);

/* cons */
addr lisp_cons(addr car, addr cdr);
addr lisp_list(addr car, ...);
addr lisp_lista(addr car, ...);
addr lisp_vector(size_t size);

addr lisp0_car(addr list);
addr lisp0_cdr(addr list);
void lisp0_carcdr(addr list, addr *car, addr *cdr);
void lisp_car(addr list, addr *ret);
void lisp_cdr(addr list, addr *ret);
void lisp_carcdr(addr list, addr *car, addr *cdr);

void lisp0_setf_car(addr cons, addr value);
void lisp0_setf_cdr(addr cons, addr value);
void lisp0_setf_carcdr(addr cons, addr car, addr cdr);
void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

/* list */
int lisp_reverse_(addr *ret, addr list);
int lisp_nreverse_(addr *ret, addr list);

/* sequence */
int lisp_getelt_(addr vector, size_t index, addr *ret);
int lisp_setelt_(addr vector, size_t index, addr value);
int lisp_length_(addr vector, size_t *ret);

/* string */
int lisp_string8_(addr *ret, const void *str);
int lisp_string16_(addr *ret, const void *str);

/* package */
int lisp_package_(addr *ret, addr pos);
int lisp_package8_(addr *ret, const void *str);
int lisp_package16_(addr *ret, const void *str);

/* intern */
int lisp_intern_(addr *ret, addr package, addr name);
int lisp_intern8_(addr *ret, const void *package, const void *name);
int lisp_intern16_(addr *ret, const void *package, const void *name);

/* reader */
int lisp_reader_(addr *ret, addr str);
int lisp_reader8_(addr *ret, const void *str);
int lisp_reader16_(addr *ret, const void *str);

/* let */
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_get_special_(addr symbol, addr *ret);
int lisp_get_special8_(const void *name, addr *ret);
int lisp_get_special16_(const void *name, addr *ret);
int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);

/* pathname */
int lisp_pathname_(addr *ret, addr name);
int lisp_namestring_(addr *ret, addr path);

/* object */
int lisp_character(addr *ret, unicode value);
void lisp_fixnum(addr *ret, fixnum value);
int lisp_float(addr *ret, float value);
int lisp_double_(addr *ret, double value);
int lisp_long_double_(addr *ret, long double value);
int lisp_zerop(addr value);
int lisp_plusp(addr value);
int lisp_minusp(addr value);
int lisp_get_character_(addr pos, unicode *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);

/* function */
int lisp_function_(addr value, addr *ret);
int lisp_function8_(const void *str, addr *ret);
int lisp_function16_(const void *str, addr *ret);
int lisp_funcall_(addr *ret, addr call, ...);
int lisp_funcall8_(addr *ret, const void *str, ...);
int lisp_funcall16_(addr *ret, const void *str, ...);
int lisp_apply_(addr *ret, addr call, ...);
int lisp_apply8_(addr *ret, const void *str, ...);
int lisp_apply16_(addr *ret, const void *str, ...);

#endif

