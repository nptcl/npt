#ifndef __LISP_EXTERN_OBJECT_HEADER__
#define __LISP_EXTERN_OBJECT_HEADER__

#include "typedef_basic.h"

/* hold */
int lisp_hold_p(addr x);
void lisp_hold_value(addr x, addr *ret);
void lisp_hold_set(addr x, addr value);
addr lisp_holdv(addr x);
void lisp_hold(addr *ret);
addr Lisp_hold(void);

/* nil, t */
void lisp0_nil(addr *ret);
void lisp0_t(addr *ret);
void lisp_nil(addr x);
void lisp_t(addr x);

/* type */
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_null_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_symbol_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);

/* cons */
void lisp0_cons(addr *ret, addr car, addr cdr);
void lisp_cons(addr x, addr car, addr cdr);
void lisp_vector(addr x, size_t size);

void lisp0_car(addr *ret, addr list);
void lisp0_cdr(addr *ret, addr list);
void lisp0_carcdr(addr *car, addr *cdr, addr list);
void lisp_car(addr x, addr list);
void lisp_cdr(addr x, addr list);
void lisp_carcdr(addr x, addr y, addr list);

void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

/* list */
void lisp0_list_va(addr *ret, va_list args);
void lisp0_lista_va(addr *ret, va_list args);
void lisp0_list(addr *ret, ...);
void lisp_list(addr x, ...);
void lisp0_lista(addr *ret, ...);
void lisp_lista(addr x, ...);
int lisp0_reverse_(addr *ret, addr list);
int lisp0_nreverse_(addr *ret, addr list);
int lisp_reverse_(addr x, addr list);
int lisp_nreverse_(addr x, addr list);

/* sequence */
int lisp0_getelt_(addr *ret, addr pos, size_t index);
int lisp_getelt_(addr x, addr pos, size_t index);
int lisp_setelt_(addr vector, size_t index, addr value);
int lisp_length_(addr vector, size_t *ret);

/* string */
int lisp0_string8_(addr *ret, const void *str);
int lisp0_string16_(addr *ret, const void *str);
int lisp0_string32_(addr *ret, const void *str);
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);

/* package */
int lisp0_package_(addr *ret, addr pos);
int lisp0_package8_(addr *ret, const void *str);
int lisp0_package16_(addr *ret, const void *str);
int lisp0_package32_(addr *ret, const void *str);
int lisp_package_(addr x, addr pos);
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);

/* intern */
int lisp0_intern_(addr *ret, addr package, addr name);
int lisp0_intern8_(addr *ret, const void *package, const void *name);
int lisp0_intern16_(addr *ret, const void *package, const void *name);
int lisp0_intern32_(addr *ret, const void *package, const void *name);
int lisp_intern_(addr x, addr package, addr name);
int lisp_intern8_(addr x, const void *package, const void *name);
int lisp_intern16_(addr x, const void *package, const void *name);
int lisp_intern32_(addr x, const void *package, const void *name);

/* reader */
int lisp0_reader_(addr *ret, addr str);
int lisp0_reader8_(addr *ret, const void *str);
int lisp0_reader16_(addr *ret, const void *str);
int lisp0_reader32_(addr *ret, const void *str);
int lisp_reader_(addr x, addr str);
int lisp_reader8_(addr x, const void *str);
int lisp_reader16_(addr x, const void *str);
int lisp_reader32_(addr x, const void *str);

/* let */
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);
int lisp0_get_special_(addr *ret, addr symbol);
int lisp0_get_special8_(addr *ret, const void *name);
int lisp0_get_special16_(addr *ret, const void *name);
int lisp0_get_special32_(addr *ret, const void *name);
int lisp_get_special_(addr x, addr symbol);
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);

int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);

/* pathname */
int lisp0_pathname_(addr *ret, addr name);
int lisp0_namestring_(addr *ret, addr path);
int lisp_pathname_(addr x, addr name);
int lisp_namestring_(addr x, addr path);

/* object */
int lisp0_character(addr *ret, unicode value);
void lisp0_fixnum(addr *ret, fixnum value);
int lisp0_float(addr *ret, float value);
int lisp0_double_(addr *ret, double value);
int lisp0_long_double_(addr *ret, long double value);

int lisp_character(addr x, unicode value);
void lisp_fixnum(addr x, fixnum value);
int lisp_float(addr x, float value);
int lisp_double_(addr x, double value);
int lisp_long_double_(addr x, long double value);

int lisp_zerop(addr value);
int lisp_plusp(addr value);
int lisp_minusp(addr value);
int lisp_get_character_(addr pos, unicode *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);

/* function */
int lisp0_function_(addr *ret, addr value);
int lisp0_function8_(addr *ret, const void *str);
int lisp0_function16_(addr *ret, const void *str);
int lisp0_function32_(addr *ret, const void *str);
int lisp_function_(addr x, addr value);
int lisp_function8_(addr x, const void *str);
int lisp_function16_(addr x, const void *str);
int lisp_function32_(const addr x, void *str);

/* funcall */
int lisp0_funcall_(addr *ret, addr call, ...);
int lisp0_funcall8_(addr *ret, const void *str, ...);
int lisp0_funcall16_(addr *ret, const void *str, ...);
int lisp0_funcall32_(addr *ret, const void *str, ...);
int lisp_funcall_(addr x, addr call, ...);
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);

/* apply */
int lisp0_apply_(addr *ret, addr call, ...);
int lisp0_apply8_(addr *ret, const void *str, ...);
int lisp0_apply16_(addr *ret, const void *str, ...);
int lisp0_apply32_(addr *ret, const void *str, ...);
int lisp_apply_(addr x, addr call, ...);
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);

#endif

