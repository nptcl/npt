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
addr lisp_car(addr list);
addr lisp_cdr(addr list);
void lisp_carcdr(addr list, addr *car, addr *cdr);

void lisp0_setf_car(addr cons, addr value);
void lisp0_setf_cdr(addr cons, addr value);
void lisp0_setf_carcdr(addr cons, addr car, addr cdr);
void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

/* list */
addr lisp_reverse(addr list);
addr lisp_nreverse(addr list);

/* sequence */
addr lisp_getelt(addr vector, size_t index);
void lisp_setelt(addr vector, size_t index, addr value);
size_t lisp_length(addr vector);

/* string */
addr lisp_string8(const void *str);
addr lisp_string16(const void *str);

/* package */
addr lisp_package(addr pos);
addr lisp_package8(const void *str);
addr lisp_package16(const void *str);

/* intern */
addr lisp_intern(addr package, addr name);
addr lisp_intern8(const void *package, const void *name);
addr lisp_intern16(const void *package, const void *name);

/* reader */
int lisp_reader(addr *ret, addr str);
int lisp_reader8(addr *ret, const void *str);
int lisp_reader16(addr *ret, const void *str);

/* let */
void lisp_push_special(addr symbol, addr value);
void lisp_push_special8(const void *name, addr value);
void lisp_push_special16(const void *name, addr value);
addr lisp_get_special(addr symbol);
addr lisp_get_special8(const void *name);
addr lisp_get_special16(const void *name);
void lisp_set_special(addr symbol, addr value);
void lisp_set_special8(const void *name, addr value);
void lisp_set_special16(const void *name, addr value);

/* pathname */
addr lisp_pathname(addr name);
addr lisp_namestring(addr path);

/* object */
addr lisp_character(unicode value);
addr lisp_fixnum(fixnum value);
addr lisp_float(float value);
addr lisp_double(double value);
addr lisp_long_double(long double value);
int lisp_zerop(addr value);
int lisp_plusp(addr value);
int lisp_minusp(addr value);
unicode lisp_get_character(addr pos);
float lisp_get_float(addr pos);
double lisp_get_double(addr pos);
long double lisp_get_long_double(addr pos);

/* function */
addr lisp_function(addr value);
addr lisp_function8(const void *str);
addr lisp_function16(const void *str);
int lisp_funcall(addr *ret, addr call, ...);
int lisp_funcall8(addr *ret, const void *str, ...);
int lisp_funcall16(addr *ret, const void *str, ...);
int lisp_apply(addr *ret, addr call, ...);
int lisp_apply8(addr *ret, const void *str, ...);
int lisp_apply16(addr *ret, const void *str, ...);

#endif

