#ifndef __LISP_EXTERN_SEQUENCE_HEADER__
#define __LISP_EXTERN_SEQUENCE_HEADER__

#include "typedef_basic.h"

/* sequence */
void lisp0_cons(addr *ret, addr car, addr cdr);
void lisp_cons(addr x, addr car, addr cdr);
void lisp0_vector(addr *ret, size_t size);
void lisp_vector(addr x, size_t size);

void lisp0_list_va(addr *ret, va_list args);
void lisp0_lista_va(addr *ret, va_list args);
void lisp0_list(addr *ret, ...);
void lisp_list(addr x, ...);
void lisp0_lista(addr *ret, ...);
void lisp_lista(addr x, ...);

int lisp0_getelt_(addr *ret, addr pos, size_t index);
int lisp_getelt_(addr x, addr pos, size_t index);
int lisp_setelt_(addr pos, size_t index, addr value);
int lisp_length_(addr pos, size_t *ret);

int lisp0_reverse_(addr *ret, addr pos);
int lisp0_nreverse_(addr *ret, addr pos);
int lisp_reverse_(addr x, addr pos);
int lisp_nreverse_(addr x, addr pos);

/* cons */
void lisp0_car(addr *ret, addr list);
void lisp0_cdr(addr *ret, addr list);
void lisp0_carcdr(addr *car, addr *cdr, addr list);
void lisp_car(addr x, addr list);
void lisp_cdr(addr x, addr list);
void lisp_carcdr(addr x, addr y, addr list);

void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

/* string */
int lisp0_string8_(addr *ret, const void *str);
int lisp0_string16_(addr *ret, const void *str);
int lisp0_string32_(addr *ret, const void *str);
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);

#endif

