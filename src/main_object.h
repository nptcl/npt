#ifndef __LISP_OBJECT_HEADER__
#define __LISP_OBJECT_HEADER__

#include "typedef_basic.h"

/* nil, t */
#define Lisp_nil   lisp_ref_nil()
#define Lisp_t     lisp_ref_t()
void lisp_get_nil(addr *ret);
void lisp_get_t(addr *ret);
addr lisp_ref_nil(void);
addr lisp_ref_t(void);

/* type */
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_symbol_p(addr x);
int lisp0_symbol_p(addr x);
int lisp0_array_p(addr x);
int lisp0_vector_p(addr x);
int lisp0_strvect_p(addr x);

/* cons */
void lisp_cons(addr *ret, addr car, addr cdr);
void lisp_list(addr *ret, ...);
void lisp_lista(addr *ret, ...);
void lisp_vector(addr *ret, size_t size);

void lisp0_car(addr list, addr *ret);
void lisp0_cdr(addr list, addr *ret);
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

/* vector */
void lisp0_getvector(addr vector, size_t index, addr *ret);
void lisp0_setvector(addr vector, size_t index, addr value);
void lisp_getvector(addr vector, size_t index, addr *ret);
void lisp_setvector(addr vector, size_t index, addr value);

#endif

