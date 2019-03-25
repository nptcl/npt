#ifndef __LISP_HEADER__
#define __LISP_HEADER__

#include "typedef.h"

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

#endif

