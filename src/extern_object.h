#ifndef __LISP_EXTERN_OBJECT_HEADER__
#define __LISP_EXTERN_OBJECT_HEADER__

#include "typedef_basic.h"

/* object */
int lisp0_character_(addr *ret, unicode value);
void lisp0_fixnum(addr *ret, fixnum value);
int lisp0_float_(addr *ret, float value);
int lisp0_double_(addr *ret, double value);
int lisp0_long_double_(addr *ret, long double value);

int lisp_character_(addr x, unicode value);
void lisp_fixnum(addr x, fixnum value);
int lisp_float_(addr x, float value);
int lisp_double_(addr x, double value);
int lisp_long_double_(addr x, long double value);

int lisp_zero_p(addr value);
int lisp_plus_p(addr value);
int lisp_minus_p(addr value);
void lisp_get_character(addr pos, unicode *ret);
void lisp_get_fixnum(addr pos, fixnum *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);

/* package */
int lisp0_package_(addr *ret, addr pos);
int lisp0_package8_(addr *ret, const void *str);
int lisp0_package16_(addr *ret, const void *str);
int lisp0_package32_(addr *ret, const void *str);
int lisp_package_(addr x, addr pos);
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);

int lisp_in_package_(addr pos);
int lisp_in_package8_(const void *str);
int lisp_in_package16_(const void *str);
int lisp_in_package32_(const void *str);
int lisp_push_and_in_package_(addr pos);
int lisp_push_and_in_package8_(const void *str);
int lisp_push_and_in_package16_(const void *str);
int lisp_push_and_in_package32_(const void *str);

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

/* pathname */
int lisp0_pathname_(addr *ret, addr name);
int lisp0_pathname8_(addr *ret, const void *str);
int lisp0_pathname16_(addr *ret, const void *str);
int lisp0_pathname32_(addr *ret, const void *str);
int lisp0_namestring_(addr *ret, addr path);
int lisp_pathname_(addr x, addr name);
int lisp_pathname8_(addr x, const void *str);
int lisp_pathname16_(addr x, const void *str);
int lisp_pathname32_(addr x, const void *str);
int lisp_namestring_(addr x, addr path);

/* paper */
int lisp0_paper_(addr *ret, size_t array, size_t body);
int lisp_paper_(addr x, size_t array, size_t body);
int lisp_paper_gettype_(addr x, byte *ret);
int lisp_paper_settype_(addr x, byte value);
int lisp_paper_lenarray_(addr x, size_t *ret);
int lisp_paper_lenbody_(addr x, size_t *ret);
int lisp_paper_getarray_(addr x, size_t index, addr *ret);
int lisp_paper_setarray_(addr x, size_t index, addr value);
int lisp_paper_getbody_(addr x, size_t index, byte *ret);
int lisp_paper_setbody_(addr x, size_t index, byte value);
int lisp_paper_getmemory_(addr x, size_t a, size_t b, void *output, size_t *ret);
int lisp_paper_setmemory_(addr x, size_t a, size_t b, const void *input, size_t *ret);

#endif

