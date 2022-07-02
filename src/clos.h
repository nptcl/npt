#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "clos_define.h"
#include "constant.h"
#include "execute.h"

#define clos_find_class_nil _n(clos_find_class_nil)
#define clos_find_class_ _n(clos_find_class_)
#define clos_define_class _n(clos_define_class)
#define clos_find_generic_nil _n(clos_find_generic_nil)
#define clos_find_generic_ _n(clos_find_generic_)
#define clos_define_generic_ _n(clos_define_generic_)
#define clos_find_combination_nil _n(clos_find_combination_nil)
#define clos_find_combination_ _n(clos_find_combination_)
#define clos_define_combination _n(clos_define_combination)
#define clos_find_specializer_nil_ _n(clos_find_specializer_nil_)
#define clos_find_specializer_ _n(clos_find_specializer_)
#define clos_define_specializer_ _n(clos_define_specializer_)
#define clos_forget_all_specializer_unsafe _n(clos_forget_all_specializer_unsafe)
#define init_clos _n(init_clos)
#define build_clos _n(build_clos)

void clos_find_class_nil(addr name, addr *ret);
int clos_find_class_(addr name, addr *ret);
void clos_define_class(addr name, addr value);

void clos_find_generic_nil(addr name, addr *ret);
int clos_find_generic_(addr name, addr *ret);
int clos_define_generic_(addr name, addr value);

void clos_find_combination_nil(addr name, addr *ret);
int clos_find_combination_(addr name, addr *ret);
void clos_define_combination(addr name, addr value);

int clos_find_specializer_nil_(addr name, addr *ret);
int clos_find_specializer_(addr name, addr *ret);
int clos_define_specializer_(addr name, addr value);
void clos_forget_all_specializer_unsafe(void);

void init_clos(void);
void build_clos(Execute ptr);

#endif

