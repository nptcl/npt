#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "clos_define.h"
#include "constant.h"
#include "execute.h"

#define Clos_standard_class _n(Clos_standard_class)
#define Clos_standard_generic _n(Clos_standard_generic)
#define Clos_standard_method _n(Clos_standard_method)
#define Clos_standard_combination _n(Clos_standard_combination)
#define Clos_standard_specializer _n(Clos_standard_specializer)

#define clos_standard_ignore _n(clos_standard_ignore)
#define clos_standard_class_p_debug _n(clos_standard_class_p_debug)
#define clos_standard_generic_p_debug _n(clos_standard_generic_p_debug)
#define clos_standard_method_p_debug _n(clos_standard_method_p_debug)
#define clos_standard_combination_p_debug _n(clos_standard_combination_p_debug)
#define clos_standard_specializer_p_debug _n(clos_standard_specializer_p_debug)

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

extern addr Clos_standard_class;
extern addr Clos_standard_generic;
extern addr Clos_standard_method;
extern addr Clos_standard_combination;
extern addr Clos_standard_specializer;

#define clos_standard_class_p_Low(x)		(Clos_standard_class == (x))
#define clos_standard_generic_p_Low(x)		(Clos_standard_generic == (x))
#define clos_standard_method_p_Low(x)		(Clos_standard_method == (x))
#define clos_standard_combination_p_Low(x)	(Clos_standard_combination == (x))
#define clos_standard_specializer_p_Low(x)	(Clos_standard_specializer == (x))

#ifdef LISP_DEBUG
#define clos_standard_class_p		clos_standard_class_p_debug
#define clos_standard_generic_p		clos_standard_generic_p_debug
#define clos_standard_method_p		clos_standard_method_p_debug
#define clos_standard_combination_p	clos_standard_combination_p_debug
#define clos_standard_specializer_p	clos_standard_specializer_p_debug
#else
#define clos_standard_class_p		clos_standard_class_p_Low
#define clos_standard_generic_p		clos_standard_generic_p_Low
#define clos_standard_method_p		clos_standard_method_p_Low
#define clos_standard_combination_p	clos_standard_combination_p_Low
#define clos_standard_specializer_p	clos_standard_specializer_p_Low
#endif

/* access */
void clos_standard_ignore(int value);
int clos_standard_class_p_debug(addr pos);
int clos_standard_generic_p_debug(addr pos);
int clos_standard_method_p_debug(addr pos);
int clos_standard_combination_p_debug(addr pos);
int clos_standard_specializer_p_debug(addr pos);

/* talbe */
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

/* build */
void init_clos(void);
void build_clos(Execute ptr);

#endif

