#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"
#include "typedef.h"

#define check_qualifiers_equal_ _n(check_qualifiers_equal_)
#define method_combination_qualifiers_count_ _n(method_combination_qualifiers_count_)
#define qualifiers_position_nil_ _n(qualifiers_position_nil_)
#define qualifiers_position_ _n(qualifiers_position_)
#define build_clos_combination _n(build_clos_combination)
#define mop_find_method_combination_ _n(mop_find_method_combination_)
#define clos_find_method_combination_ _n(clos_find_method_combination_)
#define ensure_define_combination_short_common_ _n(ensure_define_combination_short_common_)
#define ensure_define_combination_long_common_ _n(ensure_define_combination_long_common_)
#define comb_longmacro_ _n(comb_longmacro_)
#define comb_longform_ _n(comb_longform_)
#define comb_shortform_ _n(comb_shortform_)

/* control */
int check_qualifiers_equal_(Execute ptr, addr comb, addr qua, int *ret);
int method_combination_qualifiers_count_(Execute ptr, addr comb, size_t *ret);
int qualifiers_position_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret);
int qualifiers_position_(Execute ptr, addr qua, addr comb, size_t *rsize);
void build_clos_combination(Execute ptr);

/* generic-function */
int mop_find_method_combination_(Execute ptr, addr symbol, addr list, addr *ret);
int clos_find_method_combination_(Execute ptr, addr list, addr *ret);
int ensure_define_combination_short_common_(Execute ptr,
		addr name, addr doc, addr ident, addr oper);
int ensure_define_combination_long_common_(Execute ptr,
		addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form, addr decl);

/* long form */
int comb_longmacro_(addr *ret,
		addr lambda, addr spec, addr args, addr gen, addr decl, addr form);
int comb_longform_(Execute ptr, addr *ret, addr gen, addr comb, addr data);
int comb_shortform_(Execute ptr, addr *ret, addr gen, addr comb, addr data);

#endif

