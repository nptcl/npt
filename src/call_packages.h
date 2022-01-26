#ifndef __CALL_PACKAGES_HEADER__
#define __CALL_PACKAGES_HEADER__

#include "execute.h"
#include "typedef.h"

#define export_common_ _n(export_common_)
#define find_symbol_common_ _n(find_symbol_common_)
#define import_common_ _n(import_common_)
#define rename_package_common_ _n(rename_package_common_)
#define shadow_common_ _n(shadow_common_)
#define shadowing_import_common_ _n(shadowing_import_common_)
#define make_package_common_ _n(make_package_common_)
#define with_package_iterator_common_ _n(with_package_iterator_common_)
#define unexport_common_ _n(unexport_common_)
#define unintern_common_ _n(unintern_common_)
#define in_package_common_ _n(in_package_common_)
#define unuse_package_common_ _n(unuse_package_common_)
#define use_package_common_ _n(use_package_common_)
#define do_symbols_common_ _n(do_symbols_common_)
#define do_external_symbols_common_ _n(do_external_symbols_common_)
#define do_all_symbols_common_ _n(do_all_symbols_common_)
#define intern_common_ _n(intern_common_)

int export_common_(Execute ptr, addr symbols, addr pg);
int find_symbol_common_(Execute ptr, addr name, addr pg, addr *ret, addr *state);
int import_common_(Execute ptr, addr symbols, addr pg);
int rename_package_common_(Execute ptr, addr pg, addr name, addr names, addr *ret);
int shadow_common_(Execute ptr, addr symbols, addr pg);
int shadowing_import_common_(Execute ptr, addr symbols, addr pg);
int make_package_common_(Execute ptr, addr name, addr rest, addr *ret);
int with_package_iterator_common_(Execute ptr, addr form, addr env, addr *ret);
int unexport_common_(Execute ptr, addr symbols, addr pg);
int unintern_common_(Execute ptr, addr symbol, addr pg, addr *ret);
int in_package_common_(Execute ptr, addr form, addr env, addr *ret);
int unuse_package_common_(Execute ptr, addr unuse, addr pg);
int use_package_common_(Execute ptr, addr use, addr pg);
int do_symbols_common_(addr form, addr env, addr *ret);
int do_external_symbols_common_(addr form, addr env, addr *ret);
int do_all_symbols_common_(addr form, addr env, addr *ret);
int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec);

#endif

