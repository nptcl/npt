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
#define with_package_iterator_common _n(with_package_iterator_common)
#define unexport_common_ _n(unexport_common_)
#define unintern_common_ _n(unintern_common_)
#define in_package_common _n(in_package_common)
#define unuse_package_common_ _n(unuse_package_common_)
#define use_package_common_ _n(use_package_common_)
#define do_symbols_common _n(do_symbols_common)
#define do_external_symbols_common _n(do_external_symbols_common)
#define do_all_symbols_common _n(do_all_symbols_common)
#define intern_common_ _n(intern_common_)

_g int export_common_(Execute ptr, addr symbols, addr pg);
_g int find_symbol_common_(Execute ptr, addr name, addr pg, addr *ret, addr *state);
_g int import_common_(Execute ptr, addr symbols, addr pg);
_g int rename_package_common_(Execute ptr, addr pg, addr name, addr names, addr *ret);
_g int shadow_common_(Execute ptr, addr symbols, addr pg);
_g int shadowing_import_common_(Execute ptr, addr symbols, addr pg);
_g int make_package_common_(Execute ptr, addr name, addr rest, addr *ret);
_g int with_package_iterator_common(Execute ptr, addr form, addr env, addr *ret);
_g int unexport_common_(Execute ptr, addr symbols, addr pg);
_g int unintern_common_(Execute ptr, addr symbol, addr pg, addr *ret);
_g int in_package_common(Execute ptr, addr form, addr env, addr *ret);
_g int unuse_package_common_(Execute ptr, addr unuse, addr pg);
_g int use_package_common_(Execute ptr, addr use, addr pg);
_g int do_symbols_common(addr form, addr env, addr *ret);
_g int do_external_symbols_common(addr form, addr env, addr *ret);
_g int do_all_symbols_common(addr form, addr env, addr *ret);
_g int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec);

#endif

