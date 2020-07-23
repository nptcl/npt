#ifndef __CALL_PACKAGES_HEADER__
#define __CALL_PACKAGES_HEADER__

#include "execute.h"
#include "typedef.h"

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
_g int defpackage_common(addr form, addr env, addr *ret);
_g int do_symbols_common(addr form, addr env, addr *ret);
_g int do_external_symbols_common(addr form, addr env, addr *ret);
_g int do_all_symbols_common(addr form, addr env, addr *ret);
_g int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec);

#endif

