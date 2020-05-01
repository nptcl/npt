#ifndef __CALL_PACKAGES_HEADER__
#define __CALL_PACKAGES_HEADER__

#include "execute.h"
#include "typedef.h"

_g void export_common(Execute ptr, addr symbols, addr pg);
_g void find_symbol_common(Execute ptr, addr name, addr pg, addr *ret, addr *state);
_g void import_common(Execute ptr, addr symbols, addr pg);
_g void rename_package_common(Execute ptr, addr pg, addr name, addr names, addr *ret);
_g void shadow_common(Execute ptr, addr symbols, addr pg);
_g void shadowing_import_common(Execute ptr, addr symbols, addr pg);
_g void make_package_common(Execute ptr, addr name, addr rest, addr *ret);
_g int with_package_iterator_common(Execute ptr, addr form, addr env, addr *ret);
_g void unexport_common(Execute ptr, addr symbols, addr pg);
_g void unintern_common(Execute ptr, addr symbol, addr pg, addr *ret);
_g int in_package_common(Execute ptr, addr form, addr env, addr *ret);
_g void unuse_package_common(Execute ptr, addr unuse, addr pg);
_g void use_package_common(Execute ptr, addr use, addr pg);
_g int defpackage_common(addr form, addr env, addr *ret);
_g int do_symbols_common(addr form, addr env, addr *ret);
_g int do_external_symbols_common(addr form, addr env, addr *ret);
_g int do_all_symbols_common(addr form, addr env, addr *ret);
_g void intern_common(Execute ptr, addr name, addr pg, addr *ret, addr *sec);

#endif

