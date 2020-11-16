#ifndef __PACKAGE_COMMON_HEADER__
#define __PACKAGE_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_gentemp_ _n(make_gentemp_)
#define do_symbols_package _n(do_symbols_package)
#define do_external_symbols_package _n(do_external_symbols_package)
#define do_all_symbols_package_ _n(do_all_symbols_package_)
#define all_symbols_package_ _n(all_symbols_package_)

int make_gentemp_(Execute ptr, addr prefix, addr package, addr *ret);
int do_symbols_package(Execute ptr, addr call, addr package);
int do_external_symbols_package(Execute ptr, addr call, addr package);
int do_all_symbols_package_(Execute ptr, addr call);
int all_symbols_package_(addr package, addr *ret);

#endif

