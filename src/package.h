#ifndef __PACKAGE_HEADER__
#define __PACKAGE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "package_object.h"

#define PackageTable(x) (*(x) = LispRoot(PACKAGE))

_g void build_package(void);
_g void getpackage(Execute ptr, addr *ret);

_g void pushlist_package(addr package, enum PACKAGE_INDEX index, addr pos);
_g void append_nicknames_package(addr pos, addr right);
_g void make_package(addr name, addr names, addr use, addr *ret);
_g int remove_check_package(addr package, enum PACKAGE_INDEX index, addr symbol);
_g int delete_package(addr pos);
_g void delete_renameone_package(addr table, addr name);
_g void rename_package(addr pos, addr name, addr names, addr *ret);

_g enum PACKAGE_TYPE find_symbol_package(addr package, addr name, addr *ret);
_g void find_package(addr name, addr *ret);
_g void find_char_package(const char *name, addr *ret);
_g void package_designer(addr pos, addr *ret);
_g void package_size_heap(addr *ret, addr name, size_t size);
_g void package_heap(addr *ret, addr name);
_g void find_allsymbols_package(addr name, addr *ret);
_g void list_all_packages(addr *ret);

_g void in_package(Execute ptr, addr package, addr *ret);
_g void in_package_lisp_package(void);

_g int externalp_package(addr symbol, addr package);
_g int exportp_package(addr symbol, addr package);
_g int exportp_name_package(addr package, addr name, addr *ret);
_g int checksymbol_package(addr symbol, addr package);
_g void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret);

/* initialize */
_g void init_package(void);

#endif

