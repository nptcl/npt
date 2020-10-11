#ifndef __PACKAGE_HEADER__
#define __PACKAGE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "package_object.h"

#define build_package _n(build_package)
#define getpackage_ _n(getpackage_)
#define pushlist_package _n(pushlist_package)
#define append_nicknames_package_ _n(append_nicknames_package_)
#define make_package_ _n(make_package_)
#define delete_renameone_package_ _n(delete_renameone_package_)
#define rename_package_ _n(rename_package_)
#define find_symbol_package_ _n(find_symbol_package_)
#define find_package_ _n(find_package_)
#define find_char_package_ _n(find_char_package_)
#define package_designer_ _n(package_designer_)
#define package_size_heap_ _n(package_size_heap_)
#define package_heap_ _n(package_heap_)
#define find_allsymbols_package_ _n(find_allsymbols_package_)
#define list_all_packages_ _n(list_all_packages_)
#define in_package_ _n(in_package_)
#define remove_check_package _n(remove_check_package)
#define externalp_package_ _n(externalp_package_)
#define exportp_package_ _n(exportp_package_)
#define exportp_name_package_ _n(exportp_name_package_)
#define checksymbol_package_ _n(checksymbol_package_)
#define keyword_packagetype _n(keyword_packagetype)
#define init_package _n(init_package)

#define PackageTable(x) (*(x) = LispRoot(PACKAGE))

_g void build_package(void);
_g int getpackage_(Execute ptr, addr *ret);

_g void pushlist_package(addr package, enum PACKAGE_INDEX index, addr pos);
_g int append_nicknames_package_(addr pos, addr right);
_g int make_package_(addr name, addr names, addr use, addr *ret);
_g int delete_renameone_package_(addr table, addr name);
_g int rename_package_(addr pos, addr name, addr right, addr *ret);

_g int find_symbol_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int find_package_(addr name, addr *ret);
_g int find_char_package_(const char *name, addr *ret);
_g int package_designer_(addr pos, addr *ret);
_g int package_size_heap_(addr *ret, addr name, size_t size);
_g int package_heap_(addr *ret, addr name);
_g int find_allsymbols_package_(addr name, addr *ret);
_g int list_all_packages_(addr *ret);
_g int in_package_(Execute ptr, addr package, addr *ret);
_g int remove_check_package(addr package, enum PACKAGE_INDEX index, addr symbol);

_g int externalp_package_(addr symbol, addr package, int *ret);
_g int exportp_package_(addr symbol, addr package, int *ret);
_g int exportp_name_package_(addr package, addr name, addr *value, int *ret);
_g int checksymbol_package_(addr symbol, addr package, int *ret);
_g void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret);

/* initialize */
_g void init_package(void);

#endif

