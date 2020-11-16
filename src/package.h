#ifndef __PACKAGE_HEADER__
#define __PACKAGE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "package_object.h"

#define build_package _n(build_package)
#define getpackage_ _n(getpackage_)
#define append_nicknames_package_ _n(append_nicknames_package_)
#define delete_renameone_package_ _n(delete_renameone_package_)
#define rename_package_ _n(rename_package_)
#define find_symbol_package_ _n(find_symbol_package_)
#define find_package_ _n(find_package_)
#define find_char_package_ _n(find_char_package_)
#define package_size_heap_ _n(package_size_heap_)
#define package_heap_ _n(package_heap_)
#define find_allsymbols_package_ _n(find_allsymbols_package_)
#define list_all_packages_ _n(list_all_packages_)
#define in_package_ _n(in_package_)
#define externalp_package_ _n(externalp_package_)
#define exportp_package_ _n(exportp_package_)
#define exportp_name_package_ _n(exportp_name_package_)
#define checksymbol_package_ _n(checksymbol_package_)
#define keyword_packagetype _n(keyword_packagetype)
#define init_package _n(init_package)

#define PackageTable(x) (*(x) = LispRoot(PACKAGE))

void build_package(void);
int getpackage_(Execute ptr, addr *ret);

int append_nicknames_package_(addr pos, addr right);
int delete_renameone_package_(addr table, addr name);
int rename_package_(addr pos, addr name, addr right, addr *ret);

int find_symbol_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
int find_package_(addr name, addr *ret);
int find_char_package_(const char *name, addr *ret);
int package_size_heap_(addr *ret, addr name, size_t size);
int package_heap_(addr *ret, addr name);
int find_allsymbols_package_(addr name, addr *ret);
int list_all_packages_(addr *ret);
int in_package_(Execute ptr, addr package, addr *ret);

int externalp_package_(addr symbol, addr package, int *ret);
int exportp_package_(addr symbol, addr package, int *ret);
int exportp_name_package_(addr package, addr name, addr *value, int *ret);
int checksymbol_package_(addr symbol, addr package, int *ret);
void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret);

/* initialize */
void init_package(void);

#endif

