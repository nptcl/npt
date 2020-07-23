#ifndef __PACKAGE_SYMBOL_HEADER__
#define __PACKAGE_SYMBOL_HEADER__

#include "execute.h"
#include "package_object.h"
#include "typedef.h"

_g int intern_package_table_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int intern_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int intern_char_package_(addr package, const char *name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int unintern_package_(addr package, addr symbol, int *ret);
_g int import_package_(addr package, addr pos);
_g int shadow_list_package_(addr package, addr pos);
_g int shadow_package_(addr package, addr pos);
_g int shadowing_import_symbol_package_(addr package, addr symbol);
_g int shadowing_import_package_(addr package, addr pos);
_g int export_list_package_(addr package, addr pos);
_g int export_package_(addr package, addr pos);
_g int unexport_package_(addr package, addr pos);
_g int use_package_list_package_(addr package, addr pos);
_g int use_package_(addr package, addr pos);
_g int unuse_package_(addr package, addr pos);
_g int setkeyword_package_(addr pos);

_g int intern_default_package_(Execute ptr, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int internchar_(const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret);
_g int internchar_default_(Execute ptr, const char *name,
		addr *value, enum PACKAGE_TYPE *ret);
_g int internchar_null_(Execute ptr, const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret);
_g int internchar_keyword_(const char *name, addr *value, enum PACKAGE_TYPE *ret);
_g int interncommon_(const char *name, addr *value, enum PACKAGE_TYPE *ret);

_g void internchar_debug(const char *pname, const char *sname, addr *value);
_g void internchar_keyword_debug(const char *name, addr *value);
_g void interncommon_debug(const char *name, addr *value);
_g addr interncharr_debug(const char *pname, const char *sname);
_g addr interncharr_null_debug(Execute ptr, const char *pname, const char *sname);
_g addr interncommonr_debug(const char *name);

#endif

