#ifndef __PACKAGE_SYMBOL_HEADER__
#define __PACKAGE_SYMBOL_HEADER__

#include "execute.h"
#include "package_object.h"
#include "typedef.h"

#define intern_package_table_ _n(intern_package_table_)
#define intern_package_ _n(intern_package_)
#define intern_char_package_ _n(intern_char_package_)
#define unintern_package_ _n(unintern_package_)
#define import_package_ _n(import_package_)
#define shadow_list_package_ _n(shadow_list_package_)
#define shadow_package_ _n(shadow_package_)
#define shadowing_import_symbol_package_ _n(shadowing_import_symbol_package_)
#define shadowing_import_package_ _n(shadowing_import_package_)
#define unexport_package_ _n(unexport_package_)
#define use_package_list_package_ _n(use_package_list_package_)
#define use_package_ _n(use_package_)
#define unuse_package_ _n(unuse_package_)
#define setkeyword_package_ _n(setkeyword_package_)
#define intern_default_package_ _n(intern_default_package_)
#define internchar_ _n(internchar_)
#define internchar_default_ _n(internchar_default_)
#define internchar_null_ _n(internchar_null_)
#define internchar_keyword_ _n(internchar_keyword_)
#define interncommon_ _n(interncommon_)
#define internchar_debug _n(internchar_debug)
#define internchar_keyword_debug _n(internchar_keyword_debug)
#define interncommon_debug _n(interncommon_debug)
#define interncharr_debug _n(interncharr_debug)
#define interncharr_null_debug _n(interncharr_null_debug)
#define interncommonr_debug _n(interncommonr_debug)

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

