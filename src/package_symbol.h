#ifndef __PACKAGE_SYMBOL_HEADER__
#define __PACKAGE_SYMBOL_HEADER__

#include "execute.h"
#include "package_object.h"
#include "typedef.h"

_g enum PACKAGE_TYPE intern_package_table(addr package, addr name, addr *ret);
_g enum PACKAGE_TYPE intern_package(addr package, addr name, addr *ret);
_g enum PACKAGE_TYPE intern_char_package(addr package, const char *name, addr *ret);
_g int unintern_package(addr package, addr symbol);
_g void import_package(addr package, addr symbol);
_g void shadow_list_package(addr package, addr pos);
_g void shadow_package(addr package, addr symbol);
_g void shadowing_import_symbol_package(addr package, addr symbol);
_g void shadowing_import_package(addr package, addr symbol);
_g void export_list_package(addr package, addr pos);
_g void export_package(addr package, addr symbol);
_g void unexport_package(addr package, addr symbol);
_g void use_package_list_package(addr package, addr pos);
_g void use_package(addr package, addr symbol);
_g void unuse_package(addr package, addr symbol);

_g enum PACKAGE_TYPE intern_default_package(Execute ptr, addr name, addr *ret);
_g enum PACKAGE_TYPE internchar(const char *pname, const char *sname, addr *ret);
_g addr interncharr(const char *pname, const char *sname);
_g enum PACKAGE_TYPE internchar_default(Execute ptr, const char *sname, addr *ret);
_g enum PACKAGE_TYPE internchar_check(Execute ptr,
		const char *pname, const char *sname, addr *ret);
_g addr internchar_checkr(Execute ptr, const char *pname, const char *sname);
_g void setkeyword_package(addr pos);
_g enum PACKAGE_TYPE internchar_keyword(const char *sname, addr *ret);
_g enum PACKAGE_TYPE interncommon(const char *name, addr *ret);
_g addr interncommonr(const char *name);

#endif

