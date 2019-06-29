#ifndef __PACKAGE_HEADER__
#define __PACKAGE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"

enum PACKAGE_INDEX {
	PACKAGE_INDEX_TABLE = 0,
	PACKAGE_INDEX_NAME,
	PACKAGE_INDEX_NICKNAME,
	PACKAGE_INDEX_USE,
	PACKAGE_INDEX_USED,
	PACKAGE_INDEX_SHADOW,
	PACKAGE_INDEX_EXPORT,
	PACKAGE_INDEX_DOCUMENT,
	PACKAGE_INDEX_SIZE
};

enum PACKAGE_TYPE {
	PACKAGE_TYPE_NIL = 0,
	PACKAGE_TYPE_INTERNAL,
	PACKAGE_TYPE_EXTERNAL,
	PACKAGE_TYPE_INHERITED,
	PACKAGE_TYPE_SIZE
};

#define GetPackage      GetArrayA2
#define SetPackage      SetArrayA2

/* package root */
_g void build_package(void);
_g void getpackage(Execute ptr, addr *ret);
_g int packagep(addr pos);
_g int package_designer_p(addr pos);
_g int package_designer_equal(addr left, addr right);
_g void getdocument_package(addr pos, addr *ret);
_g void setdocument_package(addr pos, addr value);

/* package function */
_g void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package);
_g void make_package(addr name, addr names, addr use, addr *ret);
_g int delete_package(addr pos);
_g void rename_package(addr pos, addr name, addr names, addr *ret);

_g void getname_package(addr pos, addr *ret);
_g void getnickname_package(addr pos, addr *ret);
_g void getuselist_package(addr pos, addr *ret);
_g void getusedbylist_package(addr pos, addr *ret);
_g void getshadow_package(addr pos, addr *ret);

_g enum PACKAGE_TYPE find_symbol_package(addr package, addr name, addr *ret);
_g void find_package(addr name, addr *ret);
_g addr findr_char_package(const char *name);
_g void find_char_package(const char *name, addr *ret);
_g void find_allsymbols_package(addr name, addr *ret);
_g void list_all_packages(addr *ret);

/* symbol control */
_g enum PACKAGE_TYPE intern_package(addr package, addr name, addr *ret);
_g enum PACKAGE_TYPE intern_char_package(addr package, const char *name, addr *ret);
_g int unintern_package(addr package, addr symbol);
_g void import_package(addr package, addr symbol);
_g void shadow_package(addr package, addr symbol);
_g void shadowing_import_package(addr package, addr symbol);
_g void export_package(addr package, addr symbol);
_g void unexport_package(addr package, addr symbol);
_g void use_package(addr package, addr symbol);
_g void unuse_package(addr package, addr symbol);

_g void in_package(Execute ptr, addr package, addr *ret);

/* develop function */
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

_g int checksymbol_package(addr symbol, addr package);
_g int checksymbol_default_package(Execute ptr, addr symbol);
_g void make_gentemp(Execute ptr, addr prefix, addr package, addr *ret);

/* iterator */
_g void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret);
_g void package_iterator_alloc(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
_g void package_iterator_local(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
_g void package_iterator_heap(addr *ret,
		addr list, int internal, int external, int inherited);
_g enum PACKAGE_TYPE next_package_iterator(addr pos, addr *rets, addr *retp);

/* syscall */
_g void syscall_defpackage(Execute ptr, addr rest);
_g void syscall_do_symbols(Execute ptr, addr call, addr package);
_g void syscall_do_external_symbols(Execute ptr, addr call, addr package);
_g void syscall_do_all_symbols(Execute ptr, addr call);

/* initialize */
_g void init_package(void);

#endif

