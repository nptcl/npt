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
void build_package(void);
void getpackage(Execute ptr, addr *ret);
int packagep(addr pos);
int package_designer_p(addr pos);
int package_designer_equal(addr left, addr right);
void getdocument_package(addr pos, addr *ret);
void setdocument_package(addr pos, addr value);

/* package function */
void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package);
void make_package(addr name, addr names, addr use, addr *ret);
int delete_package(addr pos);
void rename_package(addr pos, addr name, addr names, addr *ret);

void getname_package(addr pos, addr *ret);
void getnickname_package(addr pos, addr *ret);
void getuselist_package(addr pos, addr *ret);
void getusedbylist_package(addr pos, addr *ret);
void getshadow_package(addr pos, addr *ret);

enum PACKAGE_TYPE find_symbol_package(addr package, addr name, addr *ret);
void find_package(addr name, addr *ret);
addr findr_char_package(const char *name);
void find_char_package(const char *name, addr *ret);
void find_allsymbols_package(addr name, addr *ret);
void list_all_packages(addr *ret);

/* symbol control */
enum PACKAGE_TYPE intern_package(addr package, addr name, addr *ret);
enum PACKAGE_TYPE intern_char_package(addr package, const char *name, addr *ret);
int unintern_package(addr package, addr symbol);
void import_package(addr package, addr symbol);
void shadow_package(addr package, addr symbol);
void shadowing_import_package(addr package, addr symbol);
void export_package(addr package, addr symbol);
void unexport_package(addr package, addr symbol);
void use_package(addr package, addr symbol);
void unuse_package(addr package, addr symbol);

void in_package(Execute ptr, addr package, addr *ret);

/* develop function */
enum PACKAGE_TYPE intern_default_package(Execute ptr, addr name, addr *ret);
enum PACKAGE_TYPE internchar(const char *pname, const char *sname, addr *ret);
addr interncharr(const char *pname, const char *sname);
enum PACKAGE_TYPE internchar_default(Execute ptr, const char *sname, addr *ret);
enum PACKAGE_TYPE internchar_check(Execute ptr,
		const char *pname, const char *sname, addr *ret);
addr internchar_checkr(Execute ptr, const char *pname, const char *sname);
void setkeyword_package(addr pos);
enum PACKAGE_TYPE internchar_keyword(const char *sname, addr *ret);
enum PACKAGE_TYPE interncommon(const char *name, addr *ret);
addr interncommonr(const char *name);

int checksymbol_package(addr symbol, addr package);
int checksymbol_default_package(Execute ptr, addr symbol);
void make_gentemp(Execute ptr, addr prefix, addr package, addr *ret);

/* iterator */
void keyword_packagetype(enum PACKAGE_TYPE type, addr *ret);
void package_iterator_alloc(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
void package_iterator_local(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
void package_iterator_heap(addr *ret,
		addr list, int internal, int external, int inherited);
enum PACKAGE_TYPE next_package_iterator(addr pos, addr *rets, addr *retp);

/* syscall */
void syscall_defpackage(Execute ptr, addr rest);
void syscall_do_symbols(Execute ptr, addr call, addr package);
void syscall_do_external_symbols(Execute ptr, addr call, addr package);
void syscall_do_all_symbols(Execute ptr, addr call);

#endif

