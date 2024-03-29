#ifndef __PACKAGE_OBJECT_HEADER__
#define __PACKAGE_OBJECT_HEADER__

#include "memory.h"
#include "typedef.h"

#define packagep _n(packagep)
#define getname_package_ _n(getname_package_)
#define getnickname_package_ _n(getnickname_package_)
#define getuselist_package_ _n(getuselist_package_)
#define getusedbylist_package_ _n(getusedbylist_package_)
#define getshadow_package_ _n(getshadow_package_)
#define getdocument_package _n(getdocument_package)
#define setdocument_package _n(setdocument_package)
#define getname_package_unsafe _n(getname_package_unsafe)
#define getnickname_package_unsafe _n(getnickname_package_unsafe)
#define getuselist_package_unsafe _n(getuselist_package_unsafe)
#define getusedbylist_package_unsafe _n(getusedbylist_package_unsafe)
#define getshadow_package_unsafe _n(getshadow_package_unsafe)
#define getexport_package_unsafe _n(getexport_package_unsafe)
#define get_readonly_package _n(get_readonly_package)
#define set_readonly_package _n(set_readonly_package)

#define push_list_nicknames_package _n(push_list_nicknames_package)
#define push_list_use_package _n(push_list_use_package)
#define push_list_used_package _n(push_list_used_package)
#define push_list_export_package_ _n(push_list_export_package_)
#define push_list_shadow_package_ _n(push_list_shadow_package_)
#define delete_list_use_package _n(delete_list_use_package)
#define delete_list_used_package _n(delete_list_used_package)
#define delete_list_export_package_ _n(delete_list_export_package_)
#define delete_list_shadow_package_ _n(delete_list_shadow_package_)

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

int packagep(addr pos);

int getname_package_(addr pos, addr *ret);
int getnickname_package_(addr pos, addr *ret);
int getuselist_package_(addr pos, addr *ret);
int getusedbylist_package_(addr pos, addr *ret);
int getshadow_package_(addr pos, addr *ret);
void getdocument_package(addr pos, addr *ret);
void setdocument_package(addr pos, addr value);

void getname_package_unsafe(addr pos, addr *ret);
void getnickname_package_unsafe(addr pos, addr *ret);
void getuselist_package_unsafe(addr pos, addr *ret);
void getusedbylist_package_unsafe(addr pos, addr *ret);
void getshadow_package_unsafe(addr pos, addr *ret);
void getexport_package_unsafe(addr pos, addr *ret);
int get_readonly_package(addr pos);
void set_readonly_package(addr pos, int value);

/* package list */
void push_list_nicknames_package(addr package, addr pos);
void push_list_use_package(addr package, addr pos);
void push_list_used_package(addr package, addr pos);
int push_list_export_package_(addr package, addr name);
int push_list_shadow_package_(addr package, addr symbol);
void delete_list_use_package(addr package, addr pos);
void delete_list_used_package(addr package, addr pos);
int delete_list_export_package_(addr package, addr name);
int delete_list_shadow_package_(addr package, addr symbol);

#endif

