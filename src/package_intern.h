#ifndef __PACKAGE_INTERN_HEADER__
#define __PACKAGE_INTERN_HEADER__

#include "execute.h"
#include "package_object.h"
#include "typedef.h"

#define intern_package_table_ _n(intern_package_table_)
#define intern_package_ _n(intern_package_)
#define intern_char_package_ _n(intern_char_package_)
#define unintern_package_ _n(unintern_package_)
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
#define init_package_intern _n(init_package_intern)

int intern_package_table_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
int intern_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
int intern_char_package_(addr package, const char *name,
		addr *value, enum PACKAGE_TYPE *ret);
int unintern_package_(addr package, addr symbol, int *ret);
int setkeyword_package_(addr pos);

int intern_default_package_(Execute ptr, addr name,
		addr *value, enum PACKAGE_TYPE *ret);
int internchar_(const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret);
int internchar_default_(Execute ptr, const char *name,
		addr *value, enum PACKAGE_TYPE *ret);
int internchar_null_(Execute ptr, const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret);
int internchar_keyword_(const char *name, addr *value, enum PACKAGE_TYPE *ret);
int interncommon_(const char *name, addr *value, enum PACKAGE_TYPE *ret);

void internchar_debug(const char *pname, const char *sname, addr *value);
void internchar_keyword_debug(const char *name, addr *value);
void interncommon_debug(const char *name, addr *value);
addr interncharr_debug(const char *pname, const char *sname);
addr interncharr_null_debug(Execute ptr, const char *pname, const char *sname);
addr interncommonr_debug(const char *name);

void init_package_intern(void);

#endif

