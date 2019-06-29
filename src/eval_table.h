#ifndef __EVAL_TABLE_HEADER__
#define __EVAL_TABLE_HEADER__

#include "local.h"
#include "typedef.h"

enum IgnoreType {
	IgnoreType_None = 0,
	IgnoreType_Ignore,
	IgnoreType_Ignorable
};

enum InlineType {
	InlineType_None = 0,
	InlineType_Inline,
	InlineType_NotInline
};


/*
 *  tablevalue
 */
_g void make_tablevalue(LocalRoot local, addr symbol, addr *ret);
_g void copy_tablevalue(LocalRoot local, addr *ret, addr arg);
_g void copylocal_tablevalue(LocalRoot local, addr *ret, addr arg);

_g void getname_tablevalue(addr pos, addr *ret);
_g void setname_tablevalue(addr pos, addr value);
_g void gettype_tablevalue(addr pos, addr *ret);
_g void settype_tablevalue(addr pos, addr value);
_g int getspecialp_tablevalue(addr pos);
_g void setspecialp_tablevalue(addr pos, int value);
_g int getdynamic_tablevalue(addr pos);
_g void setdynamic_tablevalue(addr pos, int value);
_g enum IgnoreType getignore_tablevalue(addr pos);
_g void setignore_tablevalue(addr pos, enum IgnoreType value);
_g int getreference_tablevalue(addr pos);
_g void setreference_tablevalue(addr pos, int value);
_g int getcheck_tablevalue(addr pos);
_g void setcheck_tablevalue(addr pos, int value);


/*
 *  tablefunction
 */
_g void make_tablefunction(LocalRoot local, addr call, addr *ret);
_g void copy_tablefunction(LocalRoot local, addr *ret, addr arg);
_g void copylocal_tablefunction(LocalRoot local, addr *ret, addr arg);

_g void getname_tablefunction(addr pos, addr *ret);
_g void setname_tablefunction(addr pos, addr value);
_g void gettype_tablefunction(addr pos, addr *ret);
_g void settype_tablefunction(addr pos, addr value);
_g int getglobalp_tablefunction(addr pos);
_g void setglobalp_tablefunction(addr pos, int value);
_g int getdynamic_tablefunction(addr pos);
_g void setdynamic_tablefunction(addr pos, int value);
_g int getreference_tablefunction(addr pos);
_g void setreference_tablefunction(addr pos, int value);
_g int getcheck_tablefunction(addr pos);
_g void setcheck_tablefunction(addr pos, int value);
_g enum IgnoreType getignore_tablefunction(addr pos);
_g void setignore_tablefunction(addr pos, enum IgnoreType value);
_g enum InlineType getinline_tablefunction(addr pos);
_g void setinline_tablefunction(addr pos, enum InlineType value);


/*
 *  tablecall
 */
_g void make_tablecall(LocalRoot local, addr *ret);
_g int getcheck_tablecall(addr pos);
_g void setcheck_tablecall(addr pos, int value);
_g void getvalue_tablecall(addr pos, addr *ret);
_g void setvalue_tablecall(addr pos, addr value);
_g void gettype_tablecall(addr pos, addr *ret);
_g void settype_tablecall(addr pos, addr value);


/*
 *  tabletagbody
 */
_g void make_tabletagbody(LocalRoot local, addr *ret, addr tag);
_g void copy_tabletagbody(LocalRoot local, addr *ret, addr arg);

_g void gettag_tabletagbody(addr pos, addr *ret);
_g void settag_tabletagbody(addr pos, addr value);
_g int getreference_tabletagbody(addr pos);
_g void setreference_tabletagbody(addr pos, int value);
_g int equal_tabletagbody(addr left, addr right);

#endif

