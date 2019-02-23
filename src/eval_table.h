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
void make_tablevalue(LocalRoot local, addr symbol, addr *ret);
void copy_tablevalue(LocalRoot local, addr *ret, addr arg);
void copylocal_tablevalue(LocalRoot local, addr *ret, addr arg);

void getname_tablevalue(addr pos, addr *ret);
void setname_tablevalue(addr pos, addr value);
void gettype_tablevalue(addr pos, addr *ret);
void settype_tablevalue(addr pos, addr value);
int getspecialp_tablevalue(addr pos);
void setspecialp_tablevalue(addr pos, int value);
int getdynamic_tablevalue(addr pos);
void setdynamic_tablevalue(addr pos, int value);
enum IgnoreType getignore_tablevalue(addr pos);
void setignore_tablevalue(addr pos, enum IgnoreType value);
int getreference_tablevalue(addr pos);
void setreference_tablevalue(addr pos, int value);
int getcheck_tablevalue(addr pos);
void setcheck_tablevalue(addr pos, int value);


/*
 *  tablefunction
 */
void make_tablefunction(LocalRoot local, addr call, addr *ret);
void copy_tablefunction(LocalRoot local, addr *ret, addr arg);
void copylocal_tablefunction(LocalRoot local, addr *ret, addr arg);

void getname_tablefunction(addr pos, addr *ret);
void setname_tablefunction(addr pos, addr value);
void gettype_tablefunction(addr pos, addr *ret);
void settype_tablefunction(addr pos, addr value);
int getglobalp_tablefunction(addr pos);
void setglobalp_tablefunction(addr pos, int value);
int getdynamic_tablefunction(addr pos);
void setdynamic_tablefunction(addr pos, int value);
int getreference_tablefunction(addr pos);
void setreference_tablefunction(addr pos, int value);
int getcheck_tablefunction(addr pos);
void setcheck_tablefunction(addr pos, int value);
enum IgnoreType getignore_tablefunction(addr pos);
void setignore_tablefunction(addr pos, enum IgnoreType value);
enum InlineType getinline_tablefunction(addr pos);
void setinline_tablefunction(addr pos, enum InlineType value);


/*
 *  tablecall
 */
void make_tablecall(LocalRoot local, addr *ret);
void copy_tablecall(LocalRoot local, addr *ret, addr arg);
void copylocal_tablecall(LocalRoot local, addr *ret, addr arg);

int getcheck_tablecall(addr pos);
void setcheck_tablecall(addr pos, int value);
void getvalue_tablecall(addr pos, addr *ret);
void setvalue_tablecall(addr pos, addr value);
void gettype_tablecall(addr pos, addr *ret);
void settype_tablecall(addr pos, addr value);


/*
 *  tabletagbody
 */
void make_tabletagbody(LocalRoot local, addr *ret, addr tag);
void copy_tabletagbody(LocalRoot local, addr *ret, addr arg);

void gettag_tabletagbody(addr pos, addr *ret);
void settag_tabletagbody(addr pos, addr value);
int getreference_tabletagbody(addr pos);
void setreference_tabletagbody(addr pos, int value);
int equal_tabletagbody(addr left, addr right);

#endif

