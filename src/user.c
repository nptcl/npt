#include "build.h"
#include "control.h"
#include "execute.h"
#include "function.h"
#include "format.h"
#include "heap.h"
#include "object.h"
#include "package.h"
#include "pointer.h"
#include "symbol.h"

/*****************************************************************************
 *  Initialize
 *****************************************************************************/
#define SetUserObject(x, v) (*(callbind_dynamic *)PtrBodyB2(x) = (v))
#define GetUserObject(x, r) (*(r) = *(callbind_dynamic *)PtrBodyB2(x))
static void make_user_object(addr *ret, callbind_dynamic call)
{
	addr pos;

	heap_body2(&pos, LISPSYSTEM_USER, sizeoft(callbind_dynamic));
	SetUserObject(pos, call);
	*ret = pos;
}

static void defuser(const char *str, callbind_dynamic call)
{
	addr symbol, pos, user;

	internchar(LISP_USER, str, &symbol);
	GetFunctionSymbol(symbol, &pos);
	if (pos == Unbound) {
		compiled_heap(&pos, symbol);
		make_user_object(&user, call);
		SetDataFunction(pos, user);
	}
	else {
		GetDataFunction(pos, &user);
		SetUserObject(user, call);
	}
	setcompiled_dynamic(pos, p_user);
	SetFunctionSymbol(symbol, pos);
}

static void init_user_call(Execute ptr, addr rest)
{
	addr user;
	callbind_dynamic call;

	getdata_control(ptr, &user);
	CheckType(user, LISPSYSTEM_USER);
	GetUserObject(user, &call);
	(*call)(ptr, rest);
}

_g void init_user(void)
{
	SetPointer_dynamic(p_user, init_user_call);
}


/*****************************************************************************
 *  User Function
 *****************************************************************************/
static void hello_user(Execute ptr, addr right)
{
	format_stdout(ptr, "~&Hello user.~%", NULL);
	setresult_control(ptr, Nil);
}


_g void build_user(void)
{
	defuser("HELLO", hello_user);
}

