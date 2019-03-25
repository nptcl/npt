#include "build.h"
#include "execute.h"
#include "function.h"
#include "object.h"
#include "package.h"
#include "symbol.h"

/*
 *  user
 */
static void hello_user(Execute ptr, addr right)
{
	info("Hello user.");
}


/*
 *  intern_user
 */
static void defuser(const char *str, calltype call)
{
	addr symbol, system;

	internchar(LISP_USER, str, &symbol);
	GetFunctionSymbol(symbol, &system);
	Check(system != Unbound, "User call already exists.");
	compiled_heap(&system, symbol);
	setcompiled_rest(system, call);
	SetFunctionSymbol(symbol, system);
}

void build_user(void)
{
	defuser("HELLO", hello_user);
}

