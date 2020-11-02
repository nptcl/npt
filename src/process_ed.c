#include "cons.h"
#include "constant.h"
#include "execute.h"
#include "hashtable.h"
#include "process.h"
#include "process_ed.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

static int find_environment_char_(Execute ptr, const char *key, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		return Result(ret, Unbound);
	else
		return find_char_hashtable_(pos, key, ret);
}

static int find_ed_program_(Execute ptr, addr *ret)
{
	addr pos;

	/* *ed-program* */
	GetConst(SYSTEM_ED_PROGRAM, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos != Unbound)
		return Result(ret, pos);

	/* *environment* */
	return find_environment_char_(ptr, "EDITOR", ret);
}

_g int ed_process_(Execute ptr, addr file)
{
	addr call, status, rest;

	Return(find_ed_program_(ptr, &call));
	if (call == Unbound)
		strvect_char_heap(&call, LISP_ED_PROCESS_DEFAULT);

	/* rest */
	GetConst(KEYWORD_SEARCH, &rest);
	list_heap(&rest, rest, T, NULL);

	return run_process_(ptr, call, file, rest, &status);
}

