#include "build.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "heap.h"
#include "local.h"
#include "readlite.h"
#include "package.h"


/*
 *  readlite  (for developer)
 */
#define READLITE_SIZE 128

#define readlite_write(var, index, c) { \
	size_t __index = index; \
	if (READLITE_SIZE - 1 <= __index) goto error; \
	var[__index] = c; \
}

static void readlite_intern(Execute ptr,
		const char *pstr, const char *str, addr *ret)
{
	char package[READLITE_SIZE];
	char name[READLITE_SIZE];
	int c;
	size_t i, n;

#ifdef LISP_DEBUG
	memset(package, 0xAA, READLITE_SIZE);
	memset(name, 0xAA, READLITE_SIZE);
#endif

	i = n = 0;
	c = str[i++];
	if (c == 0) goto error;
	if (c == ':') goto colon1;
	readlite_write(package, n++, c);

package_loop:
	c = str[i++];
	if (c == ':') goto colon1;
	if (c == 0) goto name_only;
	readlite_write(package, n++, c);
	goto package_loop;

colon1:
	readlite_write(package, n++, 0);
	c = str[i++];
	if (c == ':') goto colon2;
	if (c == 0) goto error;
	n = 0;
	readlite_write(name, n++, c);
	goto name_loop;

colon2:
	c = str[i++];
	if (c == ':') goto error;
	if (c == 0) goto error;
	n = 0;
	readlite_write(name, n++, c);

name_loop:
	c = str[i++];
	if (c == 0) goto package_name;
	readlite_write(name, n++, c);
	goto name_loop;

name_only:
	package[n] = 0;
	internchar_check(ptr, pstr, package, ret);
	return;

package_name:
	name[n] = 0;
	if (package[0] == 0)
		internchar_keyword(name, ret);
	else
		internchar(package, name, ret);
	return;

error:
	fmte("Invalid symbol name.", NULL);
}

static int readlite_group(LocalRoot local,
		addr *ret, const char *package, const char *str, int *index)
{
	char name[READLITE_SIZE];
	int c, i, k, check;
	addr pos, cons;

	/* space */
	for (i = *index; ; i++) {
		c = str[i];
		if (c == 0)
			fmte("string is empty.", NULL);
		if (c == ')') {
			*index = i + 1;
			return 1;
		}
		if (! isspaceunicode((unicode)c)) break;
	}

	/* list */
	if (c == '(') {
		i++;
		cons = Nil;
		for (;;) {
			check = readlite_group(local, &pos, package, str, &i);
			if (check < 0) fmte("abort.", NULL);
			if (check) break;
			cons_alloc(local, &cons, pos, cons);
		}
		nreverse_list_unsafe(ret, cons);
		*index = i;
		return 0;
	}

	/* name */
	for (k = 0; ; i++, k++) {
		c = str[i];
		if (c == 0 || c == ')' || isspaceunicode((unicode)c)) break;
		name[k] = toupperunicode((unicode)c);
	}
	if ((READLITE_SIZE - 1) <= k)
		fmte("The token size is overflow.", NULL);
	if (k == 0) {
		*ret = Nil;
	}
	else {
		name[k] = 0;
		readlite_intern(Execute_Thread, package, name, ret);
	}
	*index = i;

	return 0;
}

void readlite_package_alloc(LocalRoot local, addr *ret,
		const char *package, const char *str)
{
	int index, check;
	addr pos;

	/* parse */
	index = 0;
	if (readlite_group(local, &pos, package, str, &index) != 0)
		fmte("readlite_package_alloc error.", NULL);

	/* tail check */
	for (;; index++) {
		check = str[index];
		if (check == 0) break;
		if (! isspaceunicode((unicode)check))
			fmte("The tail of lisp-form must be a white space.", NULL);
	}
	*ret = pos;
}

void readlite_package_local(LocalRoot local, addr *ret,
		const char *package, const char *str)
{
	Check(local == NULL, "local error");
	readlite_package_alloc(local, ret, package, str);
}

void readlite_package_heap(addr *ret, const char *package, const char *str)
{
	readlite_package_alloc(NULL, ret, package, str);
}

void readlite_alloc(LocalRoot local, addr *ret, const char *str)
{
	readlite_package_alloc(local, ret, NULL, str);
}

void readlite_local(LocalRoot local, addr *ret, const char *str)
{
	readlite_package_local(local, ret, NULL, str);
}

void readlite_heap(addr *ret, const char *str)
{
	readlite_package_heap(ret, NULL, str);
}

