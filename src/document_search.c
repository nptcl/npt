#include "build.h"
#include "document_contents.h"
#include "document_search.h"
#include "package_object.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"
#include "unicode.h"

static int document_search_equal_char_(addr pos, const char *str, int *ret)
{
	unicode x, y;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &x));
		y = (unicode)str[i];
		if (y == 0)
			return Result(ret, 0);
		if (x != y)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int document_search_compare_char_(addr left, const char *right, int *ret)
{
	unicode x, y;
	size_t size, i;

	string_length(left, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(left, i, &x));
		y = (unicode)right[i];
		if (y == 0)
			return Result(ret, 1);
		if (x < y)
			return Result(ret, -1);
		if (x > y)
			return Result(ret, 1);
	}

	return Result(ret, right[i]? -1: 0);
}

static int document_search_result_(struct DocumentStruct *a, addr *ret)
{
	return string8_null_heap_(ret, a->value);
}

static int document_search_range_(
		addr key, size_t ai, size_t bi,
		struct DocumentStruct *root,
		addr *ret)
{
	int check;
	struct DocumentStruct *a, *b, *c;
	size_t ci;

	ci = (ai + bi) / 2UL;
	a = root + ai;
	b = root + bi;
	c = root + ci;
	/* a */
	Return(document_search_equal_char_(key, a->key, &check));
	if (check)
		return document_search_result_(a, ret);
	/* b */
	Return(document_search_equal_char_(key, b->key, &check));
	if (check)
		return document_search_result_(b, ret);
	/* range */
	if (bi <= ai)
		return Result(ret, Nil);
	/* compare */
	Return(document_search_compare_char_(key, c->key, &check));
	if (check < 0)
		return document_search_range_(key, ai + 1UL, ci, root, ret);
	else
		return document_search_range_(key, ci, bi - 1UL, root, ret);
}

static int document_search_symbol_(struct DocumentPackage *str, addr name, addr *ret)
{
	struct DocumentStruct *root;
	size_t x, y;

	x = 0;
	y = str->size;
	root = str->list;
	if (y == 0)
		return Result(ret, Nil);
	else
		return document_search_range_(name, x, y - 1UL, root, ret);
}

static int document_search_package_(addr pos,
		struct DocumentPackage *root,
		struct DocumentPackage **ret)
{
	int check;
	struct DocumentPackage *str;
	size_t i;

	Return(getname_package_(pos, &pos));
	for (i = 0; ; i++) {
		str = root + i;
		if (str->package == NULL)
			break;
		Return(document_search_equal_char_(pos, str->package, &check));
		if (check)
			return Result(ret, str);
	}

	return Result(ret, NULL);
}

static int document_search_(addr pos, addr *ret, struct DocumentPackage *root)
{
	addr pg;
	struct DocumentPackage *str;

	/* symbol, package */
	if (! symbolp(pos))
		return Result(ret, Nil);
	GetPackageSymbol(pos, &pg);
	GetNameSymbol(pos, &pos);
	if (pg == Nil)
		return Result(ret, Nil);

	Return(document_search_package_(pg, root, &str));
	if (str == NULL)
		return Result(ret, Nil);

	return document_search_symbol_(str, pos, ret);
}

int document_function_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_FUNCTION);
}

int document_variable_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_VARIABLE);
}

int document_type_(addr pos, addr *ret)
{
	return document_search_(pos, ret, Document_TYPE);
}

