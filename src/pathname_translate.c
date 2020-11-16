#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "hashtable.h"
#include "integer.h"
#include "local.h"
#include "pathname.h"
#include "pathname_localp.h"
#include "pathname_object.h"
#include "pathname_translate.h"
#include "pathname_wildcard.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  logical-pathname table
 */
void table_logical_pathname(addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	GetValueSymbol(symbol, ret);
}

int gethost_logical_pathname_(addr key, addr *ret)
{
	addr list;
	table_logical_pathname(&list);
	return findnil_hashtable_(list, key, ret);
}

int sethost_logical_pathname_(addr key, addr value)
{
	addr list;

	table_logical_pathname(&list);
	Return(intern_hashheap_(list, key, &list));
	SetCdr(list, value);

	return 0;
}


/*
 *  translate-pathname
 */
/* struct */
struct wildcard_position {
	struct wildcard_position *next;
	size_t a, b;
};

struct wildcard_struct {
	struct wildcard_position *root, *tail;
};

static struct wildcard_position *make_wildcard_position(LocalpRoot local,
		size_t a, size_t b)
{
	struct wildcard_position *ptr;

	ptr = (struct wildcard_position *)lowlevel_local(local->local,
			sizeoft(struct wildcard_position));
	ptr->next = NULL;
	ptr->a = a;
	ptr->b = b;

	return ptr;
}

static struct wildcard_struct *make_wildcard_struct(LocalpRoot local)
{
	struct wildcard_struct *ptr;

	ptr = (struct wildcard_struct *)lowlevel_local(local->local,
			sizeoft(struct wildcard_struct));
	ptr->root = ptr->tail = NULL;

	return ptr;
}

static void wildcard_struct_push(LocalpRoot local,
		struct wildcard_struct *ptr, size_t a, size_t b)
{
	struct wildcard_position *pos;

	pos = make_wildcard_position(local, a, b);
	if (ptr->root == NULL) {
		ptr->root = ptr->tail = pos;
	}
	else {
		pos->next = ptr->tail;
		ptr->tail = pos;
	}
}

static int wildcard_push_match_pathname_(
		LocalpRoot local, struct wildcard_struct *ptr,
		addr p1, size_t n1, size_t s1, addr p2, size_t n2, size_t s2,
		int *ret)
{
	int x;
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2)
		return Result(ret, 1);
	if (n1 == s1 || n2 == s2)
		return Result(ret, 0);
	Return(string_getc_(p1, n1, &c1));
	Return(string_getc_(p2, n2, &c2));
	/* (a ?) -> next */
	if (c2 == '?') {
		Return(wildcard_push_match_pathname_(local,ptr, p1,n1+1,s1,  p2,n2+1,s2, &x));
		if (x)
			wildcard_struct_push(local, ptr, n1, n1+1);
		return Result(ret, x);
	}
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (toUpperUnicode(c1) != toUpperUnicode(c2))
			return Result(ret, 0);
		return wildcard_push_match_pathname_(local,ptr,  p1,n1+1,s1,  p2,n2+1,s2, ret);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		Return(wildcard_push_match_pathname_(local,ptr, p1,i,s1,  p2,n2,s2, &x));
		if (x) {
			wildcard_struct_push(local, ptr, n1, i);
			return Result(ret, 1);
		}
	}
	return Result(ret, 0);
}

static int wildcard_push_string_pathname_(LocalpRoot local,
		struct wildcard_struct *ptr, addr a, addr b, int *ret)
{
	size_t s1, s2;

	if (! stringp(a))
		return Result(ret, 0);
	if (! stringp(b))
		return Result(ret, 0);
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_push_match_pathname_(local, ptr, a, 0, s1, b, 0, s2, ret);
}

static int push_charqueue_wildcard_(LocalpRoot local,
		addr queue, addr pos, struct wildcard_position *ptr)
{
	unicode c;
	size_t i;

	for (i = ptr->a; i < ptr->b; i++) {
		Return(string_getc_(pos, i, &c));
		Return(push_charqueue_local_(local->local, queue, c));
	}

	return 0;
}

static int wildcard_replace_pathname_(LocalpRoot local,
		struct wildcard_struct *ptr, addr *ret, addr pos, addr to)
{
	unicode c;
	struct wildcard_position *child;
	addr queue;
	size_t size, i;

	charqueue_local(local->local, &queue, 0);
	string_length(to, &size);
	child = ptr->tail;
	for (i = 0; i < size; i++) {
		Return(string_getc_(to, i, &c));
		if (c == '*' || c == '?') {
			if (child) {
				Return(push_charqueue_wildcard_(local, queue, pos, child));
				child = child->next;
			}
		}
		else {
			Return(push_charqueue_local_(local->local, queue, c));
		}
	}

	/* position check */
	if (child) {
		clear_charqueue(queue);
		Return(push_charqueue_wildcard_(local, queue, pos, child));
		make_charqueue_alloc(localp_alloc(local), queue, &pos);
		return fmte_("Cannot extract ~S pattern.", pos, NULL);
	}

	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int translate_string_pathname_(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	int check;
	struct wildcard_struct *ptr;
	addr wild;
	LocalStack stack;

	/* wildcard */
	GetConst(KEYWORD_WILD, &wild);
	push_localp(local, &stack);
	if (from == wild)
		strvect_char_local(local->local, &from, "*");
	if (to == wild)
		strvect_char_local(local->local, &to, "*");
	ptr = make_wildcard_struct(local);
	Return(wildcard_push_string_pathname_(local, ptr, pos, from, &check));
	if (! check)
		return fmte_("The string ~S doesn't match ~S.", pos, from, NULL);

	/* replace */
	Return(wildcard_replace_pathname_(local, ptr, ret, pos, to));
	rollback_localp(local, stack);

	return 0;
}

static int translate_list_pathname_(LocalpRoot local,
		addr *value, addr a, addr b,
		lisp_equal_calltype equal, int *ret)
{
	int check;
	addr a1, b1, pos1, pos2, wild, wilds;


	if (a == Nil && b == Nil)
		return Result(ret, 1);
	if (a == Nil || b == Nil)
		return Result(ret, 0);

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	Return_getcons(a, &pos1, &a1);
	Return_getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild) {
		Return(translate_list_pathname_(local, value, a1, b1, equal, &check));
		if (check)
			list_local(local->local, value, *value, pos2, pos1, Nil, NULL);
		return Result(ret, check);
	}
	/* ("str" "s*r") -> next, ("str" "a*a") -> false */
	Return(wildcard_stringp_p_(pos2, &check));
	if (check) {
		Return(wildcard_string_pathname_(pos1, pos2, equal, &check));
		if (! check)
			return Result(ret, 0);
		list_local(local->local, value, *value, pos2, pos1, Nil, NULL);
		return translate_list_pathname_(local, value, a1, b1, equal, ret);
	}
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		Return(wildcard_eq_pathname_(pos1, pos2, equal, &check));
		if (! check)
			return Result(ret, 0);
		return translate_list_pathname_(local, value, a1, b1, equal, ret);
	}
	/* ("str" **) */
	a1 = a;
	for (;;) {
		Return(translate_list_pathname_(local, value, a, b1, equal, &check));
		if (check) {
			list_local(local->local, value, *value, pos2, a1, a, NULL);
			return Result(ret, 1);
		}
		if (a == Nil)
			break;
		Return_getcdr(a, &a);
	}

	return Result(ret, 0);
}

static void replace_wild_wilds_pathname(LocalpRoot local, addr *root, addr a, addr b)
{
	addr pos;
	LocalRoot alloc;

	alloc = localp_alloc(local);
	while (a != b) {
		GetCons(a, &pos, &a);
		cons_alloc(alloc, root, pos, *root);
	}
}

static int replace_wild_string_pathname_(LocalpRoot local,
		addr *root, addr pos, addr from)
{
	addr to;

	strvect_char_local(local->local, &to, "*");
	Return(translate_string_pathname_(local, &pos, pos, from, to));
	cons_alloc(localp_alloc(local), root, pos, *root);

	return 0;
}

static int replace_wild_pathname_(LocalpRoot local, addr *root, addr *list)
{
	int check;
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1) {
		cons_alloc(localp_alloc(local), root, a, *root);
		goto final;
	}
	if (pos == wild2) {
		replace_wild_wilds_pathname(local, root, a, b);
		goto final;
	}
	Return(wildcard_stringp_p_(pos, &check));
	if (check) {
		Return(replace_wild_string_pathname_(local, root, a, pos));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, pos, *root);
	goto final;

final:
	return Result(list, next);
}

static int replace_string_wild_pathname_(LocalpRoot local,
		addr *root, addr pos, addr to)
{
	addr from;

	strvect_char_local(local->local, &from, "*");
	Return(translate_string_pathname_(local, &pos, pos, from, to));
	cons_alloc(localp_alloc(local), root, pos, *root);

	return 0;
}

static int replace_string_string_pathname_(LocalpRoot local,
		addr *root, addr pos, addr from, addr to)
{
	Return(translate_string_pathname_(local, &pos, pos, from, to));
	cons_alloc(localp_alloc(local), root, pos, *root);
	return 0;
}

static int replace_string_pathname_(LocalpRoot local, addr *root, addr *list, addr to)
{
	int check;
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1 || pos == wild2) {
		Return(replace_string_wild_pathname_(local, root, a, to));
		goto final;
	}
	Return(wildcard_stringp_p_(pos, &check));
	if (check) {
		Return(replace_string_string_pathname_(local, root, a, pos, to));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, pos, *root);
	goto final;

final:
	return Result(list, next);
}

static int translate_replace_pathname_(LocalpRoot local,
		addr *root, addr *list, addr to)
{
	int check;
	LocalRoot alloc;
	addr pos, wild1, wild2;

	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	alloc = localp_alloc(local);
	while (to != Nil) {
		Return_getcons(to, &pos, &to);
		if (pos == wild1 || pos == wild2) {
			Return(replace_wild_pathname_(local, root, list));
			continue;
		}
		Return(wildcard_stringp_p_(pos, &check));
		if (check) {
			Return(replace_string_pathname_(local, root, list, pos));
			continue;
		}
		cons_alloc(alloc, root, pos, *root);
	}
	nreverse(root, *root);

	return 0;
}

static int translate_directory_pathname_(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to,
		lisp_equal_calltype equal)
{
	int check;
	addr list;
	LocalStack stack;

	push_localp(local, &stack);
	list = Nil;
	Return(translate_list_pathname_(local, &list, pos, from, equal, &check));
	if (! check)
		return fmte_("Cannot translate ~S to ~S.", from, pos, NULL);
	*ret = Nil;
	Return(translate_replace_pathname_(local, ret, &list, to));
	rollback_localp(local, stack);

	return 0;
}

static int translate_nil_pathname_(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (from == Nil)
		from = wild;
	if (to == Nil)
		to = wild;
	return translate_string_pathname_(local, ret, pos, from, to);
}

static int translate_version_pathname_(addr *ret, addr pos, addr from, addr to)
{
	addr wild, unspec;

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (from == Nil)
		from = wild;
	if (to == Nil)
		to = wild;
	/* :unspecific */
	if (to == unspec)
		return Result(ret, unspec);

	/* value */
	if (from == wild) {
		if (to == wild)
			return Result(ret, pos);
		return fmte_(":VERSION from-wildcard is *, but to-wildcard ~S "
				"is not *.", to, NULL);
	}
	if (! eql_function(pos, to))
		return fmte_(":VERSION source ~S don't match to-wildcard ~S.", pos, to, NULL);

	/* :unspecific */
	return Result(ret, unspec);
}

static void getdirectory_translate_pathname(addr pos, addr *ret)
{
	GetDirectoryPathname(pos, &pos);
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	/* (:relative) */
	GetConst(KEYWORD_RELATIVE, &pos);
	conscar_heap(ret, pos);
}

static int setdirectory_translate_relative_p(addr list)
{
	addr x, y;

	/* (:relative) */
	if (! consp_getcons(list, &x, &list))
		return 0;
	GetConst(KEYWORD_RELATIVE, &y);
	return (x == y) && (list == Nil);
}

static void setdirectory_translate_pathname(LocalRoot local, addr pos, addr value)
{
	if (setdirectory_translate_relative_p(value)) {
		SetDirectoryPathname(pos, Nil);
	}
	else {
		copylocal_object(local, &value, value);
		SetDirectoryPathname(pos, value);
	}
}

static int translate_pathname_localp_(Execute ptr, LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	int localp;
	LocalRoot alloc;
	addr one, a, b, c;
	lisp_equal_calltype equal;

	alloc = localp_alloc(local);
	localp = local->localp;
	/* argument */
	Return(pathname_designer_alloc_(ptr, pos, &pos, localp));
	Return(pathname_designer_alloc_(ptr, from, &from, localp));
	Return(pathname_designer_alloc_(ptr, to, &to, localp));
	/* one */
	make_pathname_alloc(alloc, &one, RefLogicalPathname(to));
	/* host */
	equal = pathname_equal_function(pos);
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_HOST, one);
	/* device */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_DEVICE, one);
	/* directory */
	getdirectory_translate_pathname(pos, &a);
	getdirectory_translate_pathname(from, &b);
	getdirectory_translate_pathname(to, &c);
	Return(translate_directory_pathname_(local, &a, a, b, c, equal));
	setdirectory_translate_pathname(alloc, one, a);
	/* name */
	GetNamePathname(pos, &a);
	GetNamePathname(from, &b);
	GetNamePathname(to, &c);
	Return(translate_nil_pathname_(local, &a, a, b, c));
	copylocal_object(alloc, &a, a);
	SetNamePathname(one, a);
	/* type */
	GetTypePathname(pos, &a);
	GetTypePathname(from, &b);
	GetTypePathname(to, &c);
	Return(translate_nil_pathname_(local, &a, a, b, c));
	copylocal_object(alloc, &a, a);
	SetTypePathname(one, a);
	/* version */
	GetVersionPathname(pos, &a);
	GetVersionPathname(from, &b);
	GetVersionPathname(to, &c);
	Return(translate_version_pathname_(&a, a, b, c));
	copylocal_object(alloc, &a, a);
	SetVersionPathname(one, a);
	/* result */
	return Result(ret, one);
}

int translate_pathname_alloc_(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp)
{
	struct localp_struct buffer;

	buffer.localp = localp;
	buffer.local = ptr->local;
	return translate_pathname_localp_(ptr, &buffer, ret, pos, from, to);
}

int translate_pathname_heap_(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	return translate_pathname_alloc_(ptr, ret, pos, from, to, 0);
}


/*
 *  build
 */
void build_pathname_translate(void)
{
	addr value, symbol;

	/* logical-pathname */
	hashtable_heap(&value);
	settest_hashtable(value, HASHTABLE_TEST_EQUALP);
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	SetValueSymbol(symbol, value);

	/* universal time */
	value = intsizeh((70ULL*365 + 17ULL) * 24 * 60 * 60);
	GetConst(SYSTEM_TIME1970, &symbol);
	SetValueSymbol(symbol, value);
}

