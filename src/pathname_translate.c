#include "charqueue.h"
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
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  logical-pathname table
 */
_g void table_logical_pathname(addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	GetValueSymbol(symbol, ret);
}

/* found=1, notfound=0 */
_g int gethost_logical_pathname(addr key, addr *ret)
{
	addr list;
	table_logical_pathname(&list);
	return findvalue_hashtable(list, key, ret);
}

_g void sethost_logical_pathname(addr key, addr value)
{
	addr list;

	table_logical_pathname(&list);
	intern_hashheap(list, key, &list);
	SetCdr(list, value);
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

static int wildcard_push_match_pathname(LocalpRoot local, struct wildcard_struct *ptr,
		addr p1, size_t n1, size_t s1, addr p2, size_t n2, size_t s2)
{
	int check;
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2)
		return 1;
	if (n1 == s1 || n2 == s2)
		return 0;
	string_getc(p1, n1, &c1);
	string_getc(p2, n2, &c2);
	/* (a ?) -> next */
	if (c2 == '?') {
		check = wildcard_push_match_pathname(local,ptr,  p1,n1+1,s1,  p2,n2+1,s2);
		if (check)
			wildcard_struct_push(local, ptr, n1, n1+1);
		return check;
	}
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2)
			return 0;
		return wildcard_push_match_pathname(local,ptr,  p1,n1+1,s1,  p2,n2+1,s2);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		if (wildcard_push_match_pathname(local,ptr,  p1,i,s1,  p2,n2,s2)) {
			wildcard_struct_push(local, ptr, n1, i);
			return 1;
		}
	}
	return 0;
}

static int wildcard_push_string_pathname(LocalpRoot local,
		struct wildcard_struct *ptr, addr *ret, addr a, addr b)
{
	size_t s1, s2;

	if (! stringp(a))
		return 0;
	if (! stringp(b))
		return 0;
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_push_match_pathname(local, ptr, a, 0, s1, b, 0, s2);
}

static void push_charqueue_wildcard(LocalpRoot local,
		addr queue, addr pos, struct wildcard_position *ptr)
{
	unicode c;
	size_t i;

	for (i = ptr->a; i < ptr->b; i++) {
		string_getc(pos, i, &c);
		push_charqueue_local(local->local, queue, c);
	}
}

static void wildcard_replace_pathname(LocalpRoot local,
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
		string_getc(to, i, &c);
		if (c == '*' || c == '?') {
			if (child) {
				push_charqueue_wildcard(local, queue, pos, child);
				child = child->next;
			}
		}
		else {
			push_charqueue_local(local->local, queue, c);
		}
	}

	/* position check */
	if (child) {
		clear_charqueue(queue);
		push_charqueue_wildcard(local, queue, pos, child);
		make_charqueue_alloc(localp_alloc(local), queue, &pos);
		fmte("Cannot extract ~S pattern.", pos, NULL);
		return;
	}

	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void translate_string_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	struct wildcard_struct *ptr;
	addr wild, root;
	LocalStack stack;

	/* wildcard */
	GetConst(KEYWORD_WILD, &wild);
	push_localp(local, &stack);
	if (from == wild)
		strvect_char_local(local->local, &from, "*");
	if (to == wild)
		strvect_char_local(local->local, &to, "*");
	ptr = make_wildcard_struct(local);
	if (! wildcard_push_string_pathname(local, ptr, &root, pos, from))
		fmte("The string ~S doesn't match ~S.", pos, from, NULL);

	/* replace */
	wildcard_replace_pathname(local, ptr, ret, pos, to);
	rollback_localp(local, stack);
}

static int translate_list_pathname(LocalpRoot local, addr *ret, addr a, addr b)
{
	int check;
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil)
		return 1;
	if (a == Nil || b == Nil)
		return 0;
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	getcons(a, &pos1, &a1);
	getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild) {
		check = translate_list_pathname(local, ret, a1, b1);
		if (check)
			list_local(local->local, ret, *ret, pos2, pos1, Nil, NULL);
		return check;
	}
	/* ("str" "s*r") -> next, ("str" "a*a") -> false */
	if (wildcard_stringp_p(pos2)) {
		if (! wildcard_string_pathname(pos1, pos2))
			return 0;
		list_local(local->local, ret, *ret, pos2, pos1, Nil, NULL);
		return translate_list_pathname(local, ret, a1, b1);
	}
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		if (! wildcard_eq_pathname(pos1, pos2))
			return 0;
		return translate_list_pathname(local, ret, a1, b1);
	}
	/* ("str" **) */
	a1 = a;
	for (;;) {
		if (translate_list_pathname(local, ret, a, b1)) {
			list_local(local->local, ret, *ret, pos2, a1, a, NULL);
			return 1;
		}
		if (a == Nil)
			break;
		getcdr(a, &a);
	}
	return 0;
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

static void replace_wild_string_pathname(LocalpRoot local,
		addr *root, addr pos, addr from)
{
	addr to;

	strvect_char_local(local->local, &to, "*");
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_wild_pathname(LocalpRoot local, addr *root, addr *list)
{
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		fmte("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1)
		cons_alloc(localp_alloc(local), root, a, *root);
	else if (pos == wild2)
		replace_wild_wilds_pathname(local, root, a, b);
	else if (wildcard_stringp_p(pos))
		replace_wild_string_pathname(local, root, a, pos);
	else
		cons_alloc(localp_alloc(local), root, pos, *root);
	*list = next;
}

static void replace_string_wild_pathname(LocalpRoot local,
		addr *root, addr pos, addr to)
{
	addr from;

	strvect_char_local(local->local, &from, "*");
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_string_string_pathname(LocalpRoot local,
		addr *root, addr pos, addr from, addr to)
{
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_string_pathname(LocalpRoot local, addr *root, addr *list, addr to)
{
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		fmte("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1 || pos == wild2)
		replace_string_wild_pathname(local, root, a, to);
	else if (wildcard_stringp_p(pos))
		replace_string_string_pathname(local, root, a, pos, to);
	else
		cons_alloc(localp_alloc(local), root, pos, *root);
	*list = next;
}

static void translate_replace_pathname(LocalpRoot local,
		addr *root, addr *list, addr to)
{
	LocalRoot alloc;
	addr pos, wild1, wild2;

	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	alloc = localp_alloc(local);
	while (to != Nil) {
		getcons(to, &pos, &to);
		if (pos == wild1 || pos == wild2)
			replace_wild_pathname(local, root, list);
		else if (wildcard_stringp_p(pos))
			replace_string_pathname(local, root, list, pos);
		else
			cons_alloc(alloc, root, pos, *root);
	}
	nreverse(root, *root);
}

static void translate_directory_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	addr list;
	LocalStack stack;

	push_localp(local, &stack);
	list = Nil;
	if (! translate_list_pathname(local, &list, pos, from))
		fmte("Cannot translate ~S to ~S.", from, pos, NULL);
	*ret = Nil;
	translate_replace_pathname(local, ret, &list, to);
	rollback_localp(local, stack);
}

static void translate_nil_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (from == Nil)
		from = wild;
	if (to == Nil)
		to = wild;
	translate_string_pathname(local, ret, pos, from, to);
}

static void translate_version_pathname(addr *ret, addr pos, addr from, addr to)
{
	addr wild, unspec;

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (from == Nil)
		from = wild;
	if (to == Nil)
		to = wild;
	/* :unspecific */
	if (to == unspec) {
		*ret = unspec;
		return;
	}

	/* value */
	if (from == wild) {
		if (to == wild) {
			*ret = pos;
			return;
		}
		fmte(":VERSION from-wildcard is *, but to-wildcard ~S is not *.", to, NULL);
	}
	if (! eql_function(pos, to))
		fmte(":VERSION source ~S don't match to-wildcard ~S.", pos, to, NULL);

	/* :unspecific */
	*ret = unspec;
}

static void translate_pathname_localp(Execute ptr, LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	int localp;
	LocalRoot alloc;
	addr one, a, b, c;

	alloc = localp_alloc(local);
	localp = local->localp;
	/* argument */
	pathname_designer_alloc(ptr, pos, &pos, localp);
	pathname_designer_alloc(ptr, from, &from, localp);
	pathname_designer_alloc(ptr, to, &to, localp);
	/* one */
	make_pathname_alloc(alloc, &one, RefLogicalPathname(to));
	/* host */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_HOST, one);
	/* device */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_DEVICE, one);
	/* directory */
	GetDirectoryPathname(pos, &a);
	GetDirectoryPathname(from, &b);
	GetDirectoryPathname(to, &c);
	translate_directory_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetDirectoryPathname(one, a);
	/* name */
	GetNamePathname(pos, &a);
	GetNamePathname(from, &b);
	GetNamePathname(to, &c);
	translate_nil_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetNamePathname(one, a);
	/* type */
	GetTypePathname(pos, &a);
	GetTypePathname(from, &b);
	GetTypePathname(to, &c);
	translate_nil_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetTypePathname(one, a);
	/* version */
	GetVersionPathname(pos, &a);
	GetVersionPathname(from, &b);
	GetVersionPathname(to, &c);
	translate_version_pathname(&a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetVersionPathname(one, a);
	/* result */
	*ret = one;
}

_g void translate_pathname_alloc(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp)
{
	struct localp_struct buffer;

	buffer.localp = localp;
	buffer.local = ptr->local;
	translate_pathname_localp(ptr, &buffer, ret, pos, from, to);
}

_g void translate_pathname_heap(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	translate_pathname_alloc(ptr, ret, pos, from, to, 0);
}


/*
 *  build
 */
_g void build_pathname_translate(void)
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

