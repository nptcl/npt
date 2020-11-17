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

struct translate_struct {
	unsigned ignore_case_p : 1;
	struct wildcard_position *root, *tail;
	addr pos, var, from, to, ret;
	size_t s1, n1, s2, n2;
};

typedef struct translate_struct *TranslateInfo;

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


/*
 *  name, type
 */
static int translate_name_match_(LocalpRoot local, TranslateInfo ptr, int *ret);

#define TranslateCompareCharacter(p, x, y) \
	((p)->ignore_case_p? \
	 (toUpperUnicode(x) == toUpperUnicode(y)): \
	 ((x) == (y)))

static void translate_name_match_push(LocalpRoot local,
		struct translate_struct *ptr, size_t a, size_t b)
{
	struct wildcard_position *str;

	str = make_wildcard_position(local, a, b);
	if (ptr->root == NULL) {
		ptr->root = ptr->tail = str;
	}
	else {
		str->next = ptr->tail;
		ptr->tail = str;
	}
}

static int translate_name_match_diff_(TranslateInfo ptr, int *ret)
{
	unicode c;
	addr p2;
	size_t i, n2, s2;

	p2 = ptr->from;
	n2 = ptr->n2;
	s2 = ptr->s2;
	for (i = n2; i < s2; i++) {
		Return(string_getc_(p2, n2, &c));
		if (c != '*')
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int translate_name_match1_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	int check;
	struct translate_struct str;
	size_t n1;

	n1 = ptr->n1;
	str = *ptr;
	str.n1++;
	str.n2++;
	Return(translate_name_match_(local, &str, &check));
	ptr->root = str.root;
	ptr->tail = str.tail;
	if (check)
		translate_name_match_push(local, ptr, n1, n1+1UL);

	return Result(ret, check);
}

static int translate_name_match2_(LocalpRoot local,
		TranslateInfo ptr, unicode x, int *ret)
{
	unicode y;
	struct translate_struct str;

	Return(string_getc_(ptr->var, ptr->n1, &y));
	if (! TranslateCompareCharacter(ptr, x, y))
		return Result(ret, 0);

	str = *ptr;
	str.n1++;
	str.n2++;
	Return(translate_name_match_(local, &str, ret));
	ptr->root = str.root;
	ptr->tail = str.tail;

	return 0;
}

static int translate_name_match3_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	int check;
	size_t n1, s1, i;
	struct translate_struct str;

	n1 = ptr->n1;
	s1 = ptr->s1;
	str = *ptr;
	str.n2++;
	for (i = n1; i <= s1; i++) {
		str.n1 = i;
		Return(translate_name_match_(local, &str, &check));
		ptr->root = str.root;
		ptr->tail = str.tail;
		if (check) {
			translate_name_match_push(local, ptr, n1, i);
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int translate_name_match_(LocalpRoot local, TranslateInfo ptr, int *ret)
{
	unicode x;

	/* match */
	if (ptr->n1 == ptr->s1 && ptr->n2 == ptr->s2)
		return Result(ret, 1);

	/* unmatch */
	if (ptr->n2 == ptr->s2)
		return Result(ret, 0);

	/* check */
	if (ptr->n1 == ptr->s1)
		return translate_name_match_diff_(ptr, ret);

	/* (a ?) -> next */
	Return(string_getc_(ptr->from, ptr->n2, &x));
	if (x == '?')
		return translate_name_match1_(local, ptr, ret);

	/* (a a) -> next, (a b) -> false */
	if (x != '*')
		return translate_name_match2_(local, ptr, x, ret);

	/* (a *) */
	return translate_name_match3_(local, ptr, ret);
}

static int translate_name_push_wild_(LocalpRoot local,
		addr queue, addr var, struct wildcard_position *ptr)
{
	unicode c;
	size_t i;

	for (i = ptr->a; i < ptr->b; i++) {
		Return(string_getc_(var, i, &c));
		Return(push_charqueue_local_(local->local, queue, c));
	}

	return 0;
}

static int translate_name_wild_(LocalpRoot local, TranslateInfo ptr)
{
	unicode c;
	struct wildcard_position *str;
	addr var, to, queue;
	size_t size, i;

	var = ptr->var;
	to = ptr->to;
	charqueue_local(local->local, &queue, 0);
	string_length(to, &size);
	str = ptr->tail;
	for (i = 0; i < size; i++) {
		Return(string_getc_(to, i, &c));
		if (c == '*' || c == '?') {
			if (str) {
				Return(translate_name_push_wild_(local, queue, var, str));
				str = str->next;
			}
		}
		else {
			Return(push_charqueue_local_(local->local, queue, c));
		}
	}

	/* position check */
	if (str) {
		clear_charqueue(queue);
		Return(translate_name_push_wild_(local, queue, var, str));
		make_charqueue_alloc(localp_alloc(local), queue, &var);
		return fmte_("Cannot extract ~S pattern.", var, NULL);
	}

	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, &ptr->ret);

	return 0;
}

static void translate_name_type(LocalRoot local, addr *var, addr *from, addr *to)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	/* var */
	if (*var == Nil) {
		strvect_local(local, var, 0);
	}
	/* from */
	if (*from == Nil ||*from == wild) {
		strvect_char_local(local, from, "*");
	}
	/* to */
	if (*to == Nil || *to == wild) {
		strvect_char_local(local, to, "*");
	}
}

static void translate_name_struct(TranslateInfo ptr,
		addr pos, addr var, addr from, addr to)
{
	Check(! stringp(var), "type error");
	Check(! stringp(from), "type error");
	Check(! stringp(to), "type error");

	clearpoint(ptr);
	ptr->ignore_case_p = pathname_ignore_case_p(pos);
	ptr->pos = pos;
	ptr->var = var;
	ptr->from = from;
	ptr->to = to;
	string_length(var, &ptr->s1);
	string_length(from, &ptr->s2);
}

static int translate_name_(LocalpRoot local, addr *ret,
		addr pos, addr var, addr from, addr to)
{
	int check;
	LocalStack stack;
	struct translate_struct str;

	push_localp(local, &stack);
	translate_name_type(local->local, &var, &from, &to);
	translate_name_struct(&str, pos, var, from, to);

	/* wildcard */
	Return(translate_name_match_(local, &str, &check));
	if (! check)
		return fmte_("The string ~S doesn't match ~S.", var, from, NULL);

	/* replace */
	Return(translate_name_wild_(local, &str));

	/* result */
	*ret = str.ret;
	rollback_localp(local, stack);

	return 0;
}


/*
 *  directory
 */
static int translate_directory_list_(LocalpRoot local,
		addr *value, addr a, addr b,
		lisp_equal_calltype equal, int *ret)
{
	int check;
	addr a1, b1, var1, var2, wild, wilds;


	if (a == Nil && b == Nil)
		return Result(ret, 1);
	if (a == Nil || b == Nil)
		return Result(ret, 0);

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	Return_getcons(a, &var1, &a1);
	Return_getcons(b, &var2, &b1);
	/* ("str" *) -> next */
	if (var2 == wild) {
		Return(translate_directory_list_(local, value, a1, b1, equal, &check));
		if (check)
			list_local(local->local, value, *value, var2, var1, Nil, NULL);
		return Result(ret, check);
	}
	/* ("str" "s*r") -> next, ("str" "a*a") -> false */
	Return(wildcard_stringp_p_(var2, &check));
	if (check) {
		Return(wildcard_string_pathname_(var1, var2, equal, &check));
		if (! check)
			return Result(ret, 0);
		list_local(local->local, value, *value, var2, var1, Nil, NULL);
		return translate_directory_list_(local, value, a1, b1, equal, ret);
	}
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (var2 != wilds) {
		Return(wildcard_eq_pathname_(var1, var2, equal, &check));
		if (! check)
			return Result(ret, 0);
		return translate_directory_list_(local, value, a1, b1, equal, ret);
	}
	/* ("str" **) */
	a1 = a;
	for (;;) {
		Return(translate_directory_list_(local, value, a, b1, equal, &check));
		if (check) {
			list_local(local->local, value, *value, var2, a1, a, NULL);
			return Result(ret, 1);
		}
		if (a == Nil)
			break;
		Return_getcdr(a, &a);
	}

	return Result(ret, 0);
}

static void translate_directory_wild_wild_(LocalpRoot local, addr *root, addr a, addr b)
{
	addr var;
	LocalRoot alloc;

	alloc = localp_alloc(local);
	while (a != b) {
		GetCons(a, &var, &a);
		cons_alloc(alloc, root, var, *root);
	}
}

static int translate_directory_wild_string_(LocalpRoot local,
		addr *root, addr pos, addr var, addr from)
{
	addr to;

	strvect_char_local(local->local, &to, "*");
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);

	return 0;
}

static int translate_directory_wild_(LocalpRoot local, addr *root, addr *list, addr pos)
{
	int check;
	addr wild1, wild2, next, var, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &var, &a, &b, NULL);
	if (var == wild1) {
		cons_alloc(localp_alloc(local), root, a, *root);
		goto final;
	}
	if (var == wild2) {
		translate_directory_wild_wild_(local, root, a, b);
		goto final;
	}
	Return(wildcard_stringp_p_(var, &check));
	if (check) {
		Return(translate_directory_wild_string_(local, root, pos, a, var));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, var, *root);
	goto final;

final:
	return Result(list, next);
}

static int translate_directory_string_wild_(LocalpRoot local,
		addr *root, addr pos, addr var, addr to)
{
	addr from;

	strvect_char_local(local->local, &from, "*");
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);

	return 0;
}

static int translate_directory_string_string_(LocalpRoot local,
		addr *root, addr pos, addr var, addr from, addr to)
{
	Return(translate_name_(local, &var, pos, var, from, to));
	cons_alloc(localp_alloc(local), root, var, *root);
	return 0;
}

static int translate_directory_string_(LocalpRoot local,
		addr *root, addr *list, addr pos, addr to)
{
	int check;
	addr wild1, wild2, next, var, a, b;

	if (*list == Nil)
		return fmte_("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &var, &a, &b, NULL);
	if (var == wild1 || var == wild2) {
		Return(translate_directory_string_wild_(local, root, pos, a, to));
		goto final;
	}
	Return(wildcard_stringp_p_(var, &check));
	if (check) {
		Return(translate_directory_string_string_(local, root, pos, a, var, to));
		goto final;
	}
	cons_alloc(localp_alloc(local), root, var, *root);
	goto final;

final:
	return Result(list, next);
}

static int translate_directory_replace_(LocalpRoot local,
		addr *root, addr *list, addr pos, addr to)
{
	int check;
	LocalRoot alloc;
	addr var, wild1, wild2;

	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	alloc = localp_alloc(local);
	while (to != Nil) {
		Return_getcons(to, &var, &to);
		if (var == wild1 || var == wild2) {
			Return(translate_directory_wild_(local, root, list, pos));
			continue;
		}
		Return(wildcard_stringp_p_(var, &check));
		if (check) {
			Return(translate_directory_string_(local, root, list, pos, var));
			continue;
		}
		cons_alloc(alloc, root, var, *root);
	}
	nreverse(root, *root);

	return 0;
}

static int translate_directory_(LocalpRoot local, addr *ret,
		addr pos, addr var, addr from, addr to,
		lisp_equal_calltype equal)
{
	int check;
	addr list;
	LocalStack stack;

	push_localp(local, &stack);
	list = Nil;
	Return(translate_directory_list_(local, &list, var, from, equal, &check));
	if (! check)
		return fmte_("Cannot translate ~S to ~S.", from, var, NULL);
	*ret = Nil;
	Return(translate_directory_replace_(local, ret, &list, pos, to));
	rollback_localp(local, stack);

	return 0;
}


/*
 *  version
 */
static int translate_version_(addr *ret, addr var, addr from, addr to)
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
			return Result(ret, var);
		return fmte_(":VERSION from-wildcard is *, but to-wildcard ~S "
				"is not *.", to, NULL);
	}
	if (! eql_function(var, to))
		return fmte_(":VERSION source ~S don't match to-wildcard ~S.", var, to, NULL);

	/* :unspecific */
	return Result(ret, unspec);
}


/*
 *  translate pathname
 */
static void translate_getdirectory(addr pos, addr *ret)
{
	addr list;

	GetDirectoryPathname(pos, &list);
	if (list != Nil) {
		*ret = list;
		return;
	}

	/* (:relative) */
	GetConst(KEYWORD_RELATIVE, &list);
	conscar_heap(ret, list);
}

static int translate_setdirectory_p(addr list)
{
	addr x, y;

	/* (:relative) */
	if (! consp_getcons(list, &x, &list))
		return 0;
	GetConst(KEYWORD_RELATIVE, &y);
	return (x == y) && (list == Nil);
}

static void translate_setdirectory(LocalRoot local, addr pos, addr value)
{
	if (translate_setdirectory_p(value)) {
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
	translate_getdirectory(pos, &a);
	translate_getdirectory(from, &b);
	translate_getdirectory(to, &c);
	Return(translate_directory_(local, &a, pos, a, b, c, equal));
	translate_setdirectory(alloc, one, a);
	/* name */
	GetNamePathname(pos, &a);
	GetNamePathname(from, &b);
	GetNamePathname(to, &c);
	Return(translate_name_(local, &a, pos, a, b, c));
	copylocal_object(alloc, &a, a);
	SetNamePathname(one, a);
	/* type */
	GetTypePathname(pos, &a);
	GetTypePathname(from, &b);
	GetTypePathname(to, &c);
	Return(translate_name_(local, &a, pos, a, b, c));
	copylocal_object(alloc, &a, a);
	SetTypePathname(one, a);
	/* version */
	GetVersionPathname(pos, &a);
	GetVersionPathname(from, &b);
	GetVersionPathname(to, &c);
	Return(translate_version_(&a, a, b, c));
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

