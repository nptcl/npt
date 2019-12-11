#include "condition.h"
#include "cons.h"
#include "cons_common.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control.h"
#include "equal.h"
#include "execute.h"
#include "gc.h"
#include "integer.h"
#include "setf.h"
#include "symbol.h"

static int function_call_cons(Execute ptr, int *result,
		addr item, addr key, addr test, addr check, int notret)
{
	if (key != Nil)
		Return1(callclang_funcall(ptr, &check, key, check, NULL));
	Return1(callclang_funcall(ptr, &check, test, item, check, NULL));
	*result = (notret? (check == Nil): (check != Nil));
	return 0;
}

static int function_if_call_cons(Execute ptr, int *result,
		addr key, addr call, addr check)
{
	if (key != Nil)
		Return1(callclang_funcall(ptr, &check, key, check, NULL));
	Return1(callclang_funcall(ptr, &check, call, check, NULL));
	*result = (check != Nil);
	return 0;
}


/*
 *  sublis
 */
struct sublis_struct {
	Execute ptr;
	addr alist, key, test1, test2;
	int test;
};

static int default_sublis_cons(addr alist, addr left, addr *ret)
{
	addr right, value;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (eql_function(left, right)) {
			*ret = value;
			return 1;
		}
	}
	*ret = left;

	return 0;
}

static int test_sublis_cons(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (callclang_funcall(ptr, &check, test, left, right, NULL))
			return 1;
		if (check != Nil) {
			*result = 1;
			*ret = value;
			return 0;
		}
	}
	*result = 0;
	*ret = left;
	return 0;
}

static int test_not_sublis_cons(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (callclang_funcall(ptr, &check, test, left, right, NULL))
			return 1;
		if (check == Nil) {
			*result = 1;
			*ret = value;
			return 0;
		}
	}
	*result = 0;
	*ret = left;
	return 0;
}

static int replace_sublis_cons(struct sublis_struct *str,
		addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = default_sublis_cons(str->alist, tree, ret);
			return 0;

		case 1: /* :test */
			return test_sublis_cons(str->ptr,
					str->alist, str->test1, tree, result, ret);

		case 2: /* :test-not */
			return test_not_sublis_cons(str->ptr,
					str->alist, str->test2, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int recursive_sublis_cons(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_sublis_cons(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_sublis_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_sublis_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_sublis_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_sublis_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

static int argument_sublis_cons(Execute ptr,
		struct sublis_struct *str, addr alist, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist(rest, key, &key)) key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	/* recursive call */
	str->ptr = ptr;
	str->alist = alist;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

_g int sublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret)
{
	struct sublis_struct str;

	if (argument_sublis_cons(ptr, &str, alist, rest))
		fmte("SUBLIS don't accept both :test and :test-not parameter.", NULL);
	return recursive_sublis_cons(&str, tree, ret);
}


/*
 *  nsublis
 */
static int recursive_nsublis_cons(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_sublis_cons(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_sublis_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_nsublis_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_sublis_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_nsublis_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	*ret = tree;

	return 0;
}

_g int nsublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret)
{
	struct sublis_struct str;

	if (argument_sublis_cons(ptr, &str, alist, rest))
		fmte("NSUBLIS don't accept both :test and :test-not parameter.", NULL);
	return recursive_nsublis_cons(&str, tree, ret);
}


/*
 *  subst
 */
struct subst_struct {
	Execute ptr;
	addr make, old, key, test1, test2;
	int test;
};

static int argument_subst_cons(Execute ptr,
		struct subst_struct *str, addr one, addr old, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist(rest, key, &key)) key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->make = one;
	str->old = old;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int default_subst_cons(struct subst_struct *str, addr tree, addr *ret)
{
	if (eql_function(str->old, tree)) {
		*ret = str->make;
		return 1;
	}
	else {
		*ret = tree;
		return 0;
	}
}

static int test_subst_cons(struct subst_struct *str, addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test1, str->old, tree, NULL))
		return 1;
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int test_not_subst_cons(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test2, str->old, tree, NULL))
		return 1;
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int replace_subst_cons(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = default_subst_cons(str, tree, ret);
			return 0;

		case 1: /* :test */
			return test_subst_cons(str, tree, result, ret);

		case 2: /* :test-not */
			return test_not_subst_cons(str, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int recursive_subst_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_subst_cons(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_subst_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_subst_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_subst_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_subst_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

_g int subst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	if (argument_subst_cons(ptr, &str, one, old, key))
		fmte("SUBST don't accept both :test and :test-not parameter.", NULL);
	return recursive_subst_cons(&str, tree, ret);
}


/*
 *  nsubst
 */
static int recursive_nsubst_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_subst_cons(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_subst_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_nsubst_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_subst_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_nsubst_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	*ret = tree;

	return 0;
}

_g int nsubst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	if (argument_subst_cons(ptr, &str, one, old, key))
		fmte("NSUBST don't accept both :test and :test-not parameter.", NULL);
	return recursive_nsubst_cons(&str, tree, ret);
}


/*
 *  subst-if
 */
static int argument_subst_if_cons(Execute ptr,
		struct subst_struct *str, addr one, addr test1, addr test2, addr rest)
{
	addr key;

	GetConst(KEYWORD_KEY, &key);
	if (getplist(rest, key, &key)) key = Nil;

	clearpoint(str);
	str->ptr = ptr;
	str->make = one;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int call_subst_if_cons(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test1, tree, NULL))
		return 1;
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int call_subst_if_not_cons(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test2, tree, NULL))
		return 1;
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int replace_subst_if(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 1: /* :test */
			return call_subst_if_cons(str, tree, result, ret);

		case 2: /* :test-not */
			return call_subst_if_not_cons(str, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int recursive_subst_if_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_subst_if(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_subst_if(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_subst_if_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_subst_if(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_subst_if_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	cons_heap(ret, car, cdr);

	return 0;
}

_g int subst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	argument_subst_if_cons(ptr, &str, one, predicate, Nil, key);
	return recursive_subst_if_cons(&str, tree, ret);
}


/*
 *  nsubst-if
 */
static int recursive_nsubst_if_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	if (! consp(tree))
		return replace_subst_if(str, tree, &check, ret);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return1(replace_subst_if(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return1(recursive_nsubst_if_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return1(replace_subst_if(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return1(recursive_nsubst_if_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	*ret = tree;

	return 0;
}

_g int nsubst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	argument_subst_if_cons(ptr, &str, one, predicate, Nil, key);
	return recursive_nsubst_if_cons(&str, tree, ret);
}


/*
 *  subst-if-not
 */
_g int subst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	argument_subst_if_cons(ptr, &str, one, Nil, predicate, key);
	return recursive_subst_if_cons(&str, tree, ret);
}


/*
 *  nsubst-if-not
 */
_g int nsubst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	argument_subst_if_cons(ptr, &str, one, Nil, predicate, key);
	return recursive_nsubst_if_cons(&str, tree, ret);
}


/*
 *  tree-equal
 */
struct tree_equal_struct {
	Execute ptr;
	addr test1, test2;
	int test;
};

static int argument_tree_equal_cons(Execute ptr,
		struct tree_equal_struct *str, addr rest)
{
	addr test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int test_tree_equal_cons(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	if (callclang_funcall(ptr, &test, test, left, right, NULL)) return 1;
	*result = (test != Nil);
	return 0;
}

static int test_not_tree_equal_cons(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	if (callclang_funcall(ptr, &test, test, left, right, NULL)) return 1;
	*result = (test == Nil);
	return 0;
}

static int replace_tree_equal_cons(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	switch (str->test) {
		case 0: /* nil */
			*result = eql(tree1, tree2);
			return 0;

		case 1: /* :test */
			return test_tree_equal_cons(str->ptr, result, str->test1, tree1, tree2);

		case 2: /* :test-not */
			return test_not_tree_equal_cons(str->ptr, result, str->test2, tree1, tree2);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int recursive_tree_equal_cons(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	int check;
	addr car1, cdr1, car2, cdr2;

	if (atom(tree1) || atom(tree2))
		return replace_tree_equal_cons(str, result, tree1, tree2);
	GetCons(tree1, &car1, &cdr1);
	GetCons(tree2, &car2, &cdr2);

	if (recursive_tree_equal_cons(str, &check, car1, car2))
		return 1;
	if (! check) {
		*result = 0;
		return 0;
	}
	return recursive_tree_equal_cons(str, result, cdr1, cdr2);
}

_g int tree_equal_common(Execute ptr, addr tree1, addr tree2, addr key, int *ret)
{
	struct tree_equal_struct str;

	if (argument_tree_equal_cons(ptr, &str, key))
		fmte("TREE-EQUAL don't accept both :test and :test-not parameter.", NULL);
	return recursive_tree_equal_cons(&str, ret, tree1, tree2);
}


/*
 *  list-length
 */
static int index_list_length_cons(addr list, size_t *ret)
{
	addr fast, slow, one;
	size_t size;

	slow = fast = list;
	size = 0;
	for (;;) {
		if (fast == Nil) {
			break;
		}
		getcdr(fast, &one);
		if (one == Nil) {
			size++;
			break;
		}

		/* circular check */
		if (fast == slow && 0 < size) {
			return 1;
		}

		/* increment */
		size += 2;
		getcdr(one, &fast);
		getcdr(slow, &slow);
	}
	*ret = size;

	return 0;
}

_g void list_length_common(addr list, addr *ret)
{
	size_t size;

	if (index_list_length_cons(list, &size))
		*ret = Nil;
	else
		make_index_integer_alloc(NULL, ret, size);
}


/*
 *  make-list
 */
_g void make_list_common(addr var, addr rest, addr *ret)
{
	addr element, list;
	size_t size;

	/* argument */
	if (GetIndex_integer(var, &size))
		fmte("Too large index value ~S.", var, NULL);
	if (getplist_constant(rest, CONSTANT_KEYWORD_INITIAL_ELEMENT, &element))
		element = Nil;
	/* make-list */
	for (list = Nil; size--; )
		cons_heap(&list, element, list);
	/* result */
	*ret = list;
}


/*
 *  push
 */
static void single_push_cons(addr *ret,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (cons value r)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, cons, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (cons value r)) */
	getcar(g, &g);
	list_heap(&cons, cons, item, r, NULL);
	list_heap(&x, g, cons, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);
}

static void multiple_push_cons(Execute ptr, addr *ret,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (cons v g1))
	 *   (setq g2 (cons v g2))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, cons, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	make_gensym(ptr, &v);
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (cons v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&y, cons, v, x, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(ret, pos);
}

static int expansion_push_cons(Execute ptr, addr *ret, addr item, addr place, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	if (singlep(g))
		single_push_cons(ret, item, a, b, g, w, r);
	else
		multiple_push_cons(ptr, ret, item, a, b, g, w, r);

	return 0;
}

_g int push_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &item, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;
	return expansion_push_cons(ptr, ret, item, place, env);

error:
	fmte("PUSH argument ~S must be a (push item place) form.", form, NULL);
	*ret = Nil;
	return 0;
}


/*
 *  pop
 */
static void single_pop_cons(Execute ptr, addr *ret,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (c r)
	 *        (g (cdr c)))
	 *   (declare (ignorable a1 a2))
	 *   w
	 *   (car c))
	 */
	addr list1, list2, leta, car, cdr, declare, ignorable, args, x, y, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (c r) */
	make_gensym(ptr, &c);
	list_heap(&x, c, r, NULL);
	cons_heap(&args, x, args);
	/* (g (cdr c)) */
	getcar(g, &g);
	list_heap(&x, cdr, c, NULL);
	list_heap(&x, g, x, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(&x, car, c, NULL);
	list_heap(ret, leta, args, declare, w, x, NULL);
}

static void multiple_pop_cons(Execute ptr, addr *ret,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2 ...))
	 *   (multiple-value-bind (r1 r2 ...) r
	 *     (setq g1 (cdr r1))
	 *     (setq g2 (cdr r2))
	 *     ...)
	 *   w
	 *   (values (car r1) (car r2) ...))
	 */
	addr leta, declare, ignorable, mvbind, setq, car, cdr, values;
	addr list1, list2, args, x, y, pos, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-bind (r1 r2 ...) r
	 *   (setq g1 (cdr r1)) ...)  */
	c = Nil;
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		make_gensym(ptr, &y);
		cons_heap(&c, y, c);
		list_heap(&y, cdr, y, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&args, x, args);
	}
	nreverse_list_unsafe(&c, c);
	nreverse_list_unsafe(&args, args);
	conscar_heap(&args, mvbind);
	cons_heap(&args, c, args);
	cons_heap(&args, r, args);
	cons_heap(&pos, args, pos);
	/* w */
	cons_heap(&pos, w, pos);
	/* (values (car r1) (car r2) ...) */
	args = Nil;
	for (list1 = c; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&x, car, x, NULL);
		cons_heap(&args, x, args);
	}
	nreverse_list_unsafe(&args, args);
	cons_heap(&values, values, args);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(ret, pos);
}

static int expansion_pop_cons(Execute ptr, addr *ret, addr place, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	if (singlep(g))
		single_pop_cons(ptr, ret, a, b, g, w, r);
	else
		multiple_pop_cons(ptr, ret, a, b, g, w, r);

	return 0;
}

_g int pop_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;
	return expansion_pop_cons(ptr, ret, place, env);

error:
	fmte("POP argument ~S must be a (pop place) form.", form, NULL);
	*ret = Nil;
	return 0;
}


/*
 *  nth
 */
_g void nth_common(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		getnth_large(list, index, ret);
	else
		getnth(list, size, ret);
}


/*
 *  (setf nth)
 */
_g void setf_nth_common(addr value, addr index, addr list)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		fmte("Too large index value ~S.", index, NULL);
	setnth(list, size, value);
}


/*
 *  nthcdr
 */
_g void nthcdr_common(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		getnthcdr_large(list, index, ret);
	else
		getnthcdr(list, size, ret);
}


/*
 *  member
 */
static int test_member_cons(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr value, next;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_call_cons(ptr, &check, item, key, call, value, notret))
			return 1;
		if (check) {
			*ret = list;
			return 0;
		}
		list = next;
	}
	*ret = Nil;

	return 0;
}

_g int member_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("MEMBER don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_member_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_member_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_member_cons(ptr, ret, item, list, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  member-if
 */
_g int member_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (check) {
			*ret = list;
			return 0;
		}
		list = next;
	}
	*ret = Nil;

	return 0;
}


/*
 *  member-if-not
 */
_g int member_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (! check) {
			*ret = list;
			return 0;
		}
		list = next;
	}
	*ret = Nil;

	return 0;
}


/*
 *  mapc
 */
_g int mapc_common(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
	}

finish:
	rollback_local(local, stack);
	*ret = result;

	return 0;
}


/*
 *  mapcar
 */
_g int mapcar_common(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;
	cons_heap(&result, pos, result);
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse_list_unsafe(ret, result);

	return 0;
}


/*
 *  mapcan
 */
_g int mapcan_common(Execute ptr, addr call, addr rest, addr *ret)
{
	addr result, pos, car, cdr, args, next, temp1, temp2, head, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &head, call, args))
		return 1;
	result = head;
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				setlastcdr_safe(head, pos);
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	*ret = result;

	return 0;
}


/*
 *  mapl
 */
_g int mapl_common(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
	}

finish:
	rollback_local(local, stack);
	*ret = result;

	return 0;
}


/*
 *  maplist
 */
_g int maplist_common(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;
	cons_heap(&result, pos, result);
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse_list_unsafe(ret, result);

	return 0;
}


/*
 *  mapcon
 */
_g int mapcon_common(Execute ptr, addr call, addr rest, addr *ret)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2, head, hold;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &head, call, args))
		return 1;
	result = head;
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				setlastcdr_safe(head, pos);
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	*ret = result;

	return 0;
}


/*
 *  nconc
 */
static void concat_nconc_cons(addr list, addr args)
{
	addr last;

	last = list;
	for (;;) {
		/* update lastcdr */
		while (list != Nil) {
			last = list;
			getcdr(list, &list);
		}

		for (;;) {
			GetCons(args, &list, &args);
			if (args == Nil) {
				setcdr(last, list);
				return;
			}
			if (list == Nil) {
				continue;
			}
			if (IsCons(list)) {
				setcdr(last, list);
				break;
			}
			fmte("nconc argument ~S must be a list.", list, NULL);
		}
	}
}

_g void nconc_common(addr args, addr *ret)
{
	addr pos;

	/* (nconc) */
	if (args == Nil) {
		*ret = Nil;
		return;
	}

	/* (nconc object) */
	for (;;) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			*ret = pos;
			return;
		}
		if (pos == Nil) {
			continue;
		}
		if (IsCons(pos)) {
			break;
		}
		fmte("nconc argument ~S must be a list.", pos, NULL);
	}

	/* (nconc x x ...) */
	concat_nconc_cons(pos, args);
	*ret = pos;
}


/*
 *  append
 */
static addr push_append_cons(addr root, addr last)
{
	addr pos;

	if (! IsCons(last))
		fmte("The argument ~S must be a list.", last, NULL);
	while (last != Nil) {
		getcons(last, &pos, &last);
		cons_heap(&root, pos, root);
	}
	return root;
}

static void concat_append_cons(addr last, addr args, addr *ret)
{
	addr pos, root;

	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			if (pos != Nil) {
				root = push_append_cons(root, last);
				last = pos;
			}
			break;
		}
		if (pos == Nil) {
			continue;
		}
		root = push_append_cons(root, last);
		last = pos;
	}
	nreverse_list_unsafe_dotted(ret, root, last);
}

_g void append_common(addr args, addr *ret)
{
	addr pos;

	/* (append) */
	if (args == Nil) {
		*ret = Nil;
		return;
	}

	/* (append object) */
	for (;;) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			*ret = pos;
			return;
		}
		if (pos == Nil) {
			continue;
		}
		if (IsCons(pos)) {
			break;
		}
		fmte("append argument ~S must be a list.", pos, NULL);
	}

	/* (append x x ...) */
	concat_append_cons(pos, args, &pos);
	*ret = pos;
}


/*
 *  revappend
 */
_g void revappend_common(addr list, addr tail, addr *ret)
{
	addr pos;

	while (list != Nil) {
		getcons(list, &pos, &list);
		cons_heap(&tail, pos, tail);
	}
	*ret = tail;
}


/*
 *  nreconc
 */
_g void nreconc_common(addr list, addr tail, addr *ret)
{
	addr next;

	/* nil */
	if (list == Nil) {
		*ret = tail;
		return;
	}

	/* loop */
	for (;;) {
		getcdr(list, &next);
		setcdr(list, tail);
		if (next == Nil) break;
		tail = list;
		list = next;
	}
	*ret = list;
}


/*
 *  butlast
 */
static void index_butlast_cons(addr list, size_t index, addr *ret)
{
	addr root, pos;
	size_t size;

	size = length_list_safe_dotted(list);
	if (size <= index) {
		*ret = Nil;
		return;
	}
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void large_butlast_cons(addr list, addr index, addr *ret)
{
	addr size;
	size_t value;

	value = length_list_safe_dotted(list);
	size = intsizeh(value);
	if (! less_equal_integer(size, index))
		fmte("Too large butlast index ~S.", index);
	*ret = Nil;
}

_g void butlast_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound) {
		index_butlast_cons(list, 1, ret);
		return;
	}
	if (GetIndex_integer(index, &size))
		large_butlast_cons(list, index, ret);
	else
		index_butlast_cons(list, size, ret);
}


/*
 *  nbutlast
 */
static void index_nbutlast_cons(addr list, size_t index, addr *ret)
{
	size_t size;

	size = length_list_safe_dotted(list);
	if (size <= index) {
		*ret = Nil;
		return;
	}
	size -= index + 1;
	while (size--)
		GetCdr(list, &list);
	SetCdr(list, Nil);
}

static void large_nbutlast_cons(addr list, addr index, addr *ret)
{
	addr size;
	size_t value;

	value = length_list_safe_dotted(list);
	size = intsizeh(value);
	if (! less_equal_integer(size, index))
		fmte("Too large nbutlast index ~S.", index);
	*ret = Nil;
}

_g void nbutlast_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound) {
		index_nbutlast_cons(list, 1, ret);
		return;
	}
	if (GetIndex_integer(index, &size))
		large_nbutlast_cons(list, index, ret);
	else
		index_nbutlast_cons(list, size, ret);
}


/*
 *  last
 */
static void index_last_cons(addr list, size_t index, addr *ret)
{
	size_t size;

	size = length_list_safe_dotted(list);
	if (size < index) {
		*ret = list;
		return;
	}
	size -= index;
	while (size--)
		getcdr(list, &list);
	*ret = list;
}

static void large_last_cons(addr list, addr index, addr *ret)
{
	addr size;
	size_t value;

	value = length_list_safe_dotted(list);
	size = intsizeh(value);
	if (! less_equal_integer(size, index))
		fmte("Too large nbutlast index ~S.", index);
	*ret = list;
}

_g void last_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound) {
		index_last_cons(list, 1, ret);
		return;
	}
	if (GetIndex_integer(index, &size))
		large_last_cons(list, index, ret);
	else
		index_last_cons(list, size, ret);
}


/*
 *  ldiff
 */
_g void ldiff_common(addr list, addr object, addr *ret)
{
	addr root, pos;

	root = Nil;
	for (;;) {
		if (list == object) {
			list = Nil;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			break;
		}
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe_dotted(ret, root, list);
}


/*
 *  tailp
 */
_g void tailp_common(addr object, addr list, int *ret)
{
	int check;

	for (;;) {
		if (list == object) {
			check = 1;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			check = 0;
			break;
		}
		GetCdr(list, &list);
	}
	*ret = check;
}


/*
 *  assoc
 */
static int test_assoc_cons(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_call_cons(ptr, &check, item, key, call, value, notret))
			return 1;
		if (check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}

_g int assoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("ASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_assoc_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_assoc_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_assoc_cons(ptr, ret, item, list, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  assoc-if
 */
_g int assoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}


/*
 *  assoc-if-not
 */
_g int assoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (! check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}


/*
 *  copy-alist
 */
_g void copy_alist_common(addr list, addr *ret)
{
	addr root, cons, car, cdr;

	for (root = Nil; list != Nil; ) {
		getcons(list, &cons, &list);
		getcons(cons, &car, &cdr);
		cons_heap(&cons, car, cdr);
		cons_heap(&root, cons, root);
	}
	nreverse_list_unsafe(ret, root);
}


/*
 *  pairlis
 */
_g void pairlis_common(addr keys, addr data, addr list, addr *ret)
{
	int check1, check2;
	addr car, cdr;

	if (list == Unbound) list = Nil;
	for (;;) {
		check1 = (keys == Nil);
		check2 = (data == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			fmte("The length of keys isn't equal to the data.", NULL);
		getcons(keys, &car, &keys);
		getcons(data, &cdr, &data);
		cons_heap(&cdr, car, cdr);
		cons_heap(&list, cdr, list);
	}
	*ret = list;
}


/*
 *  rassoc
 */
static int test_rassoc_cons(Execute ptr, addr *ret,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_call_cons(ptr, &check, item, key, call, value, notret))
			return 1;
		if (check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}

_g int rassoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("RASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_rassoc_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_rassoc_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_rassoc_cons(ptr, ret, item, list, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  rassoc-if
 */
_g int rassoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}


/*
 *  rssoc-if-not
 */
_g int rassoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_if_call_cons(ptr, &check, key, call, value))
			return 1;
		if (! check) {
			*ret = cons;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}


/*
 *  get-properties
 */
_g void get_properties_common(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist)
{
	addr key, value, next, list, check;

	while (plist != Nil) {
		getcons(plist, &key, &next);
		getcons(next, &value, &next);
		for (list = indicator; list != Nil; ) {
			getcons(list, &check, &list);
			if (check == key)
				goto find;
		}
		plist = next;
	}
	*rkey = *rvalue = *rlist = Nil;
	return;
find:
	*rkey = key;
	*rvalue = value;
	*rlist = plist;
}


/*
 *  (setf remf)
 */
static int expansion_remf_cons(Execute ptr, addr *ret,
		addr place, addr indicator, addr env)
{
	/* (let* ((a1 b1)
	 *        (a2 b2))
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-bind (g c) (remlist indicator r)
	 *     w c))
	 */
	addr list1, list2, args, x, y, c;
	addr leta, remplist, declare, ignorable, mvbind;
	addr a, b, g, w, r;

	/* get-setf-expansion */
	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	/* macro */
	GetConst(COMMON_LETA, &leta);
	GetConst(SYSTEM_REMPLIST, &remplist);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* (multiple-value-bind (g c) (remplist indicator r) w c) */
	getcar(g, &g);
	make_gensym(ptr, &c);
	list_heap(&g, g, c, NULL);
	list_heap(&remplist, remplist, indicator, r, NULL);
	list_heap(&mvbind, mvbind, g, remplist, w, c, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(ret, leta, args, declare, mvbind, NULL);

	return 0;
}

_g int remf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, indicator;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (! consp(args)) goto error;
	GetCons(args, &indicator, &args);
	if (args != Nil) goto error;
	return expansion_remf_cons(ptr, ret, place, indicator, env);

error:
	fmte("REMF argument ~S must be a (place indicator) form.", form, NULL);
	*ret = Nil;
	return 0;
}


/*
 *  intersection
 */
static int check_intersection_cons(Execute ptr, int *result,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr right;

	if (key != Nil) {
		if (callclang_funcall(ptr, &left, key, left, NULL))
			return 1;
	}
	while (list != Nil) {
		getcons(list, &right, &list);
		if (function_call_cons(ptr, &check, left, key, test, right, notret))
			return 1;
		if (check) {
			*result = 1;
			return 0;
		}
	}
	*result = 0;
	return 0;
}

static int test_intersection_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (list = Nil; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	*ret = list;

	return 0;
}

_g int intersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("INTERSECTION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_intersection_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_intersection_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_intersection_cons(ptr, ret, list1, list2, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  rintersection
 */
static int test_nintersection_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left, next1, next2;

	/* first */
	list = list1;
	for (;;) {
		getcons(list1, &left, &next1);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (check)
			break;
		list = list1 = next1;
		if (list1 == Nil)
			goto finish;
	}

	/* tail */
	while (list1 != Nil) {
		getcons(list1, &left, &next1);
		while (next1 != Nil) {
			getcons(next1, &left, &next2);
			if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
				return 1;
			if (! check)
				break;
			next1 = next2;
		}
		if (next1 == Nil)
			goto finish;
		setcdr(list1, next2);
		list1 = next2;
	}
finish:
	*ret = list;

	return 0;
}

_g int nintersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NINTERSECTION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_nintersection_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nintersection_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nintersection_cons(ptr, ret, list1, list2, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  adjoin
 */
static int test_adjoin_cons(Execute ptr, addr *ret,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr find, right;

	for (find = list; find != Nil; ) {
		getcons(find, &right, &find);
		if (function_call_cons(ptr, &check, left, key, test, right, notret))
			return 1;
		if (check) {
			*ret = list;
			return 0;
		}
	}
	cons_heap(ret, left, list);

	return 0;
}

_g int adjoin_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("ADJOIN don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_adjoin_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_adjoin_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_adjoin_cons(ptr, ret, item, list, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  pushnew
 */
static void single_pushnew_cons(addr *ret,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (adjoin value r . rest)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, adjoin, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (adjoin value r . rest)) */
	getcar(g, &g);
	lista_heap(&adjoin, adjoin, item, r, rest, NULL);
	list_heap(&x, g, adjoin, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);
}

static void multiple_pushnew_cons(Execute ptr, addr *ret,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (adjoin v g1 . rest))
	 *   (setq g2 (adjoin v g2 . rest))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, adjoin, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	make_gensym(ptr, &v);
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (adjoin v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		lista_heap(&y, adjoin, v, x, rest, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(ret, pos);
}

static int expansion_pushnew_cons(Execute ptr, addr *ret,
		addr item, addr place, addr rest, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	if (singlep(g))
		single_pushnew_cons(ret, item, rest, a, b, g, w, r);
	else
		multiple_pushnew_cons(ptr, ret, item, rest, a, b, g, w, r);

	return 0;
}

_g int pushnew_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &item, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	return expansion_pushnew_cons(ptr, ret, item, place, args, env);

error:
	fmte("PUSH argument ~S must be a (item place &rest args) form.", form, NULL);
	*ret = Nil;
	return 0;
}


/*
 *  set-difference
 */
static int test_set_difference_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (list = Nil; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	*ret = list;

	return 0;
}

_g int set_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("SET-DIFFERENCE don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_set_difference_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_set_difference_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_set_difference_cons(ptr, ret, a, b, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  nset-difference
 */
static int test_nset_difference_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left, next1, next2;

	/* first */
	list = list1;
	for (;;) {
		getcons(list1, &left, &next1);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (! check)
			break;
		list = list1 = next1;
		if (list1 == Nil)
			goto finish;
	}

	/* tail */
	while (list1 != Nil) {
		getcons(list1, &left, &next1);
		while (next1 != Nil) {
			getcons(next1, &left, &next2);
			if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
				return 1;
			if (check)
				break;
			next1 = next2;
		}
		if (next1 == Nil)
			goto finish;
		setcdr(list1, next2);
		list1 = next2;
	}
finish:
	*ret = list;
	return 0;
}

_g int nset_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NSET-DIFFERENCE don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_nset_difference_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_nset_difference_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nset_difference_cons(ptr, ret, a, b, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  set-exclusive-or
 */
static int test_set_exclusive_or_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr result, list, left;
	LocalHold hold;

	result = Nil;
	/* left -> right */
	hold = LocalHold_array(ptr, 1);
	for (list = list1; list != Nil; ) {
		getcons(list, &left, &list);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* right -> left */
	for (list = list2; list != Nil; ) {
		getcons(list, &left, &list);
		if (check_intersection_cons(ptr, &check, left, list1, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* result */
	localhold_end(hold);
	*ret = result;

	return 0;
}

_g int set_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		fmte("SET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_set_exclusive_or_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_set_exclusive_or_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_set_exclusive_or_cons(ptr, ret, a, b, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  nset-exclucive-or
 */
static int test_nset_exclusive_or_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr result, list, left;
	LocalHold hold;

	result = Nil;
	/* right -> left */
	hold = LocalHold_array(ptr, 1);
	for (list = list2; list != Nil; ) {
		getcons(list, &left, &list);
		if (check_intersection_cons(ptr, &check, left, list1, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* left -> right */
	if (test_nset_difference_cons(ptr, &list1, list1, list2, key, test, notret))
		return 1;

	/* result */
	localhold_end(hold);
	nconc2_safe(result, list1, ret);

	return 0;
}

_g int nset_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		fmte("NSET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nset_exclusive_or_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_nset_exclusive_or_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nset_exclusive_or_cons(ptr, ret, a, b, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  subsetp
 */
static int test_subsetp_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left, result;

	for (result = T; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (check_intersection_cons(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (! check) {
			result = Nil;
			break;
		}
	}
	*ret = result;

	return 0;
}

_g int subsetp_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("SUBSETP don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_subsetp_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_subsetp_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_subsetp_cons(ptr, ret, list1, list2, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  union
 */
static int test_union_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	list = Nil;
	/* left */
	hold = LocalHold_array(ptr, 1);
	while (list1 != Nil) {
		getcons(list1, &left, &list1);
		if (check_intersection_cons(ptr, &check, left, list, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}

	/* right */
	while (list2 != Nil) {
		getcons(list2, &left, &list2);
		if (check_intersection_cons(ptr, &check, left, list, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}

	/* result */
	localhold_end(hold);
	*ret = list;

	return 0;
}

_g int union_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("UNION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_union_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_union_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_union_cons(ptr, ret, list1, list2, key, test, 0);
	}
	*ret = Nil;

	return 0;
}


/*
 *  nunion
 */
static int single_nunion_cons(Execute ptr, addr *ret,
		addr list1, addr key, addr test, int notret)
{
	int check;
	addr list2, list3, left, right;

	/* nil */
	if (list1 == Nil) {
		*ret = Nil;
		return 0;
	}

	/* single */
	getcons(list1, &left, &list2);
	*ret = list1;
	if (list2 == Nil) {
		return 0;
	}
	if (key != Nil) {
		if (callclang_funcall(ptr, &left, key, left, NULL))
			return 1;
	}

	/* list */
	while (list2 != Nil) {
		getcons(list2, &right, &list3);
		if (function_call_cons(ptr, &check, left, key, test, right, notret))
			return 1;
		if (check) {
			SetCdr(list1, list3);
			list2 = list3;
		}
		else {
			left = right;
			if (key != Nil) {
				if (callclang_funcall(ptr, &left, key, left, NULL))
					return 1;
			}
			list1 = list2;
			list2 = list3;
		}
	}

	return 0;
}

static int test_nunion_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left;
	LocalHold hold;

	/* left */
	if (single_nunion_cons(ptr, &list1, list1, key, test, notret))
		return 1;
	hold = LocalHold_array(ptr, 1);
	localhold_set(hold, 0, list1);

	/* right */
	while (list2 != Nil) {
		getcons(list2, &left, &list2);
		if (check_intersection_cons(ptr, &check, left, list1, key, test, notret))
			return 1;
		if (! check) {
			cons_heap(&list1, left, list1);
			localhold_set(hold, 0, list1);
		}
	}

	/* result */
	localhold_end(hold);
	*ret = list1;

	return 0;
}

_g int nunion_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NUNION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_nunion_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nunion_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nunion_cons(ptr, ret, list1, list2, key, test, 0);
	}
	*ret = Nil;

	return 0;
}

