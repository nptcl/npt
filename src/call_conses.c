#include "call_conses.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "equal.h"
#include "execute.h"
#include "hold.h"
#include "integer.h"
#include "setf.h"
#include "symbol.h"

static int function_call_cons(Execute ptr, int *result,
		addr item, addr key, addr test, addr check, int notret)
{
	if (key != Nil)
		Return(callclang_funcall(ptr, &check, key, check, NULL));
	Return(callclang_funcall(ptr, &check, test, item, check, NULL));
	*result = (notret? (check == Nil): (check != Nil));
	return 0;
}

static int function_if_call_cons(Execute ptr, int *result,
		addr key, addr call, addr check)
{
	if (key != Nil)
		Return(callclang_funcall(ptr, &check, key, check, NULL));
	Return(callclang_funcall(ptr, &check, call, check, NULL));
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

static int default_sublis_cons(addr alist, addr left, int *result, addr *ret)
{
	addr right, value;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		if (eql_function(left, right)) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int test_sublis_cons(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		Return(callclang_funcall(ptr, &check, test, left, right, NULL));
		if (check != Nil) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int test_not_sublis_cons(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		Return_getcons(alist, &right, &alist);
		if (right == Nil)
			continue;
		Return_getcons(right, &right, &value);
		Return(callclang_funcall(ptr, &check, test, left, right, NULL));
		if (check == Nil) {
			*result = 1;
			return Result(ret, value);
		}
	}
	*result = 0;
	return Result(ret, left);
}

static int replace_sublis_cons(struct sublis_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(callclang_funcall(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 0: /* nil */
			Return(default_sublis_cons(str->alist, check, result, ret));
			break;

		case 1: /* :test */
			Return(test_sublis_cons(str->ptr,
						str->alist, str->test1, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(test_not_sublis_cons(str->ptr,
						str->alist, str->test2, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_sublis_cons(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_sublis_cons(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);

	/* car */
	Return(replace_sublis_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_sublis_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_sublis_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_sublis_cons(str, cdr, &cdr));
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
		if (getplist(rest, key, &key))
			key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2))
			test2 = Nil;
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
		return fmte_("SUBLIS don't accept both :test and :test-not parameter.", NULL);
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
	Return(replace_sublis_cons(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_sublis_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsublis_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_sublis_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsublis_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
}

_g int nsublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret)
{
	struct sublis_struct str;

	if (argument_sublis_cons(ptr, &str, alist, rest))
		return fmte_("NSUBLIS don't accept both :test and :test-not parameter.", NULL);
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
		if (getplist(rest, key, &key))
			key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2))
			test2 = Nil;
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

	Return(callclang_funcall(str->ptr, &check, str->test1, str->old, tree, NULL));
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

	Return(callclang_funcall(str->ptr, &check, str->test2, str->old, tree, NULL));
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
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(callclang_funcall(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = default_subst_cons(str, check, ret);
			break;

		case 1: /* :test */
			Return(test_subst_cons(str, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(test_not_subst_cons(str, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_subst_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_cons(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_subst_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_subst_cons(str, cdr, &cdr));
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
		return fmte_("SUBST don't accept both :test and :test-not parameter.", NULL);
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
	Return(replace_subst_cons(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_cons(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsubst_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_cons(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsubst_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
}

_g int nsubst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret)
{
	struct subst_struct str;

	if (argument_subst_cons(ptr, &str, one, old, key))
		return fmte_("NSUBST don't accept both :test and :test-not parameter.", NULL);
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
	if (getplist(rest, key, &key))
		key = Nil;

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

	Return(callclang_funcall(str->ptr, &check, str->test1, tree, NULL));
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

	Return(callclang_funcall(str->ptr, &check, str->test2, tree, NULL));
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
	addr check;

	/* key */
	if (str->key != Nil) {
		Return(callclang_funcall(str->ptr, &check, str->key, tree, NULL));
	}
	else {
		check = tree;
	}

	/* test */
	switch (str->test) {
		case 1: /* :test */
			Return(call_subst_if_cons(str, check, result, ret));
			break;

		case 2: /* :test-not */
			Return(call_subst_if_not_cons(str, check, result, ret));
			break;

		default:
			return fmte_("Invalid test mode.", NULL);
	}
	if (*result == 0)
		*ret = tree;

	return 0;
}

static int recursive_subst_if_cons(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;
	LocalHold hold;

	/* atom */
	Return(replace_subst_if(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_if(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_subst_if_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_if(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_subst_if_cons(str, cdr, &cdr));
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
	Return(replace_subst_if(str, tree, &check, &tree));
	if (! consp(tree))
		return Result(ret, tree);

	/* car */
	hold = LocalHold_local(str->ptr);
	GetCons(tree, &car, &cdr);
	Return(replace_subst_if(str, car, &check, &car));
	localhold_push(hold, car);
	if (! check) {
		Return(recursive_nsubst_if_cons(str, car, &car));
		localhold_push(hold, car);
	}

	/* cdr */
	Return(replace_subst_if(str, cdr, &check, &cdr));
	localhold_push(hold, cdr);
	if (! check) {
		Return(recursive_nsubst_if_cons(str, cdr, &cdr));
		localhold_push(hold, cdr);
	}

	/* result */
	localhold_end(hold);
	SetCons(tree, car, cdr);
	return Result(ret, tree);
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
		if (getplist(rest, test1, &test1))
			test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2))
			test2 = Nil;
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
	Return(callclang_funcall(ptr, &test, test, left, right, NULL));
	return Result(result, (test != Nil));
}

static int test_not_tree_equal_cons(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	Return(callclang_funcall(ptr, &test, test, left, right, NULL));
	return Result(result, (test == Nil));
}

static int replace_tree_equal_cons(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	switch (str->test) {
		case 0: /* nil */
			*result = eql_function(tree1, tree2);
			return 0;

		case 1: /* :test */
			return test_tree_equal_cons(str->ptr, result, str->test1, tree1, tree2);

		case 2: /* :test-not */
			return test_not_tree_equal_cons(str->ptr, result, str->test2, tree1, tree2);

		default:
			return fmte_("Invalid test mode.", NULL);
	}
}

static int recursive_tree_equal_cons(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	int check, atom1, atom2;
	addr car1, cdr1, car2, cdr2;

	atom1 = atom_function(tree1);
	atom2 = atom_function(tree2);
	if (atom1 && atom2)
		return replace_tree_equal_cons(str, result, tree1, tree2);
	if (atom1 || atom2)
		return Result(result, 0);

	/* cons */
	GetCons(tree1, &car1, &cdr1);
	GetCons(tree2, &car2, &cdr2);
	Return(recursive_tree_equal_cons(str, &check, car1, car2));
	if (! check)
		return Result(result, 0);
	return recursive_tree_equal_cons(str, result, cdr1, cdr2);
}

_g int tree_equal_common(Execute ptr, addr tree1, addr tree2, addr key, int *ret)
{
	struct tree_equal_struct str;

	if (argument_tree_equal_cons(ptr, &str, key)) {
		return fmte_("TREE-EQUAL don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	return recursive_tree_equal_cons(&str, ret, tree1, tree2);
}


/*
 *  list-length
 */
static int index_list_length_cons(addr list, size_t *rsize, int *ret)
{
	addr fast, slow, one;
	size_t size;

	slow = fast = list;
	size = 0;
	for (;;) {
		if (fast == Nil) {
			break;
		}
		Return_getcdr(fast, &one);
		if (one == Nil) {
			size++;
			break;
		}

		/* circular check */
		if (fast == slow && 0 < size)
			return Result(ret, 1);

		/* increment */
		size += 2;
		Return_getcdr(one, &fast);
		Return_getcdr(slow, &slow);
	}
	*rsize = size;
	return Result(ret, 0);
}

_g int list_length_common(addr list, addr *ret)
{
	int check;
	size_t size;

	Return(index_list_length_cons(list, &size, &check));
	if (check)
		return Result(ret, Nil);
	make_index_integer_heap(ret, size);
	return 0;
}


/*
 *  make-list
 */
_g int make_list_common(addr var, addr rest, addr *ret)
{
	addr element, list;
	size_t size;

	/* argument */
	if (GetIndex_integer(var, &size))
		return fmte_("Too large index value ~S.", var, NULL);
	if (GetPlistConst(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Nil;
	/* make-list */
	for (list = Nil; size--; )
		cons_heap(&list, element, list);
	/* result */
	return Result(ret, list);
}


/*
 *  push
 */
static int single_push_cons(addr *ret,
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
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (cons value r)) */
	Return_getcar(g, &g);
	list_heap(&cons, cons, item, r, NULL);
	list_heap(&x, g, cons, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

static int multiple_push_cons(Execute ptr, addr *ret,
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
	Return(make_gensym_(ptr, &v));
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc(&args, args, g);
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
	nreverse(ret, pos);

	return 0;
}

static int expansion_push_cons(Execute ptr, addr *ret, addr item, addr place, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_push_cons(ret, item, a, b, g, w, r);
	else
		return multiple_push_cons(ptr, ret, item, a, b, g, w, r);
}

_g int push_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &item, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_push_cons(ptr, ret, item, place, env);

error:
	*ret = Nil;
	return fmte_("PUSH argument ~S must be a (push item place) form.", form, NULL);
}


/*
 *  pop
 */
static int single_pop_cons(Execute ptr, addr *ret,
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
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (c r) */
	Return(make_gensym_(ptr, &c));
	list_heap(&x, c, r, NULL);
	cons_heap(&args, x, args);
	/* (g (cdr c)) */
	Return_getcar(g, &g);
	list_heap(&x, cdr, c, NULL);
	list_heap(&x, g, x, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(&x, car, c, NULL);
	list_heap(ret, leta, args, declare, w, x, NULL);

	return 0;
}

static int multiple_pop_cons(Execute ptr, addr *ret,
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
		Return_getcons(list1, &x, &list1);
		Return_getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc(&args, args, g);
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
		Return(make_gensym_(ptr, &y));
		cons_heap(&c, y, c);
		list_heap(&y, cdr, y, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&args, x, args);
	}
	nreverse(&c, c);
	nreverse(&args, args);
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
	nreverse(&args, args);
	cons_heap(&values, values, args);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse(ret, pos);

	return 0;
}

static int expansion_pop_cons(Execute ptr, addr *ret, addr place, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_pop_cons(ptr, ret, a, b, g, w, r);
	else
		return multiple_pop_cons(ptr, ret, a, b, g, w, r);
}

_g int pop_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_pop_cons(ptr, ret, place, env);

error:
	*ret = Nil;
	return fmte_("POP argument ~S must be a (pop place) form.", form, NULL);
}


/*
 *  nth
 */
_g int nth_common(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return getnth_large(list, index, ret);
	else
		return getnth_(list, size, ret);
}


/*
 *  (setf nth)
 */
_g int setf_nth_common(addr value, addr index, addr list)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index value ~S.", index, NULL);
	return setnth_(list, size, value);
}


/*
 *  nthcdr
 */
_g int nthcdr_common(addr index, addr list, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return getnthcdr_large(list, index, ret);
	else
		return getnthcdr_(list, size, ret);
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
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		Return(function_call_cons(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
}

_g int member_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("MEMBER don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_member_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_member_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_member_cons(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  member-if
 */
_g int member_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
}


/*
 *  member-if-not
 */
_g int member_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, next;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, list);
		list = next;
	}

	return Result(ret, Nil);
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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPC arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &pos, call, args));

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCAR arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &pos, call, args));
	cons_heap(&result, pos, result);
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse(ret, result);

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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCAN arguments.", NULL);
	}
	args = next = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &head, call, args));
	result = head;
	gchold_local(local, &hold, 1);
	setgchold(hold, 0, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto finish;
			Return_getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				Return(setlastcdr_safe_(head, pos));
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPL arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &pos, call, args));

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPLIST arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &pos, call, args));
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
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
		cons_heap(&result, pos, result);
		setgchold(hold, 0, result);
	}

finish:
	rollback_local(local, stack);
	nreverse(ret, result);

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
	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few MAPCON arguments.", NULL);
	}
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		Return_getcons(rest, &pos, &rest);
		if (pos == Nil)
			goto finish;
		Return_getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil)
			loop = 0;
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(callclang_apply(ptr, &head, call, args));
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
			if (cdr == Nil)
				loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(callclang_apply(ptr, &pos, call, args));
		/* nconc */
		if (pos != Nil) {
			if (result == Nil) {
				result = head = pos;
				gchold_local(local, &hold, 1);
				setgchold(hold, 0, result);
			}
			else {
				Return(setlastcdr_safe_(head, pos));
				head = pos;
			}
		}
	}

finish:
	rollback_local(local, stack);
	return Result(ret, result);
}


/*
 *  nconc
 */
static int concat_nconc_cons(addr list, addr args)
{
	addr last;

	last = list;
	for (;;) {
		/* update lastcdr */
		while (list != Nil) {
			last = list;
			Return_getcdr(list, &list);
		}

		for (;;) {
			GetCons(args, &list, &args);
			if (args == Nil)
				return setcdr_(last, list);
			if (list == Nil)
				continue;
			if (IsCons(list)) {
				Return_setcdr(last, list);
				break;
			}
			return fmte_("nconc argument ~S must be a list.", list, NULL);
		}
	}

	return 0;
}

_g int nconc_common(addr args, addr *ret)
{
	addr pos;

	/* (nconc) */
	if (args == Nil)
		return Result(ret, Nil);

	/* (nconc object) */
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			return Result(ret, pos);
		if (pos == Nil)
			continue;
		if (IsCons(pos))
			break;
		return fmte_("nconc argument ~S must be a list.", pos, NULL);
	}

	/* (nconc x x ...) */
	Return(concat_nconc_cons(pos, args));
	return Result(ret, pos);
}


/*
 *  append
 */
static int push_append_cons(addr root, addr last, addr *ret)
{
	addr pos;

	if (! IsCons(last))
		return fmte_("The argument ~S must be a list.", last, NULL);
	while (last != Nil) {
		Return_getcons(last, &pos, &last);
		cons_heap(&root, pos, root);
	}
	return Result(ret, root);
}

static int concat_append_cons(addr last, addr args, addr *ret)
{
	addr pos, root;

	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		if (args == Nil) {
			if (pos != Nil) {
				Return(push_append_cons(root, last, &root));
				last = pos;
			}
			break;
		}
		if (pos == Nil) {
			continue;
		}
		Return(push_append_cons(root, last, &root));
		last = pos;
	}
	nreconc(ret, root, last);
	return 0;
}

_g int append_common(addr args, addr *ret)
{
	addr pos;

	/* (append) */
	if (args == Nil)
		return Result(ret, Nil);

	/* (append object) */
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			return Result(ret, pos);
		if (pos == Nil)
			continue;
		if (IsCons(pos))
			break;
		return fmte_("append argument ~S must be a list.", pos, NULL);
	}

	/* (append x x ...) */
	Return(concat_append_cons(pos, args, &pos));
	return Result(ret, pos);
}


/*
 *  revappend
 */
_g int revappend_common(addr list, addr tail, addr *ret)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		cons_heap(&tail, pos, tail);
	}

	return Result(ret, tail);
}


/*
 *  nreconc
 */
_g int nreconc_common(addr list, addr tail, addr *ret)
{
	addr next;

	/* nil */
	if (list == Nil)
		return Result(ret, tail);

	/* loop */
	for (;;) {
		Return_getcdr(list, &next);
		Return_setcdr(list, tail);
		if (next == Nil)
			break;
		tail = list;
		list = next;
	}

	return Result(ret, list);
}


/*
 *  butlast
 */
static int index_butlast_cons_(addr list, size_t index, addr *ret)
{
	addr root, pos;
	size_t size;

	length_list_p(list, &size);
	if (size <= index)
		return Result(ret, Nil);
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
	return 0;
}

static int large_butlast_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large butlast index ~S.", index, NULL);
	}

	return Result(ret, Nil);
}

_g int butlast_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_butlast_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_butlast_cons_(list, index, ret);
	else
		return index_butlast_cons_(list, size, ret);
}


/*
 *  nbutlast
 */
static int index_nbutlast_cons_(addr list, size_t index, addr *ret)
{
	size_t size;

	length_list_p(list, &size);
	if (size <= index)
		return Result(ret, Nil);
	size -= index + 1;
	while (size--)
		GetCdr(list, &list);
	SetCdr(list, Nil);

	return 0;
}

static int large_nbutlast_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large nbutlast index ~S.", index, NULL);
	}

	return Result(ret, Nil);
}

_g int nbutlast_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_nbutlast_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_nbutlast_cons_(list, index, ret);
	else
		return index_nbutlast_cons_(list, size, ret);
}


/*
 *  last
 */
static int index_last_cons_(addr list, size_t index, addr *ret)
{
	size_t size;

	length_list_p(list, &size);
	if (size < index)
		return Result(ret, list);
	size -= index;
	while (size--) {
		Return_getcdr(list, &list);
	}

	return Result(ret, list);
}

static int large_last_cons_(addr list, addr index, addr *ret)
{
	int check;
	addr size;
	size_t value;

	length_list_p(list, &value);
	size = intsizeh(value);
	Return(less_equal_integer_(size, index, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Too large nbutlast index ~S.", index, NULL);
	}

	return Result(ret, list);
}

_g int last_common(addr list, addr index, addr *ret)
{
	size_t size;

	if (index == Unbound)
		return index_last_cons_(list, 1, ret);
	if (GetIndex_integer(index, &size))
		return large_last_cons_(list, index, ret);
	else
		return index_last_cons_(list, size, ret);
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
	nreconc(ret, root, list);
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
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_call_cons(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}

_g int assoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("ASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_assoc_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_assoc_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_assoc_cons(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  assoc-if
 */
_g int assoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  assoc-if-not
 */
_g int assoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcar(cons, &value);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  copy-alist
 */
_g int copy_alist_common(addr list, addr *ret)
{
	addr root, cons, car, cdr;

	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &cons, &list);
		if (cons != Nil) {
			Return_getcons(cons, &car, &cdr);
			cons_heap(&cons, car, cdr);
		}
		cons_heap(&root, cons, root);
	}
	nreverse(ret, root);

	return 0;
}


/*
 *  pairlis
 */
_g int pairlis_common(addr keys, addr data, addr list, addr *ret)
{
	int check1, check2;
	addr car, cdr;

	if (list == Unbound)
		list = Nil;
	for (;;) {
		check1 = (keys == Nil);
		check2 = (data == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			return fmte_("The length of keys isn't equal to the data.", NULL);
		Return_getcons(keys, &car, &keys);
		Return_getcons(data, &cdr, &data);
		cons_heap(&cdr, car, cdr);
		cons_heap(&list, cdr, list);
	}

	return Result(ret, list);
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
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_call_cons(ptr, &check, item, key, call, value, notret));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}

_g int rassoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("RASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_rassoc_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_rassoc_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_rassoc_cons(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  rassoc-if
 */
_g int rassoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  rssoc-if-not
 */
_g int rassoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret)
{
	int check;
	addr key, value, cons;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	while (list != Nil) {
		if (! consp(list))
			return fmte_("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		if (cons == Nil)
			continue;
		Return_getcdr(cons, &value);
		Return(function_if_call_cons(ptr, &check, key, call, value));
		if (! check)
			return Result(ret, cons);
	}

	return Result(ret, Nil);
}


/*
 *  get-properties
 */
_g int get_properties_common(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist)
{
	addr key, value, next, list, check;

	while (plist != Nil) {
		Return_getcons(plist, &key, &next);
		if (! consp(next)) {
			*rkey = *rvalue = *rlist = Nil;
			return fmte_("The proper list ~S must be a cons object.", next, NULL);
		}
		Return_getcons(next, &value, &next);
		for (list = indicator; list != Nil; ) {
			Return_getcons(list, &check, &list);
			if (check == key)
				goto find;
		}
		plist = next;
	}
	*rkey = *rvalue = *rlist = Nil;
	return 0;

find:
	*rkey = key;
	*rvalue = value;
	*rlist = plist;
	return 0;
}


/*
 *  getf
 */
_g int getf_common(addr list, addr key, addr value, addr *ret)
{
	addr x, y;

	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! consp(list)) {
			*ret = Nil;
			return fmte_("The proper list ~S must be a cons object.", list, NULL);
		}
		GetCons(list, &y, &list);
		if (x == key)
			return Result(ret, y);
	}

	return Result(ret, value == Unbound? Nil: value);
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
	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
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
	Return_getcar(g, &g);
	Return(make_gensym_(ptr, &c));
	list_heap(&g, g, c, NULL);
	list_heap(&remplist, remplist, indicator, r, NULL);
	list_heap(&mvbind, mvbind, g, remplist, w, c, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, mvbind, NULL);

	return 0;
}

_g int remf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, indicator;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &place, &args))
		goto error;
	if (! consp_getcons(args, &indicator, &args))
		goto error;
	if (args != Nil)
		goto error;
	return expansion_remf_cons(ptr, ret, place, indicator, env);

error:
	*ret = Nil;
	return fmte_("REMF argument ~S must be a (place indicator) form.", form, NULL);
}


/*
 *  intersection
 */
static int check_intersection_cons(Execute ptr, int *ret,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr right;

	if (key != Nil) {
		Return(callclang_funcall(ptr, &left, key, left, NULL));
	}
	while (list != Nil) {
		Return_getcons(list, &right, &list);
		Return(function_call_cons(ptr, &check, left, key, test, right, notret));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int test_intersection_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (list = Nil; list1 != Nil; ) {
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons(ptr, &check, left, list2, key, test, notret));
		if (check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	return Result(ret, list);
}

_g int intersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("INTERSECTION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_intersection_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_intersection_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_intersection_cons(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  rintersection
 */
static int test_nintersection_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, pos, x, y, z;

	if (list1 == Nil)
		return Result(ret, Nil);
	if (list2 == Nil)
		return Result(ret, Nil);

	/* first */
	list = list1;
	for (;;) {
		Return_getcons(list, &pos, &x);
		Return(check_intersection_cons(ptr, &check, pos, list2, key, test, notret));
		if (check)
			break;
		if (x == Nil) {
			list = Nil;
			goto finish;
		}
		list = x;
	}
	list1 = x;

	/* tail */
	z = list;
	while (list1 != Nil) {
		Return_getcons(list1, &pos, &y);
		Return(check_intersection_cons(ptr, &check, pos, list2, key, test, notret));
		if (! check) {
			list1 = y;
			continue;
		}
		if (list1 == x) {
			z = x;
			list1 = x = y;
			continue;
		}
		GetCar(list1, &pos);
		SetCar(x, pos);
		z = x;
		GetCdr(x, &x);
		list1 = y;
	}
	SetCdr(z, Nil);

finish:
	return Result(ret, list);
}

_g int nintersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NINTERSECTION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nintersection_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nintersection_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nintersection_cons(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  adjoin
 */
static int test_adjoin_cons(Execute ptr, addr *ret,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr find, item, right;

	/* item */
	if (key != Nil) {
		Return(callclang_funcall(ptr, &item, key, left, NULL));
	}
	else {
		item = left;
	}

	/* adjoin */
	for (find = list; find != Nil; ) {
		Return_getcons(find, &right, &find);
		Return(function_call_cons(ptr, &check, item, key, test, right, notret));
		if (check)
			return Result(ret, list);
	}
	cons_heap(ret, left, list);
	return 0;
}

_g int adjoin_common(Execute ptr, addr item, addr list, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("ADJOIN don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_adjoin_cons(ptr, ret, item, list, key, testnot, 1);
	else if (check1)
		return test_adjoin_cons(ptr, ret, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_adjoin_cons(ptr, ret, item, list, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  pushnew
 */
static int single_pushnew_cons(addr *ret,
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
	Return_getcar(g, &g);
	lista_heap(&adjoin, adjoin, item, r, rest, NULL);
	list_heap(&x, g, adjoin, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse(&args, args);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

static int multiple_pushnew_cons(Execute ptr, addr *ret,
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
	Return(make_gensym_(ptr, &v));
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
	nreconc(&args, args, g);
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
	nreverse(ret, pos);

	return 0;
}

static int expansion_pushnew_cons(Execute ptr, addr *ret,
		addr item, addr place, addr rest, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return single_pushnew_cons(ret, item, rest, a, b, g, w, r);
	else
		return multiple_pushnew_cons(ptr, ret, item, rest, a, b, g, w, r);
}

_g int pushnew_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, item, place;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &item, &args))
		goto error;
	if (! consp_getcons(args, &place, &args))
		goto error;
	return expansion_pushnew_cons(ptr, ret, item, place, args, env);

error:
	*ret = Nil;
	return fmte_("PUSH argument ~S "
			"must be a (item place &rest args) form.", form, NULL);
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
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			cons_heap(&list, left, list);
			localhold_set(hold, 0, list);
		}
	}
	localhold_end(hold);
	return Result(ret, list);
}

_g int set_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("SET-DIFFERENCE don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_set_difference_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_set_difference_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_set_difference_cons(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  nset-difference
 */
static int test_nset_difference_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, pos, x, y, z;

	if (list1 == Nil)
		return Result(ret, Nil);
	if (list2 == Nil)
		return Result(ret, list1);

	/* first */
	list = list1;
	for (;;) {
		Return_getcons(list, &pos, &x);
		Return(check_intersection_cons(ptr, &check, pos, list2, key, test, notret));
		if (! check)
			break;
		if (x == Nil) {
			list = Nil;
			goto finish;
		}
		list = x;
	}
	list1 = x;

	/* tail */
	z = list;
	while (list1 != Nil) {
		Return_getcons(list1, &pos, &y);
		Return(check_intersection_cons(ptr, &check, pos, list2, key, test, notret));
		if (check) {
			list1 = y;
			continue;
		}
		if (list1 == x) {
			z = x;
			list1 = x = y;
			continue;
		}
		GetCar(list1, &pos);
		SetCar(x, pos);
		z = x;
		GetCdr(x, &x);
		list1 = y;
	}
	SetCdr(z, Nil);

finish:
	return Result(ret, list);
}


_g int nset_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NSET-DIFFERENCE don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nset_difference_cons(ptr, ret, a, b, key, testnot, 1);
	else if (check1)
		return test_nset_difference_cons(ptr, ret, a, b, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nset_difference_cons(ptr, ret, a, b, key, test, 0);
	}

	return Result(ret, Nil);
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
		Return_getcons(list, &left, &list);
		Return(check_intersection_cons(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* right -> left */
	for (list = list2; list != Nil; ) {
		Return_getcons(list, &left, &list);
		Return(check_intersection_cons(ptr, &check, left, list1, key, test, notret));
		if (! check) {
			cons_heap(&result, left, result);
			localhold_set(hold, 0, result);
		}
	}

	/* result */
	localhold_end(hold);
	return Result(ret, result);
}

_g int set_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("SET-EXCLUSIVE-OR "
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

	return Result(ret, Nil);
}


/*
 *  nset-exclucive-or
 */
static int test_nset_exclusive_or_remove_(Execute ptr, int *ret, addr *rlist,
		addr x, addr list, addr key, addr test, int notret)
{
	int check, remove;
	addr y, z, next, tail;

	/* nil */
	remove = 0;
	if (list == Nil)
		goto finish;

	/* key */
	if (key != Nil) {
		Return(callclang_funcall(ptr, &x, key, x, NULL));
	}

	/* first */
	next = Nil;
	while (list != Nil) {
		Return_getcons(list, &y, &next);
		Return(function_call_cons(ptr, &check, x, key, test, y, notret));
		if (! check)
			break;
		remove = 1;
		list = next;
	}
	if (list == Nil)
		goto finish;

	/* tail */
	tail = list;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &y, &next);
		Return(function_call_cons(ptr, &check, x, key, test, y, notret));
		if (! check) {
			tail = z;
			z = next;
			continue;
		}
		SetCdr(tail, next);
		z = next;
		remove = 1;
	}

finish:
	*ret = remove;
	*rlist = list;
	return 0;
}

static int test_nset_exclusive_or_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr x, z, next, tail;

	/* nil */
	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	/* first */
	next = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &x, &next);
		Return(test_nset_exclusive_or_remove_(ptr,
					&check, &list2, x, list2, key, test, notret));
		if (! check)
			break;
		if (list2 == Nil)
			return Result(ret, next);
		list1 = next;
	}
	if (list1 == Nil)
		return Result(ret, list2);

	/* tail */
	tail = list1;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &x, &next);
		Return(test_nset_exclusive_or_remove_(ptr,
					&check, &list2, x, list2, key, test, notret));
		if (! check) {
			tail = z;
			z = next;
			continue;
		}
		SetCdr(tail, next);
		z = next;
	}

	/* result */
	SetCdr(tail, list2);
	return Result(ret, list1);
}

_g int nset_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("NSET-EXCLUSIVE-OR "
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

	return Result(ret, Nil);
}


/*
 *  subsetp
 */
static int test_subsetp_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left, result;

	if (list1 == Nil)
		return Result(ret, T);
	if (list2 == Nil)
		return Result(ret, Nil);

	for (result = T; list1 != Nil; ) {
		Return_getcons(list1, &left, &list1);
		Return(check_intersection_cons(ptr, &check, left, list2, key, test, notret));
		if (! check) {
			result = Nil;
			break;
		}
	}

	return Result(ret, result);
}

_g int subsetp_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		return fmte_("SUBSETP don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		return test_subsetp_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_subsetp_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_subsetp_cons(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  nunion
 */
static int test_nunion_remove_(Execute ptr, addr *ret,
		addr x, addr list, addr key, addr test, int notret)
{
	int check;
	addr y, z, next, tail;

	/* nil */
	if (list == Nil)
		return Result(ret, Nil);

	/* key */
	if (key != Nil) {
		Return(callclang_funcall(ptr, &x, key, x, NULL));
	}

	/* first */
	Return_getcons(list, &y, &next);
	Return(function_call_cons(ptr, &check, x, key, test, y, notret));
	if (check)
		return Result(ret, next);

	/* tail */
	tail = list;
	z = next;
	while (z != Nil) {
		Return_getcons(z, &y, &next);
		Return(function_call_cons(ptr, &check, x, key, test, y, notret));
		if (check) {
			SetCdr(tail, next);
			break;
		}
		tail = z;
		z = next;
	}

	return Result(ret, list);
}

static int test_nunion_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	addr list, x, next;

	/* nil */
	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	/* nunion */
	list = list1;
	for (;;) {
		Return_getcons(list, &x, &next);
		Return(test_nunion_remove_(ptr, &list2, x, list2, key, test, notret));
		if (next == Nil) {
			SetCdr(list, list2);
			break;
		}
		list = next;
	}

	return Result(ret, list1);
}

_g int nunion_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)
{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("UNION/NUNION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_nunion_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_nunion_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_nunion_cons(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}


/*
 *  union
 */
static int test_union_cons(Execute ptr, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	LocalHold hold;

	if (list1 == Nil)
		return Result(ret, list2);
	if (list2 == Nil)
		return Result(ret, list1);

	copy_list_heap_safe(&list1, list1);
	copy_list_heap_safe(&list2, list2);

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, list1, list2, NULL);
	Return(test_nunion_cons(ptr, ret, list1, list2, key, test, notret));
	localhold_end(hold);

	return 0;
}

_g int union_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret)

{
	int check1, check2;
	addr key, test, testnot;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test))
		test = Unbound;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &testnot))
		testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		return fmte_("UNION/NUNION don't accept "
				"both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		return test_union_cons(ptr, ret, list1, list2, key, testnot, 1);
	else if (check1)
		return test_union_cons(ptr, ret, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		return test_union_cons(ptr, ret, list1, list2, key, test, 0);
	}

	return Result(ret, Nil);
}

