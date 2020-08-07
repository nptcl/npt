/*
 *  Reference:
 *    Common Lisp the Language, 2nd Edition
 *    Guy L. Steele, Thinking Machines, Inc., Digital Press, 1990.
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
 *
 *    Appendix C.  Backquote
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html
 */
#include "build.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "cons.h"
#include "cons_list.h"
#include "equal.h"
#include "heap.h"
#include "object.h"
#include "quote.h"
#include "reader.h"
#include "stream.h"

enum QuoteType {
	QuoteType_Back,
	QuoteType_Comma,
	QuoteType_AtSign,
	QuoteType_Dot,
	QuoteExecute_Quote,
	QuoteExecute_Append,
	QuoteExecute_Nconc,
	QuoteExecute_List,
	QuoteExecute_Lista,
	QuoteExecute_Clobberable,
	QuoteType_Size
};

enum QuoteIndex {
	QuoteIndex_Value,
	QuoteIndex_Print,
	QuoteIndex_Size
};

#define RefQuote			RefArrayA2
#define GetQuote			GetArrayA2
#define SetQuote			SetArrayA2
#define RefQuoteType(x)		((enum QuoteType)GetUser(x))
#define GetQuoteType(x,v)	(*(v) = (enum QuoteType)GetUser(x))
#define SetQuoteType(x,v)	SetUser((x), (byte)(v))

static int bq_simplify_p = 1;

static void quote2_heap(addr *ret, enum QuoteType type, addr value, addr print)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_QUOTE, QuoteIndex_Size);
	SetQuote(pos, QuoteIndex_Value, value);
	SetQuote(pos, QuoteIndex_Print, print);
	SetQuoteType(pos, type);
	*ret = pos;
}

static void quote_heap(addr *ret, enum QuoteType type, addr value)
{
	quote2_heap(ret, type, value, value);
}

_g void getvalue_quote(addr pos, addr *ret)
{
	Check(! quotep(pos), "type error");
	GetQuote(pos, QuoteIndex_Value, ret);
}

_g void getprint_quote(addr pos, addr *ret)
{
	Check(! quotep(pos), "type error");
	GetQuote(pos, QuoteIndex_Print, ret);
}

_g int quotep(addr pos)
{
	return GetType(pos) == LISPTYPE_QUOTE;
}

static int quote_type_p(addr pos, enum QuoteType type)
{
	return GetType(pos) == LISPTYPE_QUOTE
		&& RefQuoteType(pos) == type;
}

_g int quote_back_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Back);
}

_g int quote_comma_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Comma);
}

_g int quote_atsign_p(addr pos)
{
	return quote_type_p(pos, QuoteType_AtSign);
}

_g int quote_dot_p(addr pos)
{
	return quote_type_p(pos, QuoteType_Dot);
}

_g int quote_quote_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Quote);
}

_g int quote_append_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Append);
}

_g int quote_nconc_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Nconc);
}

_g int quote_list_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_List);
}

_g int quote_lista_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Lista);
}

_g int quote_clobberable_p(addr pos)
{
	return quote_type_p(pos, QuoteExecute_Clobberable);
}


/*
 *  bq_process
 */
static int bq_process_(addr pos, addr *ret);

static int bq_atom(addr pos)
{
	return (! quotep(pos)) && atom_function(pos);
}

static int bq_bracket_(addr pos, addr *ret)
{
	/* `(... atom) */
	if (bq_atom(pos)) {
		Return(bq_process_(pos, &pos));
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* `(... ,expr) */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, &pos);
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* `(... ,@expr) */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, ret);
		return 0;
	}

	/* `(... ,.expr) */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		quote_heap(ret, QuoteExecute_Clobberable, pos);
		return 0;
	}

	/* `(... [quote]) */
	if (quotep(pos)) {
		getvalue_quote(pos, &pos);
		return fmte_("quote error, ~S", pos, NULL);
	}

	/* others */
	Return(bq_process_(pos, &pos));
	conscar_heap(&pos, pos);
	quote_heap(ret, QuoteExecute_List, pos);
	return 0;
}

static int bq_process_list_(addr pos, addr *ret)
{
	addr root, car;

	root = Nil;
	for (;;) {
		Return_getcons(pos, &car, &pos);
		Return(bq_bracket_(car, &car));
		cons_heap(&root, car, root);

		/* nil */
		if (pos == Nil) {
			break;
		}

		/* dot list */
		if (bq_atom(pos)) {
			quote_heap(&pos, QuoteExecute_Quote, pos);
			break;
		}

		/* `(x . ,pos)  */
		if (quote_comma_p(pos)) {
			getvalue_quote(pos, &pos);
			break;
		}

		/* `(x . ,@pos) error */
		if (quote_atsign_p(pos)) {
			getvalue_quote(pos, &pos);
			return fmte_("Dotted ,@~S", pos, NULL);
		}

		/* `(x . ,.pos) error */
		if (quote_dot_p(pos)) {
			getvalue_quote(pos, &pos);
			return fmte_("Dotted ,.~S", pos, NULL);
		}

		/* `(x . [quote]) */
		if (quotep(pos))
			return fmte_("quote error ~S.", pos, NULL);
	}

	/* *bq-append* */
	if (pos != Nil)
		conscar_heap(&pos, pos);
	Return(nreconc_safe_(&root, root, pos));
	quote_heap(ret, QuoteExecute_Append, root);
	return 0;
}

static int bq_process_(addr pos, addr *ret)
{
	/* `atom */
	if (bq_atom(pos)) {
		quote_heap(ret, QuoteExecute_Quote, pos);
		return 0;
	}

	/* ``... */
	if (quote_back_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_process_(pos, ret);
	}

	/* ,expr */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, ret);
		return 0;
	}

	/* ,@expr error */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, &pos);
		return fmte_(",@~S after `", pos, NULL);
	}

	/* ,.expr error */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		return fmte_(",.~S after `", pos, NULL);
	}

	/* [quote] */
	if (quotep(pos)) {
		return fmte_("quote error, ~S", pos, NULL);
	}

	/* list */
	return bq_process_list_(pos, ret);
}


/*
 *  bq_simplify
 */
static int bq_maptree_(int (*call)(addr, addr *), addr pos, addr *ret);

static int bq_maptree_quote_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	enum QuoteType type;
	addr a, b;

	GetQuoteType(pos, &type);
	getvalue_quote(pos, &a);
	Return(bq_maptree_(call, a, &b));
	if (a == b)
		return Result(ret, pos);

	getprint_quote(pos, &a);
	quote2_heap(ret, type, b, a);
	return 0;
}

static int bq_maptree_cons_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	addr car, cdr, a, b;

	Return_getcons(pos, &car, &cdr);
	Return((*call)(car, &a));
	Return(bq_maptree_(call, cdr, &b));
	if (car == a && cdr == b)
		*ret = pos;
	else
		cons_heap(ret, a, b);
	
	return 0;
}

static int bq_maptree_(int (*call)(addr, addr *), addr pos, addr *ret)
{
	if (bq_atom(pos)) {
		return (*call)(pos, ret);
	}
	if (quotep(pos)) {
		return bq_maptree_quote_(call, pos, ret);
	}
	else {
		return bq_maptree_cons_(call, pos, ret);
	}
}

static int bq_null_or_quoted(addr pos)
{
	return pos == Nil || quote_quote_p(pos);
}

static void getvalue_null_or_quoted(addr pos, addr *ret)
{
	if (pos == Nil)
		*ret = Nil;
	else
		getvalue_quote(pos, ret);
}

static int quote_nil_p(addr pos)
{
	if (! quote_quote_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	return pos == Nil;
}

static int bq_splicing_frob(addr pos)
{
	return quote_atsign_p(pos) || quote_dot_p(pos);
}

static int bq_attach_concat_(addr pos, addr result, addr *ret,
		enum QuoteType QuoteType_value,
		int (*quote_call_p)(addr))
{
	/* (append '(a b c) '(d e f g)) => (a b c d e f g) */
	if (bq_null_or_quoted(pos) && bq_null_or_quoted(result)) {
		getvalue_null_or_quoted(pos, &pos);
		getvalue_null_or_quoted(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_Quote, pos);
		return 0;
	}

	/* (append item nil) */
	if (result == Nil || quote_nil_p(result)) {
		if (bq_splicing_frob(pos)) {
			conscar_heap(&pos, pos);
			quote_heap(ret, QuoteType_value, pos);
		}
		else {
			*ret = pos;
		}
		return 0;
	}

	/* (append item '(append a b c)) -> (append item a b c) */
	if ((*quote_call_p)(result)) {
		getvalue_quote(result, &result);
		cons_heap(&pos, pos, result);
		quote_heap(ret, QuoteType_value, pos);
		return 0;
	}

	/* otherwise */
	list_heap(&pos, pos, result, NULL);
	quote_heap(ret, QuoteType_value, pos);
	return 0;
}

static int bq_attach_append_(addr pos, addr result, addr *ret)
{
	return bq_attach_concat_(pos, result, ret, QuoteExecute_Append, quote_append_p);
}

static int bq_attach_nconc_(addr pos, addr result, addr *ret)
{
	return bq_attach_concat_(pos, result, ret, QuoteExecute_Nconc, quote_nconc_p);
}

static int bq_notany_splicing_frob_(addr pos, int *ret)
{
	addr cons;

	getvalue_quote(pos, &cons);
	while (cons != Nil) {
		Return_getcons(cons, &pos, &cons);
		if (bq_splicing_frob(pos))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bq_every_null_or_quoted_(addr list, int *ret)
{
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! bq_null_or_quoted(pos))
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bq_attach_conses_mapcar_(addr list, addr result, addr *ret)
{
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		getvalue_null_or_quoted(pos, &pos);
		cons_heap(&root, pos, root);
	}
	getvalue_null_or_quoted(result, &result);
	Return(nreconc_safe_(&root, root, result));
	quote_heap(ret, QuoteExecute_Quote, root);

	return 0;
}

/* (list* ,@pos result) */
static int bq_attach_conses_p_(addr pos, addr result, int *ret)
{
	if (! bq_null_or_quoted(result))
		return Result(ret, 0);
	else
		return bq_every_null_or_quoted_(pos, ret);
}

static int bq_attach_conses_(addr pos, addr result, addr *ret)
{
	int check;

	/* (list* 'a 'b 'c 'd) -> '(a b c . d) */
	Return(bq_attach_conses_p_(pos, result, &check));
	if (check) {
		return bq_attach_conses_mapcar_(pos, result, ret);
	}

	/* (list* a b c nil) -> (list a b c) */
	if (result == Nil || quote_nil_p(result)) {
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* (list* a b c (list* d e f g)) -> (list* a b c d e f g) */
	if (quote_lista_p(result)) {
		getvalue_quote(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_Lista, pos);
		return 0;
	}

	/* (list* a b c (list d e f g)) -> (list a b c d e f g) */
	if (quote_list_p(result)) {
		getvalue_quote(result, &result);
		Return(append2_safe_(pos, result, &pos));
		quote_heap(ret, QuoteExecute_List, pos);
		return 0;
	}

	/* otherwise */
	conscar_heap(&result, result);
	Return(append2_safe_(pos, result, &pos));
	quote_heap(ret, QuoteExecute_Lista, pos);

	return 0;
}

static int bq_attach_conses_lista_(addr pos, addr root, addr *ret)
{
	addr but, last;

	getvalue_quote(pos, &pos);
	butandlast_safe(&but, &last, pos, 1);
	GetCar(last, &last);
	Return(bq_attach_append_(last, root, &last));
	return bq_attach_conses_(but, last, ret);
}

static int bq_frob(addr pos)
{
	return quote_comma_p(pos)
		|| quote_atsign_p(pos)
		|| quote_dot_p(pos);
}

static int bq_simplify_quote_p(addr pos, addr *ret)
{
	addr car;

	/* (quote (x)) => x */
	if (! quote_quote_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCons(pos, &car, &pos);
	if (bq_frob(car))
		return 0;
	if (pos != Nil)
		return 0;
	*ret = car;

	return 1;
}

static int bq_simplify_list_p_(addr pos, int *ret)
{
	if (! quote_list_p(pos))
		return Result(ret, 0);
	else
		return bq_notany_splicing_frob_(pos, ret);
}

static int bq_simplify_lista_p_(addr pos, int *ret)
{
	if (! quote_lista_p(pos))
		return Result(ret, 0);
	else
		return bq_notany_splicing_frob_(pos, ret);
}

static int bq_simplify_args_(addr args, addr *ret)
{
	int check;
	addr pos, root;

	Return(reverse_list_heap_safe_(&args, args));
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		/* (append atom root) */
		if (bq_atom(pos)) {
			Return(bq_attach_append_(pos, root, &root));
			continue;
		}

		/* (append (list a b c) root) -> (list* a b c root) */
		Return(bq_simplify_list_p_(pos, &check));
		if (check) {
			getvalue_quote(pos, &pos);
			Return(bq_attach_conses_(pos, root, &root));
			continue;
		}

		/* (append (list* a b c) root) -> (list* a b (append c root)) */
		Return(bq_simplify_lista_p_(pos, &check));
		if (check) {
			Return(bq_attach_conses_lista_(pos, root, &root));
			continue;
		}

		/* (append (quote (x)) root) -> (list* (quote x) root) */
		if (bq_simplify_quote_p(pos, &pos)) {
			quote_heap(&pos, QuoteExecute_Quote, pos);
			conscar_heap(&pos, pos);
			Return(bq_attach_conses_(pos, root, &root));
			continue;
		}

		/* (append (clobberable x) root) -> (nconc x foo) */
		if (quote_clobberable_p(pos)) {
			getvalue_quote(pos, &pos);
			Return(bq_attach_nconc_(pos, root, &root));
			continue;
		}

		/* otherwise */
		Return(bq_attach_append_(pos, root, &root));
	}

	return Result(ret, root);
}

static int bq_simplify_(addr pos, addr *ret)
{
	enum QuoteType type;
	addr value, print;

	/* atom */
	if (bq_atom(pos)) {
		return Result(ret, pos);
	}

	/* quote_quote */
	if (quote_quote_p(pos)) {
		goto append;
	}

	/* quote */
	if (quotep(pos)) {
		GetQuoteType(pos, &type);
		getvalue_quote(pos, &value);
		getprint_quote(pos, &print);
		Return(bq_maptree_(bq_simplify_, value, &value));
		quote2_heap(&pos, type, value, print);
		goto append;
	}

	/* list */
	Return(bq_maptree_(bq_simplify_, pos, &pos));
	goto append;

append:
	/* (append ...) */
	if (quote_append_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_simplify_args_(pos, ret);
	}

	return Result(ret, pos);
}


/*
 *  bq_remove_tokens
 */
static int bq_remove_tokens_(addr pos, addr *ret);
static int bq_remove_tokens_list_(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	cons_heap(ret, common, pos);

	return 0;
}

static int bq_remove_tokens_args_(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	list_heap(ret, common, pos, NULL);

	return 0;
}

static int quote_lista_cons_p(addr pos)
{
	/* (list* a b) */
	if (! quote_lista_p(pos))
		return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCdr(pos, &pos);
	if (! consp(pos))
		return 0;
	GetCdr(pos, &pos);

	return pos == Nil;
}

static int bq_remove_tokens_cons_(addr pos, addr *ret)
{
	addr common;

	/* (conc a b) */
	getvalue_quote(pos, &pos);
	Return(bq_maptree_(bq_remove_tokens_, pos, &pos));
	GetConst(COMMON_CONS, &common);
	cons_heap(ret, common, pos);

	return 0;
}

static int bq_remove_tokens_(addr pos, addr *ret)
{
	/* (list ...) */
	if (quote_list_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_LIST, pos, ret);
	}

	/* (append ...) */
	if (quote_append_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_APPEND, pos, ret);
	}

	/* (nconc ...) */
	if (quote_nconc_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_NCONC, pos, ret);
	}

	/* (list* a b) -> (conc a b) */
	if (quote_lista_cons_p(pos)) {
		return bq_remove_tokens_cons_(pos, ret);
	}

	/* (list* ...) */
	if (quote_lista_p(pos)) {
		return bq_remove_tokens_list_(CONSTANT_COMMON_LISTA, pos, ret);
	}

	/* (quote x) */
	if (quote_quote_p(pos)) {
		return bq_remove_tokens_args_(CONSTANT_COMMON_QUOTE, pos, ret);
	}

	/* (clobberable x) -> x */
	if (quote_clobberable_p(pos)) {
		getvalue_quote(pos, &pos);
		return bq_remove_tokens_(pos, ret);
	}

	/* x */
	if (bq_atom(pos)) {
		return Result(ret, pos);
	}

	/* (a . b) */
	return bq_maptree_(bq_remove_tokens_, pos, ret);
}


/*
 *  interface
 */
static int bq_completely_process_(addr pos, addr *ret)
{
	Return(bq_process_(pos, &pos));
	if (bq_simplify_p) {
		Return(bq_simplify_(pos, &pos));
	}
	return bq_remove_tokens_(pos, ret);
}

_g int quote_back_heap_(addr *ret, addr form)
{
	addr pos, value;

	Return(bq_completely_process_(form, &value));
	quote2_heap(&pos, QuoteType_Back, value, form);
	return Result(ret, pos);
}

_g void quote_comma_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_Comma, form);
}

_g void quote_atsign_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_AtSign, form);
}

_g void quote_dot_heap(addr *ret, addr form)
{
	quote_heap(ret, QuoteType_Dot, form);
}

