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
static void bq_process(addr pos, addr *ret);

static int bq_atom(addr pos)
{
	return (! quotep(pos)) && atom(pos);
}

static void bq_bracket(addr pos, addr *ret)
{
	/* `(... atom) */
	if (bq_atom(pos)) {
		bq_process(pos, &pos);
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return;
	}

	/* `(... ,expr) */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, &pos);
		conscar_heap(&pos, pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return;
	}

	/* `(... ,@expr) */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, ret);
		return;
	}

	/* `(... ,.expr) */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		quote_heap(ret, QuoteExecute_Clobberable, pos);
		return;
	}

	/* `(... [quote]) */
	if (quotep(pos)) {
		getvalue_quote(pos, &pos);
		fmte("quote error, ~S", pos, NULL);
		return;
	}

	/* others */
	bq_process(pos, &pos);
	conscar_heap(&pos, pos);
	quote_heap(ret, QuoteExecute_List, pos);
}

static void bq_process_list(addr pos, addr *ret)
{
	addr root, car;

	root = Nil;
	for (;;) {
		getcons(pos, &car, &pos);
		bq_bracket(car, &car);
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
			fmte("Dotted ,@~S", pos, NULL);
			return;
		}

		/* `(x . ,.pos) error */
		if (quote_dot_p(pos)) {
			getvalue_quote(pos, &pos);
			fmte("Dotted ,.~S", pos, NULL);
			return;
		}

		/* `(x . [quote]) */
		if (quotep(pos)) {
			fmte("quote error ~S.", pos, NULL);
			return;
		}
	}

	/* *bq-append* */
	if (pos != Nil)
		conscar_heap(&pos, pos);
	nreconc(&root, root, pos);
	quote_heap(ret, QuoteExecute_Append, root);
}

static void bq_process(addr pos, addr *ret)
{
	/* `atom */
	if (bq_atom(pos)) {
		quote_heap(ret, QuoteExecute_Quote, pos);
		return;
	}

	/* ``... */
	if (quote_back_p(pos)) {
		getvalue_quote(pos, &pos);
		bq_process(pos, ret);
		return;
	}

	/* ,expr */
	if (quote_comma_p(pos)) {
		getvalue_quote(pos, ret);
		return;
	}

	/* ,@expr error */
	if (quote_atsign_p(pos)) {
		getvalue_quote(pos, &pos);
		fmte(",@~S after `", pos, NULL);
		return;
	}

	/* ,.expr error */
	if (quote_dot_p(pos)) {
		getvalue_quote(pos, &pos);
		fmte(",.~S after `", pos, NULL);
		return;
	}

	/* [quote] */
	if (quotep(pos)) {
		fmte("quote error, ~S", pos, NULL);
		return;
	}

	/* list */
	bq_process_list(pos, ret);
}


/*
 *  bq_simplify
 */
static void bq_maptree(void (*call)(addr, addr *), addr pos, addr *ret);

static void bq_maptree_quote(void (*call)(addr, addr *), addr pos, addr *ret)
{
	enum QuoteType type;
	addr a, b;

	GetQuoteType(pos, &type);
	getvalue_quote(pos, &a);
	bq_maptree(call, a, &b);
	if (a == b)
		*ret = pos;
	else {
		getprint_quote(pos, &a);
		quote2_heap(ret, type, b, a);
	}
}

static void bq_maptree_cons(void (*call)(addr, addr *), addr pos, addr *ret)
{
	addr car, cdr, a, b;

	getcons(pos, &car, &cdr);
	call(car, &a);
	bq_maptree(call, cdr, &b);
	if (car == a && cdr == b)
		*ret = pos;
	else
		cons_heap(ret, a, b);
}

static void bq_maptree(void (*call)(addr, addr *), addr pos, addr *ret)
{
	if (bq_atom(pos)) {
		call(pos, ret);
		return;
	}
	if (quotep(pos)) {
		bq_maptree_quote(call, pos, ret);
		return;
	}
	else {
		bq_maptree_cons(call, pos, ret);
		return;
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
	if (! quote_quote_p(pos)) return 0;
	getvalue_quote(pos, &pos);
	return pos == Nil;
}

static int bq_splicing_frob(addr pos)
{
	return quote_atsign_p(pos) || quote_dot_p(pos);
}

static void bq_attach_concat(addr pos, addr result, addr *ret,
		enum QuoteType QuoteType_value,
		int (*quote_call_p)(addr))
{
	/* (append '(a b c) '(d e f g)) => (a b c d e f g) */
	if (bq_null_or_quoted(pos) && bq_null_or_quoted(result)) {
		getvalue_null_or_quoted(pos, &pos);
		getvalue_null_or_quoted(result, &result);
		append2_safe(pos, result, &pos);
		quote_heap(ret, QuoteExecute_Quote, pos);
		return;
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
		return;
	}

	/* (append item '(append a b c)) -> (append item a b c) */
	if (quote_call_p(result)) {
		getvalue_quote(result, &result);
		cons_heap(&pos, pos, result);
		quote_heap(ret, QuoteType_value, pos);
		return;
	}

	/* otherwise */
	list_heap(&pos, pos, result, NULL);
	quote_heap(ret, QuoteType_value, pos);
}

static void bq_attach_append(addr pos, addr result, addr *ret)
{
	bq_attach_concat(pos, result, ret, QuoteExecute_Append, quote_append_p);
}

static void bq_attach_nconc(addr pos, addr result, addr *ret)
{
	bq_attach_concat(pos, result, ret, QuoteExecute_Nconc, quote_nconc_p);
}

static int bq_notany_splicing_frob(addr pos)
{
	addr cons;

	getvalue_quote(pos, &cons);
	while (cons != Nil) {
		getcons(cons, &pos, &cons);
		if (bq_splicing_frob(pos))
			return 0;
	}

	return 1;
}

static int bq_every_null_or_quoted(addr list)
{
	addr pos;

	while (list != Nil) {
		getcons(list, &pos, &list);
		if (! bq_null_or_quoted(pos))
			return 0;
	}

	return 1;
}

static void bq_attach_conses_mapcar(addr list, addr result, addr *ret)
{
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		getcons(list, &pos, &list);
		getvalue_null_or_quoted(pos, &pos);
		cons_heap(&root, pos, root);
	}
	getvalue_null_or_quoted(result, &result);
	nreconc(&root, root, result);
	quote_heap(ret, QuoteExecute_Quote, root);
}

/* (list* ,@pos result) */
static void bq_attach_conses(addr pos, addr result, addr *ret)
{
	/* (list* 'a 'b 'c 'd) -> '(a b c . d) */
	if (bq_every_null_or_quoted(pos) && bq_null_or_quoted(result)) {
		bq_attach_conses_mapcar(pos, result, ret);
		return;
	}

	/* (list* a b c nil) -> (list a b c) */
	if (result == Nil || quote_nil_p(result)) {
		quote_heap(ret, QuoteExecute_List, pos);
		return;
	}

	/* (list* a b c (list* d e f g)) -> (list* a b c d e f g) */
	if (quote_lista_p(result)) {
		getvalue_quote(result, &result);
		append2_safe(pos, result, &pos);
		quote_heap(ret, QuoteExecute_Lista, pos);
		return;
	}

	/* (list* a b c (list d e f g)) -> (list a b c d e f g) */
	if (quote_list_p(result)) {
		getvalue_quote(result, &result);
		append2_safe(pos, result, &pos);
		quote_heap(ret, QuoteExecute_List, pos);
		return;
	}

	/* otherwise */
	conscar_heap(&result, result);
	append2_safe(pos, result, &pos);
	quote_heap(ret, QuoteExecute_Lista, pos);
}

static void bq_attach_conses_lista(addr pos, addr root, addr *ret)
{
	addr but, last;

	getvalue_quote(pos, &pos);
	butandlast_safe(&but, &last, pos, 1);
	GetCar(last, &last);
	bq_attach_append(last, root, &last);
	bq_attach_conses(but, last, ret);
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
	if (! quote_quote_p(pos)) return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos)) return 0;
	GetCons(pos, &car, &pos);
	if (bq_frob(car)) return 0;
	if (pos != Nil) return 0;
	*ret = car;

	return 1;
}

static void bq_simplify_args(addr args, addr *ret)
{
	addr pos, root;

	reverse_list_heap_safe(&args, args);
	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		/* (append atom root) */
		if (bq_atom(pos)) {
			bq_attach_append(pos, root, &root);
			continue;
		}

		/* (append (list a b c) root) -> (list* a b c root) */
		if (quote_list_p(pos) && bq_notany_splicing_frob(pos)) {
			getvalue_quote(pos, &pos);
			bq_attach_conses(pos, root, &root);
			continue;
		}

		/* (append (list* a b c) root) -> (list* a b (append c root)) */
		if (quote_lista_p(pos) && bq_notany_splicing_frob(pos)) {
			bq_attach_conses_lista(pos, root, &root);
			continue;
		}

		/* (append (quote (x)) root) -> (list* (quote x) root) */
		if (bq_simplify_quote_p(pos, &pos)) {
			quote_heap(&pos, QuoteExecute_Quote, pos);
			conscar_heap(&pos, pos);
			bq_attach_conses(pos, root, &root);
			continue;
		}

		/* (append (clobberable x) root) -> (nconc x foo) */
		if (quote_clobberable_p(pos)) {
			getvalue_quote(pos, &pos);
			bq_attach_nconc(pos, root, &root);
			continue;
		}

		/* otherwise */
		bq_attach_append(pos, root, &root);
	}
	*ret = root;
}

static void bq_simplify(addr pos, addr *ret)
{
	enum QuoteType type;
	addr value, print;

	/* atom */
	if (bq_atom(pos)) {
		*ret = pos;
		return;
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
		bq_maptree(bq_simplify, value, &value);
		quote2_heap(&pos, type, value, print);
		goto append;
	}

	/* list */
	bq_maptree(bq_simplify, pos, &pos);
	goto append;

append:
	/* (append ...) */
	if (quote_append_p(pos)) {
		getvalue_quote(pos, &pos);
		bq_simplify_args(pos, ret);
		return;
	}

	*ret = pos;
}


/*
 *  bq_remove_tokens
 */
static void bq_remove_tokens(addr pos, addr *ret);
static void bq_remove_tokens_list(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	bq_maptree(bq_remove_tokens, pos, &pos);
	cons_heap(ret, common, pos);
}

static void bq_remove_tokens_args(constindex index, addr pos, addr *ret)
{
	addr common;

	getvalue_quote(pos, &pos);
	GetConstant(index, &common);
	bq_maptree(bq_remove_tokens, pos, &pos);
	list_heap(ret, common, pos, NULL);
}

static int quote_lista_cons_p(addr pos)
{
	/* (list* a b) */
	if (! quote_lista_p(pos)) return 0;
	getvalue_quote(pos, &pos);
	if (! consp(pos)) return 0;
	GetCdr(pos, &pos);
	if (! consp(pos)) return 0;
	GetCdr(pos, &pos);

	return pos == Nil;
}

static void bq_remove_tokens_cons(addr pos, addr *ret)
{
	addr common;

	/* (conc a b) */
	getvalue_quote(pos, &pos);
	bq_maptree(bq_remove_tokens, pos, &pos);
	GetConst(COMMON_CONS, &common);
	cons_heap(ret, common, pos);
}

static void bq_remove_tokens(addr pos, addr *ret)
{
	/* (list ...) */
	if (quote_list_p(pos)) {
		bq_remove_tokens_list(CONSTANT_COMMON_LIST, pos, ret);
		return;
	}

	/* (append ...) */
	if (quote_append_p(pos)) {
		bq_remove_tokens_list(CONSTANT_COMMON_APPEND, pos, ret);
		return;
	}

	/* (nconc ...) */
	if (quote_nconc_p(pos)) {
		bq_remove_tokens_list(CONSTANT_COMMON_NCONC, pos, ret);
		return;
	}

	/* (list* a b) -> (conc a b) */
	if (quote_lista_cons_p(pos)) {
		bq_remove_tokens_cons(pos, ret);
		return;
	}

	/* (list* ...) */
	if (quote_lista_p(pos)) {
		bq_remove_tokens_list(CONSTANT_COMMON_LISTA, pos, ret);
		return;
	}

	/* (quote x) */
	if (quote_quote_p(pos)) {
		bq_remove_tokens_args(CONSTANT_COMMON_QUOTE, pos, ret);
		return;
	}

	/* (clobberable x) -> x */
	if (quote_clobberable_p(pos)) {
		getvalue_quote(pos, &pos);
		bq_remove_tokens(pos, ret);
		return;
	}

	/* x */
	if (bq_atom(pos)) {
		*ret = pos;
		return;
	}

	/* (a . b) */
	bq_maptree(bq_remove_tokens, pos, ret);
}


/*
 *  interface
 */
static void bq_completely_process(addr pos, addr *ret)
{
	bq_process(pos, &pos);
	if (bq_simplify_p)
		bq_simplify(pos, &pos);
	bq_remove_tokens(pos, ret);
}

_g void quote_back_heap(addr *ret, addr form)
{
	addr pos, value;

	bq_completely_process(form, &value);
	quote2_heap(&pos, QuoteType_Back, value, form);
	*ret = pos;
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

