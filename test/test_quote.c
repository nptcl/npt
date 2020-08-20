#include "quote.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_quote_heap(void)
{
	addr pos, check;

	quote_heap(&pos, QuoteExecute_Nconc, T);
	test(GetType(pos) == LISPTYPE_QUOTE, "quote_heap1");
	test(RefQuoteType(pos) == QuoteExecute_Nconc, "quote_heap2");
	GetQuote(pos, QuoteIndex_Value, &check);
	test(check == T, "quote_heap3");
	GetQuote(pos, QuoteIndex_Print, &check);
	test(check == T, "quote_heap4");

	test(quote_nconc_p(pos), "quote_heap5");
	test(! quote_back_p(pos), "quote_heap6");

	quote_heap(&pos, QuoteType_Back, Nil);
	test(quote_back_p(pos), "quote_heap7");
	quote_heap(&pos, QuoteType_Comma, Nil);
	test(quote_comma_p(pos), "quote_heap8");
	quote_heap(&pos, QuoteType_AtSign, Nil);
	test(quote_atsign_p(pos), "quote_heap9");
	quote_heap(&pos, QuoteType_Dot, Nil);
	test(quote_dot_p(pos), "quote_heap10");

	RETURN;
}


/*
 *  bq_process
 */
static int test_bq_atom(void)
{
	addr pos;

	fixnum_heap(&pos, 10);
	test(bq_atom(pos), "bq_atom1");
	consnil_heap(&pos);
	test(! bq_atom(pos), "bq_atom2");
	quote_heap(&pos, QuoteExecute_Append, Nil);
	test(! bq_atom(pos), "bq_atom3");

	RETURN;
}

static int test_bq_bracket(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	bq_bracket_(pos, &pos);
	test(quote_list_p(pos), "bq_bracket1");
	getvalue_quote(pos, &pos);
	test(consp(pos), "bq_bracket2");
	GetCons(pos, &check, &pos);
	test(quote_quote_p(check), "bq_bracket3");
	test(pos == Nil, "bq_bracket4");
	getvalue_quote(check, &check);
	test(RefFixnum(check) == 10, "bq_bracket5");

	fixnum_heap(&pos, 20);
	quote_heap(&pos, QuoteType_Comma, pos);
	bq_bracket_(pos, &pos);
	test(quote_list_p(pos), "bq_bracket6");
	getvalue_quote(pos, &pos);
	test(consp(pos), "bq_bracket7");
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 20, "bq_bracket8");
	test(pos == Nil, "bq_bracket9");

	fixnum_heap(&pos, 30);
	quote_heap(&pos, QuoteType_AtSign, pos);
	bq_bracket_(pos, &pos);
	test(RefFixnum(pos) == 30, "bq_bracket10");

	fixnum_heap(&pos, 40);
	quote_heap(&pos, QuoteType_Dot, pos);
	bq_bracket_(pos, &pos);
	test(quote_clobberable_p(pos), "bq_bracket11");
	getvalue_quote(pos, &pos);
	test(RefFixnum(pos) == 40, "bq_bracket12");

	fixnum_heap(&pos, 10);
	list_heap(&pos, pos, NULL);
	bq_bracket_(pos, &pos);
	test(quote_list_p(pos), "bq_bracket13");
	getvalue_quote(pos, &pos);

	RETURN;
}

static int test_bq_process_list(void)
{
	addr pos, check, value;

	readstring_debug(&pos, "(10 20)");
	bq_process_list_(pos, &pos);
	test(quote_append_p(pos), "bq_process_list1");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 2, "bq_process_list2");
	GetCons(pos, &check, &pos);
	test(quote_list_p(check), "bq_process_list3");
	getvalue_quote(check, &check);
	test(consp(check), "bq_process_list4");
	GetCons(check, &value, &check);
	test(quote_quote_p(value), "bq_process_list5");
	getvalue_quote(value, &value);
	test(RefFixnum(value) == 10, "bq_process_list6");
	test(check == Nil, "bq_process_list7");

	GetCons(pos, &check, &pos);
	test(quote_list_p(check), "bq_process_list8");
	getvalue_quote(check, &check);
	test(consp(check), "bq_process_list9");
	GetCons(check, &value, &check);
	test(quote_quote_p(value), "bq_process_list10");
	getvalue_quote(value, &value);
	test(RefFixnum(value) == 20, "bq_process_list11");
	test(check == Nil, "bq_process_list12");

	readstring_debug(&pos, "(10 . 20)");
	bq_process_list_(pos, &pos);
	test(quote_append_p(pos), "bq_process_list13");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 2, "bq_process_list14");
	GetCons(pos, &check, &pos);
	test(quote_list_p(check), "bq_process_list15");
	getvalue_quote(check, &check);
	test(consp(check), "bq_process_list16");
	GetCons(check, &value, &check);
	test(quote_quote_p(value), "bq_process_list17");
	getvalue_quote(value, &value);
	test(RefFixnum(value) == 10, "bq_process_list18");
	test(check == Nil, "bq_process_list19");

	GetCons(pos, &check, &pos);
	test(quote_quote_p(check), "bq_process_list20");
	getvalue_quote(check, &check);
	test(RefFixnum(check) == 20, "bq_process_list21");

	quote_heap(&pos, QuoteType_Comma, readr_debug("20"));
	cons_heap(&pos, readr_debug("10"), pos);
	bq_process_list_(pos, &pos);
	test(quote_append_p(pos), "bq_process_list22");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 2, "bq_process_list23");
	GetCons(pos, &check, &pos);
	test(quote_list_p(check), "bq_process_list24");
	getvalue_quote(check, &check);
	test(consp(check), "bq_process_list25");
	GetCons(check, &value, &check);
	test(quote_quote_p(value), "bq_process_list26");
	getvalue_quote(value, &value);
	test(RefFixnum(value) == 10, "bq_process_list27");
	test(check == Nil, "bq_process_list28");

	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 20, "bq_process_list29");

	RETURN;
}

static int test_bq_process(void)
{
	addr pos, check, value;

	bq_process_(readr_debug("10"), &pos);
	test(quote_quote_p(pos), "bq_process1");
	getvalue_quote(pos, &pos);
	test(RefFixnum(pos) == 10, "bq_process2");

	quote_heap(&pos, QuoteType_Comma, readr_debug("10"));
	bq_process_(pos, &pos);
	test(RefFixnum(pos) == 10, "bq_process3");

	readstring_debug(&pos, "(10 20)");
	bq_process_(pos, &pos);
	test(quote_append_p(pos), "bq_process4");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 2, "bq_process5");
	GetCons(pos, &check, &pos);
	test(quote_list_p(check), "bq_process6");
	getvalue_quote(check, &check);
	test(consp(check), "bq_process7");
	GetCons(check, &value, &check);
	test(quote_quote_p(value), "bq_process8");
	getvalue_quote(value, &value);
	test(RefFixnum(value) == 10, "bq_process9");
	test(check == Nil, "bq_process10");

	RETURN;
}


/*
 *  bq_simplify
 */
static int test_bq_maptree1(addr pos, addr *ret)
{
	if (GetType(pos) == LISPTYPE_FIXNUM)
		fixnum_heap(ret, RefFixnum(pos) + 1);
	else
		*ret = pos;
	return 0;
}

static int test_bq_maptree(void)
{
	addr pos, check;

	fixnum_heap(&pos, 10);
	bq_maptree_(test_bq_maptree1, pos, &pos);
	test(RefFixnum(pos) == 11, "bq_maptree1");

	fixnum_heap(&pos, 20);
	quote_heap(&pos, QuoteExecute_Quote, pos);
	bq_maptree_(test_bq_maptree1, pos, &pos);
	test(quote_quote_p(pos), "bq_maptree2");
	getvalue_quote(pos, &pos);
	test(RefFixnum(pos) == 21, "bq_maptree3");

	readstring_debug(&pos, "(10 20)");
	bq_maptree_(test_bq_maptree1, pos, &pos);
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 11, "bq_maptree4");
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 21, "bq_maptree5");
	test(pos == Nil, "bq_maptree6");

	RETURN;
}

static int test_bq_null_or_quoted(void)
{
	addr pos;

	test(bq_null_or_quoted(Nil), "bq_null_or_quoted1");
	test(! bq_null_or_quoted(T), "bq_null_or_quoted2");
	quote_heap(&pos, QuoteExecute_Quote, T);
	test(bq_null_or_quoted(pos), "bq_null_or_quoted3");

	RETURN;
}

static int test_getvalue_null_or_quoted(void)
{
	addr pos;

	getvalue_null_or_quoted(Nil, &pos);
	test(pos == Nil, "getvalue_null_or_quoted1");
	quote_heap(&pos, QuoteExecute_Quote, T);
	getvalue_null_or_quoted(pos, &pos);
	test(pos == T, "getvalue_null_or_quoted2");

	RETURN;
}

static int test_quote_nil_p(void)
{
	addr pos;

	quote_heap(&pos, QuoteExecute_Quote, Nil);
	test(quote_nil_p(pos), "quote_nil_p1");
	quote_heap(&pos, QuoteExecute_Quote, T);
	test(! quote_nil_p(pos), "quote_nil_p2");
	test(! quote_nil_p(T), "quote_nil_p3");
	test(! quote_nil_p(Nil), "quote_nil_p4");

	RETURN;
}

static int test_bq_splicing_frob(void)
{
	addr pos;

	quote_heap(&pos, QuoteType_AtSign, Nil);
	test(bq_splicing_frob(pos), "bq_splicing_frob1");
	quote_heap(&pos, QuoteType_Dot, Nil);
	test(bq_splicing_frob(pos), "bq_splicing_frob2");
	quote_heap(&pos, QuoteType_Comma, Nil);
	test(! bq_splicing_frob(pos), "bq_splicing_frob3");
	test(! bq_splicing_frob(Nil), "bq_splicing_frob4");

	RETURN;
}

static int test_bq_attach_append(void)
{
	addr pos, left, right;

	bq_attach_append_(readr_debug("10"), Nil, &pos);
	test(RefFixnum(pos) == 10, "bq_attach_append1");

	quote_heap(&pos, QuoteExecute_Quote, Nil);
	bq_attach_append_(readr_debug("20"), pos, &pos);
	test(RefFixnum(pos) == 20, "bq_attach_append2");

	quote_heap(&pos, QuoteType_AtSign, readr_debug("30"));
	bq_attach_append_(pos, Nil, &pos);
	test(quote_append_p(pos), "bq_attach_append3");
	getvalue_quote(pos, &pos);
	test(singlep(pos), "bq_attach_append4");
	GetCar(pos, &pos);
	test(quote_atsign_p(pos), "bq_attach_append5");
	getvalue_quote(pos, &pos);
	test(RefFixnum(pos) == 30, "bq_attach_append6");

	quote_heap(&left, QuoteExecute_Quote, readr_debug("(a b)"));
	quote_heap(&right, QuoteExecute_Quote, readr_debug("(c d e)"));
	bq_attach_append_(left, right, &pos);
	test(quote_quote_p(pos), "bq_attach_append7");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 5, "bq_attach_append8");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("a"), "bq_attach_append9");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("b"), "bq_attach_append10");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("c"), "bq_attach_append11");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("d"), "bq_attach_append12");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("e"), "bq_attach_append13");
	test(pos == Nil, "bq_attach_append14");

	readstring_debug(&left, "a");
	quote_heap(&right, QuoteExecute_Append, readr_debug("(b c)"));
	bq_attach_append_(left, right, &pos);
	test(quote_append_p(pos), "bq_attach_append15");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 3, "bq_attach_append16");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("a"), "bq_attach_append17");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("b"), "bq_attach_append18");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("c"), "bq_attach_append19");
	test(pos == Nil, "bq_attach_append20");

	bq_attach_append_(readr_debug("a"), readr_debug("b"), &pos);
	test(quote_append_p(pos), "bq_attach_append21");
	getvalue_quote(pos, &pos);
	test(length_list_unsafe(pos) == 2, "bq_attach_append22");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("a"), "bq_attach_append23");
	GetCons(pos, &left, &pos);
	test(left == readr_debug("b"), "bq_attach_append24");
	test(pos == Nil, "bq_attach_append25");

	RETURN;
}

static int test_bq_notany_splicing_frob(void)
{
	int check;
	addr pos, a;

	quote_heap(&pos, QuoteExecute_List, Nil);
	bq_notany_splicing_frob_(pos, &check);
	test(check, "bq_notany_splicing_frob1");

	quote_heap(&a, QuoteType_AtSign, Nil);
	list_heap(&a, a, NULL);
	quote_heap(&pos, QuoteExecute_List, a);
	bq_notany_splicing_frob_(pos, &check);
	test(! check, "bq_notany_splicing_frob2");

	list_heap(&a, T, NULL);
	quote_heap(&pos, QuoteExecute_List, a);
	bq_notany_splicing_frob_(pos, &check);
	test(check, "bq_notany_splicing_frob3");

	quote_heap(&a, QuoteType_AtSign, Nil);
	list_heap(&a, T, a, NULL);
	quote_heap(&pos, QuoteExecute_List, a);
	bq_notany_splicing_frob_(pos, &check);
	test(! check, "bq_notany_splicing_frob4");

	quote_heap(&a, QuoteType_AtSign, Nil);
	list_heap(&a, T, T, NULL);
	quote_heap(&pos, QuoteExecute_List, a);
	bq_notany_splicing_frob_(pos, &check);
	test(check, "bq_notany_splicing_frob5");

	RETURN;
}

static int test_bq_every_null_or_quoted(void)
{
	int check;
	addr pos;

	bq_every_null_or_quoted_(Nil, &check);
	test(check, "bq_every_null_or_quoted1");

	readstring_debug(&pos, "(nil)");
	bq_every_null_or_quoted_(pos, &check);
	test(check, "bq_every_null_or_quoted2");

	readstring_debug(&pos, "(t)");
	bq_every_null_or_quoted_(pos, &check);
	test(! check, "bq_every_null_or_quoted3");

	quote_heap(&pos, QuoteExecute_Quote, T);
	list_heap(&pos, Nil, pos, NULL);
	bq_every_null_or_quoted_(pos, &check);
	test(check, "bq_every_null_or_quoted4");

	readstring_debug(&pos, "(nil t)");
	bq_every_null_or_quoted_(pos, &check);
	test(! check, "bq_every_null_or_quoted5");

	RETURN;
}

static int test_bq_attach_conses_mapcar(void)
{
	addr a, b, c, pos, check;

	quote_heap(&a, QuoteExecute_Quote, readr_debug("a"));
	quote_heap(&b, QuoteExecute_Quote, readr_debug("b"));
	quote_heap(&c, QuoteExecute_Quote, readr_debug("c"));
	list_heap(&pos, a, b, Nil, NULL);
	bq_attach_conses_mapcar_(pos, c, &pos);
	test(quote_quote_p(pos), "bq_attach_conses_mapcar1");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_attach_conses_mapcar2");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_attach_conses_mapcar3");
	GetCons(pos, &check, &pos);
	test(check == Nil, "bq_attach_conses_mapcar4");
	test(pos == readr_debug("c"), "bq_attach_conses_mapcar5");

	RETURN;
}

static int test_bq_attach_conses(void)
{
	addr a, b, pos, check;

	quote_heap(&a, QuoteExecute_Quote, readr_debug("a"));
	quote_heap(&b, QuoteExecute_Quote, readr_debug("b"));
	list_heap(&pos, a, NULL);
	bq_attach_conses_(pos, b, &pos);
	test(quote_quote_p(pos), "bq_attach_conses1");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_attach_conses2");
	test(pos == readr_debug("b"), "bq_attach_conses3");

	list_heap(&pos, readr_debug("a"), NULL);
	bq_attach_conses_(pos, Nil, &pos);
	test(quote_list_p(pos), "bq_attach_conses4");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_attach_conses5");
	test(pos == Nil, "bq_attach_conses6");

	list_heap(&a, readr_debug("a"), readr_debug("b"), NULL);
	list_heap(&b, readr_debug("c"), readr_debug("d"), NULL);
	quote_heap(&b, QuoteExecute_Lista, b);
	bq_attach_conses_(a, b, &pos);
	test(quote_lista_p(pos), "bq_attach_conses7");
	getvalue_quote(pos, &pos);
	readstring_debug(&check, "(a b c d)");
	test(equal_debug(pos, check), "bq_attach_conses8");

	list_heap(&a, readr_debug("a"), readr_debug("b"), NULL);
	list_heap(&b, readr_debug("c"), readr_debug("d"), NULL);
	quote_heap(&b, QuoteExecute_List, b);
	bq_attach_conses_(a, b, &pos);
	test(quote_list_p(pos), "bq_attach_conses9");
	getvalue_quote(pos, &pos);
	readstring_debug(&check, "(a b c d)");
	test(equal_debug(pos, check), "bq_attach_conses10");

	list_heap(&a, readr_debug("a"), readr_debug("b"), NULL);
	readstring_debug(&b, "c");
	bq_attach_conses_(a, b, &pos);
	test(quote_lista_p(pos), "bq_attach_conses11");
	getvalue_quote(pos, &pos);
	readstring_debug(&check, "(a b c)");
	test(equal_debug(pos, check), "bq_attach_conses12");

	RETURN;
}

static int test_bq_attach_conses_lista(void)
{
	addr a, b, pos, check;

	readstring_debug(&a, "(a b c)");
	readstring_debug(&b, "d");
	quote_heap(&a, QuoteExecute_List, a);
	bq_attach_conses_lista_(a, b, &pos);
	test(quote_lista_p(pos), "bq_attach_conses_lista1");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_attach_conses_lista2");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_attach_conses_lista3");
	GetCons(pos, &check, &pos);
	test(quote_append_p(check), "bq_attach_conses_lista4");
	test(pos == Nil, "bq_attach_conses_lista5");
	getvalue_quote(check, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("c"), "bq_attach_conses_lista6");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("d"), "bq_attach_conses_lista7");
	test(pos == Nil, "bq_attach_conses_lista8");

	RETURN;
}

static int test_bq_frob(void)
{
	addr pos;

	quote_heap(&pos, QuoteType_Comma, T);
	test(bq_frob(pos), "bq_frob1");
	quote_heap(&pos, QuoteType_AtSign, Nil);
	test(bq_frob(pos), "bq_frob2");
	quote_heap(&pos, QuoteType_Dot, T);
	test(bq_frob(pos), "bq_frob3");
	quote_heap(&pos, QuoteExecute_Quote, T);
	test(! bq_frob(pos), "bq_frob4");
	test(! bq_frob(T), "bq_frob5");

	RETURN;
}

static int test_bq_simplify_quote_p(void)
{
	addr pos;

	list_heap(&pos, readr_debug("x"), NULL);
	quote_heap(&pos, QuoteExecute_Quote, pos);
	test(bq_simplify_quote_p(pos, &pos), "bq_simplify_quote_p1");
	test(pos == readr_debug("x"), "bq_simplify_quote_p2");

	list_heap(&pos, readr_debug("x"), NULL);
	quote_heap(&pos, QuoteExecute_Append, pos);
	test(! bq_simplify_quote_p(pos, &pos), "bq_simplify_quote_p3");

	list_heap(&pos, readr_debug("x"), readr_debug("y"), NULL);
	quote_heap(&pos, QuoteExecute_Quote, pos);
	test(! bq_simplify_quote_p(pos, &pos), "bq_simplify_quote_p4");

	RETURN;
}

static int test_bq_simplify_args(void)
{
	addr a, b, pos, check;

	bq_simplify_args_(Nil, &pos);
	test(pos == Nil, "bq_simplify_args1");

	readstring_debug(&pos, "(a)");
	bq_simplify_args_(pos, &pos);
	test(pos == readr_debug("a"), "bq_simplify_args2");

	readstring_debug(&pos, "(a b)");
	bq_simplify_args_(pos, &pos);
	test(quote_append_p(pos), "bq_simplify_args3");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_simplify_args4");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_simplify_args5");
	test(pos == Nil, "bq_simplify_args6");

	readstring_debug(&a, "(a b)");
	quote_heap(&a, QuoteExecute_List, a);
	readstring_debug(&b, "c");
	list_heap(&pos, a, b, NULL);
	bq_simplify_args_(pos, &pos);
	test(quote_lista_p(pos), "bq_simplify_args7");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(a b c)")), "bq_simplify_args8");

	readstring_debug(&a, "(a b)");
	quote_heap(&a, QuoteExecute_Lista, a);
	readstring_debug(&b, "c");
	list_heap(&pos, a, b, NULL);
	bq_simplify_args_(pos, &pos);
	test(quote_lista_p(pos), "bq_simplify_args9");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_simplify_args10");
	GetCons(pos, &check, &pos);
	test(quote_append_p(check), "bq_simplify_args11");
	test(pos == Nil, "bq_simplify_args12");
	getvalue_quote(check, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_simplify_args13");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("c"), "bq_simplify_args14");
	test(pos == Nil, "bq_simplify_args15");

	readstring_debug(&a, "(a)");
	quote_heap(&a, QuoteExecute_Quote, a);
	readstring_debug(&b, "b");
	list_heap(&pos, a, b, NULL);
	bq_simplify_args_(pos, &pos);
	test(quote_lista_p(pos), "bq_simplify_args16");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(quote_quote_p(check), "bq_simplify_args17");
	getvalue_quote(check, &check);
	test(check == readr_debug("a"), "bq_simplify_args18");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_simplify_args19");
	test(pos == Nil, "bq_simplify_args20");

	quote_heap(&a, QuoteExecute_Clobberable, readr_debug("a"));
	readstring_debug(&b, "b");
	list_heap(&pos, a, b, NULL);
	bq_simplify_args_(pos, &pos);
	test(quote_nconc_p(pos), "bq_simplify_args21");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(check == readr_debug("a"), "bq_simplify_args22");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_simplify_args23");
	test(pos == Nil, "bq_simplify_args24");

	quote_heap(&a, QuoteType_Comma, readr_debug("a"));
	readstring_debug(&b, "b");
	list_heap(&pos, a, b, NULL);
	bq_simplify_args_(pos, &pos);
	test(quote_append_p(pos), "bq_simplify_args25");
	getvalue_quote(pos, &pos);
	GetCons(pos, &check, &pos);
	test(quote_comma_p(check), "bq_simplify_args26");
	GetCons(pos, &check, &pos);
	test(check == readr_debug("b"), "bq_simplify_args27");
	test(pos == Nil, "bq_simplify_args28");

	RETURN;
}

static int test_bq_simplify(void)
{
	addr pos;

	bq_simplify_(readr_debug("10"), &pos);
	test(RefFixnum(pos) == 10, "bq_simplify1");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Quote, pos);
	bq_simplify_(pos, &pos);
	test(quote_quote_p(pos), "bq_simplify2");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(a b c)")), "bq_simplify3");

	RETURN;
}


/*
 *  bq_remove_tokens
 */
static int test_bq_remove_tokens_list(void)
{
	addr pos;

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_List, pos);
	bq_remove_tokens_list_(CONSTANT_COMMON_LIST, pos, &pos);
	test(equal_debug(pos, readr_debug("(list a b c)")), "bq_remove_tokens_list1");

	RETURN;
}

static int test_quote_lista_cons_p(void)
{
	addr pos;

	readstring_debug(&pos, "(a b)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	test(quote_lista_cons_p(pos), "quote_lista_cons_p1");

	readstring_debug(&pos, "(a b)");
	quote_heap(&pos, QuoteExecute_List, pos);
	test(! quote_lista_cons_p(pos), "quote_lista_cons_p2");

	readstring_debug(&pos, "(a)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	test(! quote_lista_cons_p(pos), "quote_lista_cons_p3");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	test(! quote_lista_cons_p(pos), "quote_lista_cons_p4");

	RETURN;
}

static int test_bq_remove_tokens_cons(void)
{
	addr pos;

	readstring_debug(&pos, "(a b)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	bq_remove_tokens_cons_(pos, &pos);
	test(equal_debug(pos, readr_debug("(cons a b)")), "bq_remove_tokens_cons1");

	RETURN;
}

static int test_bq_remove_tokens(void)
{
	addr pos;

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_List, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(list a b c)")), "bq_remove_tokens1");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Append, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(append a b c)")), "bq_remove_tokens2");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Nconc, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(nconc a b c)")), "bq_remove_tokens3");

	readstring_debug(&pos, "(a b)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(cons a b)")), "bq_remove_tokens4");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Lista, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(list* a b c)")), "bq_remove_tokens5");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Quote, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(quote (a b c))")), "bq_remove_tokens6");

	readstring_debug(&pos, "(a b c)");
	quote_heap(&pos, QuoteExecute_Clobberable, pos);
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(a b c)")), "bq_remove_tokens7");

	readstring_debug(&pos, "a");
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("a")), "bq_remove_tokens8");

	readstring_debug(&pos, "(a . b)");
	bq_remove_tokens_(pos, &pos);
	test(equal_debug(pos, readr_debug("(a . b)")), "bq_remove_tokens9");

	RETURN;
}


/*
 *  interface
 */
static int test_quote_back_heap(void)
{
	addr pos;

	readstring_debug(&pos, "`(a ,b)");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(list 'a b)")), "quote_back_heap1");

	readstring_debug(&pos, "`(10 20 ,(+ 10 20))");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(list '10 '20 (+ 10 20))")), "quote_back_heap2");

	readstring_debug(&pos, "`(10 20 ,@(list 30 40))");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(list* '10 '20 (list 30 40))")), "quote_back_heap3");

	readstring_debug(&pos, "``(,,q)");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(list 'list q)")), "quote_back_heap4");

	readstring_debug(&pos, "``(,@,q)");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("q")), "quote_back_heap5");

	readstring_debug(&pos, "``(,,@q)");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(cons 'list q)")), "quote_back_heap6");

	readstring_debug(&pos, "``(,@,@q)");
	getvalue_quote(pos, &pos);
	test(equal_debug(pos, readr_debug("(cons 'append q)")), "quote_back_heap7");

	RETURN;
}


/*
 *  main
 */
static int testcase_quote(void)
{
	TestBreak(test_quote_heap);
	/* bq_process */
	TestBreak(test_bq_atom);
	TestBreak(test_bq_bracket);
	TestBreak(test_bq_process_list);
	TestBreak(test_bq_process);
	/* bq_simplify */
	TestBreak(test_bq_maptree);
	TestBreak(test_bq_null_or_quoted);
	TestBreak(test_getvalue_null_or_quoted);
	TestBreak(test_quote_nil_p);
	TestBreak(test_bq_splicing_frob);
	TestBreak(test_bq_attach_append);
	TestBreak(test_bq_notany_splicing_frob);
	TestBreak(test_bq_every_null_or_quoted);
	TestBreak(test_bq_attach_conses_mapcar);
	TestBreak(test_bq_attach_conses);
	TestBreak(test_bq_attach_conses_lista);
	TestBreak(test_bq_frob);
	TestBreak(test_bq_simplify_quote_p);
	TestBreak(test_bq_simplify_args);
	TestBreak(test_bq_simplify);
	/* bq_remove_tokens */
	TestBreak(test_bq_remove_tokens_list);
	TestBreak(test_quote_lista_cons_p);
	TestBreak(test_bq_remove_tokens_cons);
	TestBreak(test_bq_remove_tokens);
	/* interface */
	TestBreak(test_quote_back_heap);

	return 0;
}

static void testinit_quote(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_common();
	build_reader();
}

int test_quote(void)
{
	DegradeTitle;
	return DegradeCode(quote);
}

