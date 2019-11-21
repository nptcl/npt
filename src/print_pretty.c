#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "integer.h"
#include "object.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "stream.h"
#include "stream_pretty.h"
#include "symbol.h"

_g void expand_pprint_logical_block_common(Execute ptr,
		addr *ret, addr symbol, addr pos,
		addr prefix, addr perline, addr suffix, addr decl, addr body)
{
	/* `(let ((,stream (system::make-pprint-stream ,symbol ',pos
	 *                   ',prefix ',per-line-prefix ',suffix)))
	 *    ,@decl
	 *    (flet ((,call ()
	 *             (unwind-protect
	 *               (catch (system::pprint-catch ,stream)
	 *                 (macrolet
	 *                   ((pprint-exit-if-list-exhausted ()
	 *                      (list (quote system::pprint-exit) (quote ,stream)))
	 *                    (pprint-pop ()
	 *                      (list (quote system::pprint-pop) (quote ,stream))))
	 *                   ,@body))
	 *               (system::pprint-close ,stream))))
	 *      (,call)
	 *      (system::pprint-next ,stream)
	 *      (,call)))
	 */
	addr let, flet, unwind, catch, macrolet, list, quote;
	addr spmake, spcatch, spexit, sppop, spclose, spnext, ppexit, pppop;
	addr x, stream, call;

	/* (check-type symbol symbol) */
	if (! symbolp(symbol))
		TypeError(symbol, SYMBOL);

	/* stream */
	if (symbol == T) {
		/* *terminal-io* */
		GetConst(SPECIAL_TERMINAL_IO, &symbol);
		make_gensym(ptr, &stream);
	}
	else if (symbol == Nil) {
		/* *standard-output* */
		GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
		make_gensym(ptr, &stream);
	}
	else {
		stream = symbol;
	}

	/* body */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FLET, &flet);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_CATCH, &catch);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_PPRINT_EXIT_IF_LIST_EXHAUSTED, &ppexit);
	GetConst(COMMON_PPRINT_POP, &pppop);
	GetConst(SYSTEM_MAKE_PPRINT_STREAM, &spmake);
	GetConst(SYSTEM_PPRINT_CATCH, &spcatch);
	GetConst(SYSTEM_PPRINT_EXIT, &spexit);
	GetConst(SYSTEM_PPRINT_POP, &sppop);
	GetConst(SYSTEM_PPRINT_CLOSE, &spclose);
	GetConst(SYSTEM_PPRINT_NEXT, &spnext);
	make_gensym(ptr, &call);

	list_heap(&spclose, spclose, stream, NULL);
	list_heap(&x, quote, stream, NULL);
	list_heap(&spexit, quote, spexit, NULL);
	list_heap(&spexit, list, spexit, x, NULL);
	list_heap(&ppexit, ppexit, Nil, spexit, NULL);
	list_heap(&sppop, quote, sppop, NULL);
	list_heap(&sppop, list, sppop, x, NULL);
	list_heap(&pppop, pppop, Nil, sppop, NULL);
	list_heap(&x, ppexit, pppop, NULL);
	lista_heap(&macrolet, macrolet, x, body, NULL);
	list_heap(&spcatch, spcatch, stream, NULL);
	list_heap(&catch, catch, spcatch, macrolet, NULL);
	list_heap(&unwind, unwind, catch, spclose, NULL);
	list_heap(&x, call, Nil, unwind, NULL);
	list_heap(&x, x, NULL);
	list_heap(&call, call, NULL);
	list_heap(&spnext, spnext, stream, NULL);
	list_heap(&flet, flet, x, call, spnext, call, NULL);
	list_heap(&pos, quote, pos, NULL);
	list_heap(&prefix, quote, prefix, NULL);
	list_heap(&perline, quote, perline, NULL);
	list_heap(&suffix, quote, suffix, NULL);
	list_heap(&x, spmake, symbol, pos, prefix, perline, suffix, NULL);
	list_heap(&x, stream, x, NULL);
	list_heap(&x, x, NULL);
	/* let */
	conscar_heap(&let, let);
	cons_heap(&let, x, let);
	while (decl != Nil) {
		getcons(decl, &x, &decl);
		cons_heap(&let, x, let);
	}
	cons_heap(&let, flet, let);
	nreverse_list_unsafe(ret, let);
}

static int pprint_throw(Execute ptr, addr stream)
{
	catch_pretty_stream(stream, &stream);
	throw_control(ptr, stream);
	return 1;
}

_g int pprint_exit_common(Execute ptr, addr stream)
{
	addr pos;

	object_pretty_stream(stream, &pos);
	if (pos != Nil)
		return 0;

	suffix_pretty_stream(stream, &pos);
	if (pos != Nil)
		print_string_stream(stream, pos);

	return pprint_throw(ptr, stream);
}

static int pprint_pop_nil(Execute ptr, addr stream)
{
	return 0;
}

static int pprint_pop_cons(Execute ptr, addr stream, addr *ret)
{
	pop_pretty_stream(stream, ret);
	return 0;
}

static int pprint_pop_atom(Execute ptr, addr stream)
{
	int check;
	addr pos;

	check = first_pretty_stream(stream);
	pop_pretty_stream(stream, &pos);
	if (! check)
		print_ascii_stream(stream, ". ");

	return princ_print(ptr, stream, pos)
		|| pprint_throw(ptr, stream);
}

static int pprint_length_check(Execute ptr, addr stream)
{
	addr pos;
	size_t a, b;

	/* *print-length* */
	GetConst(SPECIAL_PRINT_LENGTH, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	getindex_error(pos, &a);
	/* stream */
	b = length_pretty_stream(stream);

	return a < b;
}

static int pprint_circle_check(Execute ptr, addr stream)
{
	addr pos;

	/* *print-circle* */
	GetConst(SPECIAL_PRINT_LENGTH, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (pos == Nil)
		return 0;

	/* TODO */
	return 0;
}

static int pprint_pop_circle(Execute ptr, addr stream, addr *ret)
{
	return 1;
}

_g int pprint_pop_common(Execute ptr, addr stream, addr *ret)
{
	addr pos;

	object_pretty_stream(stream, &pos);
	/* atom */
	if (! listp(pos))
		return pprint_pop_atom(ptr, stream);
	/* length */
	if (pprint_length_check(ptr, stream)) {
		print_ascii_stream(stream, "...");
		return pprint_throw(ptr, stream);
	}
	/* list */
	if (pos == Nil)
		return pprint_pop_nil(ptr, stream);
	/* circle */
	if (pprint_circle_check(ptr, stream))
		return pprint_pop_circle(ptr, stream, ret);
	else
		return pprint_pop_cons(ptr, stream, ret);
}

_g void pprint_fill_common(addr stream, addr pos, int colon)
{
}

_g void pprint_linear_common(addr stream, addr pos, int colon)
{
}

_g void pprint_tabular_common(addr stream, addr pos, int colon, size_t size)
{
}

_g void pprint_indent(int block_p, fixnum n, addr stream)
{
}

_g void pprint_newline_common(enum pprint_newline kind, addr stream)
{
}

_g void pprint_tab_common(enum pprint_tabular kind,
		size_t column, size_t colinc, addr stream)
{
}

_g void print_tab_relative(addr stream, size_t column, size_t colinc)
{
	pprint_tab_common(pprint_tabular_section_relative, column, colinc, stream);

}

_g void print_tab_section(addr stream, size_t column, size_t colinc)
{
	pprint_tab_common(pprint_tabular_section, column, colinc, stream);
}

