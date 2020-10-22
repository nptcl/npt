#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_operator.h"
#include "eastasian.h"
#include "heap.h"
#include "gc.h"
#include "integer.h"
#include "object.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_pretty.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

enum print_pretty {
	print_pretty_newline_linear,
	print_pretty_newline_fill,
	print_pretty_newline_miser,
	print_pretty_newline_mandatory,
	print_pretty_newline_terpri,
	print_pretty_indent_block,
	print_pretty_indent_current,
	print_pretty_tabular_line,
	print_pretty_tabular_section,
	print_pretty_tabular_liner,
	print_pretty_tabular_sectionr
};

struct print_pretty_struct {
	unsigned colon : 1;
	enum print_pretty type;
	fixnum value, colinc;
};

static struct print_pretty_struct *struct_print_pretty(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_PRETTY);
	return (struct print_pretty_struct *)PtrBodySS(pos);
}

static void print_pretty_heap(addr *ret, enum print_pretty type)
{
	addr pos;
	struct print_pretty_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_PRETTY,
			1UL, sizeoft(struct print_pretty_struct));
	str = struct_print_pretty(pos);
	str->type = type;
	*ret = pos;
}

static int print_pretty_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_PRINT_PRETTY;
}

static int pretty_print_newline_p(addr pos)
{
	if (! print_pretty_p(pos))
		return 0;

	switch (struct_print_pretty(pos)->type) {
		case print_pretty_newline_linear:
		case print_pretty_newline_fill:
		case print_pretty_newline_miser:
		case print_pretty_newline_mandatory:
		case print_pretty_newline_terpri:
			return 1;

		default:
			return 0;
	}
}

static int pretty_print_linear_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_linear;
}

static int pretty_print_miser_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_miser;
}

static int pretty_print_fill_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_fill;
}

static int pretty_print_mandatory_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_mandatory;
}

static int pretty_print_terpri_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_newline_terpri;
}

static int pretty_print_force_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_newline_mandatory
		|| str->type == print_pretty_newline_terpri;
}

static int pretty_print_indent_block_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_indent_block;
}

static int pretty_print_indent_current_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_indent_current;
}

static int pretty_print_indent_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_indent_block
		|| str->type == print_pretty_indent_current;
}

static int pretty_print_tabular_line_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_line;
}

static int pretty_print_tabular_section_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_section;
}

static int pretty_print_tabular_liner_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_liner;
}

static int pretty_print_tabular_sectionr_p(addr pos)
{
	return print_pretty_p(pos)
		&& struct_print_pretty(pos)->type == print_pretty_tabular_sectionr;
}

static int pretty_print_tabular_p(addr pos)
{
	struct print_pretty_struct *str;

	if (! print_pretty_p(pos))
		return 0;
	str = struct_print_pretty(pos);
	return str->type == print_pretty_tabular_line
		|| str->type == print_pretty_tabular_section
		|| str->type == print_pretty_tabular_liner
		|| str->type == print_pretty_tabular_sectionr;
}

static void fixnum_pretty_heap(addr *ret, enum print_pretty type, fixnum a)
{
	addr pos;
	struct print_pretty_struct *str;

	print_pretty_heap(&pos, type);
	str = struct_print_pretty(pos);
	str->value = a;
	*ret = pos;
}

static void size2_pretty_heap(addr *ret, enum print_pretty type, fixnum a, fixnum b)
{
	addr pos;
	struct print_pretty_struct *str;

	print_pretty_heap(&pos, type);
	str = struct_print_pretty(pos);
	str->value = a;
	str->colinc = b;
	*ret = pos;
}


/*
 *  common
 */
_g int expand_pprint_logical_block_common_(addr *ret, addr symbol, addr pos,
		addr prefix, addr perline, addr suffix, addr decl, addr body)
{
	/* `(let ((,symbol (system::make-pprint-stream
	 *                   ,symbol ,pos ,prefix ,perline ,suffix)))
	 *    ,@decl
	 *    (system::pprint-pretty
	 *      ,symbol
	 *      (lambda ()
	 *        (unwind-protect
	 *          (catch (system::pprint-gensym ,symbol)
	 *            (macrolet
	 *              ((pprint-exit-if-list-exhausted ()
	 *                 (list 'system::pprint-exit ',symbol))
	 *               (pprint-pop ()
	 *                 (list 'system::pprint-pop ',symbol)))
	 *              (system::pprint-check ,symbol)
	 *              ,@body))
	 *          (system::pprint-close ,symbol)))))
	 */
	addr let, flet, lambda, unwind, catchs, macrolet, list, quote;
	addr make, pretty, gensym, exit, pop, check, close, ppexit, pppop;
	addr x;

	/* (check-type symbol symbol) */
	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);

	/* symbol */
	if (symbol == T) {
		/* *terminal-io* */
		GetConst(SPECIAL_TERMINAL_IO, &symbol);
	}
	else if (symbol == Nil) {
		/* *standard-output* */
		GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
	}

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FLET, &flet);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_CATCH, &catchs);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_PPRINT_EXIT_IF_LIST_EXHAUSTED, &ppexit);
	GetConst(COMMON_PPRINT_POP, &pppop);
	GetConst(SYSTEM_MAKE_PPRINT_STREAM, &make);
	GetConst(SYSTEM_PPRINT_PRETTY, &pretty);
	GetConst(SYSTEM_PPRINT_GENSYM, &gensym);
	GetConst(SYSTEM_PPRINT_EXIT, &exit);
	GetConst(SYSTEM_PPRINT_POP, &pop);
	GetConst(SYSTEM_PPRINT_CHECK, &check);
	GetConst(SYSTEM_PPRINT_CLOSE, &close);
	/* body */
	list_heap(&close, close, symbol, NULL);
	list_heap(&x, quote, symbol, NULL);
	list_heap(&exit, quote, exit, NULL);
	list_heap(&exit, list, exit, x, NULL);
	list_heap(&ppexit, ppexit, Nil, exit, NULL);
	list_heap(&pop, quote, pop, NULL);
	list_heap(&pop, list, pop, x, NULL);
	list_heap(&pppop, pppop, Nil, pop, NULL);
	list_heap(&x, ppexit, pppop, NULL);
	list_heap(&check, check, symbol, NULL);
	lista_heap(&macrolet, macrolet, x, check, body, NULL);
	list_heap(&gensym, gensym, symbol, NULL);
	list_heap(&catchs, catchs, gensym, macrolet, NULL);
	list_heap(&unwind, unwind, catchs, close, NULL);
	list_heap(&lambda, lambda, Nil, unwind, NULL);
	list_heap(&pretty, pretty, symbol, lambda, NULL);
	list_heap(&x, make, symbol, pos, prefix, perline, suffix, NULL);
	list_heap(&x, symbol, x, NULL);
	list_heap(&x, x, NULL);
	/* let */
	conscar_heap(&let, let);
	cons_heap(&let, x, let);
	while (decl != Nil) {
		Return_getcons(decl, &x, &decl);
		cons_heap(&let, x, let);
	}
	cons_heap(&let, pretty, let);
	nreverse(ret, let);

	return 0;
}

_g int pprint_throw(Execute ptr, addr stream)
{
	Return(gensym_pretty_stream_(stream, &stream));
	return throw_control_(ptr, stream);
}

_g int pprint_exit_common(Execute ptr, addr stream)
{
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	if (pos == Nil)
		return pprint_throw(ptr, stream);

	return 0;
}

static int pprint_pop_atom(Execute ptr, addr stream)
{
	int check;
	addr pos;

	Return(first_pretty_stream_(stream, &check));
	if (! check) {
		Return(print_ascii_stream_(stream, ". "));
	}
	Return(pop_pretty_stream_(stream, &pos, &check));
	Return(write_print(ptr, stream, pos));

	return pprint_throw(ptr, stream);
}

static int pprint_length_check_(Execute ptr, addr stream, int *ret)
{
	int check;
	size_t x, y;

	Return(length_print_(ptr, &x, &check));
	if (! check)
		return Result(ret, 0);
	Return(length_pretty_stream_(stream, &y));

	return Result(ret, x <= y);
}

_g int pprint_pop_common(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr pos;

	Return(root_pretty_stream_(stream, &pos));
	/* atom */
	if (! listp(pos))
		return pprint_pop_atom(ptr, stream);
	/* length */
	Return(pprint_length_check_(ptr, stream, &check));
	if (check) {
		Return(print_ascii_stream_(stream, "..."));
		return pprint_throw(ptr, stream);
	}
	/* list */
	if (pos == Nil)
		return Result(ret, Nil);
	/* circle */
	Return(circle_print_(ptr, &check));
	if (check) {
		Return(first_pretty_stream_(stream, &check));
		if (! check) {
			Return(pprint_pop_circle_(ptr, stream, pos, &check));
			if (check)
				return pprint_throw(ptr, stream);
		}
	}
	/* cons */
	return pop_pretty_stream_(stream, ret, &check);
}

_g int check_pretty_stream(Execute ptr, addr stream)
{
	int check;
	addr root;
	size_t level, depth;

	/* *print-level* */
	Return(level_print_(ptr, &level, &check));
	if (check) {
		getdepth_print_write(ptr, &depth);
		if (level <= depth) {
			setlistp_pretty_stream(stream, 0);
			Return(write_char_stream_(stream, '#'));
			return pprint_throw(ptr, stream);
		}
	}

	/* atom */
	Return(root_pretty_stream_(stream, &root));
	if (! listp(root)) {
		Return(write_print(ptr, stream, root));
		return pprint_throw(ptr, stream);
	}

	/* increment depth */
	setdepth_pretty_stream(ptr, stream, 1);

	/* circle */
	Return(circle_print_(ptr, &check));
	if (check) {
		if (consp(root)) {
			Return(pprint_check_circle_(ptr, root, &root, &check));
			if (check) {
				setlistp_pretty_stream(stream, 0);
				Return(print_string_stream_(stream, root));
				return pprint_throw(ptr, stream);
			}
			if (root != Nil)
				setsharp_pretty_stream(stream, root);
		}
		return 0;
	}

	return 0;
}

static int pretty_common_p_(Execute ptr, addr stream, int *ret)
{
	int check;

	Return(pretty_print_(ptr, &check));
	if (! check)
		return Result(ret, 0);

	return Result(ret, pretty_stream_p(stream));
}

_g int pprint_indent_print_(Execute ptr, int block_p, fixnum n, addr stream)
{
	int check;
	enum print_pretty type;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	type = block_p? print_pretty_indent_block: print_pretty_indent_current;
	fixnum_pretty_heap(&pos, type, n);
	return push_pretty_stream_(stream, pos);
}

_g int pprint_newline_print_(Execute ptr, enum pprint_newline kind, addr stream)
{
	int check;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	switch (kind) {
		case pprint_newline_linear:
			print_pretty_heap(&pos, print_pretty_newline_linear);
			break;

		case pprint_newline_fill:
			print_pretty_heap(&pos, print_pretty_newline_fill);
			break;

		case pprint_newline_miser:
			print_pretty_heap(&pos, print_pretty_newline_miser);
			break;

		case pprint_newline_mandatory:
			print_pretty_heap(&pos, print_pretty_newline_mandatory);
			break;

		default:
			return fmte_("Invalid newline type.", NULL);
	}

	return push_pretty_stream_(stream, pos);
}

_g int pprint_newline_terpri_(addr stream)
{
	addr pos;

	print_pretty_heap(&pos, print_pretty_newline_terpri);
	return push_pretty_stream_(stream, pos);
}

_g int pprint_tab_print_(Execute ptr,
		addr stream, enum pprint_tabular kind, fixnum a, fixnum b)
{
	int check;
	addr pos;

	Return(pretty_common_p_(ptr, stream, &check));
	if (! check)
		return 0;
	switch (kind) {
		case pprint_tabular_line:
			size2_pretty_heap(&pos, print_pretty_tabular_line, a, b);
			break;

		case pprint_tabular_section:
			size2_pretty_heap(&pos, print_pretty_tabular_section, a, b);
			break;

		case pprint_tabular_line_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_liner, a, b);
			break;

		case pprint_tabular_section_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_sectionr, a, b);
			break;

		default:
			return fmte_("Invalid newline type.", NULL);
	}

	return push_pretty_stream_(stream, pos);
}

_g int pprint_tab_section_(Execute ptr, addr stream, fixnum column, fixnum colinc)
{
	return pprint_tab_print_(ptr, stream,
			pprint_tabular_section, column, colinc);
}

_g int pprint_tab_section_relative_(Execute ptr,
		addr stream, fixnum column, fixnum colinc)
{
	return pprint_tab_print_(ptr, stream,
			pprint_tabular_section_relative, column, colinc);
}

static int pprint_tab_output_(addr stream, fixnum size)
{
	for (; 0 < size; size--) {
		Return(write_char_stream_(stream, ' '));
	}

	return 0;
}

static fixnum pprint_colinc_division(fixnum size, fixnum colinc)
{
	return ((size / colinc) + 1) * colinc;
}

static void pprint_tab_absolute_size(fixnum *ret,
		fixnum column, fixnum colinc, fixnum base, fixnum now)
{
	fixnum plus;

	now -= base;
	if (now < column) {
		*ret = column - now;
		return;
	}
	if (colinc == 0) {
		*ret = 0;
		return;
	}
	plus = column + pprint_colinc_division(now - column, colinc);
	*ret = plus - now;
}

static void pprint_tab_relative_size(fixnum *ret,
		fixnum column, fixnum colinc, fixnum base, fixnum now)
{
	now += column - base;
	if (colinc) {
		now %= colinc;
		if (now)
			column += colinc - now;
	}
	*ret = (column < 0)? 0: column;
}

_g int pprint_tab_absolute_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_absolute_size(&now, column, colinc, 0, now);
	return pprint_tab_output_(stream, now);
}

_g int pprint_tab_relative_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_relative_size(&now, column, colinc, 0, now);
	return pprint_tab_output_(stream, now);
}


/*
 *  output
 */
static int write_char_white_(addr stream, size_t *white)
{
	for (; *white; (*white)--) {
		Return(write_char_stream_(stream, ' '));
	}

	return 0;
}

static int pretty_write_string_(addr stream, addr pos, size_t *white)
{
	unicode c;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == ' ') {
			(*white)++;
			continue;
		}
		Return(write_char_white_(stream, white));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static void pretty_write_space(addr stream, addr pos, size_t *white)
{
	size_t value;
	GetIndex(pos, &value);
	*white += value;
}

static int pretty_write_(addr stream, addr list)
{
	addr pos;
	size_t white;

	nreverse(&list, list);
	white = 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (stringp(pos)) {
			Return(pretty_write_string_(stream, pos, &white));
			continue;
		}
		if (GetType(pos) == LISPTYPE_INDEX) {
			pretty_write_space(stream, pos, &white);
			continue;
		}
		if (pos == T) {
			Return(write_char_white_(stream, &white));
			Return(terpri_stream_(stream));
			white = 0;
			continue;
		}
		if (pos == Nil) {
			Return(terpri_stream_(stream));
			white = 0;
			continue;
		}
		return fmte_("Invalid pretty-output object ~S.", pos, NULL);
	}

	return write_char_white_(stream, &white);
}


/*
 *  block / section
 */
struct pretty_block {
	addr pretty, root, list, perline;
	unsigned miserp : 1;
	unsigned newlinep : 1;
	unsigned previous : 1;
	unsigned break_lines_p : 1;
	unsigned print_lines_p : 1;
	unsigned print_miser_p : 1;
	size_t print_lines, print_miser, print_margin;
	size_t indent, base, now, current, section, lines;
};

static void pretty_push_object(struct pretty_block *ptr, addr pos)
{
	cons_heap(&(ptr->root), pos, ptr->root);
}

static void pretty_push_terpri(struct pretty_block *ptr)
{
	pretty_push_object(ptr, T);
}

static void pretty_push_newline(struct pretty_block *ptr)
{
	pretty_push_object(ptr, Nil);
}

static int pretty_push_char_(struct pretty_block *ptr, const char *str)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, str);
	pretty_push_object(ptr, pos);
	Return(eastasian_length_(pos, &size, NULL));
	ptr->now += size;

	return 0;
}

static int pretty_push_string_(struct pretty_block *ptr, addr pos, size_t *ret)
{
	size_t size;

	Check(! stringp(pos), "type error");
	pretty_push_object(ptr, pos);
	Return(eastasian_length_(pos, &size, NULL));
	ptr->now += size;
	if (ret)
		*ret = size;

	return 0;
}

static void pretty_push_size(struct pretty_block *ptr, size_t value)
{
	addr pos;
	index_heap(&pos, value);
	pretty_push_object(ptr, pos);
	ptr->now += value;
}

static void pretty_push_index(struct pretty_block *ptr, addr pos)
{
	size_t value;
	GetIndex(pos, &value);
	pretty_push_object(ptr, pos);
	ptr->now += value;
}

static int pretty_prefix_plus_(addr pos, size_t *ret)
{
	addr x;
	size_t size, value;

	/* prefix */
	size = 0;
	prefix_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* sharp */
	sharp_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* per-line-prefix */
	perline_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		size += value;
	}

	/* result */
	return Result(ret, size);
}

static int pretty_suffix_plus_(addr pos, size_t *size)
{
	addr x;
	size_t value;

	*size = 0;
	suffix_pretty_stream(pos, &x);
	if (x != Nil) {
		Return(eastasian_length_(x, &value, NULL));
		*size += value;
	}

	return 0;
}

static int pretty_tabular_plus_(struct pretty_block *ptr, addr pos, size_t *ret)
{
	fixnum value, column, colinc, now;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	column = str->value;
	colinc = str->colinc;
	now = ptr->now;
	switch (str->type) {
		case print_pretty_tabular_line:
			pprint_tab_absolute_size(&value, column, colinc, 0, now);
			break;

		case print_pretty_tabular_section:
			pprint_tab_absolute_size(&value, column, colinc, ptr->section, now);
			break;

		case print_pretty_tabular_liner:
			pprint_tab_relative_size(&value, column, colinc, 0, now);
			break;

		case print_pretty_tabular_sectionr:
			pprint_tab_relative_size(&value, column, colinc, ptr->section, now);
			break;

		default:
			*ret = 0;
			return fmte_("Invalid tabular type ~S.", pos, NULL);
	}

	return Result(ret, (size_t)value);
}

static int pretty_front_stream_(struct pretty_block *ptr,
		addr pos, size_t *rsize, int *ret)
{
	int listp, check;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		Return(pretty_prefix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	ptr->section = ptr->now;
	/* loop */
	result_pretty_stream(pos, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			ptr->section = section;
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_front_stream_(ptr, x, &value, &check));
			if (check) {
				ptr->section = section;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		Return(pretty_suffix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_front_newline_(struct pretty_block *ptr,
		addr list, addr *next, size_t *rsize, int *ret)
{
	int check;
	addr x, cons;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		cons = list;
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			*rsize = size;
			*next = cons;
			return Result(ret, 0);
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_front_stream_(ptr, x, &value, &check));
			if (check)
				return Result(ret, 1);
		}
		else {
			*next = NULL;
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	*rsize = 0;
	*next = Nil;
	return Result(ret, 0);
}

static int pretty_tail_stream_(struct pretty_block *ptr,
		addr pos, size_t *rsize, int *ret)
{
	int listp, check;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		Return(pretty_prefix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	ptr->section = ptr->now;
	/* loop */
	result_pretty_stream(pos, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			ptr->section = section;
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_tail_stream_(ptr, x, &value, &check));
			if (check) {
				ptr->section = section;
				*rsize = size + value;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		Return(pretty_suffix_plus_(pos, &value));
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_tail_section_loop_(struct pretty_block *ptr,
		addr list, size_t *rsize, int *ret)
{
	int check;
	addr x;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			*rsize = size;
			return Result(ret, 1);
		}
		else if (pretty_print_newline_p(x)) {
			goto newline;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			Return(pretty_tabular_plus_(ptr, x, &value));
		}
		else if (stringp(x)) {
			Return(eastasian_length_(x, &value, NULL));
		}
		else if (pretty_stream_p(x)) {
			Return(pretty_tail_stream_(ptr, x, &value, &check));
			if (check) {
				*rsize = size + value;
				return Result(ret, 1);
			}
		}
		else {
			*rsize = 0;
			*ret = 0;
			return fmte_("Invalid print object ~S.", x, NULL);
		}
		size += value;
		ptr->now += value;
	}
	Return(pretty_suffix_plus_(ptr->pretty, &value));
	size += value;
	ptr->now += value;
	/* result */
newline:
	*rsize = size;
	return Result(ret, 0);
}

static int pretty_tail_section_(struct pretty_block *ptr,
		addr list, size_t *rsize, int *ret)
{
	int check;
	size_t now, section;

	now = ptr->now;
	section = ptr->section;
	Return(pretty_tail_section_loop_(ptr, list, rsize, &check));
	ptr->now = now;
	ptr->section = section;

	return Result(ret, check);
}

static int pretty_section_(struct pretty_block *ptr, int *ret)
{
	int check;
	addr list;
	size_t a, b, c;

	list = ptr->list;
	c = ptr->now;
	for (;;) {
		/* front */
		Return(pretty_front_newline_(ptr, list, &list, &a, &check));
		if (check)
			return Result(ret, 1);
		if (list == Nil)
			break;
		/* tail */
		GetCdr(list, &list);
		Return(pretty_tail_section_(ptr, list, &b, &check));
		if (check)
			return Result(ret, 1);
		/* check */
		if (ptr->print_margin < a + b + c)
			return Result(ret, 1);
		c += a;
	}

	return Result(ret, 0);
}

static int pretty_newline_(struct pretty_block *ptr)
{
	int check;
	size_t now;

	if (ptr->newlinep)
		return 0;
	now = ptr->now;
	Return(pretty_section_(ptr, &check));
	if (check)
		ptr->newlinep = 1;
	ptr->now = now;
	ptr->section = now;

	return 0;
}

static int pretty_output_perline_(struct pretty_block *ptr)
{
	addr list, x;

	for (list = ptr->perline; list != Nil; ) {
		GetCons(list, &x, &list);
		if (stringp(x)) {
			Return(pretty_push_string_(ptr, x, NULL));
		}
		else if (GetType(x) == LISPTYPE_INDEX) {
			pretty_push_index(ptr, x);
		}
		else {
			return fmte_("Invalid perline type ~S.", x, NULL);
		}
	}

	return 0;
}

static int pretty_output_lines_(struct pretty_block *ptr, int *ret)
{
	if (! ptr->print_lines_p)
		return Result(ret, 0);
	ptr->lines++;
	if (ptr->lines < ptr->print_lines)
		return Result(ret, 0);
	Return(pretty_push_char_(ptr, " .."));
	ptr->break_lines_p = 1;
	return Result(ret, 1);
}

static int pretty_output_terpri_(struct pretty_block *ptr)
{
	int check;

	Return(pretty_output_lines_(ptr, &check));
	if (check)
		return 0;
	/* terpri */
	pretty_push_terpri(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	Return(pretty_output_perline_(ptr));
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;

	return 0;
}

static int pretty_output_newline_(struct pretty_block *ptr)
{
	int check;

	Return(pretty_output_lines_(ptr, &check));
	if (check)
		return 0;
	/* terpri */
	pretty_push_newline(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	Return(pretty_output_perline_(ptr));
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* indent */
	if (ptr->miserp == 0 && ptr->indent)
		pretty_push_size(ptr, ptr->indent);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;

	return 0;
}

static int pretty_output_fill_(struct pretty_block *ptr, addr list)
{
	int check;
	size_t size;

	/* miser */
	if (ptr->miserp) {
		if (ptr->newlinep) {
			Return(pretty_output_newline_(ptr));
		}
		return 0;
	}

	/* previous section */
	if (ptr->previous) {
		return pretty_output_newline_(ptr);
	}

	/* normal */
	Return(pretty_tail_section_(ptr, list, &size, &check));
	if (ptr->print_margin < ptr->now + size) {
		Return(pretty_output_newline_(ptr));
	}

	return 0;
}

static void pretty_output_indent_block(struct pretty_block *ptr, addr pos)
{
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	ptr->indent = (size_t)str->value;
	if (ptr->indent < 0)
		ptr->indent = 0;
}

static void pretty_output_indent_current(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now - ptr->current;
	value += str->value;
	if (value < 0)
		value = 0;
	ptr->indent = (size_t)value;
	if (ptr->indent < 0)
		ptr->indent = 0;
}

static void pretty_output_tabular_line(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_absolute_size(&value, str->value, str->colinc, 0, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_section(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_absolute_size(&value, str->value, str->colinc, ptr->section, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_liner(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_relative_size(&value, str->value, str->colinc, 0, value);
	pretty_push_size(ptr, (size_t)value);
}

static void pretty_output_tabular_sectionr(struct pretty_block *ptr, addr pos)
{
	fixnum value;
	struct print_pretty_struct *str;

	str = struct_print_pretty(pos);
	value = ptr->now;
	pprint_tab_relative_size(&value, str->value, str->colinc, ptr->section, value);
	pretty_push_size(ptr, (size_t)value);
}

static int pretty_struct_(struct pretty_block *ptr, addr pretty);
static int pretty_output_(struct pretty_block *ptr)
{
	addr list, x;

	list = ptr->list;
	while (list != Nil) {
		/* lines */
		if (ptr->break_lines_p) {
			break;
		}
		/* list */
		GetCons(list, &x, &list);
		if (stringp(x)) {
			Return(pretty_push_string_(ptr, x, NULL));
			continue;
		}
		if (pretty_print_linear_p(x)) {
			if (ptr->newlinep) {
				Return(pretty_output_newline_(ptr));
			}
			continue;
		}
		if (pretty_print_miser_p(x)) {
			if (ptr->miserp && ptr->newlinep) {
				Return(pretty_output_newline_(ptr));
			}
			continue;
		}
		if (pretty_print_fill_p(x)) {
			Return(pretty_output_fill_(ptr, list));
			continue;
		}
		if (pretty_print_mandatory_p(x)) {
			Return(pretty_output_newline_(ptr));
			continue;
		}
		if (pretty_print_terpri_p(x)) {
			Return(pretty_output_terpri_(ptr));
			continue;
		}
		if (pretty_print_indent_block_p(x)) {
			pretty_output_indent_block(ptr, x);
			continue;
		}
		if (pretty_print_indent_current_p(x)) {
			pretty_output_indent_current(ptr, x);
			continue;
		}
		if (pretty_print_tabular_line_p(x)) {
			pretty_output_tabular_line(ptr, x);
			continue;
		}
		if (pretty_print_tabular_section_p(x)) {
			pretty_output_tabular_section(ptr, x);
			continue;
		}
		if (pretty_print_tabular_liner_p(x)) {
			pretty_output_tabular_liner(ptr, x);
			continue;
		}
		if (pretty_print_tabular_sectionr_p(x)) {
			pretty_output_tabular_sectionr(ptr, x);
			continue;
		}
		if (pretty_stream_p(x)) {
			Return(pretty_struct_(ptr, x));
			continue;
		}
		return fmte_("Invalid pretty-object ~S.", x, NULL);
	}

	return 0;
}

static void pretty_push_perline(struct pretty_block *ptr, addr pos)
{
	if (ptr->perline == Nil) {
		cons_heap(&(ptr->perline), pos, Nil);
	}
	else {
		cons_heap(&pos, pos, Nil);
		nconc2_unsafe(ptr->perline, pos, &(ptr->perline));
	}
}

static void pretty_push_perline_index(struct pretty_block *ptr, size_t size)
{
	addr pos;
	index_heap(&pos, size);
	pretty_push_perline(ptr, pos);
}

static int pretty_prefix_(struct pretty_block *ptr)
{
	addr pos;
	size_t size;

	/* copy-list */
	copy_list_heap_unsafe(&(ptr->perline), ptr->perline);

	/* per-line-prefix */
	perline_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, NULL));
		pretty_push_perline(ptr, pos);
	}

	/* sharp */
	sharp_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, &size));
		pretty_push_perline_index(ptr, size);
	}

	/* prefix */
	prefix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, &size));
		pretty_push_perline_index(ptr, size);
	}

	/* current */
	ptr->current = ptr->now;
	ptr->section = ptr->now;

	return 0;
}

static int pretty_suffix_(struct pretty_block *ptr)
{
	addr pos;

	suffix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		Return(pretty_push_string_(ptr, pos, NULL));
	}

	return 0;
}

static void pretty_result(struct pretty_block *str, struct pretty_block *ptr)
{
	addr list, root, x;

	nreverse(&list, str->root);
	for (root = ptr->root; list != Nil; ) {
		GetCons(list, &x, &list);
		cons_heap(&root, x, root);
	}
	ptr->root = root;
}

static void pretty_miser(struct pretty_block *ptr)
{
	/* nil */
	if (! ptr->print_miser_p) {
		ptr->miserp = 0;
		return;
	}
	/* over */
	if (ptr->print_margin <= ptr->print_miser) {
		ptr->miserp = 1;
		return;
	}
	/* check */
	ptr->miserp = (ptr->print_margin - ptr->print_miser) <= ptr->current;
}

static int pretty_struct_(struct pretty_block *ptr, addr pretty)
{
	struct pretty_block str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	/* addr */
	str = *ptr;
	str.pretty = pretty;
	str.root = Nil;
	str.miserp = 0;
	str.newlinep = 0;
	str.previous = 0;
	result_pretty_stream(pretty, &(str.list));
	/* value */
	str.base = ptr->base;
	str.current = 0;
	str.section = 0;
	str.indent = 0;
	str.lines = 0;
	/* print */
	if (listp_pretty_stream(pretty)) {
		Return(pretty_prefix_(&str));
		pretty_miser(&str);
		Return(pretty_newline_(&str));
		Return(pretty_output_(&str));
		Return(pretty_suffix_(&str));
	}
	else {
		Return(pretty_output_(&str));
	}
	/* result */
	pretty_result(&str, ptr);
	ptr->now = str.now;
	ptr->previous = str.newlinep;

	return 0;
}


/*
 *  interface
 */
static int pprint_initialize_(struct pretty_block *str, Execute ptr, addr stream)
{
	int check;
	size_t size;

	/* pointer */
	str->pretty = Nil;
	str->root = Nil;
	str->list = Nil;
	str->perline = Nil;

	/* value */
	str->newlinep = 0;
	str->previous = 0;
	str->miserp = 0;
	Return(getleft_stream_(stream, &size));
	str->base = size;
	str->now = size;
	str->current = 0;
	str->section = 0;
	str->indent = 0;
	str->lines = 0;
	str->print_lines = 0;
	str->print_miser = 0;
	str->print_margin = 0;
	str->break_lines_p = 0;

	Return(lines_print_(ptr, &size, &check));
	str->print_lines_p = check;
	str->print_lines = check? size: 0;

	Return(miser_width_print_(ptr, &size, &check));
	str->print_miser_p = check;
	str->print_miser = check? size: 0;

	return right_margin_print_(ptr, stream, &(str->print_margin));
}

_g int pprint_output_(Execute ptr, addr stream, addr pretty)
{
	struct pretty_block str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	/* argument check */
	if (pretty_stream_p(stream))
		return fmte_("Invalid output-stream ~S.", stream, NULL);
	if (! pretty_stream_p(pretty))
		return fmte_("Invalid pretty-stream ~S.", pretty, NULL);
	/* pretty-start */
	Return(pprint_initialize_(&str, ptr, stream));
	Return(pretty_struct_(&str, pretty));
	/* output */
	Return(pretty_write_(stream, str.root));
	return exitpoint_stream_(stream);
}

