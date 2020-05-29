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
_g void expand_pprint_logical_block_common(addr *ret, addr symbol, addr pos,
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
	addr let, flet, lambda, unwind, catch, macrolet, list, quote;
	addr make, pretty, gensym, exit, pop, check, close, ppexit, pppop;
	addr x;

	/* (check-type symbol symbol) */
	if (! symbolp(symbol))
		TypeError(symbol, SYMBOL);

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
	GetConst(COMMON_CATCH, &catch);
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
	list_heap(&catch, catch, gensym, macrolet, NULL);
	list_heap(&unwind, unwind, catch, close, NULL);
	list_heap(&lambda, lambda, Nil, unwind, NULL);
	list_heap(&pretty, pretty, symbol, lambda, NULL);
	list_heap(&x, make, symbol, pos, prefix, perline, suffix, NULL);
	list_heap(&x, symbol, x, NULL);
	list_heap(&x, x, NULL);
	/* let */
	conscar_heap(&let, let);
	cons_heap(&let, x, let);
	while (decl != Nil) {
		getcons(decl, &x, &decl);
		cons_heap(&let, x, let);
	}
	cons_heap(&let, pretty, let);
	nreverse(ret, let);
}

_g int pprint_throw(Execute ptr, addr stream)
{
	gensym_pretty_stream(stream, &stream);
	return throw_control_(ptr, stream);
}

_g int pprint_exit_common(Execute ptr, addr stream)
{
	addr pos;

	root_pretty_stream(stream, &pos);
	if (pos == Nil)
		return pprint_throw(ptr, stream);

	return 0;
}

static int pprint_pop_atom(Execute ptr, addr stream)
{
	addr pos;

	if (! first_pretty_stream(stream))
		print_ascii_stream(stream, ". ");
	pop_pretty_stream(stream, &pos);
	Return(write_print(ptr, stream, pos));

	return pprint_throw(ptr, stream);
}

static int pprint_length_check(Execute ptr, addr stream)
{
	size_t size;
	return length_print(ptr, &size)
		&& size <= length_pretty_stream(stream);
}

_g int pprint_pop_common(Execute ptr, addr stream, addr *ret)
{
	addr pos;

	root_pretty_stream(stream, &pos);
	/* atom */
	if (! listp(pos))
		return pprint_pop_atom(ptr, stream);
	/* length */
	if (pprint_length_check(ptr, stream)) {
		print_ascii_stream(stream, "...");
		return pprint_throw(ptr, stream);
	}
	/* list */
	if (pos == Nil) {
		*ret = Nil;
		return 0;
	}
	/* circle */
	if (circle_print(ptr)) {
		if (! first_pretty_stream(stream) && pprint_pop_circle(ptr, stream, pos))
			return pprint_throw(ptr, stream);
	}
	/* cons */
	pop_pretty_stream(stream, ret);

	return 0;
}

_g int check_pretty_stream(Execute ptr, addr stream)
{
	addr root;
	size_t level, depth;

	/* *print-level* */
	if (level_print(ptr, &level)) {
		getdepth_print_write(ptr, &depth);
		if (level <= depth) {
			setlistp_pretty_stream(stream, 0);
			write_char_stream(stream, '#');
			return pprint_throw(ptr, stream);
		}
	}

	/* atom */
	root_pretty_stream(stream, &root);
	if (! listp(root)) {
		return write_print(ptr, stream, root) || pprint_throw(ptr, stream);
	}

	/* increment depth */
	setdepth_pretty_stream(ptr, stream, 1);

	/* circle */
	if (circle_print(ptr)) {
		if (consp(root)) {
			if (pprint_check_circle(ptr, root, &root)) {
				setlistp_pretty_stream(stream, 0);
				print_string_stream(stream, root);
				return pprint_throw(ptr, stream);
			}
			if (root != Nil)
				setsharp_pretty_stream(stream, root);
		}
		return 0;
	}

	return 0;
}

static int pretty_common_p(Execute ptr, addr stream)
{
	return pretty_print(ptr) && pretty_stream_p(stream);
}

_g void pprint_indent_print(Execute ptr, int block_p, fixnum n, addr stream)
{
	enum print_pretty type;
	addr pos;

	if (! pretty_common_p(ptr, stream)) return;
	type = block_p? print_pretty_indent_block: print_pretty_indent_current;
	fixnum_pretty_heap(&pos, type, n);
	push_pretty_stream(stream, pos);
}

_g void pprint_newline_print(Execute ptr, enum pprint_newline kind, addr stream)
{
	addr pos;

	if (! pretty_common_p(ptr, stream)) return;
	switch (kind) {
		case pprint_newline_linear:
			print_pretty_heap(&pos, print_pretty_newline_linear);
			push_pretty_stream(stream, pos);
			break;

		case pprint_newline_fill:
			print_pretty_heap(&pos, print_pretty_newline_fill);
			push_pretty_stream(stream, pos);
			break;

		case pprint_newline_miser:
			print_pretty_heap(&pos, print_pretty_newline_miser);
			push_pretty_stream(stream, pos);
			break;

		case pprint_newline_mandatory:
			print_pretty_heap(&pos, print_pretty_newline_mandatory);
			push_pretty_stream(stream, pos);
			break;

		default:
			fmte("Invalid newline type.", NULL);
			break;
	}
}

_g void pprint_newline_terpri(addr stream)
{
	addr pos;

	print_pretty_heap(&pos, print_pretty_newline_terpri);
	push_pretty_stream(stream, pos);
}

_g void pprint_tab_print(Execute ptr,
		addr stream, enum pprint_tabular kind, fixnum a, fixnum b)
{
	addr pos;

	if (! pretty_common_p(ptr, stream)) return;
	switch (kind) {
		case pprint_tabular_line:
			size2_pretty_heap(&pos, print_pretty_tabular_line, a, b);
			push_pretty_stream(stream, pos);
			break;

		case pprint_tabular_section:
			size2_pretty_heap(&pos, print_pretty_tabular_section, a, b);
			push_pretty_stream(stream, pos);
			break;

		case pprint_tabular_line_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_liner, a, b);
			push_pretty_stream(stream, pos);
			break;

		case pprint_tabular_section_relative:
			size2_pretty_heap(&pos, print_pretty_tabular_sectionr, a, b);
			push_pretty_stream(stream, pos);
			break;

		default:
			fmte("Invalid newline type.", NULL);
			break;
	}
}

_g void pprint_tab_section(Execute ptr, addr stream, fixnum column, fixnum colinc)
{
	pprint_tab_print(ptr, stream, pprint_tabular_section, column, colinc);
}

_g void pprint_tab_section_relative(Execute ptr,
		addr stream, fixnum column, fixnum colinc)
{
	pprint_tab_print(ptr, stream, pprint_tabular_section_relative, column, colinc);
}

static void pprint_tab_output(addr stream, fixnum size)
{
	for (; 0 < size; size--)
		write_char_stream(stream, ' ');
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

_g void pprint_tab_absolute_force(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_absolute_size(&now, column, colinc, 0, now);
	pprint_tab_output(stream, now);
}

_g void pprint_tab_relative_force(addr stream,
		fixnum column, fixnum colinc, fixnum now)
{
	pprint_tab_relative_size(&now, column, colinc, 0, now);
	pprint_tab_output(stream, now);
}


/*
 *  output
 */
static void write_char_white(addr stream, size_t *white)
{
	for (; *white; (*white)--)
		write_char_stream(stream, ' ');
}

static void pretty_write_string(addr stream, addr pos, size_t *white)
{
	unicode c;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c == ' ') {
			(*white)++;
			continue;
		}
		write_char_white(stream, white);
		write_char_stream(stream, c);
	}
}

static void pretty_write_space(addr stream, addr pos, size_t *white)
{
	size_t value;
	GetIndex(pos, &value);
	*white += value;
}

static void pretty_write(addr stream, addr list)
{
	addr pos;
	size_t white;

	nreverse(&list, list);
	white = 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (stringp(pos)) {
			pretty_write_string(stream, pos, &white);
			continue;
		}
		if (GetType(pos) == LISPTYPE_INDEX) {
			pretty_write_space(stream, pos, &white);
			continue;
		}
		if (pos == T) {
			write_char_white(stream, &white);
			terpri_stream(stream);
			white = 0;
			continue;
		}
		if (pos == Nil) {
			terpri_stream(stream);
			white = 0;
			continue;
		}
		fmte("Invalid pretty-output object ~S.", pos, NULL);
	}
	write_char_white(stream, &white);
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

static void pretty_push_char(struct pretty_block *ptr, const char *str)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, str);
	pretty_push_object(ptr, pos);
	eastasian_length(pos, &size);
	ptr->now += size;
}

static void pretty_push_string(struct pretty_block *ptr, addr pos, size_t *ret)
{
	size_t size;

	Check(! stringp(pos), "type error");
	pretty_push_object(ptr, pos);
	eastasian_length(pos, &size);
	ptr->now += size;
	if (ret) *ret = size;
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

static void pretty_prefix_plus(addr pos, size_t *ret)
{
	addr x;
	size_t size, value;

	/* prefix */
	size = 0;
	prefix_pretty_stream(pos, &x);
	if (x != Nil) {
		eastasian_length(x, &value);
		size += value;
	}

	/* sharp */
	sharp_pretty_stream(pos, &x);
	if (x != Nil) {
		eastasian_length(x, &value);
		size += value;
	}

	/* per-line-prefix */
	perline_pretty_stream(pos, &x);
	if (x != Nil) {
		eastasian_length(x, &value);
		size += value;
	}

	/* result */
	*ret = size;
}

static void pretty_suffix_plus(addr pos, size_t *size)
{
	addr x;
	size_t value;

	*size = 0;
	suffix_pretty_stream(pos, &x);
	if (x != Nil) {
		eastasian_length(x, &value);
		*size += value;
	}
}

static void pretty_tabular_plus(struct pretty_block *ptr, addr pos, size_t *size)
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
			fmte("Invalid tabular type ~S.", pos, NULL);
			value = 0;
			break;
	}
	*size = (size_t)value;
}

static int pretty_front_stream(struct pretty_block *ptr, addr pos, size_t *ret)
{
	int listp;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		pretty_prefix_plus(pos, &value);
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
			*ret = size;
			return 1;
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			pretty_tabular_plus(ptr, x, &value);
		}
		else if (stringp(x)) {
			eastasian_length(x, &value);
		}
		else if (pretty_stream_p(x)) {
			if (pretty_front_stream(ptr, x, &value)) {
				ptr->section = section;
				return 1;
			}
		}
		else {
			fmte("Invalid print object ~S.", x, NULL);
			break;
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		pretty_suffix_plus(pos, &value);
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*ret = size;

	return 0;
}

static int pretty_front_newline(struct pretty_block *ptr,
		addr list, addr *next, size_t *ret)
{
	addr x, cons;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		cons = list;
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			return 1;
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			*ret = size;
			*next = cons;
			return 0;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			pretty_tabular_plus(ptr, x, &value);
		}
		else if (stringp(x)) {
			eastasian_length(x, &value);
		}
		else if (pretty_stream_p(x)) {
			if (pretty_front_stream(ptr, x, &value))
				return 1;
		}
		else {
			fmte("Invalid print object ~S.", x, NULL);
			break;
		}
		size += value;
		ptr->now += value;
	}
	*ret = 0;
	*next = Nil;

	return 0;
}

static int pretty_tail_stream(struct pretty_block *ptr, addr pos, size_t *ret)
{
	int listp;
	addr list, x;
	size_t size, value, section;

	/* rollback */
	size = 0;
	section = ptr->now;
	/* prefix */
	listp = listp_pretty_stream(pos);
	if (listp) {
		pretty_prefix_plus(pos, &value);
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
			*ret = size;
			return 1;
		}
		else if (pretty_print_newline_p(x)) {
			ptr->section = ptr->now;
			continue;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			pretty_tabular_plus(ptr, x, &value);
		}
		else if (stringp(x)) {
			eastasian_length(x, &value);
		}
		else if (pretty_stream_p(x)) {
			if (pretty_tail_stream(ptr, x, &value)) {
				ptr->section = section;
				*ret = size + value;
				return 1;
			}
		}
		else {
			fmte("Invalid print object ~S.", x, NULL);
			break;
		}
		size += value;
		ptr->now += value;
	}
	/* suffix */
	if (listp) {
		pretty_suffix_plus(pos, &value);
		size += value;
		ptr->now += value;
	}
	/* result */
	ptr->section = section;
	*ret = size;

	return 0;
}

static int pretty_tail_section_loop(struct pretty_block *ptr, addr list, size_t *ret)
{
	addr x;
	size_t size, value;

	size = 0;
	while (list != Nil) {
		GetCons(list, &x, &list);
		if (pretty_print_force_p(x)) {
			*ret = size;
			return 1;
		}
		else if (pretty_print_newline_p(x)) {
			goto newline;
		}
		else if (pretty_print_indent_p(x)) {
			continue;
		}
		else if (pretty_print_tabular_p(x)) {
			pretty_tabular_plus(ptr, x, &value);
		}
		else if (stringp(x)) {
			eastasian_length(x, &value);
		}
		else if (pretty_stream_p(x)) {
			if (pretty_tail_stream(ptr, x, &value)) {
				*ret = size + value;
				return 1;
			}
		}
		else {
			fmte("Invalid print object ~S.", x, NULL);
			break;
		}
		size += value;
		ptr->now += value;
	}
	pretty_suffix_plus(ptr->pretty, &value);
	size += value;
	ptr->now += value;
	/* result */
newline:
	*ret = size;

	return 0;
}

static int pretty_tail_section(struct pretty_block *ptr, addr list, size_t *ret)
{
	int check;
	size_t now, section;

	now = ptr->now;
	section = ptr->section;
	check = pretty_tail_section_loop(ptr, list, ret);
	ptr->now = now;
	ptr->section = section;

	return check;
}

static int pretty_section(struct pretty_block *ptr)
{
	addr list;
	size_t a, b, c;

	list = ptr->list;
	c = ptr->now;
	for (;;) {
		/* front */
		if (pretty_front_newline(ptr, list, &list, &a))
			return 1;
		if (list == Nil)
			break;
		/* tail */
		GetCdr(list, &list);
		if (pretty_tail_section(ptr, list, &b))
			return 1;
		/* check */
		if (ptr->print_margin < a + b + c)
			return 1;
		c += a;
	}

	return 0;
}

static void pretty_newline(struct pretty_block *ptr)
{
	size_t now;

	if (ptr->newlinep)
		return;
	now = ptr->now;
	if (pretty_section(ptr))
		ptr->newlinep = 1;
	ptr->now = now;
	ptr->section = now;
}

static void pretty_output_perline(struct pretty_block *ptr)
{
	addr list, x;

	for (list = ptr->perline; list != Nil; ) {
		GetCons(list, &x, &list);
		if (stringp(x))
			pretty_push_string(ptr, x, NULL);
		else if (GetType(x) == LISPTYPE_INDEX)
			pretty_push_index(ptr, x);
		else
			fmte("Invalid perline type ~S.", x, NULL);
	}
}

static int pretty_output_lines(struct pretty_block *ptr)
{
	if (! ptr->print_lines_p)
		return 0;
	ptr->lines++;
	if (ptr->lines < ptr->print_lines)
		return 0;
	pretty_push_char(ptr, " ..");
	ptr->break_lines_p = 1;
	return 1;
}

static void pretty_output_terpri(struct pretty_block *ptr)
{
	if (pretty_output_lines(ptr))
		return;
	/* terpri */
	pretty_push_terpri(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	pretty_output_perline(ptr);
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;
}

static void pretty_output_newline(struct pretty_block *ptr)
{
	if (pretty_output_lines(ptr))
		return;
	/* terpri */
	pretty_push_newline(ptr);
	ptr->now = 0;
	/* base */
	if (ptr->base)
		pretty_push_size(ptr, ptr->base);
	/* perline */
	pretty_output_perline(ptr);
	/* current */
	if (ptr->now < ptr->current)
		pretty_push_size(ptr, ptr->current - ptr->now);
	/* indent */
	if (ptr->miserp == 0 && ptr->indent)
		pretty_push_size(ptr, ptr->indent);
	/* current */
	ptr->section = ptr->now;
	ptr->previous = 0;
}

static void pretty_output_fill(struct pretty_block *ptr, addr list)
{
	size_t size;

	/* miser */
	if (ptr->miserp) {
		if (ptr->newlinep)
			pretty_output_newline(ptr);
		return;
	}

	/* previous section */
	if (ptr->previous) {
		pretty_output_newline(ptr);
		return;
	}

	/* normal */
	(void)pretty_tail_section(ptr, list, &size);
	if (ptr->print_margin < ptr->now + size) {
		pretty_output_newline(ptr);
	}
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

static void pretty_struct(struct pretty_block *ptr, addr pretty);
static void pretty_output(struct pretty_block *ptr)
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
			pretty_push_string(ptr, x, NULL);
			continue;
		}
		if (pretty_print_linear_p(x)) {
			if (ptr->newlinep)
				pretty_output_newline(ptr);
			continue;
		}
		if (pretty_print_miser_p(x)) {
			if (ptr->miserp && ptr->newlinep)
				pretty_output_newline(ptr);
			continue;
		}
		if (pretty_print_fill_p(x)) {
			pretty_output_fill(ptr, list);
			continue;
		}
		if (pretty_print_mandatory_p(x)) {
			pretty_output_newline(ptr);
			continue;
		}
		if (pretty_print_terpri_p(x)) {
			pretty_output_terpri(ptr);
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
			pretty_struct(ptr, x);
			continue;
		}
		fmte("Invalid pretty-object ~S.", x, NULL);
	}
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

static void pretty_prefix(struct pretty_block *ptr)
{
	addr pos;
	size_t size;

	/* copy-list */
	copy_list_heap_unsafe(&(ptr->perline), ptr->perline);

	/* per-line-prefix */
	perline_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		pretty_push_string(ptr, pos, NULL);
		pretty_push_perline(ptr, pos);
	}

	/* sharp */
	sharp_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		pretty_push_string(ptr, pos, &size);
		pretty_push_perline_index(ptr, size);
	}

	/* prefix */
	prefix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil) {
		pretty_push_string(ptr, pos, &size);
		pretty_push_perline_index(ptr, size);
	}

	/* current */
	ptr->current = ptr->now;
	ptr->section = ptr->now;
}

static void pretty_suffix(struct pretty_block *ptr)
{
	addr pos;

	suffix_pretty_stream(ptr->pretty, &pos);
	if (pos != Nil)
		pretty_push_string(ptr, pos, NULL);
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

static void pretty_struct(struct pretty_block *ptr, addr pretty)
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
		pretty_prefix(&str);
		pretty_miser(&str);
		pretty_newline(&str);
		pretty_output(&str);
		pretty_suffix(&str);
	}
	else {
		pretty_output(&str);
	}
	/* result */
	pretty_result(&str, ptr);
	ptr->now = str.now;
	ptr->previous = str.newlinep;
}


/*
 *  interface
 */
static void pprint_initialize(struct pretty_block *str, Execute ptr, addr stream)
{
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
	size = getleft_stream(stream);
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
	str->print_lines_p = lines_print(ptr, &(str->print_lines));
	str->print_miser_p = miser_width_print(ptr, &(str->print_miser));
	right_margin_print(ptr, stream, &(str->print_margin));
}

_g void pprint_output(Execute ptr, addr stream, addr pretty)
{
	struct pretty_block str;

#ifdef LISP_DEBUG
	aatype(str);
#endif
	/* argument check */
	if (pretty_stream_p(stream))
		fmte("Invalid output-stream ~S.", stream, NULL);
	if (! pretty_stream_p(pretty))
		fmte("Invalid pretty-stream ~S.", pretty, NULL);
	/* pretty-start */
	pprint_initialize(&str, ptr, stream);
	pretty_struct(&str, pretty);
	/* output */
	pretty_write(stream, str.root);
	exitpoint_stream(stream);
}

