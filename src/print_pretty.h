#ifndef __PRINT_PRETTY_HEADER__
#define __PRINT_PRETTY_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define pprint_throw_ _n(pprint_throw_)
#define pprint_exit_common_ _n(pprint_exit_common_)
#define pprint_pop_common_ _n(pprint_pop_common_)
#define check_pretty_stream_ _n(check_pretty_stream_)
#define expand_pprint_logical_block_common_ _n(expand_pprint_logical_block_common_)
#define pprint_indent_print_ _n(pprint_indent_print_)
#define pprint_newline_print_ _n(pprint_newline_print_)
#define pprint_newline_terpri_ _n(pprint_newline_terpri_)
#define pprint_tab_print_ _n(pprint_tab_print_)
#define pprint_tab_section_ _n(pprint_tab_section_)
#define pprint_tab_section_relative_ _n(pprint_tab_section_relative_)
#define pprint_tab_absolute_force_ _n(pprint_tab_absolute_force_)
#define pprint_tab_relative_force_ _n(pprint_tab_relative_force_)
#define pprint_output_ _n(pprint_output_)

enum pprint_newline {
	pprint_newline_linear,
	pprint_newline_fill,
	pprint_newline_miser,
	pprint_newline_mandatory
};

enum pprint_tabular {
	pprint_tabular_line,
	pprint_tabular_line_relative,
	pprint_tabular_section,
	pprint_tabular_section_relative
};

int pprint_throw_(Execute ptr, addr stream);
int pprint_exit_common_(Execute ptr, addr stream);
int pprint_pop_common_(Execute ptr, addr stream, addr *ret);
int check_pretty_stream_(Execute ptr, addr stream);
int expand_pprint_logical_block_common_(addr *ret, addr stream, addr pos,
		addr prefix, addr per, addr suffix, addr decl, addr body);
int pprint_indent_print_(Execute ptr, int block_p, fixnum n, addr stream);
int pprint_newline_print_(Execute ptr, enum pprint_newline kind, addr stream);
int pprint_newline_terpri_(addr stream);
int pprint_tab_print_(Execute ptr,
		addr stream, enum pprint_tabular kind, fixnum column, fixnum colinc);
int pprint_tab_section_(Execute ptr, addr stream, fixnum column, fixnum colinc);
int pprint_tab_section_relative_(Execute ptr,
		addr stream, fixnum column, fixnum colinc);
int pprint_tab_absolute_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now);
int pprint_tab_relative_force_(addr stream,
		fixnum column, fixnum colinc, fixnum now);
int pprint_output_(Execute ptr, addr stream, addr pos);

#endif

