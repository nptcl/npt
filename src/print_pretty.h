#ifndef __PRINT_PRETTY_HEADER__
#define __PRINT_PRETTY_HEADER__

#include "define.h"
#include "typedef.h"

enum pprint_newline {
	pprint_newline_linear,
	pprint_newline_fill,
	pprint_newline_miser,
	pprint_newline_mandatory
};

enum pprint_tabular {
	pprint_tabular_line,
	pprint_tabular_section,
	pprint_tabular_line_relative,
	pprint_tabular_section_relative
};

_g int pprint_exit_common(Execute ptr, addr stream);
_g int pprint_pop_common(Execute ptr, addr stream, addr *ret);
_g void pprint_fill_common(addr stream, addr pos, int colon);
_g void pprint_linear_common(addr stream, addr pos, int colon);
_g void pprint_tabular_common(addr stream, addr pos, int colon, size_t size);
_g void expand_pprint_logical_block_common(Execute ptr,
		addr *ret, addr stream, addr pos,
		addr prefix, addr per, addr suffix, addr decl, addr body);
_g void pprint_indent(int block_p, fixnum n, addr stream);
_g void pprint_newline_common(enum pprint_newline kind, addr stream);
_g void pprint_tab_common(enum pprint_tabular kind,
		size_t column, size_t colinc, addr stream);
_g void print_tab_relative(addr stream, size_t column, size_t colinc);
_g void print_tab_section(addr stream, size_t column, size_t colinc);

#endif

