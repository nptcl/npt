#ifndef __PRINT_PRETTY_HEADER__
#define __PRINT_PRETTY_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

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

_g int pprint_throw(Execute ptr, addr stream);
_g int pprint_exit_common(Execute ptr, addr stream);
_g int pprint_pop_common(Execute ptr, addr stream, addr *ret);
_g int check_pretty_stream(Execute ptr, addr stream);
_g void expand_pprint_logical_block_common(addr *ret, addr stream, addr pos,
		addr prefix, addr per, addr suffix, addr decl, addr body);
_g void pprint_indent_common(Execute ptr, int block_p, fixnum n, addr stream);
_g void pprint_newline_common(Execute ptr, enum pprint_newline kind, addr stream);
_g void pprint_newline_terpri(addr stream);
_g void pprint_tab_common(Execute ptr,
		addr stream, enum pprint_tabular kind, fixnum column, fixnum colinc);
_g void pprint_tab_section(Execute ptr, addr stream, fixnum column, fixnum colinc);
_g void pprint_tab_section_relative(Execute ptr,
		addr stream, fixnum column, fixnum colinc);
_g void pprint_tab_absolute_force(addr stream,
		fixnum column, fixnum colinc, fixnum now);
_g void pprint_tab_relative_force(addr stream,
		fixnum column, fixnum colinc, fixnum now);
_g void pprint_output(Execute ptr, addr stream, addr pos);

#endif

