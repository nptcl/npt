#ifndef __CALL_PRINTER_HEADER__
#define __CALL_PRINTER_HEADER__

#include "execute.h"
#include "typedef.h"

_g int formatter_common(LocalRoot local, addr var, addr env, addr *ret);
_g int pprint_fill_common(Execute ptr, addr stream, addr list, addr colon);
_g int pprint_linear_common(Execute ptr, addr stream, addr list, addr colon);
_g int pprint_tabular_common(Execute ptr,
		addr stream, addr list, addr colon, addr tabsize);
_g int pprint_indent_common(Execute ptr, addr rel, addr n, addr stream);
_g int pprint_logical_block_common(addr form, addr env, addr *ret);
_g int pprint_newline_common(Execute ptr, addr kind, addr stream);
_g int pprint_tab_common(Execute ptr, addr kind, addr column, addr colinc, addr stream);
_g int print_unreadable_object_common(addr form, addr env, addr *ret);
_g int set_pprint_dispatch_common(Execute ptr,
		addr spec, addr call, addr priority, addr table);
_g int write_common(Execute ptr, addr var, addr args);
_g int prin1_common(Execute ptr, addr var, addr stream);
_g int princ_common(Execute ptr, addr var, addr stream);
_g int print_common(Execute ptr, addr var, addr stream);
_g int pprint_common(Execute ptr, addr var, addr stream);
_g int write_to_string_common(Execute ptr, addr var, addr args, addr *ret);
_g int prin1_to_string_common(Execute ptr, addr var, addr *ret);
_g int princ_to_string_common(Execute ptr, addr var, addr *ret);

_g void init_call_printer(void);

#endif

