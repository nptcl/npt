#ifndef __CALL_PRINTER_HEADER__
#define __CALL_PRINTER_HEADER__

#include "execute.h"
#include "typedef.h"

#define formatter_common _n(formatter_common)
#define pprint_fill_common _n(pprint_fill_common)
#define pprint_linear_common _n(pprint_linear_common)
#define pprint_tabular_common _n(pprint_tabular_common)
#define pprint_indent_common _n(pprint_indent_common)
#define pprint_logical_block_common _n(pprint_logical_block_common)
#define pprint_newline_common _n(pprint_newline_common)
#define pprint_tab_common _n(pprint_tab_common)
#define print_unreadable_object_common _n(print_unreadable_object_common)
#define set_pprint_dispatch_common _n(set_pprint_dispatch_common)
#define write_common _n(write_common)
#define prin1_common _n(prin1_common)
#define princ_common _n(princ_common)
#define print_common _n(print_common)
#define pprint_common _n(pprint_common)
#define write_to_string_common _n(write_to_string_common)
#define prin1_to_string_common _n(prin1_to_string_common)
#define princ_to_string_common _n(princ_to_string_common)
#define init_call_printer _n(init_call_printer)

int formatter_common(LocalRoot local, addr var, addr env, addr *ret);
int pprint_fill_common(Execute ptr, addr stream, addr list, addr colon);
int pprint_linear_common(Execute ptr, addr stream, addr list, addr colon);
int pprint_tabular_common(Execute ptr,
		addr stream, addr list, addr colon, addr tabsize);
int pprint_indent_common(Execute ptr, addr rel, addr n, addr stream);
int pprint_logical_block_common(addr form, addr env, addr *ret);
int pprint_newline_common(Execute ptr, addr kind, addr stream);
int pprint_tab_common(Execute ptr, addr kind, addr column, addr colinc, addr stream);
int print_unreadable_object_common(addr form, addr env, addr *ret);
int set_pprint_dispatch_common(Execute ptr,
		addr spec, addr call, addr priority, addr table);
int write_common(Execute ptr, addr var, addr args);
int prin1_common(Execute ptr, addr var, addr stream);
int princ_common(Execute ptr, addr var, addr stream);
int print_common(Execute ptr, addr var, addr stream);
int pprint_common(Execute ptr, addr var, addr stream);
int write_to_string_common(Execute ptr, addr var, addr args, addr *ret);
int prin1_to_string_common(Execute ptr, addr var, addr *ret);
int princ_to_string_common(Execute ptr, addr var, addr *ret);

void init_call_printer(void);

#endif

