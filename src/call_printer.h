#ifndef __CALL_PRINTER_HEADER__
#define __CALL_PRINTER_HEADER__

#include "execute.h"
#include "typedef.h"

#define formatter_common_ _n(formatter_common_)
#define pprint_fill_common_ _n(pprint_fill_common_)
#define pprint_linear_common_ _n(pprint_linear_common_)
#define pprint_tabular_common_ _n(pprint_tabular_common_)
#define pprint_indent_common_ _n(pprint_indent_common_)
#define pprint_logical_block_common_ _n(pprint_logical_block_common_)
#define pprint_newline_common_ _n(pprint_newline_common_)
#define pprint_tab_common_ _n(pprint_tab_common_)
#define print_unreadable_object_common_ _n(print_unreadable_object_common_)
#define set_pprint_dispatch_common_ _n(set_pprint_dispatch_common_)
#define write_common_ _n(write_common_)
#define prin1_common_ _n(prin1_common_)
#define princ_common_ _n(princ_common_)
#define print_common_ _n(print_common_)
#define pprint_common_ _n(pprint_common_)
#define write_to_string_common_ _n(write_to_string_common_)
#define prin1_to_string_common_ _n(prin1_to_string_common_)
#define princ_to_string_common_ _n(princ_to_string_common_)
#define init_call_printer _n(init_call_printer)

int formatter_common_(LocalRoot local, addr var, addr env, addr *ret);
int pprint_fill_common_(Execute ptr, addr stream, addr list, addr colon);
int pprint_linear_common_(Execute ptr, addr stream, addr list, addr colon);
int pprint_tabular_common_(Execute ptr,
		addr stream, addr list, addr colon, addr tabsize);
int pprint_indent_common_(Execute ptr, addr rel, addr n, addr stream);
int pprint_logical_block_common_(addr form, addr env, addr *ret);
int pprint_newline_common_(Execute ptr, addr kind, addr stream);
int pprint_tab_common_(Execute ptr, addr kind, addr column, addr colinc, addr stream);
int print_unreadable_object_common_(addr form, addr env, addr *ret);
int set_pprint_dispatch_common_(Execute ptr,
		addr spec, addr call, addr priority, addr table);
int write_common_(Execute ptr, addr var, addr args);
int prin1_common_(Execute ptr, addr var, addr stream);
int princ_common_(Execute ptr, addr var, addr stream);
int print_common_(Execute ptr, addr var, addr stream);
int pprint_common_(Execute ptr, addr var, addr stream);
int write_to_string_common_(Execute ptr, addr var, addr args, addr *ret);
int prin1_to_string_common_(Execute ptr, addr var, addr *ret);
int princ_to_string_common_(Execute ptr, addr var, addr *ret);

void init_call_printer(void);

#endif

