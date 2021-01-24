#ifndef __CALL_READER_HEADER__
#define __CALL_READER_HEADER__

#include "execute.h"
#include "typedef.h"

#define copy_readtable_common _n(copy_readtable_common)
#define make_dispatch_macro_character_common _n(make_dispatch_macro_character_common)
#define read_common_ _n(read_common_)
#define read_preserving_whitespace_common_ _n(read_preserving_whitespace_common_)
#define read_delimited_list_common _n(read_delimited_list_common)
#define read_from_string_common _n(read_from_string_common)
#define readtable_case_common _n(readtable_case_common)
#define setf_readtable_case_common _n(setf_readtable_case_common)
#define get_dispatch_macro_character_common _n(get_dispatch_macro_character_common)
#define set_dispatch_macro_character_common _n(set_dispatch_macro_character_common)
#define get_macro_character_common _n(get_macro_character_common)
#define set_macro_character_common _n(set_macro_character_common)
#define set_syntax_from_char_common _n(set_syntax_from_char_common)
#define with_standard_io_syntax_common _n(with_standard_io_syntax_common)

int copy_readtable_common(Execute ptr, addr from, addr to, addr *ret);
int make_dispatch_macro_character_common(Execute ptr,
		addr code, addr nonterm, addr readtable);
int read_common_(Execute ptr, addr stream, addr errorp, addr eof, addr recp, addr *ret);
int read_preserving_whitespace_common_(Execute ptr,
		addr stream, addr errorp, addr eof, addr recp, addr *ret);
int read_delimited_list_common(Execute ptr, addr code, addr stream, addr recp);
int read_from_string_common(Execute ptr, addr args, addr *ret, addr *sec);
int readtable_case_common(addr var, addr *ret);
int setf_readtable_case_common(addr value, addr var);
int get_dispatch_macro_character_common(Execute ptr,
		addr x, addr y, addr readtable, addr *ret);
int set_dispatch_macro_character_common(Execute ptr,
		addr x, addr y, addr call, addr readtable);
int get_macro_character_common(Execute ptr,
		addr code, addr readtable, addr *ret, addr *sec);
int set_macro_character_common(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable);
int set_syntax_from_char_common(Execute ptr, addr x, addr y, addr z, addr w);
int with_standard_io_syntax_common(addr form, addr env, addr *ret);

#endif

