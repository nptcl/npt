#ifndef __CALL_READER_HEADER__
#define __CALL_READER_HEADER__

#include "execute.h"
#include "typedef.h"

#define copy_readtable_common_ _n(copy_readtable_common_)
#define make_dispatch_macro_character_common_ _n(make_dispatch_macro_character_common_)
#define read_common_ _n(read_common_)
#define read_preserving_whitespace_common_ _n(read_preserving_whitespace_common_)
#define read_delimited_list_common_ _n(read_delimited_list_common_)
#define read_from_string_common_ _n(read_from_string_common_)
#define readtable_case_common_ _n(readtable_case_common_)
#define setf_readtable_case_common_ _n(setf_readtable_case_common_)
#define get_dispatch_macro_character_common_ _n(get_dispatch_macro_character_common_)
#define set_dispatch_macro_character_common_ _n(set_dispatch_macro_character_common_)
#define get_macro_character_common_ _n(get_macro_character_common_)
#define set_macro_character_common_ _n(set_macro_character_common_)
#define set_syntax_from_char_common_ _n(set_syntax_from_char_common_)
#define with_standard_io_syntax_common_ _n(with_standard_io_syntax_common_)

int copy_readtable_common_(Execute ptr, addr from, addr to, addr *ret);
int make_dispatch_macro_character_common_(Execute ptr,
		addr code, addr nonterm, addr readtable);
int read_common_(Execute ptr, addr stream, addr errorp, addr eof, addr recp, addr *ret);
int read_preserving_whitespace_common_(Execute ptr,
		addr stream, addr errorp, addr eof, addr recp, addr *ret);
int read_delimited_list_common_(Execute ptr, addr code, addr stream, addr recp);
int read_from_string_common_(Execute ptr, addr args, addr *ret, addr *sec);
int readtable_case_common_(addr var, addr *ret);
int setf_readtable_case_common_(addr value, addr var);
int get_dispatch_macro_character_common_(Execute ptr,
		addr x, addr y, addr readtable, addr *ret);
int set_dispatch_macro_character_common_(Execute ptr,
		addr x, addr y, addr call, addr readtable);
int get_macro_character_common_(Execute ptr,
		addr code, addr readtable, addr *ret, addr *sec);
int set_macro_character_common_(Execute ptr,
		addr code, addr call, addr nonterm, addr readtable);
int set_syntax_from_char_common_(Execute ptr, addr x, addr y, addr z, addr w);
int with_standard_io_syntax_common_(addr form, addr env, addr *ret);

#endif

