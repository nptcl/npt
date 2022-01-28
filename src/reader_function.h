#ifndef __READER_FUNCTION_HEADER__
#define __READER_FUNCTION_HEADER__

#include "local.h"
#include "typedef.h"

#define double_quote_reader_ _n(double_quote_reader_)
#define single_quote_reader_ _n(single_quote_reader_)
#define read_delimited_list_ _n(read_delimited_list_)
#define parensis_open_reader_ _n(parensis_open_reader_)
#define parensis_close_reader_ _n(parensis_close_reader_)
#define semicolon_reader_ _n(semicolon_reader_)
#define backquote_reader_ _n(backquote_reader_)
#define comma_reader_ _n(comma_reader_)
#define sharp_reader_ _n(sharp_reader_)
#define error_dispatch_ _n(error_dispatch_)
#define equal_dispatch_ _n(equal_dispatch_)
#define sharp_dispatch_ _n(sharp_dispatch_)
#define single_quote_dispatch_ _n(single_quote_dispatch_)
#define parensis_open_dispatch_ _n(parensis_open_dispatch_)
#define parensis_close_dispatch_ _n(parensis_close_dispatch_)
#define asterisk_dispatch_ _n(asterisk_dispatch_)
#define colon_dispatch_ _n(colon_dispatch_)
#define less_dispatch_ _n(less_dispatch_)
#define backslash_dispatch_ _n(backslash_dispatch_)
#define or_dispatch_ _n(or_dispatch_)
#define plus_dispatch_ _n(plus_dispatch_)
#define minus_dispatch_ _n(minus_dispatch_)
#define dot_dispatch_ _n(dot_dispatch_)
#define radix_dispatch_ _n(radix_dispatch_)
#define binary_dispatch_ _n(binary_dispatch_)
#define octal_dispatch_ _n(octal_dispatch_)
#define hexadecimal_dispatch_ _n(hexadecimal_dispatch_)
#define complex_dispatch_ _n(complex_dispatch_)
#define array_dispatch_ _n(array_dispatch_)
#define pathname_dispatch_ _n(pathname_dispatch_)
#define structure_dispatch_ _n(structure_dispatch_)
#define init_reader_function _n(init_reader_function)

/* reader */
int double_quote_reader_(LocalRoot local, addr stream, addr *ret);
int single_quote_reader_(Execute ptr, addr stream, addr *ret);
int read_delimited_list_(Execute ptr, addr stream, unicode limit, int recursive);
int parensis_open_reader_(Execute ptr, addr stream);
int parensis_close_reader_(void);
int semicolon_reader_(addr stream);
int backquote_reader_(Execute ptr, addr stream, addr *ret);
int comma_reader_(Execute ptr, addr stream, addr *ret);
int sharp_reader_(Execute ptr, addr stream, addr code);

/* dispatch */
int error_dispatch_(addr code);
int equal_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret);
int sharp_dispatch_(Execute ptr, addr y, addr *ret);
int single_quote_dispatch_(Execute ptr, addr stream, addr *ret);
int parensis_open_dispatch_(Execute ptr, addr stream, addr y, addr *ret);
int parensis_close_dispatch_(void);
int asterisk_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret);
int colon_dispatch_(Execute ptr, addr stream, addr *ret);
int less_dispatch_(void);
int backslash_dispatch_(Execute ptr, addr stream, addr *ret);
int or_dispatch_(addr stream);
int plus_dispatch_(Execute ptr, addr stream);
int minus_dispatch_(Execute ptr, addr stream);
int dot_dispatch_(Execute ptr, addr stream, addr *ret);
int radix_dispatch_(Execute ptr, addr stream, addr y, addr *ret);
int binary_dispatch_(Execute ptr, addr stream, addr *ret);
int octal_dispatch_(Execute ptr, addr stream, addr *ret);
int hexadecimal_dispatch_(Execute ptr, addr stream, addr *ret);
int complex_dispatch_(Execute ptr, addr stream, addr *ret);
int array_dispatch_(Execute ptr, addr stream, addr y, addr *ret);
int pathname_dispatch_(Execute ptr, addr stream, addr *ret);
int structure_dispatch_(Execute ptr, addr stream, addr *ret);

/* initialize */
void init_reader_function(void);

#endif

