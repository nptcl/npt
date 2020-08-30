#ifndef __READER_FUNCTION_HEADER__
#define __READER_FUNCTION_HEADER__

#include "local.h"
#include "typedef.h"

#define double_quote_reader _n(double_quote_reader)
#define single_quote_reader _n(single_quote_reader)
#define read_delimited_list _n(read_delimited_list)
#define parensis_open_reader _n(parensis_open_reader)
#define parensis_close_reader _n(parensis_close_reader)
#define semicolon_reader_ _n(semicolon_reader_)
#define backquote_reader _n(backquote_reader)
#define comma_reader _n(comma_reader)
#define sharp_reader _n(sharp_reader)
#define error_dispatch _n(error_dispatch)
#define equal_dispatch _n(equal_dispatch)
#define sharp_dispatch _n(sharp_dispatch)
#define single_quote_dispatch _n(single_quote_dispatch)
#define parensis_open_dispatch _n(parensis_open_dispatch)
#define parensis_close_dispatch _n(parensis_close_dispatch)
#define asterisk_dispatch_ _n(asterisk_dispatch_)
#define colon_dispatch _n(colon_dispatch)
#define less_dispatch _n(less_dispatch)
#define backslash_dispatch _n(backslash_dispatch)
#define or_dispatch_ _n(or_dispatch_)
#define plus_dispatch _n(plus_dispatch)
#define minus_dispatch _n(minus_dispatch)
#define dot_dispatch _n(dot_dispatch)
#define radix_dispatch _n(radix_dispatch)
#define binary_dispatch _n(binary_dispatch)
#define octal_dispatch _n(octal_dispatch)
#define hexadecimal_dispatch _n(hexadecimal_dispatch)
#define complex_dispatch _n(complex_dispatch)
#define array_dispatch _n(array_dispatch)
#define pathname_dispatch _n(pathname_dispatch)
#define structure_dispatch _n(structure_dispatch)
#define init_reader_function _n(init_reader_function)

/* reader */
_g int double_quote_reader(LocalRoot local, addr stream, addr *ret);
_g int single_quote_reader(Execute ptr, addr stream, addr *ret);
_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive);
_g int parensis_open_reader(Execute ptr, addr stream);
_g int parensis_close_reader(void);
_g int semicolon_reader_(addr stream);
_g int backquote_reader(Execute ptr, addr stream, addr *ret);
_g int comma_reader(Execute ptr, addr stream, addr *ret);
_g int sharp_reader(Execute ptr, addr stream, addr code);

/* dispatch */
_g int error_dispatch(addr code);
_g int equal_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret);
_g int sharp_dispatch(Execute ptr, addr y, addr *ret);
_g int single_quote_dispatch(Execute ptr, addr stream, addr *ret);
_g int parensis_open_dispatch(Execute ptr, addr stream, addr y, addr *ret);
_g int parensis_close_dispatch(void);
_g int asterisk_dispatch_(Execute ptr, addr stream, addr x, addr y, addr *ret);
_g int colon_dispatch(Execute ptr, addr stream, addr *ret);
_g int less_dispatch(void);
_g int backslash_dispatch(Execute ptr, addr stream, addr *ret);
_g int or_dispatch_(addr stream);
_g int plus_dispatch(Execute ptr, addr stream);
_g int minus_dispatch(Execute ptr, addr stream);
_g int dot_dispatch(Execute ptr, addr stream, addr *ret);
_g int radix_dispatch(Execute ptr, addr stream, addr y, addr *ret);
_g int binary_dispatch(Execute ptr, addr stream, addr *ret);
_g int octal_dispatch(Execute ptr, addr stream, addr *ret);
_g int hexadecimal_dispatch(Execute ptr, addr stream, addr *ret);
_g int complex_dispatch(Execute ptr, addr stream, addr *ret);
_g int array_dispatch(Execute ptr, addr stream, addr y, addr *ret);
_g int pathname_dispatch(Execute ptr, addr stream, addr *ret);
_g int structure_dispatch(Execute ptr, addr stream, addr *ret);

/* initialize */
_g void init_reader_function(void);

#endif

