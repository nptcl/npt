#ifndef __READER_FUNCTION_HEADER__
#define __READER_FUNCTION_HEADER__

#include "local.h"
#include "typedef.h"

/* reader */
_g int double_quote_reader(LocalRoot local, addr stream, addr *ret);
_g int single_quote_reader(Execute ptr, addr stream, addr *ret);
_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive);
_g int parensis_open_reader(Execute ptr, addr stream);
_g int parensis_close_reader(void);
_g void semicolon_reader(addr stream);
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
_g void asterisk_dispatch(Execute ptr, addr stream, addr x, addr y, addr *ret);
_g int colon_dispatch(Execute ptr, addr stream, addr *ret);
_g int less_dispatch(void);
_g int backslash_dispatch(Execute ptr, addr stream, addr *ret);
_g void or_dispatch(addr stream);
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

