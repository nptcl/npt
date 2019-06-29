#ifndef __PRINT_HEADER__
#define __PRINT_HEADER__

#include <stdarg.h>
#include "format.h"
#include "readtable.h"
#include "typedef.h"

enum PrintCase {
	PrintCase_unread = 0,
	PrintCase_upcase,
	PrintCase_downcase,
	PrintCase_capitalize,
	PrintCase_preserve,
	PrintCase_invert,
	PrintCase_escape,
	PrintCase_size
};

struct PrintFormat {
	unsigned array : 1; /* *print-array* */
	unsigned circle : 1; /* *print-circle* */
	unsigned escape : 1; /* *print-escape* */
	unsigned gensym : 1; /* *print-gensym* */
	unsigned pretty : 1; /* *print-pretty* */
	unsigned radix : 1; /* *print-radix* */
	unsigned readably : 1; /* *print-readably* */
	enum PrintCase printcase : 5; /* *print-case* */
	enum PrintCase readcase : 5;
	enum ReadTable_float readfloat : 4;
	unsigned base; /* *print-base* */
	Execute ptr;
	addr dispatch; /* *print-pprint-dispatch* */
	fixnum length; /* *print-length* */
	fixnum level, now; /* *print-level* */
	fixnum lines; /* *print-lines* */
	fixnum width; /* *print-miser-width* */
	fixnum margin; /* *print-right-margin* */
};

_g int write_print(struct PrintFormat *format, addr stream, addr object);
_g void format_print(Execute ptr, struct PrintFormat *format);
_g int princ_print(Execute ptr, addr stream, addr object);
_g int prin1_print(Execute ptr, addr stream, addr object);
_g int princ_string(Execute ptr, LocalRoot local, addr *ret, addr object);
_g int prin1_string(Execute ptr, LocalRoot local, addr *ret, addr object);

/* pretty print */
_g void pprint_dispatch_heap(addr *ret);

/* initialize */
_g void init_print();

#endif

