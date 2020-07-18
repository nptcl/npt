#ifndef __FORMAT_PRINT_HEADER__
#define __FORMAT_PRINT_HEADER__

#include "execute.h"
#include "format_typedef.h"
#include "local.h"
#include "typedef.h"

/*
 *  fmtprint
 */
struct fmtstack {
	addr root;
	addr front;
	size_t index;
};

enum fmtcase {
	fmtcase_normal,
	fmtcase_upcase,
	fmtcase_downcase,
	fmtcase_capitalize_all,
	fmtcase_capitalize_first
};

struct fmtprint_struct {
	unsigned delete_space : 1;
	unsigned loop_colon : 1;
	unsigned loop : 1;
	unsigned word : 1;
	unsigned first : 1;
	unsigned last : 1;
	unsigned escape : 1;
	unsigned pretty : 1;
	unsigned fill : 1;
	unsigned fill_white : 1;
	unsigned fill_ignore : 1;
	enum fmtcase conversion : 4; /* signed */
	Execute ptr;
	LocalRoot local;
	addr stream, format, string;
	struct fmtstack *rest;
	size_t now;
};

typedef struct fmtprint_struct *fmtprint;


/*
 *  function
 */
_g int fmtprint_abort_(fmtprint print, size_t index, const char *str, ...);
_g int fmtprop_abort_(fmtprint print,
		struct format_operator *fmt, const char *str, ...);

_g struct format_operator *fmtprint_operator(fmtprint print);
_g void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format);
_g void fmtprint_copy(fmtprint print, fmtprint src);
_g int fmtprint_make_string_(fmtprint print, addr *ret, addr *backup);
_g int fmtprint_stream_(fmtprint print, addr *ret);
_g int fmtprint_stream_output_(fmtprint print);

_g int fmtprint_putc_(fmtprint print, unicode u);
_g int fmtprint_putc_times_(fmtprint print, unicode c, size_t size);
_g int fmtprint_string_(fmtprint print, addr string);
_g int fmtprint_pop_(fmtprint print, struct format_operator *str, addr *ret);
_g int fmtprint_peek_(fmtprint print, struct format_operator *str, addr *ret);
_g int fmtprint_forward_(fmtprint print, struct format_operator *str, size_t n);
_g int fmtprint_absolute_(fmtprint print, struct format_operator *str, size_t n);
_g int fmtprint_rollback_(fmtprint print, struct format_operator *str, size_t n);
_g void fmtprint_clear(fmtprint print);

#endif

