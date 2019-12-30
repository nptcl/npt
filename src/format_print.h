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
_g void fmtprint_abort(fmtprint print, size_t index, const char *str, ...);
_g void fmtprop_abort(fmtprint print,
		struct format_operator *fmt, const char *str, ...);

_g struct format_operator *fmtprint_operator(fmtprint print);
_g void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format);
_g void fmtprint_copy(fmtprint print, fmtprint src);
_g addr fmtprint_make_string(fmtprint print, addr *ret);
_g void fmtprint_stream(fmtprint print, addr *ret);
_g void fmtprint_stream_output(fmtprint print);

_g void fmtprint_putc(fmtprint print, unicode u);
_g void fmtprint_putc_times(fmtprint print, unicode c, size_t size);
_g void fmtprint_string(fmtprint print, addr string);
_g int fmtprint_pop(fmtprint print, struct format_operator *str, addr *ret);
_g void fmtprint_peek(fmtprint print, struct format_operator *str, addr *ret);
_g void fmtprint_forward(fmtprint print, struct format_operator *str, size_t n);
_g void fmtprint_absolute(fmtprint print, struct format_operator *str, size_t n);
_g void fmtprint_rollback(fmtprint print, struct format_operator *str, size_t n);
_g void fmtprint_clear(fmtprint print);

#endif

