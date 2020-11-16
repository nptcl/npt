#ifndef __FORMAT_PRINT_HEADER__
#define __FORMAT_PRINT_HEADER__

#include "execute.h"
#include "format_typedef.h"
#include "local.h"
#include "typedef.h"

#define fmtprint_abort_ _n(fmtprint_abort_)
#define fmtprop_abort_ _n(fmtprop_abort_)
#define fmtprint_operator _n(fmtprint_operator)
#define fmtprint_make _n(fmtprint_make)
#define fmtprint_copy _n(fmtprint_copy)
#define fmtprint_make_string_ _n(fmtprint_make_string_)
#define fmtprint_stream_ _n(fmtprint_stream_)
#define fmtprint_stream_output_ _n(fmtprint_stream_output_)
#define fmtprint_putc_ _n(fmtprint_putc_)
#define fmtprint_putc_times_ _n(fmtprint_putc_times_)
#define fmtprint_string_ _n(fmtprint_string_)
#define fmtprint_pop_ _n(fmtprint_pop_)
#define fmtprint_peek_ _n(fmtprint_peek_)
#define fmtprint_forward_ _n(fmtprint_forward_)
#define fmtprint_absolute_ _n(fmtprint_absolute_)
#define fmtprint_rollback_ _n(fmtprint_rollback_)
#define fmtprint_clear_ _n(fmtprint_clear_)

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
int fmtprint_abort_(fmtprint print, size_t index, const char *str, ...);
int fmtprop_abort_(fmtprint print,
		struct format_operator *fmt, const char *str, ...);

struct format_operator *fmtprint_operator(fmtprint print);
void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format);
void fmtprint_copy(fmtprint print, fmtprint src);
int fmtprint_make_string_(fmtprint print, addr *ret, addr *backup);
int fmtprint_stream_(fmtprint print, addr *ret);
int fmtprint_stream_output_(fmtprint print);

int fmtprint_putc_(fmtprint print, unicode u);
int fmtprint_putc_times_(fmtprint print, unicode c, size_t size);
int fmtprint_string_(fmtprint print, addr string);
int fmtprint_pop_(fmtprint print, struct format_operator *str, addr *ret);
int fmtprint_peek_(fmtprint print, struct format_operator *str, addr *ret);
int fmtprint_forward_(fmtprint print, struct format_operator *str, size_t n);
int fmtprint_absolute_(fmtprint print, struct format_operator *str, size_t n);
int fmtprint_rollback_(fmtprint print, struct format_operator *str, size_t n);
int fmtprint_clear_(fmtprint print);

#endif

