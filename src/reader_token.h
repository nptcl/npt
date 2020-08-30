#ifndef __READER_TOKEN_HEADER__
#define __READER_TOKEN_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_reader_token _n(init_reader_token)
#define tokentype _n(tokentype)
#define getreadbase_ _n(getreadbase_)
#define read_suppress_p_ _n(read_suppress_p_)
#define maketoken_ _n(maketoken_)

enum TokenType {
	TokenType_symbol = 0,
	TokenType_potential,
	TokenType_integer,
	TokenType_float,
	TokenType_ratio,
	TokenType_dot,
	TokenType_empty,
	TokenType_error
};

_g void init_reader_token(void);
_g enum TokenType tokentype(unsigned base, addr queue);
_g int getreadbase_(Execute ptr, unsigned *ret);
_g int read_suppress_p_(Execute ptr, int *ret);
_g int maketoken_(Execute ptr, addr *ret);

#endif

