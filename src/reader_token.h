#ifndef __READER_TOKEN_HEADER__
#define __READER_TOKEN_HEADER__

#include "execute.h"
#include "typedef.h"

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
_g unsigned getreadbase(Execute ptr);
_g int read_suppress_p(Execute ptr);
_g void maketoken(Execute ptr, addr *ret);

#endif

