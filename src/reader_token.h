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

void init_reader_token(void);
enum TokenType tokentype(unsigned base, addr queue);
int getreadbase_(Execute ptr, unsigned *ret);
int read_suppress_p_(Execute ptr, int *ret);
int maketoken_(Execute ptr, addr *ret);

#endif

