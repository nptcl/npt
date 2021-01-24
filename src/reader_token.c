#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "package.h"
#include "package_intern.h"
#include "reader_info.h"
#include "reader_token.h"
#include "symbol.h"
#include "token.h"
#include "typedef.h"

/*
 *  chartable
 */
struct chartable {
	unsigned chartype : 1;
	unsigned exponent : 1;
};
static struct chartable Reader_CharTable[0x80];

void init_reader_token(void)
{
	static const char *const str1 =
		"!\"#$%&'(),;<=>?[\\]^_`{|}~.+-*/@"
		"0123456789"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static const char *const str2 = "defslDEFSL";
	const char *str;

	/* Illegal */
	cleartype(Reader_CharTable);
	/* char */
	for (str = str1; *str; str++)
		Reader_CharTable[(int)*str].chartype = 1;
	/* exponent */
	for (str = str2; *str; str++)
		Reader_CharTable[(int)*str].exponent = 1;
}


/*
 *  tokentype
 */
static int checktable_char(unicode c)
{
	return (c < 0x80) && (Reader_CharTable[c].chartype);
}

static int checktable_base(unsigned base, unicode c)
{
	return ! getvalue_digit(base, c, &base);
}

static int checktable_sign(unicode c)
{
	return (c == '+') || (c == '-');
}

static int checktable_exponent(unicode c)
{
	return (c < 0x80) && (Reader_CharTable[c].exponent);
}

static int checktable_potential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '/'
		|| c == '.' || c == '^' || c == '_'
		|| checktable_exponent(c);
}

static int checktable_firstpotential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '.' || c == '^' || c == '_';
}

static int checktable_isdigit(unicode c)
{
	return (c < 0x80) && isDigitCase(c);
}

static int checktable_isalpha(unicode c)
{
	return (c < 0x80) && isAlphabetic(c);
}

static int checktable_force(unicode c)
{
	return (0x80 <= c);
}

/* for debug */
/* #define OUTPUTSTATE(x, c)  printf("[%s:%c]\n", x, c) */
#define OUTPUTSTATE(x, c)  /*do nothing*/

#define ReadtableNext() { \
	getchar_charqueue(queue, i++, &c); \
	if (c && !checktable_char(c)) { \
		goto error; \
	} \
	if (!pot) { \
		pot = checktable_base(base, c); \
	} \
}

#define checkbasegoto(base, digit, c, label) { \
	if (checktable_base(base, c)) { \
		if (digit) { \
			digit = checktable_isdigit(c); \
		} \
		goto label; \
	} \
}

enum TokenType tokentype(unsigned base, addr queue)
{
	unicode c;
	size_t i;
	int first, pot, digit;

	/* init */
	i = 0;
	pot = 0;
	digit = 1;

	/* first */
	ReadtableNext();
	first = checktable_firstpotential(base, c);
	OUTPUTSTATE("first", c);
	if (c == 0)
		goto error;
	if (checktable_sign(c))
		goto digit1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (c == '.')
		goto dot1;
	goto symbol;

	/* dot check */
dot1:
	ReadtableNext();
	OUTPUTSTATE("dot1", c);
	if (c == 0)
		goto token_dot;
	if (checktable_isdigit(c))
		goto float6;
	if (c == '.')
		goto dot2;
	goto symbol;

dot2:
	ReadtableNext();
	OUTPUTSTATE("dot2", c);
	if (c == '.')
		goto dot2;
	if (c == 0)
		goto error;
	goto symbol;

	/* integer */
digit1:
	ReadtableNext();
	OUTPUTSTATE("digit1", c);
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (c == '.')
		goto float5;
	goto symbol_sign;

digit2:
	ReadtableNext();
	OUTPUTSTATE("digit2", c);
	if (c == 0)
		goto token_digit;
	if (c == '.')
		goto digit3;
	if (c == '/')
		goto ratio1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c))
		goto float1;
	if (checktable_exponent(c))
		goto exponent1;
	goto symbol;

digit3:
	ReadtableNext();
	OUTPUTSTATE("digit3", c);
	if (c == 0)
		goto token_decimal;
	if (checktable_isdigit(c))
		goto float4;
	if (checktable_exponent(c))
		goto exponent3;
	goto symbol;

	/* ratio */
ratio1:
	ReadtableNext();
	OUTPUTSTATE("ratio1", c);
	if (checktable_base(base, c))
		goto ratio2;
	goto symbol;

ratio2:
	ReadtableNext();
	OUTPUTSTATE("ratio2", c);
	if (c == 0)
		goto token_ratio;
	if (checktable_base(base, c))
		goto ratio2;
	goto symbol;

	/* float */
float1:
	ReadtableNext();
	OUTPUTSTATE("float1", c);
	if (checktable_isdigit(c))
		goto float1;
	if (checktable_exponent(c))
		goto exponent1;
	if (c == '.')
		goto float3;
	goto symbol;

float3:
	ReadtableNext();
	OUTPUTSTATE("float3", c);
	if (checktable_exponent(c))
		goto exponent1;
	if (checktable_isdigit(c))
		goto float4;
	if (c == 0)
		goto token_decimal;
	goto symbol;

float4:
	ReadtableNext();
	OUTPUTSTATE("float4", c);
	if (c == 0)
		goto check_float;
	if (checktable_exponent(c))
		goto exponent1;
	if (checktable_isdigit(c))
		goto float4;
	goto symbol;

float5:
	ReadtableNext();
	OUTPUTSTATE("float5", c);
	if (checktable_isdigit(c))
		goto float6;
	goto symbol;

float6:
	ReadtableNext();
	OUTPUTSTATE("float6", c);
	if (c == 0)
		goto check_float;
	if (checktable_isdigit(c))
		goto float6;
	if (checktable_exponent(c))
		goto exponent1;
	goto symbol;

check_float:
	if (digit)
		goto token_float;
	goto token_potential;

	/* exponent */
exponent1:
	ReadtableNext();
	OUTPUTSTATE("exponent1", c);
	if (checktable_sign(c))
		goto exponent2;
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol_exponent;

exponent2:
	ReadtableNext();
	OUTPUTSTATE("exponent2", c);
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol_sign;

exponent3:
	ReadtableNext();
	OUTPUTSTATE("exponent3", c);
	if (c == 0)
		goto check_float;
	if (checktable_isdigit(c))
		goto exponent3;
	goto symbol;

	/* symbol */
symbol:
	OUTPUTSTATE("symbol", c);
	if (first == 0)
		goto token_symbol;
	goto potential_symbol;

potential:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_symbol:
	if (c == 0)
		goto token_potential;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto potential_marker;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* symbol-sign */
symbol_sign:
	OUTPUTSTATE("symbol_sign", c);
	if (first == 0)
		goto token_symbol;
	goto potential_sign_symbol;

potential_sign:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_sign_symbol:
	if (c == 0)
		goto token_symbol;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto potential_marker;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* symbol-marker */
symbol_exponent:
	OUTPUTSTATE("symbol_exponent", c);
	if (first == 0)
		goto token_symbol;
	goto potential_marker_symbol;

potential_marker:
	ReadtableNext();
	OUTPUTSTATE("potential_marker", c);
potential_marker_symbol:
	if (c == 0)
		goto token_potential;
	if (checktable_base(base, c))
		goto potential;
	if (checktable_isalpha(c))
		goto token_symbol;
	if (checktable_sign(c))
		goto potential_sign;
	if (checktable_potential(base, c))
		goto potential;
	goto token_symbol;

	/* token */
token_digit:
	OUTPUTSTATE("token_digit", '-');
	return TokenType_integer;

token_decimal:
	OUTPUTSTATE("token_decimal", '-');
	return TokenType_decimal;

token_ratio:
	OUTPUTSTATE("token_ratio", '-');
	return TokenType_ratio;

token_float:
	OUTPUTSTATE("token_float", '-');
	return TokenType_float;

token_dot:
	OUTPUTSTATE("token_dot", '-');
	return TokenType_dot;

token_symbol:
	OUTPUTSTATE("token_symbol", '-');
	return TokenType_symbol;

token_potential:
	OUTPUTSTATE("token_potential", '-');
	return pot? TokenType_potential: TokenType_symbol;

error:
	if (checktable_force(c))
		goto force;
	OUTPUTSTATE("error", '-');
	return TokenType_error;

force:
	OUTPUTSTATE("force_unicode", '-');
	return TokenType_symbol;
}


/*
 *  maketoken
 */
int getreadbase_(Execute ptr, unsigned *ret)
{
	addr one;
	fixnum value;

	GetConst(SPECIAL_READ_BASE, &one);
	Return(getspecialcheck_local_(ptr, one, &one));
	Check(GetType(one) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(one, &value);
	if (! isBaseChar(value)) {
		*ret = 0;
		fixnum_heap(&one, (fixnum)value);
		return fmte_("base ~a must be a number between 2 and 36.", one, NULL);
	}

	return Result(ret, (unsigned)value);
}

static int maketoken_intern_(Execute ptr,
		addr package, addr name, int unexport, addr *ret)
{
	int check;
	addr value;

	/* keyword */
	if (package == T) {
		GetConst(PACKAGE_KEYWORD, &package);
		return intern_package_(package, name, ret, NULL);
	}

	/* package::name */
	if (unexport)
		return intern_package_(package, name, ret, NULL);

	/* package:name, unexport */
	Return(exportp_name_package_(package, name, &value, &check));
	if (check)
		return Result(ret, value);

	/* package:name, export, OK */
	if (value != Unbound) {
		*ret = Nil;
		return fmte_("The symbol ~S is not exported in ~S.", name, package, NULL);
	}

	/* package:name, unbound */
	Return(getpackage_(ptr, &value));
	if (package == value)
		return intern_package_(package, name, ret, NULL);

	/* package:name, error */
	*ret = Nil;
	return fmte_("Cannot intern the symbol ~S in ~S.", name, package, NULL);
}

static int maketoken_package_(Execute ptr, addr *ret, addr queue, addr package)
{
	enum TokenType token;
	struct readinfo_struct *str;
	unsigned base;
	addr name;

	str = getreadinfo_struct(ptr);
	if (str->escape) {
		/* escapemonde, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern package - name */
		return maketoken_intern_(ptr, package, name, str->unexport, ret);
	}

	Return(getreadbase_(ptr, &base));
	token = tokentype(base, queue);
	if (token == TokenType_symbol || token == TokenType_potential) {
		make_charqueue_heap(queue, &name);
		/* intern package - symbol */
		return maketoken_intern_(ptr, package, name, str->unexport, ret);
	}
	else {
		*ret = Nil;
		return fmte_("Token-type error", NULL);
	}
}

static int maketoken_normal_(Execute ptr, addr *ret)
{
	unsigned base;
	addr package, queue, name;

	/* table have package */
	getpackage_readinfo(ptr, &package);
	getqueue_readinfo(ptr, &queue);
	if (package != Nil)
		return maketoken_package_(ptr, ret, queue, package);

	/* no package */
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern name */
		return intern_default_package_(ptr, name, ret, NULL);
	}

	Return(getreadbase_(ptr, &base));
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			/* intern *package* - symbol */
			Return(intern_default_package_(ptr, name, ret, NULL));
			break;

		case TokenType_decimal:
			maketoken_integer(ptr->local, queue, 10, ret);
			break;

		case TokenType_integer:
			maketoken_integer(ptr->local, queue, base, ret);
			break;

		case TokenType_float:
			return maketoken_float_(ptr, queue, ret);

		case TokenType_ratio:
			maketoken_ratio(ptr->local, queue, base, ret);
			break;

		case TokenType_dot:
			if (getdot_readinfo(ptr) == 0) {
				*ret = Nil;
				return fmte_("dot no allowed here.", NULL);
			}
			GetConst(SYSTEM_READTABLE_DOT, ret);
			break;

		case TokenType_error:
		default:
			*ret = Nil;
			return fmte_("token error", NULL);
	}

	return 0;
}

static int maketoken_gensym_(Execute ptr, addr *ret)
{
	unsigned base;
	addr queue, symbol, name;

	/* no package */
	getqueue_readinfo(ptr, &queue);
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		symbol_heap(&symbol);
		SetNameSymbol(symbol, name);
		return Result(ret, symbol);
	}

	Return(getreadbase_(ptr, &base));
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			symbol_heap(&symbol);
			SetNameSymbol(symbol, name);
			return Result(ret, symbol);

		default:
			*ret = Nil;
			return fmte_("token error (gensym)", NULL);
	}
}

int read_suppress_p_(Execute ptr, int *ret)
{
	addr pos;

	/* *read-suppress */
	GetConst(SPECIAL_READ_SUPPRESS, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	return Result(ret, pos != Nil);
}

int maketoken_(Execute ptr, addr *ret)
{
	int check;

	/* *read-suppress */
	Return(read_suppress_p_(ptr, &check));
	if (check)
		return Result(ret, Nil);

	/* make token */
	if (getstate_readinfo(ptr) != ReadInfo_State_Gensym)
		return maketoken_normal_(ptr, ret);
	else
		return maketoken_gensym_(ptr, ret);
}

