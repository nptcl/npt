#include "clos.h"
#include "constant.h"
#include "symbol.h"
#include "type_constant.h"
#include "type_deftype.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"

/*
 *  build-type-symbol
 */
#define DefSymbolType(a,b) define_symbol_type(CONSTANT_##a, TypeTable_##b)
static void define_symbol_type(constindex name, enum TypeTable type)
{
	addr symbol, value;

	GetConstant(name, &symbol);
	gettypetable(type, &value);
	setsymboltype_symbol(symbol, value);
}

void build_type_symbol(void)
{
	DefSymbolType(SYSTEM_INVALID,              Invalid             );
	DefSymbolType(COMMON_ASTERISK,             Asterisk            );

	/* Extract-type */
	DefSymbolType(COMMON_ATOM,                 Atom                );
	DefSymbolType(COMMON_LIST,                 List                );
	DefSymbolType(COMMON_BOOLEAN,              Boolean             );
	DefSymbolType(COMMON_VECTOR,               Vector              );
	DefSymbolType(COMMON_SIMPLE_VECTOR,        SimpleVector        );
	DefSymbolType(COMMON_BIT_VECTOR,           BitVector           );
	DefSymbolType(COMMON_SIMPLE_BIT_VECTOR,    SimpleBitVector     );
	DefSymbolType(COMMON_EXTENDED_CHAR,        ExtendedChar        );
	DefSymbolType(COMMON_STRING,               String              );
	DefSymbolType(COMMON_BASE_STRING,          BaseString          );
	DefSymbolType(COMMON_SIMPLE_STRING,        SimpleString        );
	DefSymbolType(COMMON_SIMPLE_BASE_STRING,   SimpleBaseString    );
	DefSymbolType(COMMON_SIGNED_BYTE,          SignedByte          );
	DefSymbolType(COMMON_UNSIGNED_BYTE,        UnsignedByte        );
	DefSymbolType(COMMON_BIT,                  Bit                 );
	DefSymbolType(COMMON_FIXNUM,               Fixnum              );
	DefSymbolType(COMMON_BIGNUM,               Bignum              );

	/* Atomic-type */
	DefSymbolType(COMMON_NIL,                  Nil                 );
	DefSymbolType(COMMON_T,                    T                   );
	DefSymbolType(COMMON_NULL,                 Null                );
	DefSymbolType(COMMON_CONS,                 Cons                );
	DefSymbolType(COMMON_HASH_TABLE,           Hashtable           );
	DefSymbolType(COMMON_SYMBOL,               Symbol              );
	DefSymbolType(COMMON_KEYWORD,              Keyword             );
	DefSymbolType(COMMON_PACKAGE,              Package             );
	DefSymbolType(COMMON_RANDOM_STATE,         RandomState         );
	DefSymbolType(COMMON_READTABLE,            Readtable           );
	DefSymbolType(COMMON_FUNCTION,             Function            );
	DefSymbolType(COMMON_COMPILED_FUNCTION,    CompiledFunction    );
	DefSymbolType(COMMON_PATHNAME,             Pathname            );
	DefSymbolType(COMMON_LOGICAL_PATHNAME,     LogicalPathname     );
	DefSymbolType(COMMON_SEQUENCE,             Sequence            );
	DefSymbolType(COMMON_ARRAY,                Array               );
	DefSymbolType(COMMON_SIMPLE_ARRAY,         SimpleArray         );
	DefSymbolType(COMMON_CHARACTER,            Character           );
	DefSymbolType(COMMON_BASE_CHAR,            BaseChar            );
	DefSymbolType(COMMON_STANDARD_CHAR,        StandardChar        );
	DefSymbolType(COMMON_NUMBER,               Number              );
	DefSymbolType(COMMON_REAL,                 Real                );
	DefSymbolType(COMMON_RATIONAL,             Rational            );
	DefSymbolType(COMMON_RATIO,                Ratio               );
	DefSymbolType(COMMON_INTEGER,              Integer             );
	DefSymbolType(COMMON_COMPLEX,              Complex             );
	DefSymbolType(COMMON_FLOAT,                Float               );
	DefSymbolType(COMMON_SHORT_FLOAT,          ShortFloat          );
	DefSymbolType(COMMON_SINGLE_FLOAT,         SingleFloat         );
	DefSymbolType(COMMON_DOUBLE_FLOAT,         DoubleFloat         );
	DefSymbolType(COMMON_LONG_FLOAT,           LongFloat           );
	DefSymbolType(COMMON_RESTART,              Restart             );
	DefSymbolType(SYSTEM_ENVIRONMENT,          Environment         );
	DefSymbolType(COMMON_STREAM,               Stream              );
	DefSymbolType(COMMON_BROADCAST_STREAM,     BroadcastStream     );
	DefSymbolType(COMMON_CONCATENATED_STREAM,  ConcatenatedStream  );
	DefSymbolType(COMMON_ECHO_STREAM,          EchoStream          );
	DefSymbolType(COMMON_FILE_STREAM,          FileStream          );
	DefSymbolType(COMMON_STRING_STREAM,        StringStream        );
	DefSymbolType(COMMON_SYNONYM_STREAM,       SynonymStream       );
	DefSymbolType(COMMON_TWO_WAY_STREAM,       TwoWayStream        );
	DefSymbolType(SYSTEM_PROMPT_STREAM,        PromptStream        );
	DefSymbolType(SYSTEM_PRETTY_STREAM,        PrettyStream        );
	DefSymbolType(SYSTEM_MEMORY_STREAM,        MemoryStream        );
	DefSymbolType(SYSTEM_QUOTE,                Quote               );
	DefSymbolType(SYSTEM_BYTESPEC,             ByteSpec            );
	DefSymbolType(SYSTEM_PRINT_DISPATCH,       PrintDispatch       );
	DefSymbolType(SYSTEM_EVAL,                 Eval                );
}


/*
 *  init-type-symbol
 */
static constindex TypeSymbolTable[LISPDECL_SIZE];
#define DefTypeSymbol(a,b) define_type_symbol(LISPDECL_##a, CONSTANT_##b)
static void define_type_symbol(enum LISPDECL type, constindex name)
{
	TypeSymbolTable[(int)type] = name;
}

void init_type_symbol(void)
{
	DefTypeSymbol(EMPTY,                EMPTY                       );
	DefTypeSymbol(INVALID,              SYSTEM_INVALID              );
	DefTypeSymbol(OPTIMIZED,            EMPTY                       );
	DefTypeSymbol(SUBTYPEP,             EMPTY                       );
	DefTypeSymbol(TYPE,                 SYSTEM_TYPE                 );
	DefTypeSymbol(CLOS,                 EMPTY                       );
	DefTypeSymbol(ASTERISK,             COMMON_ASTERISK             );

	/* Compound-type */
	DefTypeSymbol(AND,                  EMPTY                       );
	DefTypeSymbol(EQL,                  EMPTY                       );
	DefTypeSymbol(MEMBER,               EMPTY                       );
	DefTypeSymbol(MOD,                  EMPTY                       );
	DefTypeSymbol(NOT,                  EMPTY                       );
	DefTypeSymbol(OR,                   EMPTY                       );
	DefTypeSymbol(SATISFIES,            EMPTY                       );
	DefTypeSymbol(VALUES,               EMPTY                       );

	/* Extract-type */
	DefTypeSymbol(ATOM,                 COMMON_ATOM                 );
	DefTypeSymbol(LIST,                 COMMON_LIST                 );
	DefTypeSymbol(BOOLEAN,              COMMON_BOOLEAN              );
	DefTypeSymbol(VECTOR,               COMMON_VECTOR               );
	DefTypeSymbol(SIMPLE_VECTOR,        COMMON_SIMPLE_VECTOR        );
	DefTypeSymbol(BIT_VECTOR,           COMMON_BIT_VECTOR           );
	DefTypeSymbol(SIMPLE_BIT_VECTOR,    COMMON_SIMPLE_BIT_VECTOR    );
	DefTypeSymbol(EXTENDED_CHAR,        COMMON_EXTENDED_CHAR        );
	DefTypeSymbol(STRING,               COMMON_STRING               );
	DefTypeSymbol(BASE_STRING,          COMMON_BASE_STRING          );
	DefTypeSymbol(SIMPLE_STRING,        COMMON_SIMPLE_STRING        );
	DefTypeSymbol(SIMPLE_BASE_STRING,   COMMON_SIMPLE_BASE_STRING   );
	DefTypeSymbol(SIGNED_BYTE,          COMMON_SIGNED_BYTE          );
	DefTypeSymbol(UNSIGNED_BYTE,        COMMON_UNSIGNED_BYTE        );
	DefTypeSymbol(BIT,                  COMMON_BIT                  );
	DefTypeSymbol(FIXNUM,               COMMON_FIXNUM               );
	DefTypeSymbol(BIGNUM,               COMMON_BIGNUM               );

	/* Atomic-type */
	DefTypeSymbol(NIL,                  COMMON_NIL                  );
	DefTypeSymbol(T,                    COMMON_T                    );
	DefTypeSymbol(NULL,                 COMMON_NULL                 );
	DefTypeSymbol(CONS,                 COMMON_CONS                 );
	DefTypeSymbol(HASH_TABLE,           COMMON_HASH_TABLE           );
	DefTypeSymbol(SYMBOL,               COMMON_SYMBOL               );
	DefTypeSymbol(KEYWORD,              COMMON_KEYWORD              );
	DefTypeSymbol(PACKAGE,              COMMON_PACKAGE              );
	DefTypeSymbol(RANDOM_STATE,         COMMON_RANDOM_STATE         );
	DefTypeSymbol(READTABLE,            COMMON_READTABLE            );
	DefTypeSymbol(FUNCTION,             COMMON_FUNCTION             );
	DefTypeSymbol(COMPILED_FUNCTION,    COMMON_COMPILED_FUNCTION    );
	DefTypeSymbol(PATHNAME,             COMMON_PATHNAME             );
	DefTypeSymbol(LOGICAL_PATHNAME,     COMMON_LOGICAL_PATHNAME     );
	DefTypeSymbol(SEQUENCE,             COMMON_SEQUENCE             );
	DefTypeSymbol(ARRAY,                COMMON_ARRAY                );
	DefTypeSymbol(SIMPLE_ARRAY,         COMMON_SIMPLE_ARRAY         );
	DefTypeSymbol(CHARACTER,            COMMON_CHARACTER            );
	DefTypeSymbol(BASE_CHAR,            COMMON_BASE_CHAR            );
	DefTypeSymbol(STANDARD_CHAR,        COMMON_STANDARD_CHAR        );
	DefTypeSymbol(NUMBER,               COMMON_NUMBER               );
	DefTypeSymbol(REAL,                 COMMON_REAL                 );
	DefTypeSymbol(RATIONAL,             COMMON_RATIONAL             );
	DefTypeSymbol(RATIO,                COMMON_RATIO                );
	DefTypeSymbol(INTEGER,              COMMON_INTEGER              );
	DefTypeSymbol(COMPLEX,              COMMON_COMPLEX              );
	DefTypeSymbol(FLOAT,                COMMON_FLOAT                );
	DefTypeSymbol(SHORT_FLOAT,          COMMON_SHORT_FLOAT          );
	DefTypeSymbol(SINGLE_FLOAT,         COMMON_SINGLE_FLOAT         );
	DefTypeSymbol(DOUBLE_FLOAT,         COMMON_DOUBLE_FLOAT         );
	DefTypeSymbol(LONG_FLOAT,           COMMON_LONG_FLOAT           );
	DefTypeSymbol(RESTART,              COMMON_RESTART              );
	DefTypeSymbol(ENVIRONMENT,          SYSTEM_ENVIRONMENT          );
	DefTypeSymbol(STREAM,               COMMON_STREAM               );
	DefTypeSymbol(BROADCAST_STREAM,     COMMON_BROADCAST_STREAM     );
	DefTypeSymbol(CONCATENATED_STREAM,  COMMON_CONCATENATED_STREAM  );
	DefTypeSymbol(ECHO_STREAM,          COMMON_ECHO_STREAM          );
	DefTypeSymbol(FILE_STREAM,          COMMON_FILE_STREAM          );
	DefTypeSymbol(STRING_STREAM,        COMMON_STRING_STREAM        );
	DefTypeSymbol(SYNONYM_STREAM,       COMMON_SYNONYM_STREAM       );
	DefTypeSymbol(TWO_WAY_STREAM,       COMMON_TWO_WAY_STREAM       );
	DefTypeSymbol(PROMPT_STREAM,        SYSTEM_PROMPT_STREAM        );
	DefTypeSymbol(PRETTY_STREAM,        SYSTEM_PRETTY_STREAM        );
	DefTypeSymbol(MEMORY_STREAM,        SYSTEM_MEMORY_STREAM        );
	DefTypeSymbol(QUOTE,                SYSTEM_QUOTE                );
	DefTypeSymbol(BYTESPEC,             SYSTEM_BYTESPEC             );
	DefTypeSymbol(PRINT_DISPATCH,       SYSTEM_PRINT_DISPATCH       );
	DefTypeSymbol(EVAL,                 SYSTEM_EVAL                 );
}


/*
 *  symbol -> type
 *    1. find symbol.
 *    2. find clos.
 *    3. find deftype.
 *    4. return unbound.
 */
int find_symbol_type(Execute ptr, addr *ret, addr symbol, addr env)
{
	addr check;

	Check(! symbolp(symbol), "type error");
	/* find symbol */
	getsymboltype_symbol(symbol, &check);
	if (check != Nil) {
		*ret = check;
		return 0;
	}

	/* find clos */
	clos_find_class_nil(symbol, &check);
	if (check != Nil) {
		type_clos_heap(check, ret);
		return 0;
	}

	/* find deftype */
	if (execute_symbol_deftype(ptr, &check, symbol, env))
		return 1;
	if (check)
		return parse_type(ptr, ret, check, env);

	/* error */
	*ret = NULL;
	return 0;
}


/*
 *  type -> symbol
 */
constindex getdeclname(enum LISPDECL type)
{
	Check(LISPDECL_SIZE <= type, "index error");
	return TypeSymbolTable[type];
}


/*
 *  function
 */
int type_symbol_p(addr symbol)
{
	addr check;

	/* symbol check */
	if (! symbolp(symbol))
		return 0;

	/* find symbol */
	getsymboltype_symbol(symbol, &check);
	if (check != Nil)
		return 1;

	/* find clos */
	clos_find_class_nil(symbol, &check);
	if (check != Nil)
		return 1;

	/* find deftype */
	return symbol_deftypep(symbol);
}

