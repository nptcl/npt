#ifndef __TYPEDEF_VALUE_HEADER__
#define __TYPEDEF_VALUE_HEADER__

enum LISPTYPE {
	LISPTYPE_NIL = 0,
	LISPTYPE_T,
	LISPTYPE_TYPE,
	LISPTYPE_CLOS,
	LISPTYPE_CONS,
	LISPTYPE_ARRAY,
	LISPTYPE_VECTOR,
	LISPTYPE_CHARACTER,
	LISPTYPE_STRING,
	LISPTYPE_HASHTABLE,
	LISPTYPE_READTABLE,
	LISPTYPE_SYMBOL,
	LISPTYPE_FIXNUM,
	LISPTYPE_BIGNUM,
	LISPTYPE_RATIO,
	LISPTYPE_SHORT_FLOAT,
	LISPTYPE_SINGLE_FLOAT,
	LISPTYPE_DOUBLE_FLOAT,
	LISPTYPE_LONG_FLOAT,
	LISPTYPE_COMPLEX,
	LISPTYPE_CONTROL,
	LISPTYPE_CODE,
	LISPTYPE_CALLNAME,
	LISPTYPE_FUNCTION,
	LISPTYPE_INDEX,
	LISPTYPE_PACKAGE,
	LISPTYPE_RANDOM_STATE,
	LISPTYPE_PATHNAME,
	LISPTYPE_STREAM,
	LISPTYPE_QUOTE,
	LISPTYPE_RESTART,
	LISPTYPE_EVAL,
	LISPTYPE_ENVIRONMENT,
	LISPTYPE_BITVECTOR,
	LISPTYPE_PRINT_DISPATCH,
	LISPTYPE_BYTESPEC,
	LISPTYPE_FORMAT,
	LISPTYPE_LOAD_TIME_VALUE,
	LISPTYPE_PAPER,
	LISPTYPE_COMPILE, /* for faslwrite */

	LISPSYSTEM_CONSTANT,
	LISPSYSTEM_FIXNUM_CACHE,
	LISPSYSTEM_CHARACTER_CACHE,
	LISPSYSTEM_BIGBUFFER,
	LISPSYSTEM_BIGCONS,
	LISPSYSTEM_BIGDATA,
	LISPSYSTEM_CHARACTER2,
	LISPSYSTEM_CHARQUEUE,
	LISPSYSTEM_CHARBIT,
	LISPSYSTEM_SYMSTACK,
	LISPSYSTEM_BITTYPE,
	LISPSYSTEM_READLABEL,
	LISPSYSTEM_READINFO,
	LISPSYSTEM_READTYPE,
	LISPSYSTEM_BITCONS,
	LISPSYSTEM_BITBUFFER,
	LISPSYSTEM_HASHITERATOR,
	LISPSYSTEM_PACKAGEITERATOR,
	LISPSYSTEM_TAGINFO,
	LISPSYSTEM_ARRAY_DIMENSION,
	LISPSYSTEM_ARRAY_GENERAL,
	LISPSYSTEM_ARRAY_SPECIALIZED,
	LISPSYSTEM_CODE,
	LISPSYSTEM_PROMPT,
	LISPSYSTEM_ENVROOT,
	LISPSYSTEM_ENVSTACK,
	LISPSYSTEM_SLOT,
	LISPSYSTEM_SLOT_VECTOR,
	LISPSYSTEM_CLOS_VALUE,
	LISPSYSTEM_GENERIC,
	LISPSYSTEM_ARGUMENT,
	LISPSYSTEM_UNICODE,
	LISPSYSTEM_TYPE_PARSE,
	LISPSYSTEM_STRUCTURE,
	LISPSYSTEM_STRUCTURE_TYPE,
	LISPSYSTEM_PRINT_TABLE,
	LISPSYSTEM_PRINT_WRITE,
	LISPSYSTEM_PRINT_CHECK,
	LISPSYSTEM_PRINT_PRETTY,
	LISPSYSTEM_EVALSTACK,
	LISPSYSTEM_GCHOLD,
	LISPSYSTEM_FORMAT_PRETTY,
	LISPSYSTEM_SLEEP,
	LISPSYSTEM_REDEFINE,
	LISPSYSTEM_HANDLER,
	LISPSYSTEM_SPECIAL,
	LISPSYSTEM_VALUES,
	LISPSYSTEM_EXECUTE,
	LISPSYSTEM_LEXICAL,
	LISPSYSTEM_CLOSURE,
	LISPSYSTEM_REFERENCE,
	LISPSYSTEM_HOLD,
	LISPSYSTEM_BUFFERING,
	LISPSYSTEM_BUFCELL,
	LISPSYSTEM_TERME,
	LISPSYSTEM_CHECK,

	LISPSYSTEM_UNBOUND = 0xFB,
	LISPSYSTEM_SPACE = 0xFC,
	LISPSYSTEM_SPACE1 = 0xFD,
	LISPSYSTEM_RESERVED = 0xFE,
	LISPSYSTEM_END = 0xFF,  /* for core file */
	LISPTYPE_SIZE
};
typedef enum LISPTYPE LispType;

enum LISPINDEX {
	LISPINDEX_NIL = 0,
	LISPINDEX_T,
	LISPINDEX_PACKAGE,
	LISPINDEX_SPECIALIZER,
	LISPINDEX_CONST,
	LISPINDEX_NAME_CHAR,
	LISPINDEX_CHAR_NAME,
	LISPINDEX_DECLARE,
	LISPINDEX_TYPETABLE,
	LISPINDEX_SIZE
};

enum EVAL_OPTIMIZE {
	EVAL_OPTIMIZE_COMPILATION = 0,
	EVAL_OPTIMIZE_DEBUG,
	EVAL_OPTIMIZE_SAFETY,
	EVAL_OPTIMIZE_SPACE,
	EVAL_OPTIMIZE_SPEED,
	EVAL_OPTIMIZE_SIZE
};

enum GcMode {
	GcMode_Off,
	GcMode_Default,
	GcMode_Partial,
	GcMode_Full
};

#endif

