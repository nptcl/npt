#ifndef __TYPEDEF_ENUM_HEADER__
#define __TYPEDEF_ENUM_HEADER__

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
	LISPTYPE_SYSTEM,
	LISPTYPE_PACKAGE,
	LISPTYPE_RANDOM_STATE,
	LISPTYPE_PATHNAME,
	LISPTYPE_STREAM,
	LISPTYPE_QUOTE,
	LISPTYPE_RESTART,
	LISPTYPE_EVAL,
	LISPTYPE_ENVIRONMENT,
	LISPTYPE_BITVECTOR,
	LISPTYPE_PPRINT,
	LISPTYPE_BYTESPEC,
	LISPSYSTEM_FIXNUM_CACHE,
	LISPSYSTEM_CHARACTER_CACHE,
	LISPSYSTEM_BIGDATA,
	LISPSYSTEM_CHARACTER2,
	LISPSYSTEM_CHARQUEUE,
	LISPSYSTEM_CHARBIT,
	LISPSYSTEM_SYMSTACK,
	LISPSYSTEM_SYMARRAY,
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
	LISPSYSTEM_STRUCTURE_TYPE,
	LISPSYSTEM_USER,
	LISPSYSTEM_CHECK,

	LISPSYSTEM_UNBOUND = 0xFB,
	LISPSYSTEM_SPACE = 0xFC,
	LISPSYSTEM_SPACE1 = 0xFD,
	LISPSYSTEM_RESERVED = 0xFE,
	LISPSYSTEM_END = 0xFF,  /* for core file */
	LISPTYPE_SIZE
};

enum LISPINDEX {
	LISPINDEX_NIL = 0,
	LISPINDEX_T,
	LISPINDEX_PACKAGE,
	LISPINDEX_TYPE_PARAM,
	LISPINDEX_CLOS,
	LISPINDEX_COMBINATION,
	LISPINDEX_SPECIALIZER,
	LISPINDEX_CONTROL,
	LISPINDEX_CODE,
	LISPINDEX_CONST,
	LISPINDEX_NAME_CHAR,
	LISPINDEX_CHAR_NAME,
	LISPINDEX_DECLARE,
	LISPINDEX_TYPETABLE,
	LISPINDEX_SIZE
};

enum LISPPROP {
	LISPPROP_ABORT = 0,
	LISPPROP_ERROR,
	LISPPROP_GC,
	LISPPROP_GC_FULL,
	LISPPROP_GC_PREVENT,
	LISPPROP_EXIT,
	LISPPROP_JUMP,
	LISPPROP_SIZE
};

enum LISPCODE {
	LISPCODE_EXECUTE = 0,
	LISPCODE_SUCCESS,
	LISPCODE_ERROR,
	LISPCODE_ABORT,
	LISPCODE_MEMORY,
	LISPCODE_CONFLICT,
	LISPCODE_CONTROL,
	LISPCODE_SAVECORE,
	LISPCODE_EXIT,
	LISPCODE_SIZE
};

enum LISPDECL {
	LISPDECL_EMPTY,
	LISPDECL_OPTIMIZED,
	LISPDECL_SUBTYPEP,
	LISPDECL_TYPE,
	LISPDECL_CLOS,
	LISPDECL_ASTERISK,

	/* Compound-type */
	LISPDECL_AND,
	LISPDECL_OR,
	LISPDECL_EQL,
	LISPDECL_MEMBER,
	LISPDECL_MOD,
	LISPDECL_NOT,
	LISPDECL_SATISFIES,
	LISPDECL_VALUES,

	/* Extract-type */
	LISPDECL_ATOM,
	LISPDECL_LIST,
	LISPDECL_BOOLEAN,
	LISPDECL_VECTOR,
	LISPDECL_SIMPLE_VECTOR,
	LISPDECL_BIT_VECTOR,
	LISPDECL_SIMPLE_BIT_VECTOR,
	LISPDECL_EXTENDED_CHAR,
	LISPDECL_STRING,
	LISPDECL_BASE_STRING,
	LISPDECL_SIMPLE_STRING,
	LISPDECL_SIMPLE_BASE_STRING,
	LISPDECL_SIGNED_BYTE,
	LISPDECL_UNSIGNED_BYTE,
	LISPDECL_BIT,
	LISPDECL_FIXNUM,
	LISPDECL_BIGNUM,

	/* Atomic-type */
	LISPDECL_NIL,
	LISPDECL_T,
	LISPDECL_NULL,
	LISPDECL_CONS,
	LISPDECL_HASH_TABLE,
	LISPDECL_SYMBOL,
	LISPDECL_KEYWORD,
	LISPDECL_PACKAGE,
	LISPDECL_RANDOM_STATE,
	LISPDECL_READTABLE,
	LISPDECL_FUNCTION,
	LISPDECL_COMPILED_FUNCTION,
	LISPDECL_PATHNAME,
	LISPDECL_LOGICAL_PATHNAME,
	LISPDECL_SEQUENCE,
	LISPDECL_ARRAY,
	LISPDECL_SIMPLE_ARRAY,
	LISPDECL_CHARACTER,
	LISPDECL_BASE_CHAR,
	LISPDECL_STANDARD_CHAR,
	LISPDECL_NUMBER,
	LISPDECL_REAL,
	LISPDECL_RATIONAL,
	LISPDECL_RATIO,
	LISPDECL_INTEGER,
	LISPDECL_COMPLEX,
	LISPDECL_FLOAT,
	LISPDECL_SHORT_FLOAT,
	LISPDECL_SINGLE_FLOAT,
	LISPDECL_DOUBLE_FLOAT,
	LISPDECL_LONG_FLOAT,
	LISPDECL_RESTART,
	LISPDECL_ENVIRONMENT,
	LISPDECL_STREAM,
	LISPDECL_BROADCAST_STREAM,
	LISPDECL_CONCATENATED_STREAM,
	LISPDECL_ECHO_STREAM,
	LISPDECL_FILE_STREAM,
	LISPDECL_STRING_STREAM,
	LISPDECL_SYNONYM_STREAM,
	LISPDECL_TWO_WAY_STREAM,
	LISPDECL_PROMPT_STREAM,
	LISPDECL_QUOTE,
	LISPDECL_BYTESPEC,

	/* Size */
	LISPDECL_SIZE
};

enum ARRAY_TYPE {
	ARRAY_TYPE_EMPTY,
	ARRAY_TYPE_T,             /* addr */
	ARRAY_TYPE_BIT,           /* bit */
	ARRAY_TYPE_CHARACTER,     /* base-string */
	ARRAY_TYPE_SIGNED,        /* 8, 16, 32, (64)bit signed-integer */
	ARRAY_TYPE_UNSIGNED,      /* 8, 16, 32, (64)bit unsigned-integer */
	ARRAY_TYPE_SINGLE_FLOAT,  /* 32bit float */
	ARRAY_TYPE_DOUBLE_FLOAT,  /* 64bit float */
	ARRAY_TYPE_LONG_FLOAT,    /* 64, 80, 128bit float */
	ARRAY_TYPE_SIZE
};

#endif

