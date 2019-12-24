#ifndef __FORMAT_TYPEDEF_HEADER__
#define __FORMAT_TYPEDEF_HEADER__

#include <stddef.h>
#include "typedef.h"

enum FormatType {
	FormatType_Error = 0,
	FormatType_End,
	FormatType_Format,
	FormatType_Output,              /* text */
	FormatType_Aesthetic,           /* A */
	FormatType_Standard,            /* S */
	FormatType_Binary,              /* B */
	FormatType_Octal,               /* O */
	FormatType_Decimal,             /* D */
	FormatType_Hexadecimal,         /* X */
	FormatType_Radix,               /* R */
	FormatType_RadixText,           /* R */
	FormatType_Plural,              /* P */
	FormatType_Character,           /* C */
	FormatType_Fixed,               /* F */
	FormatType_Exponential,         /* E */
	FormatType_General,             /* G */
	FormatType_Monetary,            /* $ */
	FormatType_Newline,             /* % */
	FormatType_FreshLine,           /* & */
	FormatType_Page,                /* | */
	FormatType_Tilde,               /* ~ */
	FormatType_IgnoredNewline,      /* \n */
	FormatType_Tabulate,            /* T */
	FormatType_GoTo,                /* * */
	FormatType_Recursive,           /* ? */
	FormatType_ConditionalNewline,  /* _ */
	FormatType_Write,               /* W */
	FormatType_Indent,              /* I */
	FormatType_Case,                /* () */
	FormatType_Condition,           /* [] */
	FormatType_Iteration,           /* {} */
	FormatType_Justification,       /* <> */
	FormatType_LogicalBlock,        /* <> */
	FormatType_EscapeUpward,        /* ^ */
	FormatType_ClauseSeparator,     /* ; */
	FormatType_CallFunction,        /* / */
	FormatType_size
};

enum fmtargs_type {
	fmtargs_nil,
	fmtargs_integer,
	fmtargs_character,
	fmtargs_argument,
	fmtargs_count,
	fmtargs_index,
	fmtargs_size
};

union format_union {
	unicode character;
	fixnum value;
	size_t index;
};

struct format_argument {
	enum fmtargs_type type;
	size_t position;
	union format_union u;
};

struct format_operator {
	enum FormatType type;
	size_t size;
	unsigned colon : 1;
	unsigned atsign : 1;
	unsigned close_colon : 1;
	unsigned close_atsign : 1;
	unsigned option_check : 1;
	unsigned prefix : 1;
	unsigned suffix : 1;
	size_t args_size;
	size_t position, colon_pos, atsign_pos;
};

#endif

