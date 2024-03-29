#ifndef __TYPEDEF_OBJECT_HEADER__
#define __TYPEDEF_OBJECT_HEADER__

enum CHARACTER_TYPE {
	CHARACTER_TYPE_EMPTY,
	CHARACTER_TYPE_STANDARD,
	CHARACTER_TYPE_BASE,
	CHARACTER_TYPE_EXTENDED,
	CHARACTER_TYPE_INVALID,
	CHARACTER_TYPE_SIZE
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

