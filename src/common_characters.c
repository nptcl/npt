/*
 *  ANSI COMMON LISP: 13. Characters
 */
#include "character.h"
#include "common_header.h"
#include "cons.h"
#include "hashtable.h"
#include "strtype.h"
#include "type_parse.h"
#include "unicode.h"


/* (defconstnat char-code-limit UnicodeCount) */
static void defconstant_char_code_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_CHAR_CODE_LIMIT, &symbol);
	fixnum_heap(&value, UnicodeCount);
	defconstant_symbol(symbol, value);
}


/* char-check */
static void char_check(Execute ptr, addr var, addr list, int (*call)(unicode, unicode))
{
	addr pos;
	unicode left, right;

	GetCharacter(var, &left);
	while (list != Nil) {
		getcons(list, &pos, &list);
		GetCharacter(pos, &right);
		if (! (call(left, right))) {
			setresult_control(ptr, Nil);
			return;
		}
		left = right;
	}
	setresult_control(ptr, T);
}

static void type_char_eql(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Character);
	typeargs_var1rest(&arg, arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_char_check(constindex index, pointer p)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_eql(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char= (character &rest character) ...) -> boolean */
static int char_eql(unicode a, unicode b)
{
	return a == b;
}
static void function_char_eql(Execute ptr, addr var, addr list)
{
	char_check(ptr, var, list, char_eql);
}
static void defun_char_eql(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQL, p_defun_char_eql);
}


/* (defun char/= (character &rest character) ...) -> boolean */
static void function_char_not_eql(Execute ptr, addr list)
{
	addr left, right, loop;
	unicode a, b;

	for (;;) {
		getcons(list, &left, &list);
		if (list == Nil) break;
		GetCharacter(left, &a);
		for (loop = list; loop != Nil; ) {
			getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			if (a == b) {
				setresult_control(ptr, Nil);
				return;
			}
		}
	}
	setresult_control(ptr, T);
}
static void defun_char_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_eql(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char< (character &rest character) ...) -> boolean */
static int char_less(unicode a, unicode b)
{
	return a < b;
}
static void function_char_less(Execute ptr, addr var, addr list)
{
	char_check(ptr, var, list, char_less);
}
static void defun_char_less(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS, p_defun_char_less);
}


/* (defun char> (character &rest character) ...) -> boolean */
static int char_greater(unicode a, unicode b)
{
	return a > b;
}
static void function_char_greater(Execute ptr, addr var, addr list)
{
	char_check(ptr, var, list, char_greater);
}
static void defun_char_greater(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER, p_defun_char_greater);
}


/* (defun char<= (character &rest character) ...) -> boolean */
static int char_less_equal(unicode a, unicode b)
{
	return a <= b;
}
static void function_char_less_equal(Execute ptr, addr var, addr list)
{
	char_check(ptr, var, list, char_less_equal);
}
static void defun_char_less_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESS_EQUAL, p_defun_char_less_equal);
}


/* (defun char>= (character &rest character) ...) -> boolean */
static int char_greater_equal(unicode a, unicode b)
{
	return a >= b;
}
static void function_char_greater_equal(Execute ptr, addr var, addr list)
{
	char_check(ptr, var, list, char_greater_equal);
}
static void defun_char_greater_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATER_EQUAL, p_defun_char_greater_equal);
}


/* (defun char-equal (character &rest character) ...) -> boolean */
static void char_equal_check(Execute ptr, addr var, addr list,
		int (*call)(unicode, unicode))
{
	addr pos;
	unicode left, right;

	GetCharacter(var, &left);
	left = toUpperUnicode(left);
	while (list != Nil) {
		getcons(list, &pos, &list);
		GetCharacter(pos, &right);
		right = toUpperUnicode(right);
		if (! (call(left, right))) {
			setresult_control(ptr, Nil);
			return;
		}
		left = right;
	}
	setresult_control(ptr, T);
}

static void function_char_equal(Execute ptr, addr var, addr list)
{
	char_equal_check(ptr, var, list, char_eql);
}
static void defun_char_equal(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_EQUAL, p_defun_char_equal);
}


/* (defun char-not-equal (character &rest character) ...) -> boolean */
static void function_char_not_equal(Execute ptr, addr list)
{
	addr left, right, loop;
	unicode a, b;

	for (;;) {
		getcons(list, &left, &list);
		if (list == Nil) break;
		GetCharacter(left, &a);
		a = toUpperUnicode(a);
		for (loop = list; loop != Nil; ) {
			getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			b = toUpperUnicode(b);
			if (a == b) {
				setresult_control(ptr, Nil);
				return;
			}
		}
	}
	setresult_control(ptr, T);
}
static void defun_char_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NOT_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_char_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_eql(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-lessp (character &rest character) ...) -> boolean */
static void function_char_lessp(Execute ptr, addr var, addr list)
{
	char_equal_check(ptr, var, list, char_less);
}
static void defun_char_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_LESSP, p_defun_char_lessp);
}


/* (defun char-greaterp (character &rest character) ...) -> boolean */
static void function_char_greaterp(Execute ptr, addr var, addr list)
{
	char_equal_check(ptr, var, list, char_greater);
}
static void defun_char_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_GREATERP, p_defun_char_greaterp);
}


/* (defun char-not-lessp (character &rest character) ...) -> boolean */
static void function_char_not_lessp(Execute ptr, addr var, addr list)
{
	char_equal_check(ptr, var, list, char_greater_equal);
}
static void defun_char_not_lessp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_LESSP, p_defun_char_not_lessp);
}


/* (defun char-not-greaterp (character &rest character) ...) -> boolean */
static void function_char_not_greaterp(Execute ptr, addr var, addr list)
{
	char_equal_check(ptr, var, list, char_less_equal);
}
static void defun_char_not_greaterp(void)
{
	defun_char_check(CONSTANT_COMMON_CHAR_NOT_GREATERP, p_defun_char_not_greaterp);
}


/* (defun character (character) ...) -> character */
static void function_character(Execute ptr, addr var)
{
	unicode u;
	size_t size;

	if (symbolp(var)) {
		/* string check */
		GetNameSymbol(var, &var);
		string_length(var, &size);
		if (size != 1)
			fmte("The length of symbol ~S name must be 1.", NULL);
		string_getc(var, 0, &u);
		character_heap(&var, u);
	}
	else if (stringp(var)) {
		string_length(var, &size);
		if (size != 1)
			fmte("The length of string ~S name must be 1.", NULL);
		string_getc(var, 0, &u);
		character_heap(&var, u);
	}
	else if (GetType(var) != LISPTYPE_CHARACTER) {
		type_error_constant(var, CONSTANT_COMMON_CHARACTER);
	}
	setresult_control(ptr, var);
}

static void type_character_stringone(addr *ret)
{
	addr pos;
	fixnum_heap(&pos, 1);
	type1_heap(LISPDECL_SIMPLE_BASE_STRING, pos, ret);
}

static void type_character_arguments(addr *ret)
{
	addr type1, type2, type3, pos;

	/* (or character
	 *     (simple-base-string 1)
	 *     symbol)  ;; (= (length (symbol-name x)) 1)
	 */
	GetTypeTable(&type1, Character);
	type_character_stringone(&type2);
	GetTypeTable(&type3, Symbol);
	type3or_heap(type1, type2, type3, &pos);
	typeargs_var1(ret, pos);
}

static void type_character(addr *ret)
{
	addr arg, values;

	type_character_arguments(&arg);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_character);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun characterp (object) ...) -> boolean */
static void function_characterp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_CHARACTER);
}

static void defun_characterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHARACTERP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_characterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alpha-char-p (character) ...) -> boolean */
static void function_alpha_char_p(Execute ptr, addr pos)
{
	unicode u;
	GetCharacter(pos, &u);
	setbool_control(ptr, isAlphabetic(u));
}

static void type_character_boolean(addr *ret)
{
	addr arg, values;

	/* (function (character) boolean) */
	GetTypeTable(&arg, Character);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_alpha_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHA_CHAR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_alpha_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun alphanumericp (character) ...) -> boolean */
static void function_alphanumericp(Execute ptr, addr pos)
{
	unicode u;
	GetCharacter(pos, &u);
	setbool_control(ptr, isAlphanumeric(u));
}

static void defun_alphanumericp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ALPHANUMERICP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_alphanumericp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char (weight &optional radix) ...) -> result
 *   weight  (integer 0 *)
 *   radix   (integer 2 36), default 10
 *   result  (or null character)
 */
static void function_digit_char(Execute ptr, addr var, addr opt)
{
	fixnum w, r;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);

	/* digit */
	GetFixnum(var, &w);
	if (0 <= w && w < r) {
		character_heap(&var, (unicode)(w < 10? ('0' + w): (w - 10 + 'A')));
		setresult_control(ptr, var);
	}
	else {
		setresult_control(ptr, Nil);
	}
}

static void type_digit_char(addr *ret)
{
	addr weight, radix, arg, values;

	/* ((integer 0 *) &optional (integer 2 36)) */
	GetTypeTable(&weight, Intplus);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&arg, weight, radix);
	/* (or character null) */
	GetTypeValues(&values, CharacterNull);
	/* function */
	type_compiled_heap(arg, values, ret);
}

static void defun_digit_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun digit-char-p (character &optional radix) ...) -> weight
 *   character  character
 *   radix      (integer 2 36), default 10
 *   weight     (or (integer 0 *) null)
 */
static void function_digit_char_p(Execute ptr, addr var, addr opt)
{
	fixnum r, w;
	unicode u;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);
	/* character */
	GetCharacter(var, &u);
	if (isDigitCase(u))
		w = (fixnum)(u - '0');
	else if (isLowerCase(u))
		w = (fixnum)(u - 'a' + 10);
	else if (isUpperCase(u))
		w = (fixnum)(u - 'A' + 10);
	else {
		setresult_control(ptr, Nil);
		return;
	}
	if (r <= w) {
		setresult_control(ptr, Nil);
	}
	else {
		fixnum_heap(&var, w);
		setresult_control(ptr, var);
	}
}

static void type_digit_char_p(addr *ret)
{
	addr character, radix, arg, values;

	/* (character &optional (integer 2 36)) */
	GetTypeTable(&character, Character);
	GetTypeTable(&radix, RadixInteger);
	typeargs_var1opt1(&arg, character, radix);
	/* (or (integer 0 *) null) */
	GetTypeValues(&values, IntplusNull);
	/* function */
	type_compiled_heap(arg, values, ret);
}

static void defun_digit_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DIGIT_CHAR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_digit_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_digit_char_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun graphic-char-p (character) ...) -> boolean */
static void function_graphic_char_p(Execute ptr, addr var)
{
	unicode u;

	GetCharacter(var, &u);
	if (u < 0x80)
		setbool_control(ptr, (u == ' ' || isgraphunicode(u)));
	else
		setresult_control(ptr, T);
}

static void defun_graphic_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GRAPHIC_CHAR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_graphic_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun standard-char-p (character) ...) -> boolean */
static void function_standard_char_p(Execute ptr, addr var)
{
	unicode u;
	GetCharacter(var, &u);
	setbool_control(ptr, isStandardType(u));
}

static void defun_standard_char_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STANDARD_CHAR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_standard_char_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-upcase (character) ...) -> character */
static void function_char_upcase(Execute ptr, addr var)
{
	unicode u;

	GetCharacter(var, &u);
	if (isLowerCase(u))
		character_heap(&var, (u - 'a' + 'A'));
	setresult_control(ptr, var);
}

static void type_character_character(addr *ret)
{
	addr arg, values;

	/* (function (character) character) */
	GetTypeTable(&arg, Character);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_char_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_UPCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-downcase (character) ...) -> character */
static void function_char_downcase(Execute ptr, addr var)
{
	unicode u;

	GetCharacter(var, &u);
	if (isUpperCase(u))
		character_heap(&var, (u - 'A' + 'a'));
	setresult_control(ptr, var);
}

static void defun_char_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_DOWNCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upper-case-p (character) ...) -> boolean */
static void function_upper_case_p(Execute ptr, addr var)
{
	unicode u;
	GetCharacter(var, &u);
	setbool_control(ptr, isUpperCase(u));
}

static void defun_upper_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPPER_CASE_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_upper_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lower-case-p (character) ...) -> boolean */
static void function_lower_case_p(Execute ptr, addr var)
{
	unicode u;
	GetCharacter(var, &u);
	setbool_control(ptr, isLowerCase(u));
}

static void defun_lower_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOWER_CASE_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_lower_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun both-case-p (character) ...) -> boolean */
static void function_both_case_p(Execute ptr, addr var)
{
	unicode u;
	GetCharacter(var, &u);
	setbool_control(ptr, (isUpperCase(u) || isLowerCase(u)));
}

static void defun_both_case_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOTH_CASE_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_both_case_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_character_boolean(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-code (character) ...) -> code
 *   code  (integer 0 UnicodeCount)
 */
static void function_char_code(Execute ptr, addr var)
{
	unicode u;

	GetCharacter(var, &u);
	fixnum_heap(&var, (fixnum)u);
	setresult_control(ptr, var);
}

static void type_char_code(addr *ret)
{
	addr arg, values;

	/* (function (character) (values (integer 0 UnicodeCount))) */
	GetTypeTable(&arg, Character);
	typeargs_var1(&arg, arg);
	type4integer_heap(Nil, 0, Nil, (fixnum)UnicodeCount, &values);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_char_code(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_CODE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void defun_char_int(void)
{
	/* char-code == char-int */
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_INT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_code);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_code(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun code-char (code) ...) -> char-p
 *   code    (integer 0 UnicodeCount)
 *   char-p  (or character boolean)
 */
static void function_code_char(Execute ptr, addr var)
{
	fixnum v;

	GetFixnum(var, &v);
	if (0 <= v && v < (fixnum)UnicodeCount) {
		character_heap(&var, (unicode)v);
		setresult_control(ptr, var);
	}
	else {
		setresult_control(ptr, Nil);
	}
}

static void type_code_char(addr *ret)
{
	addr arg, values, type;

	/* (function
	 *   ((integer 0 UnicodeCount))
	 *   (values (or character boolean))) */
	type4integer_heap(Nil, 0, Nil, (fixnum)UnicodeCount, &arg);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, Character);
	GetTypeTable(&type, Boolean);
	type2or_heap(values, type, &values);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_code_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CODE_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_code_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_code_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char-name (character) ...) -> name
 *   name  (or string null)
 */
static void function_char_name(Execute ptr, addr var)
{
	if (! findtable_char_name(&var, var))
		setresult_control(ptr, var);
	else
		setresult_control(ptr, Nil);
}

static void type_char_name(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Character);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_char_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_char_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun name-char (name) ...) -> char-p
 *   name    (or string symbol character)  ;; string-designer
 *   char-p  (or character null)
 */
static void function_name_char(Execute ptr, addr var)
{
	LocalRoot local;
	LocalStack stack;
	unicode u;

	if (GetType(var) == LISPTYPE_CHARACTER) {
		/* character */
		local = ptr->local;
		push_local(local, &stack);
		GetCharacter(var, &u);
		strvect_local(local, &var, 1);
		strvect_setc(var, 0, u);
		if (! findtable_name_char(&var, var))
			setresult_control(ptr, var);
		else
			setresult_control(ptr, Nil);
		rollback_local(local, stack);
	}
	else {
		/* symbol, string */
		if (symbolp(var))
			GetNameSymbol(var, &var);
		if (! findtable_name_char(&var, var))
			setresult_control(ptr, var);
		else
			setresult_control(ptr, Nil);
	}
}

static void type_name_char(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, CharacterNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_name_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NAME_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_name_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_name_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_characters(void)
{
	SetPointerCall(defun, var1dynamic, char_eql);
	SetPointerCall(defun, var1dynamic, char_less);
	SetPointerCall(defun, var1dynamic, char_greater);
	SetPointerCall(defun, var1dynamic, char_less_equal);
	SetPointerCall(defun, var1dynamic, char_greater_equal);
	SetPointerCall(defun, var1dynamic, char_equal);
	SetPointerCall(defun, var1dynamic, char_lessp);
	SetPointerCall(defun, var1dynamic, char_greaterp);
	SetPointerCall(defun, var1dynamic, char_not_lessp);
	SetPointerCall(defun, var1dynamic, char_not_greaterp);
	SetPointerCall(defun, dynamic, char_not_eql);
	SetPointerCall(defun, dynamic, char_not_equal);
	SetPointerCall(defun, var1, character);
	SetPointerCall(defun, var1, characterp);
	SetPointerCall(defun, var1, alpha_char_p);
	SetPointerCall(defun, var1, alphanumericp);
	SetPointerCall(defun, var1opt1, digit_char);
	SetPointerCall(defun, var1opt1, digit_char_p);
	SetPointerCall(defun, var1, graphic_char_p);
	SetPointerCall(defun, var1, standard_char_p);
	SetPointerCall(defun, var1, char_upcase);
	SetPointerCall(defun, var1, char_downcase);
	SetPointerCall(defun, var1, upper_case_p);
	SetPointerCall(defun, var1, lower_case_p);
	SetPointerCall(defun, var1, both_case_p);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, char_code);
	SetPointerCall(defun, var1, code_char);
	SetPointerCall(defun, var1, char_name);
	SetPointerCall(defun, var1, name_char);
}

void build_common_characters(void)
{
	defconstant_char_code_limit();
	defun_char_eql();
	defun_char_not_eql();
	defun_char_less();
	defun_char_greater();
	defun_char_less_equal();
	defun_char_greater_equal();
	defun_char_equal();
	defun_char_not_equal();
	defun_char_lessp();
	defun_char_greaterp();
	defun_char_not_lessp();
	defun_char_not_greaterp();
	defun_character();
	defun_characterp();
	defun_alpha_char_p();
	defun_alphanumericp();
	defun_digit_char();
	defun_digit_char_p();
	defun_graphic_char_p();
	defun_standard_char_p();
	defun_char_upcase();
	defun_char_downcase();
	defun_upper_case_p();
	defun_lower_case_p();
	defun_both_case_p();
	defun_char_code();
	defun_char_int();
	defun_code_char();
	defun_char_name();
	defun_name_char();
}

