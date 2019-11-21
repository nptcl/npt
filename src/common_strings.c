/*
 *  ANSI COMMON LISP: 16. Strings
 */
#include "array.h"
#include "array_object.h"
#include "character.h"
#include "common_header.h"
#include "cons.h"
#include "integer.h"
#include "strtype.h"
#include "type_parse.h"
#include "type_subtypep.h"

/* (defun stringp (object) ...) -> boolean */
static void function_stringp(Execute ptr, addr var)
{
	setbool_control(ptr, stringp(var));
}

static void defun_stringp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRINGP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_stringp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-string-p (object) ...) -> boolean */
static void function_simple_string_p(Execute ptr, addr var)
{
	int check;

	switch (GetType(var)) {
		case LISPTYPE_STRING:
			check = 1;
			break;

		case LISPTYPE_ARRAY:
			check = array_simple_p(var) && array_stringp(var);
			break;

		default:
			check = 0;
			break;
	}
	setbool_control(ptr, check);
}

static void defun_simple_string_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_STRING_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_string_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun char (string index) ...) -> character
 *   string   string
 *   index    index
 */
static void function_char(Execute ptr, addr var1, addr var2)
{
	unicode u;
	size_t index, size;

	if (getindex_integer(var2, &index))
		fmte("Too large index value ~S.", var2, NULL);
	if (GetType(var1) == LISPTYPE_STRING) {
		strvect_length(var1, &size);
		if (size <= index)
			fmte("Out of valid string index, ~S.", var2, NULL);
		strvect_getc(var1, index, &u);
	}
	else if (strarrayp(var1)) {
		strarray_length_buffer(var1, &size); /* Don't use strarray_length */
		if (size <= index)
			fmte("Out of valid string index, ~S.", var2, NULL);
		strarray_getc(var1, index, &u);
	}
	else {
		fmte("The object ~S must be a string type.", var1, NULL);
		return;
	}
	character_heap(&var1, u);
	setresult_control(ptr, var1);
}

static void type_char(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, String);
	GetTypeTable(&type, Index);
	typeargs_var2(&arg, arg, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun schar (string index) ...) -> character
 *   string   string
 *   index    index
 */
static void function_schar(Execute ptr, addr var1, addr var2)
{
	unicode u;
	size_t index, size;

	if (getindex_integer(var2, &index))
		fmte("Too large index value ~S.", var2, NULL);
	if (GetType(var1) == LISPTYPE_STRING) {
		strvect_length(var1, &size);
		if (size <= index)
			fmte("Out of valid string index, ~S.", var2, NULL);
		strvect_getc(var1, index, &u);
	}
	else if (strarrayp(var1)) {
		strarray_length(var1, &size); /* Don't use strarray_length_buffer */
		if (size <= index)
			fmte("Out of valid string index, ~S.", var2, NULL);
		strarray_getc(var1, index, &u);
	}
	else {
		fmte("The object ~S must be a string type.", var1, NULL);
		return;
	}
	character_heap(&var1, u);
	setresult_control(ptr, var1);
}

static void type_schar(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var2(&arg, arg, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_schar);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_schar(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf char) (character string index) ...) -> character */
static void function_setf_char(Execute ptr, addr value, addr pos, addr index)
{
	size_t size;
	unicode c;

	if (getindex_integer(index, &size))
		fmte("Too large index value ~S.", index, NULL);
	GetCharacter(value, &c);
	switch (GetType(pos)) {
		case LISPTYPE_STRING:
			strvect_setc(pos, size, c);
			break;

		case LISPTYPE_ARRAY:
			if (! array_stringp(pos))
				TypeError(pos, STRING);
			if (array_set_character(NULL, pos, size, c))
				TypeError(pos, STRING);
			break;

		default:
			TypeError(pos, STRING);
			break;
	}
	setresult_control(ptr, value);
}

static void type_setf_char(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Character);
	GetTypeTable(&values, String);
	GetTypeTable(&type, Index);
	typeargs_var3(&arg, arg, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CHAR, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_char(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun (setf schar) (character simple-string index) ...) -> character */
static void type_setf_schar(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Character);
	GetTypeTable(&values, SimpleString);
	GetTypeTable(&type, Index);
	typeargs_var3(&arg, arg, values, type);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_schar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SCHAR, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_char);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_schar(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun string (x) ...) -> string
 *   x  (or string symbol character)  ;; string-designer
 */
#if 0
static void function_string(Execute ptr, addr var)
{
	unicode u;

	switch (GetType(var)) {
		case LISPTYPE_STRING:
			goto finish;

		case LISPTYPE_ARRAY:
			if (strarrayp(var))
				goto finish;
			break;

		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			GetNameSymbol(var, &var);
			goto finish;

		case LISPTYPE_CHARACTER:
			GetCharacter(var, &u);
			strvect_heap(&var, 1);
			strvect_setc(var, 0, u);
			goto finish;

		default:
			break;
	}
	fmte("Type error.", NULL);
	return;

finish:
	setresult_control(ptr, var);
}
#endif
static void function_string(Execute ptr, addr var)
{
	if (! string_designer_heap(&var, var))
		TypeError(var, STRING);
	setresult_control(ptr, var);
}

static void type_string(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, String);
	type_compiled_heap(arg, values, ret);
}

static void defun_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static void function_string_case(Execute ptr, addr var, addr rest,
		size_t (*call)(size_t, size_t, addr, addr))
{
	addr pos;
	unicode u;
	size_t start, end, size, i;

	if (! string_designer_heap(&var, var))
		TypeError(var, STRING);
	string_length(var, &size);
	keyword_start_end(size, rest, &start, &end);
	strvect_heap(&pos, size);

	/* start */
	for (i = 0; i < start; i++) {
		string_getc(var, i, &u);
		strvect_setc(pos, i, u);
	}
	/* case */
	i = call(i, end, var, pos);
	/* end */
	for (; i < size; i++) {
		string_getc(var, i, &u);
		strvect_setc(pos, i, u);
	}

	/* result */
	setresult_control(ptr, pos);
}

static size_t string_upcase_call(size_t i, size_t end, addr var, addr pos)
{
	unicode u;

	for (; i < end; i++) {
		string_getc(var, i, &u);
		strvect_setc(pos, i, toUpperUnicode(u));
	}

	return i;
}

static void function_string_upcase(Execute ptr, addr var, addr rest)
{
	function_string_case(ptr, var, rest, string_upcase_call);
}

static void defun_string_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_UPCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static size_t string_downcase_call(size_t i, size_t end, addr var, addr pos)
{
	unicode u;

	for (; i < end; i++) {
		string_getc(var, i, &u);
		strvect_setc(pos, i, toLowerUnicode(u));
	}

	return i;
}

static void function_string_downcase(Execute ptr, addr var, addr rest)
{
	function_string_case(ptr, var, rest, string_downcase_call);
}

static void defun_string_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_DOWNCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static size_t string_capitalize_call(size_t i, size_t end, addr var, addr pos)
{
	int alphabet, mode;
	unicode c;

	mode = alphabet = 0;
	for (; i < end; i++) {
		string_getc(var, i, &c);
		if (mode == 0) {
			/* not alphabet */
			if (isAlphanumeric(c)) {
				alphabet = 1; /* upper */
				mode = 1;
			}
			else {
				alphabet = 0; /* not alphabet */
			}
		}
		else {
			/* alphabet */
			if (isAlphanumeric(c)) {
				alphabet = 2; /* lower */
			}
			else {
				alphabet = 0; /* not alphabet */
				mode = 0;
			}
		}
		switch (alphabet) {
			case 1: c = toUpperUnicode(c); break;
			case 2: c = toLowerUnicode(c); break;
			default: break;
		}
		strvect_setc(pos, i, c);
	}

	return i;
}

static void function_string_capitalize(Execute ptr, addr var, addr rest)
{
	function_string_case(ptr, var, rest, string_capitalize_call);
}

static void defun_string_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_CAPITALIZE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_string_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-upcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static void function_nstring_case(Execute ptr, addr var, addr rest,
		size_t (*call)(size_t, size_t, addr, addr))
{
	size_t start, end, size;

	if (GetStatusReadOnly(var))
		fmte("Cannot update the constant object ~S.", var, NULL);
	string_length(var, &size);
	keyword_start_end(size, rest, &start, &end);
	call(start, end, var, var);
	setresult_control(ptr, var);
}

static void function_nstring_upcase(Execute ptr, addr var, addr rest)
{
	function_nstring_case(ptr, var, rest, string_upcase_call);
}

static void defun_nstring_upcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_UPCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_upcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-downcase (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static void function_nstring_downcase(Execute ptr, addr var, addr rest)
{
	function_nstring_case(ptr, var, rest, string_downcase_call);
}

static void defun_nstring_downcase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_DOWNCASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_downcase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nstring-capitalize (string &key start end) ...) -> string
 *   start  keyword-start
 *   end    keyword-end
 */
static void function_nstring_capitalize(Execute ptr, addr var, addr rest)
{
	function_nstring_case(ptr, var, rest, string_capitalize_call);
}

static void defun_nstring_capitalize(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSTRING_CAPITALIZE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_nstring_capitalize);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, NStringCase);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-trim (sequence string) ...) -> string */
static int trim_string_check(addr string, unicode u)
{
	unicode c;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		string_getc(string, i, &c);
		if (c == u) return 1;
	}

	return 0;
}

static int trim_list_check(addr list, unicode u)
{
	unicode c;
	addr pos;

	while (list != Nil) {
		getcons(list, &pos, &list);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &c);
			if (c == u) return 1;
		}
	}

	return 0;
}

static int trim_vector_check(addr vector, unicode u)
{
	unicode c;
	addr pos;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &pos);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &c);
			if (c == u) return 1;
		}
	}

	return 0;
}

static int trim_array_check(addr pos, unicode u)
{
	unicode c;
	size_t size, i;

	if (array_stringp(pos))
		return trim_string_check(pos, u);
	if (! array_vector_p(pos))
		TypeError(pos, SEQUENCE);
	size = array_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		if (array_get_unicode(pos, i, &c)) {
			array_get(NULL, pos, i, &pos);
			TypeError(pos, CHARACTER);
		}
		if (c == u) return 1;
	}

	return 0;
}

static int trim_sequence_check(addr seq, unicode u)
{
	switch (GetType(seq)) {
		case LISPTYPE_NIL:
			return 0;

		case LISPTYPE_CONS:
			return trim_list_check(seq, u);

		case LISPTYPE_STRING:
			return trim_string_check(seq, u);

		case LISPTYPE_VECTOR:
			return trim_vector_check(seq, u);

		case LISPTYPE_ARRAY:
			return trim_array_check(seq, u);

		default:
			TypeError(seq, SEQUENCE);
			return 0;
	}
}

static void move_start_trim(addr seq, addr var, size_t *start, size_t end)
{
	unicode u;
	size_t i;

	for (i = *start; i < end; i++) {
		string_getc(var, i, &u);
		if (! trim_sequence_check(seq, u)) break;
	}
	*start = i;
}

static void move_end_trim(addr seq, addr var, size_t start, size_t *end)
{
	unicode u;
	size_t i;

	for (i = *end; start < i; i--) {
		string_getc(var, i - 1, &u);
		if (! trim_sequence_check(seq, u)) break;
	}
	*end = i;
}

static void function_string_trim(Execute ptr, addr var1, addr var2)
{
	unicode u;
	size_t start, end, size, i;

	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	start = 0;
	string_length(var2, &end);
	move_start_trim(var1, var2, &start, end);
	if (end <= start)
		goto null_string;
	move_end_trim(var1, var2, start, &end);
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&var1, size);
	for (i = 0; i < size; i++) {
		string_getc(var2, i + start, &u);
		strvect_setc(var1, i, u);
	}
	setresult_control(ptr, var1);
	return;

null_string:
	strvect_heap(&var1, 0);
	setresult_control(ptr, var1);
}

static void defun_string_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_TRIM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_string_left_trim(Execute ptr, addr var1, addr var2)
{
	unicode u;
	size_t start, end, size, i;

	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	start = 0;
	string_length(var2, &end);
	move_start_trim(var1, var2, &start, end);
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&var1, size);
	for (i = 0; i < size; i++) {
		string_getc(var2, i + start, &u);
		strvect_setc(var1, i, u);
	}
	setresult_control(ptr, var1);
	return;

null_string:
	strvect_heap(&var1, 0);
	setresult_control(ptr, var1);
}

static void defun_string_left_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LEFT_TRIM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_left_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_string_right_trim(Execute ptr, addr var1, addr var2)
{
	unicode u;
	size_t start, end, size, i;

	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	start = 0;
	string_length(var2, &end);
	move_end_trim(var1, var2, start, &end);
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&var1, size);
	for (i = 0; i < size; i++) {
		string_getc(var2, i + start, &u);
		strvect_setc(var1, i, u);
	}
	setresult_control(ptr, var1);
	return;

null_string:
	strvect_heap(&var1, 0);
	setresult_control(ptr, var1);
}

static void defun_string_right_trim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_RIGHT_TRIM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_string_right_trim);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringTrim);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string= (string1 string2 &key start1 end1 start2 end2) ...) -> boolean
 *   string1   (or string symbol character)  ;;string-designer
 *   string2   (or string symbol character)  ;;string-designer
 *   start1    keyword-start
 *   end1      keyword-end
 *   start2    keyword-start
 *   end2      keyword-end
 *   boolean   boolean
 *   mismatch  indexnull
 */
static void function_string_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	if (! string_designer_heap(&var1, var1))
		TypeError(var1, STRING);
	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	string_length(var1, &size1);
	keyword_start1_end1(size1, rest, &start1, &end1);
	string_length(var2, &size2);
	keyword_start2_end2(size2, rest, &start2, &end2);
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2) {
		setresult_control(ptr, Nil);
		return;
	}
	for (i = 0; i < diff1; i++) {
		string_getc(var1, start1 + i, &a);
		string_getc(var2, start2 + i, &b);
		if (a != b) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_string_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string/= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static void function_string_call(Execute ptr, addr var1, addr var2, addr rest,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	if (! string_designer_heap(&var1, var1))
		TypeError(var1, STRING);
	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	string_length(var1, &size1);
	keyword_start1_end1(size1, rest, &start1, &end1);
	string_length(var2, &size2);
	keyword_start2_end2(size2, rest, &start2, &end2);
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		string_getc(var1, start1 + i, &a);
		string_getc(var2, start2 + i, &b);
		if (a != b) {
			if (callu(a, b))
				goto finish;
			else
				goto finish_nil;
		}
	}
	if (calli(diff1, diff2))
		goto finish;

finish_nil:
	setresult_control(ptr, Nil);
	return;

finish:
	make_index_integer_alloc(NULL, &var1, start1 + i);
	setresult_control(ptr, var1);
}

static int character_not_eql(unicode a, unicode b)
{
	return a != b;
}
static int index_not_eql(size_t a, size_t b)
{
	return a != b;
}
static void function_string_not_eql(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_call(ptr, var1, var2, rest, character_not_eql, index_not_eql);
}

static void defun_string_not_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string< (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int character_less(unicode a, unicode b)
{
	return a < b;
}
static int index_less(size_t a, size_t b)
{
	return a < b;
}
static void function_string_less(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_call(ptr, var1, var2, rest, character_less, index_less);
}

static void defun_string_less(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string> (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int character_greater(unicode a, unicode b)
{
	return a > b;
}
static int index_greater(size_t a, size_t b)
{
	return a > b;
}
static void function_string_greater(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_call(ptr, var1, var2, rest, character_greater, index_greater);
}

static void defun_string_greater(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string<= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int character_less_equal(unicode a, unicode b)
{
	return a <= b;
}
static int index_less_equal(size_t a, size_t b)
{
	return a <= b;
}
static void function_string_less_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_call(ptr, var1, var2, rest,
			character_less_equal, index_less_equal);
}

static void defun_string_less_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESS_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_less_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static int character_greater_equal(unicode a, unicode b)
{
	return a >= b;
}
static int index_greater_equal(size_t a, size_t b)
{
	return a >= b;
}
static void function_string_greater_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_call(ptr, var1, var2, rest,
			character_greater_equal, index_greater_equal);
}

static void defun_string_greater_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATER_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greater_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static void function_string_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	if (! string_designer_heap(&var1, var1))
		TypeError(var1, STRING);
	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	string_length(var1, &size1);
	keyword_start1_end1(size1, rest, &start1, &end1);
	string_length(var2, &size2);
	keyword_start2_end2(size2, rest, &start2, &end2);
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2) {
		setresult_control(ptr, Nil);
		return;
	}
	for (i = 0; i < diff1; i++) {
		string_getc(var1, start1 + i, &a);
		string_getc(var2, start2 + i, &b);
		if (toUpperUnicode(a) != toUpperUnicode(b)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_string_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringEqual);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-equal (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static void function_string_callp(Execute ptr, addr var1, addr var2, addr rest,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	if (! string_designer_heap(&var1, var1))
		TypeError(var1, STRING);
	if (! string_designer_heap(&var2, var2))
		TypeError(var2, STRING);
	string_length(var1, &size1);
	keyword_start1_end1(size1, rest, &start1, &end1);
	string_length(var2, &size2);
	keyword_start2_end2(size2, rest, &start2, &end2);
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		string_getc(var1, start1 + i, &a);
		string_getc(var2, start2 + i, &b);
		a = toUpperUnicode(a);
		b = toUpperUnicode(b);
		if (a != b) {
			if (callu(a, b))
				goto finish;
			else
				goto finish_nil;
		}
	}
	if (calli(diff1, diff2))
		goto finish;

finish_nil:
	setresult_control(ptr, Nil);
	return;

finish:
	make_index_integer_alloc(NULL, &var1, start1 + i);
	setresult_control(ptr, var1);
}

static void function_string_not_equal(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_callp(ptr, var1, var2, rest, character_not_eql, index_not_eql);
}

static void defun_string_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-lessp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static void function_string_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_callp(ptr, var1, var2, rest, character_less, index_less);
}

static void defun_string_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_LESSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-greaterp (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static void function_string_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_callp(ptr, var1, var2, rest, character_greater, index_greater);
}

static void defun_string_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_GREATERP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string-not-graeter (string1 string2 &key start1 end1 start2 end2) ...)
 *   -> mismatch
 */
static void function_string_not_greaterp(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_callp(ptr, var1, var2, rest,
			character_less_equal, index_less_equal);
}

static void defun_string_not_greaterp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_GREATERP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_greaterp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun string>= (string1 string2 &key start1 end1 start2 end2) ...) -> mismatch */
static void function_string_not_lessp(Execute ptr, addr var1, addr var2, addr rest)
{
	function_string_callp(ptr, var1, var2, rest,
			character_greater_equal, index_greater_equal);
}

static void defun_string_not_lessp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STRING_NOT_LESSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_string_not_lessp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StringMismatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string (size &key initial-element element-type) ...) -> string
 *   size             index
 *   initial-elemnet  character
 *   element-type     t   ;; type-specifier
 *   string           simple-string
 */
static void function_make_string(Execute ptr, addr var, addr rest)
{
	int invalid;
	addr symbol, value;
	unicode u;
	size_t size;

	/* size */
	if (getindex_integer(var, &size))
		fmte("Too large index value ~S.", var, NULL);

	/* initial-elemnet */
	u = 0;
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	if (getplist(rest, symbol, &value) == 0) {
		if (GetType(value) != LISPTYPE_CHARACTER)
			fmte("Invalid :initial-element ~S.", value, NULL);
		GetCharacter(value, &u);
	}

	/* element-type */
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	if (getplist(rest, symbol, &value) == 0) {
		GetTypeTable(&symbol, Character);
		if (parse_type(ptr, &value, value, Nil))
			return;
		if (! subtypep_clang(value, symbol, &invalid))
			fmte(":element-type ~S must be a subtype of character.", value, NULL);
		/* check only */
	}

	/* make-string */
	strvect_heap(&var, size);
	strvect_setall(var, u);
	setresult_control(ptr, var);
}

static void type_make_string(addr *ret)
{
	/* (function (size &key
	 *             (initial-element character)
	 *             (element-type t))
	 *           (values simple-string &rest nil))
	 */
	addr arg, values, symbol, type1, type2;

	/* arg */
	GetTypeTable(&arg, Index);
	GetTypeTable(&type1, Character);
	GetTypeTable(&type2, T);
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	cons_heap(&type1, symbol, type1);
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	cons_heap(&type2, symbol, type2);
	list_heap(&type1, type1, type2, NULL);
	typeargs_var1key(&arg, arg, type1);
	/* values */
	GetTypeValues(&values, SimpleString);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_strings(void)
{
	SetPointerCall(defun, var1, stringp);
	SetPointerCall(defun, var1, simple_string_p);
	SetPointerCall(defun, var2, char);
	SetPointerCall(defun, var2, schar);
	SetPointerCall(defun, var3, setf_char);
	SetPointerCall(defun, var3, setf_char);
	SetPointerCall(defun, var1, string);
	SetPointerCall(defun, var1dynamic, string_upcase);
	SetPointerCall(defun, var1dynamic, string_downcase);
	SetPointerCall(defun, var1dynamic, string_capitalize);
	SetPointerCall(defun, var1dynamic, nstring_upcase);
	SetPointerCall(defun, var1dynamic, nstring_downcase);
	SetPointerCall(defun, var1dynamic, nstring_capitalize);
	SetPointerCall(defun, var2, string_trim);
	SetPointerCall(defun, var2, string_left_trim);
	SetPointerCall(defun, var2, string_right_trim);
	SetPointerCall(defun, var2dynamic, string_eql);
	SetPointerCall(defun, var2dynamic, string_not_eql);
	SetPointerCall(defun, var2dynamic, string_less);
	SetPointerCall(defun, var2dynamic, string_greater);
	SetPointerCall(defun, var2dynamic, string_less_equal);
	SetPointerCall(defun, var2dynamic, string_greater_equal);
	SetPointerCall(defun, var2dynamic, string_equal);
	SetPointerCall(defun, var2dynamic, string_not_equal);
	SetPointerCall(defun, var2dynamic, string_lessp);
	SetPointerCall(defun, var2dynamic, string_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_greaterp);
	SetPointerCall(defun, var2dynamic, string_not_lessp);
	SetPointerCall(defun, var1dynamic, make_string);
}

_g void build_common_strings(void)
{
	defun_stringp();
	defun_simple_string_p();
	defun_char();
	defun_schar();
	defun_setf_char();
	defun_setf_schar();
	defun_string();
	defun_string_upcase();
	defun_string_downcase();
	defun_string_capitalize();
	defun_nstring_upcase();
	defun_nstring_downcase();
	defun_nstring_capitalize();
	defun_string_trim();
	defun_string_left_trim();
	defun_string_right_trim();
	defun_string_eql();
	defun_string_not_eql();
	defun_string_less();
	defun_string_greater();
	defun_string_less_equal();
	defun_string_greater_equal();
	defun_string_equal();
	defun_string_not_equal();
	defun_string_lessp();
	defun_string_greaterp();
	defun_string_not_greaterp();
	defun_string_not_lessp();
	defun_make_string();
}

