#include "array.h"
#include "array_access.h"
#include "call_strings.h"
#include "character.h"
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "integer.h"
#include "memory.h"
#include "strtype.h"
#include "strvect.h"
#include "type_subtypep.h"
#include "type_parse.h"
#include "type_typep.h"

/*
 *  simple-string-p
 */
void simple_string_p_common(addr var, addr *ret)
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
	*ret = check? T: Nil;
}


/*
 *  char
 */
int char_common(addr str, addr pos, addr *ret)
{
	unicode c;
	size_t index, size;

	if (GetIndex_integer(pos, &index))
		goto error;
	if (GetType(str) == LISPTYPE_STRING) {
		strvect_length(str, &size);
		if (size <= index)
			goto error;
		strvect_getc(str, index, &c);
	}
	else if (strarrayp(str)) {
		strarray_length_buffer(str, &size); /* Don't use strarray_length */
		if (size <= index)
			goto error;
		Return(strarray_getc_(str, index, &c));
	}
	else {
		return TypeError_(str, STRING);
	}
	character_heap(ret, c);
	return 0;

error:
	*ret = Nil;
	return fmte_("Too large index value ~S at ~S.", pos, str, NULL);
}


/*
 *  (setf char)
 */
int setf_char_common(addr value, addr str, addr pos)
{
	size_t size, index;
	unicode c;

	if (GetIndex_integer(pos, &index))
		goto error;
	GetCharacter(value, &c);
	switch (GetType(str)) {
		case LISPTYPE_STRING:
			strvect_length(str, &size);
			if (size <= index)
				goto error;
			return strvect_setc_(str, index, c);

		case LISPTYPE_ARRAY:
			if (! array_stringp(str))
				return TypeError_(str, STRING);
			strarray_length_buffer(str, &size); /* Don't use strarray_length */
			if (size <= index)
				goto error;
			return array_set_character_(str, index, c);

		default:
			break;
	}
	return TypeError_(str, STRING);

error:
	return fmte_("Too large index value ~S at ~S.", pos, str, NULL);
}


/*
 *  string
 */
int string_common(addr var, addr *ret)
{
	int check;

	Return(string_designer_heap_(ret, var, &check));
	if (! check)
		return TypeError_(var, STRING);

	return 0;
}


/*
 *  string-upcase
 */
static int string_case_common(addr var, addr rest, addr *ret,
		int (*call)(size_t, size_t, addr, addr, size_t *))
{
	int check;
	addr pos;
	unicode c;
	size_t start, end, size, i;

	Return(string_designer_heap_(&var, var, &check));
	if (! check)
		return TypeError_(var, STRING);
	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	strvect_heap(&pos, size);

	/* start */
	for (i = 0; i < start; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, c));
	}
	/* case */
	Return((*call)(i, end, var, pos, &i));
	/* end */
	for (; i < size; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, c));
	}

	/* result */
	return Result(ret, pos);
}

static int string_upcase_call_common(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	unicode c;

	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, toUpperUnicode(c)));
	}

	return Result(ret, i);
}

int string_upcase_common(addr var, addr rest, addr *ret)
{
	return string_case_common(var, rest, ret, string_upcase_call_common);
}


/*
 *  string-downcase
 */
static int string_downcase_call_common(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	unicode c;

	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(strvect_setc_(pos, i, toLowerUnicode(c)));
	}

	return Result(ret, i);
}

int string_downcase_common(addr var, addr rest, addr *ret)
{
	return string_case_common(var, rest, ret, string_downcase_call_common);
}


/*
 *  string-capitalize
 */
static int string_capitalize_call_common(
		size_t i, size_t end, addr var, addr pos, size_t *ret)
{
	int alphabet, mode;
	unicode c;

	mode = alphabet = 0;
	for (; i < end; i++) {
		Return(string_getc_(var, i, &c));
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
		Return(strvect_setc_(pos, i, c));
	}

	return Result(ret, i);
}

int string_capitalize_common(addr var, addr rest, addr *ret)
{
	return string_case_common(var, rest, ret, string_capitalize_call_common);
}


/*
 *  nstring-upcase
 */
static int nstring_case_common(addr var, addr rest,
		int (*call)(size_t, size_t, addr, addr, size_t *))
{
	size_t start, end, size;

	if (GetStatusReadOnly(var))
		return fmte_("Cannot update the constant object ~S.", var, NULL);
	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	return (*call)(start, end, var, var, &size);
}

int nstring_upcase_common(addr var, addr rest)
{
	return nstring_case_common(var, rest, string_upcase_call_common);
}


/*
 *  nstring-downcase
 */
int nstring_downcase_common(addr var, addr rest)
{
	return nstring_case_common(var, rest, string_downcase_call_common);
}


/*
 *  nstring-capitalize
 */
int nstring_capitalize_common(addr var, addr rest)
{
	return nstring_case_common(var, rest, string_capitalize_call_common);
}


/*
 *  string-trim
 */
static int string_trim_string_common(addr string, unicode c, int *ret)
{
	unicode check;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(string, i, &check));
		if (check == c)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int string_trim_list_common(addr list, unicode c, int *ret)
{
	unicode check;
	addr pos;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &check);
			if (check == c)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int string_trim_vector_common(addr vector, unicode c, int *ret)
{
	unicode check;
	addr pos;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &pos);
		if (GetType(pos) == LISPTYPE_CHARACTER) {
			GetCharacter(pos, &check);
			if (check == c)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int string_trim_array_common(addr pos, unicode c, int *ret)
{
	unicode check;
	size_t size, i;

	if (array_stringp(pos))
		return string_trim_string_common(pos, c, ret);
	if (! array_vector_p(pos))
		return TypeError_(pos, SEQUENCE);
	Return(array_get_vector_length_(pos, 1, &size));
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i, &check));
		if (check == c)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int string_trim_sequence_common(addr seq, unicode c, int *ret)
{
	switch (GetType(seq)) {
		case LISPTYPE_NIL:
			return Result(ret, 0);

		case LISPTYPE_CONS:
			return string_trim_list_common(seq, c, ret);

		case LISPTYPE_STRING:
			return string_trim_string_common(seq, c, ret);

		case LISPTYPE_VECTOR:
			return string_trim_vector_common(seq, c, ret);

		case LISPTYPE_ARRAY:
			return string_trim_array_common(seq, c, ret);

		default:
			return TypeError_(seq, SEQUENCE);
	}
}

static int string_trim_start_common(addr seq, addr var, size_t *start, size_t end)
{
	int check;
	unicode c;
	size_t i;

	for (i = *start; i < end; i++) {
		Return(string_getc_(var, i, &c));
		Return(string_trim_sequence_common(seq, c, &check));
		if (! check)
			break;
	}

	return Result(start, i);
}

static int string_trim_end_common(addr seq, addr var, size_t start, size_t *end)
{
	int check;
	unicode c;
	size_t i;

	for (i = *end; start < i; i--) {
		Return(string_getc_(var, i - 1, &c));
		Return(string_trim_sequence_common(seq, c, &check));
		if (! check)
			break;
	}

	return Result(end, i);
}

int string_trim_common(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designer_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_start_common(trim, pos, &start, end));
	if (end <= start)
		goto null_string;
	Return(string_trim_end_common(trim, pos, start, &end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string-left-trim
 */
int string_left_trim_common(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designer_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_start_common(trim, pos, &start, end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string-right-trim
 */
int string_right_trim_common(addr trim, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t start, end, size, i;

	Return(string_designer_heap_(&pos, pos, &check));
	if (! check)
		return TypeError_(pos, STRING);
	start = 0;
	string_length(pos, &end);
	Return(string_trim_end_common(trim, pos, start, &end));
	if (end <= start)
		goto null_string;
	/* new string */
	size = end - start;
	strvect_heap(&trim, size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i + start, &c));
		Return(strvect_setc_(trim, i, c));
	}
	return Result(ret, trim);

null_string:
	strvect_heap(ret, 0);
	return 0;
}


/*
 *  string=
 */
int string_eql_common(addr var1, addr var2, addr rest, addr *ret)
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designer_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designer_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2)
		return Result(ret, Nil);
	for (i = 0; i < diff1; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		if (a != b)
			return Result(ret, Nil);
	}

	return Result(ret, T);
}


/*
 *  string/=
 */
static int string_call_common(addr var1, addr var2, addr rest, addr *ret,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designer_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designer_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
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
	return Result(ret, Nil);

finish:
	make_index_integer_heap(ret, start1 + i);
	return 0;
}

static int string_not_equal1_common(unicode a, unicode b)
{
	return a != b;
}
static int string_not_equal2_common(size_t a, size_t b)
{
	return a != b;
}
int string_not_eql_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common(var1, var2, rest, ret,
			string_not_equal1_common, string_not_equal2_common);
}


/*
 *  string<
 */
static int string_less1_common(unicode a, unicode b)
{
	return a < b;
}
static int string_less2_common(size_t a, size_t b)
{
	return a < b;
}
int string_less_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common(var1, var2, rest, ret,
			string_less1_common, string_less2_common);
}


/*
 *  string>
 */
static int string_greater1_common(unicode a, unicode b)
{
	return a > b;
}
static int string_greater2_common(size_t a, size_t b)
{
	return a > b;
}
int string_greater_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common(var1, var2, rest, ret,
			string_greater1_common, string_greater2_common);
}


/*
 *  string<=
 */
static int string_less_equal1_common(unicode a, unicode b)
{
	return a <= b;
}
static int string_less_equal2_common(size_t a, size_t b)
{
	return a <= b;
}
int string_less_equal_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common(var1, var2, rest, ret,
			string_less_equal1_common, string_less_equal2_common);
}


/*
 *  string>=
 */
static int string_greater_equal1_common(unicode a, unicode b)
{
	return a >= b;
}
static int string_greater_equal2_common(size_t a, size_t b)
{
	return a >= b;
}
int string_greater_equal_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_call_common(var1, var2, rest, ret,
			string_greater_equal1_common, string_greater_equal2_common);
}


/*
 *  string-equal
 */
int string_equal_common(addr var1, addr var2, addr rest, addr *ret)
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designer_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designer_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	if (diff1 != diff2)
		return Result(ret, Nil);
	for (i = 0; i < diff1; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
		if (toUpperUnicode(a) != toUpperUnicode(b))
			return Result(ret, Nil);
	}

	return Result(ret, T);
}


/*
 *  string-not-equal
 */
static int string_callp_common(addr var1, addr var2, addr rest, addr *ret,
		int (*callu)(unicode, unicode),
		int (*calli)(size_t, size_t))
{
	int check;
	size_t size1, size2, start1, start2, end1, end2;
	size_t diff1, diff2, i;
	unicode a, b;

	Return(string_designer_heap_(&var1, var1, &check));
	if (! check)
		return TypeError_(var1, STRING);
	Return(string_designer_heap_(&var2, var2, &check));
	if (! check)
		return TypeError_(var2, STRING);
	string_length(var1, &size1);
	Return(keyword_start1_end1_(size1, rest, &start1, &end1));
	string_length(var2, &size2);
	Return(keyword_start2_end2_(size2, rest, &start2, &end2));
	diff1 = end1 - start1;
	diff2 = end2 - start2;
	for (i = 0; i < diff1 && i < diff2; i++) {
		Return(string_getc_(var1, start1 + i, &a));
		Return(string_getc_(var2, start2 + i, &b));
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
	return Result(ret, Nil);

finish:
	make_index_integer_heap(ret, start1 + i);
	return 0;
}

int string_not_equal_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common(var1, var2, rest, ret,
			string_not_equal1_common, string_not_equal2_common);
}


/*
 *  string-lessp
 */
int string_lessp_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common(var1, var2, rest, ret,
			string_less1_common, string_less2_common);
}


/*
 *  string-greaterp
 */
int string_greaterp_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common(var1, var2, rest, ret,
			string_greater1_common, string_greater2_common);
}


/*
 *  string-not-greaterp
 */
int string_not_greaterp_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common(var1, var2, rest, ret,
			string_less_equal1_common, string_less_equal2_common);
}


/*
 *  string-not-lessp
 */
int string_not_lessp_common(addr var1, addr var2, addr rest, addr *ret)
{
	return string_callp_common(var1, var2, rest, ret,
			string_greater_equal1_common, string_greater_equal2_common);
}


/*
 *  make-string
 */
int make_string_common(Execute ptr, addr var, addr rest, addr *ret)
{
	int check;
	addr symbol, value, type;
	unicode c;
	size_t size;

	/* size */
	if (GetIndex_integer(var, &size))
		return fmte_("Too large index value ~S.", var, NULL);

	/* initial-elemnet */
	value = NULL;
	c = 0;
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	if (getplist_safe(rest, symbol, &value) == 0) {
		if (GetType(value) != LISPTYPE_CHARACTER)
			return fmte_("Invalid :initial-element ~S.", value, NULL);
		GetCharacter(value, &c);
	}

	/* element-type */
	GetConst(KEYWORD_ELEMENT_TYPE, &symbol);
	if (getplist_safe(rest, symbol, &type) == 0) {
		GetTypeTable(&symbol, Character);
		Return(parse_type(ptr, &type, type, Nil));
		Return(subtypep_check_(ptr, type, symbol, Nil, &check, NULL));
		if (! check) {
			return fmte_(":element-type ~S "
					"must be a subtype of character.", type, NULL);
		}
		/* type check */
		if (value == NULL)
			character_heap(&value, c);
		Return(typep_clang_(ptr, value, type, &check));
		if (! check) {
			return fmte_("The initial-element ~S "
					"must be a ~S type.", value, type, NULL);
		}
	}

	/* make-string */
	strvect_heap(&var, size);
	Return(strvect_setall_(var, c));
	return Result(ret, var);
}

