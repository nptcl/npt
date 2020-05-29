/*
 *  format radix ~R
 */
#include "bigdata.h"
#include "bignum.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "local.h"
#include "object.h"
#include "radix.h"
#include "sequence.h"
#include "stream.h"
#include "strvect.h"
#include "typedef.h"

#define ENGLISH_RADIX_MODE1	","
#define ENGLISH_RADIX_MODE2	"and"


/*
 *  The Conway-Wechsler System.
 *    http://www.mrob.com/pub/math/largenum.html#conway-wechsler
 *    Large Numbers, Robert P. Munafo.
 */
struct english_struct {
	LocalRoot local;
	addr root, pos, string;
	int cardinal;
	fixnum index;
};
typedef struct english_struct *english;

static const char *name_standard_char(fixnum index, int llion)
{
	static const char *table[][2] = {
		{ "ni",     "thousand"    },
		{ "mi",     "million"     },
		{ "bi",     "billion"     },
		{ "tri",    "trillion"    },
		{ "quadri", "quadrillion" },
		{ "quinti", "quintillion" },
		{ "sexti",  "sextillion"  },
		{ "septi",  "septillion"  },
		{ "octi",   "octillion"   },
		{ "noni",   "nonillion"   }
	};
	Check(! (0 <= index && index <= 9), "index error");
	return table[index][llion != 0];
}

static void english_string(english input, const char *value)
{
	addr pos;
	LocalRoot local;

	local = input->local;
	strvect_char_local(local, &pos, value);
	cons_local(local, &(input->string), pos, input->string);
}

static void english_char(english input, char c)
{
	char value[2] = {c, 0};
	english_string(input, value);
}

static void name_table_1(const char **a, const char **b, bigtype v)
{
	static const char *table[][2] = {
		{ NULL,       ""   },  /* 0 */
		{ "un",       ""   },  /* 1 */
		{ "duo",      ""   },  /* 2 */
		{ "tre",      "*"  },  /* 3 */
		{ "quattuor", ""   },  /* 4 */
		{ "quin",     ""   },  /* 5 */
		{ "se",       "sx" },  /* 6 */
		{ "septe",    "mn" },  /* 7 */
		{ "octo",     ""   },  /* 8 */
		{ "nove",     "mn" }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_table_10(const char **a, const char **b, bigtype v)
{
	static const char *table[][2] = {
		{ NULL,           ""   },  /* 0 */
		{ "deci",         "n"  },  /* 1 */
		{ "viginti",      "ms" },  /* 2 */
		{ "triginta",     "ns" },  /* 3 */
		{ "quadraginta",  "ns" },  /* 4 */
		{ "quinquaginta", "ns" },  /* 5 */
		{ "sexaginta",    "n"  },  /* 6 */
		{ "septuaginta",  "n"  },  /* 7 */
		{ "octoginta",    "mx" },  /* 8 */
		{ "nonaginta",    ""   }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_table_100(const char **a, const char **b, bigtype v)
{
	static const char *table[][2] = {
		{ NULL,           ""   },  /* 0 */
		{ "centi",        "nx" },  /* 1 */
		{ "ducenti",      "n"  },  /* 2 */
		{ "trecenti",     "ns" },  /* 3 */
		{ "quadringenti", "ns" },  /* 4 */
		{ "quingenti",    "ns" },  /* 5 */
		{ "sescenti",     "n"  },  /* 6 */
		{ "septingenti",  "n"  },  /* 7 */
		{ "octingenti",   "mx" },  /* 8 */
		{ "nongenti",     ""   }   /* 9 */
	};
	Check(10 <= v, "index error");
	*a = table[v][0];
	*b = table[v][1];
}

static void name_concat(english input,
		const char *a1, const char *b1,
		const char *a2, const char *b2)
{
	char c;

	english_string(input, a1);
	if (b1[0] == '*') {
		if (strchr(b2, 's') || strchr(b2, 'x'))
			english_char(input, 's');
	}
	else {
		while ((c = *(b1++)) != 0) {
			if (strchr(b2, c))
				english_char(input, c);
		}
	}
	english_string(input, a2);
}

static void number_name_front(english input, fixnum quot)
{
	int n1, n2, n3;
	fixnum s1, s10, s100;
	const char *a1, *b1;
	const char *a2, *b2;
	const char *a3, *b3;

	s100 = quot;
	s1 = s100 % 10; s100 /= 10;
	s10 = s100 % 10; s100 /= 10;
	s100 %= 10;
	name_table_1(&a1, &b1, s1);
	name_table_10(&a2, &b2, s10);
	name_table_100(&a3, &b3, s100);
	n1 = (a1 == NULL);
	n2 = (a2 == NULL);
	n3 = (a3 == NULL);
	if (n1 && n2) {
		english_string(input, a3);
		return;
	}
	if (n1 && n3) {
		english_string(input, a2);
		return;
	}
	if (n1) {
		english_string(input, a2);
		english_string(input, a3);
		return;
	}
	if (n2) {
		name_concat(input, a1, b1, a3, b3);
		return;
	}
	if (n3) {
		name_concat(input, a1, b1, a2, b2);
		return;
	}
	else {
		name_concat(input, a1, b1, a2, b2);
		english_string(input, a3);
		return;
	}
}

static void number_name_standard(english input, fixnum quot, int llion)
{
	const char *ptr = name_standard_char(quot, llion);
	english_string(input, ptr);
}

static void number_name_1000(english input, fixnum quot)
{
	if (quot < 10) {
		number_name_standard(input, quot, 0);
	}
	else {
		number_name_front(input, quot);
	}
}

static void number_name_vowel(english input)
{
	addr pos;
	size_t size;
	unicode u;

	GetCar(input->string, &pos);
	strvect_length(pos, &size);
	Check(size <= 1, "size error");
	strvect_getc(pos, size - 1, &u);
	if (u == 'a' || u == 'e' || u == 'i' || u == 'o' || u == 'u') {
		SetStringSize(pos, size - 1);
	}
}

static void number_name_extend(english input, fixnum quot)
{
	number_name_1000(input, quot);
	number_name_vowel(input);
	english_string(input, "illi");
}

static void number_name_recursive(english input, fixnum quot)
{
	fixnum high, low;

	high = quot / 1000;
	low = quot % 1000;
	/* high */
	if (high != 0) {
		number_name_recursive(input, high);
	}
	/* low */
	number_name_extend(input, low);
}

static void number_name_index(english input, fixnum quot)
{
	Check(quot < 0, "index error");
	if (quot < 10) {
		number_name_standard(input, quot, 1);
	}
	else {
		number_name_recursive(input, quot);
		english_string(input, "on");
	}
}

static const char *radix_table_20(bigtype value, int cardinal)
{
	static const char *table[][2] = {
		{ "zero",      "zeroth"      },
		{ "one",       "first"       },
		{ "two",       "second"      },
		{ "three",     "third"       },
		{ "four",      "fourth"      },
		{ "five",      "fifth"       },
		{ "six",       "sixth"       },
		{ "seven",     "seventh"     },
		{ "eight",     "eighth"      },
		{ "nine",      "ninth"       },
		{ "ten",       "tenth"       },
		{ "eleven",    "eleventh"    },
		{ "twelve",    "twelfth"     },
		{ "thirteen",  "thirteenth"  },
		{ "fourteen",  "fourteenth"  },
		{ "fifteen",   "fifteenth"   },
		{ "sixteen",   "sixteenth"   },
		{ "seventeen", "seventeenth" },
		{ "eighteen",  "eighteenth"  },
		{ "nineteen",  "nineteenth"  }
	};
	Check(! (0 <= value && value <= 19), "value error");
	return table[value][cardinal == 0];
}

static const char *radix_table_100(bigtype value, int cardinal)
{
	static const char *table[][2] = {
		{ NULL,      NULL         },
		{ NULL,      NULL         },
		{ "twenty",  "twentieth"  },
		{ "thirty",  "thirtieth"  },
		{ "forty",   "fortieth"   },
		{ "fifty",   "fiftieth"   },
		{ "sixty",   "sixtieth"   },
		{ "seventy", "seventieth" },
		{ "eighty",  "eightieth"  },
		{ "ninety",  "ninetieth"  }
	};
	Check(! (2 <= value && value <= 9), "value error");
	return table[value][cardinal == 0];
}

static void push_radix(english input, addr pos)
{
	cons_local(input->local, &(input->root), pos, input->root);
	input->cardinal = 1;
}

static void push_radix_char(english input, const char *ptr)
{
	addr pos;
	strvect_char_local(input->local, &pos, ptr);
	push_radix(input, pos);
}

static void push_radix_list(english input, addr pos)
{
	nreverse(&pos, pos);
	push_radix(input, pos);
}

static void push_radix_20(english input, bigtype value)
{
	const char *ptr;

	ptr = radix_table_20(value, input->cardinal);
	push_radix_char(input, ptr);
}

static void push_radix_100(english input, bigtype c)
{
	const char *ptr;
	bigtype a, b;

	Check(100 <= c, "value error");
	if (c < 20) {
		push_radix_20(input, c);
		return;
	}

	a = c / 10;
	b = c % 10;
	if (a && b) {
		input->string = Nil;
		ptr = radix_table_100(a, 1); /* force cardinal */
		english_string(input, ptr);
		english_string(input, "-");
		ptr = radix_table_20(b, input->cardinal);
		english_string(input, ptr);
		push_radix_list(input, input->string);
		return;
	}
	if (a) {
		ptr = radix_table_100(a, input->cardinal);
		push_radix_char(input, ptr);
		return;
	}
	else {
		push_radix_20(input, b);
		return;
	}
}

static void push_radix_hundred(english input)
{
	push_radix_char(input, input->cardinal? "hundred": "hundredth");
}

static void number_name_cardinal(english input)
{
	input->string = Nil;
	number_name_index(input, input->index);
	if (! input->cardinal) {
		english_string(input, "th");
	}
#ifdef ENGLISH_RADIX_MODE1
	if (input->root != Nil) {
		english_string(input, ENGLISH_RADIX_MODE1);
	}
#endif
	push_radix_list(input, input->string);
}

static void english_execute_loop(english input)
{
	addr pos;
	bigtype b, c;

	pos = input->pos;
	c = letdiv_half_bigdata(pos, 100);
	b = letdiv_half_bigdata(pos, 10);
	if (b || c) {
		if (0 <= input->index)
			number_name_cardinal(input);
		if (b && c) {
			push_radix_100(input, c);
#ifdef ENGLISH_RADIX_MODE2
			push_radix_char(input, ENGLISH_RADIX_MODE2);
#endif
			push_radix_hundred(input);
			push_radix_20(input, b);
		}
		else if (b) {
			push_radix_hundred(input);
			push_radix_20(input, b);
		}
		else if (c) {
			push_radix_100(input, c);
		}
	}
	if (! zerop_bignum(pos)) {
		input->index++;
		english_execute_loop(input);
	}
}

static void english_execute(english input)
{
	english_execute_loop(input);
	if (input->root == Nil)
		push_radix_20(input, 0);
}

static void english_output(addr stream, english input, int minus)
{
	int first;
	addr left, right, pos;

	/* sign */
	if (minus) {
		print_ascii_stream(stream, "minus");
		first = 0;
	}
	else {
		first = 1;
	}

	/* body */
	for (right = input->root; right != Nil; ) {
		GetCons(right, &left, &right);
		/* space */
		if (first)
			first = 0;
		else
			write_char_stream(stream, ' ');
		/* output */
		if (consp(left)) {
			while (left != Nil) {
				GetCons(left, &pos, &left);
				print_string_stream(stream, pos);
			}
		}
		else {
			print_string_stream(stream, left);
		}
	}
}

static void english_bignum(LocalRoot local, addr stream, addr pos, int cardinal)
{
	int minus;
	struct english_struct str;

	/* input */
	cleartype(str);
	str.local = local;
	str.root = Nil;
	str.pos = pos;
	str.string = Nil;
	str.index = -1;

	/* sign */
	minus = minusp_bignum(pos);
	cardinal = (cardinal != 0);
	if (minus && (! cardinal))
		cardinal = 0;
	str.cardinal = cardinal;

	/* execute */
	english_execute(&str);
	english_output(stream, &str, minus);
}

_g void english_integer(LocalRoot local, addr stream, addr pos, int cardinal)
{
	int sign;
	addr copy;
	bigtype value;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			castfixed_fixnum(pos, &sign, &value);
			bignum_value_local(local, &copy, sign, value);
			english_bignum(local, stream, copy, cardinal);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_local(local, &copy, pos);
			english_bignum(local, stream, copy, cardinal);
			break;

		default:
			TypeError(pos, INTEGER);
			break;
	}

	rollback_local(local, stack);
}

static void english_unit_string(LocalRoot local,
		addr *ret, fixnum value, int cardinal, int localp)
{
	addr list, pos, queue;
	struct english_struct str;

	cleartype(str);
	str.local = local;
	str.root = Nil;
	str.index = value;
	str.cardinal = (cardinal != 0);
	str.string = Nil;
	number_name_cardinal(&str);

	/* output */
	charqueue_local(local, &queue, 0);
	GetCar(str.root, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		pushstring_charqueue_local(local, queue, pos);
	}
	if (localp)
		make_charqueue_local(local, queue, ret);
	else
		make_charqueue_heap(queue, ret);
}

_g void english_unit_local(LocalRoot local, addr *ret, addr pos, int cardinal)
{
	fixnum value;

	Check(local == NULL, "local error");
	if (GetFixnum_signed(pos, &value))
		fmte("The argument ~S must be a positive fixnum.", pos, NULL);
	english_unit_string(local, ret, value, cardinal, 1);
}

_g void english_unit_heap(LocalRoot local, addr *ret, addr pos, int cardinal)
{
	fixnum value;
	LocalStack stack;

	Check(local == NULL, "local error");
	if (GetFixnum_signed(pos, &value))
		fmte("The argument ~S must be a positive fixnum.", pos, NULL);

	push_local(local, &stack);
	english_unit_string(local, ret, value, cardinal, 0);
	rollback_local(local, stack);
}


/*
 *  Roma number
 */
static void roma_10(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "I"; break;
		case 2: ptr = "II"; break;
		case 3: ptr = "III"; break;
		case 4: ptr = subp? "IIII": "IV"; break;
		case 5: ptr = "V"; break;
		case 6: ptr = "VI"; break;
		case 7: ptr = "VII"; break;
		case 8: ptr = "VIII"; break;
		case 9: ptr = subp? "VIIII": "IX"; break;
		default: Abort("Invalid value."); return;
	}
	print_ascii_stream(stream, ptr);
}

static void roma_100(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "X"; break;
		case 2: ptr = "XX"; break;
		case 3: ptr = "XXX"; break;
		case 4: ptr = subp? "XXXX": "XL"; break;
		case 5: ptr = "L"; break;
		case 6: ptr = "LX"; break;
		case 7: ptr = "LXX"; break;
		case 8: ptr = "LXXX"; break;
		case 9: ptr = subp? "LXXXX": "XC"; break;
		default: Abort("Invalid value."); return;
	}
	print_ascii_stream(stream, ptr);
}

static void roma_1000(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "C"; break;
		case 2: ptr = "CC"; break;
		case 3: ptr = "CCC"; break;
		case 4: ptr = subp? "CCCC": "CD"; break;
		case 5: ptr = "D"; break;
		case 6: ptr = "DC"; break;
		case 7: ptr = "DCC"; break;
		case 8: ptr = "DCCC"; break;
		case 9: ptr = subp? "DCCCC": "CM"; break;
		default: Abort("Invalid value."); return;
	}
	print_ascii_stream(stream, ptr);
}

static void roma_4000(addr stream, unsigned value, int subp)
{
	const char *ptr;

	switch (value) {
		case 1: ptr = "M"; break;
		case 2: ptr = "MM"; break;
		case 3: ptr = "MMM"; break;
		default: Abort("Invalid value."); return;
	}
	print_ascii_stream(stream, ptr);
}

static void roma_call(addr stream, int value, int subp)
{
	int a, b;

	if (value <= 0) {
		return;
	}

	if (value < 10) {
		roma_10(stream, value, subp);
	}
	else if (value < 100) {
		a = value / 10;
		b = value % 10;
		roma_100(stream, a, subp);
		roma_call(stream, b, subp);
	}
	else if (value < 1000) {
		a = value / 100;
		b = value % 100;
		roma_1000(stream, a, subp);
		roma_call(stream, b, subp);
	}
	else {
		a = value / 1000;
		b = value % 1000;
		roma_4000(stream, a, subp);
		roma_call(stream, b, subp);
	}
}

_g void roma_integer(addr stream, fixnum value, int subp)
{
	Check(! (1 <= value && value <= 3999), "value error");
	roma_call(stream, (int)value, subp);
}

