#include "arch.h"
#include "array.h"
#include "build.h"
#include "bytespec.h"
#include "character.h"
#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "eval.h"
#include "eval_parse.h"
#include "eval_scope.h"
#include "eval_stack.h"
#include "function.h"
#include "info.h"
#include "memory.h"
#include "object.h"
#include "package.h"
#include "quote.h"
#include "strtype.h"
#include "stream.h"
#include "symbol.h"
#include "type.h"

#define INFO_STREAM		stdout
#define INFO_DEPTH		100

static void infobit_body(addr pos);
static void infoprint_stream(addr pos, int depth);
static FILE *InfoStream = NULL;


/*
 *  object string
 */
static const char *infochar_lisp(enum LISPTYPE type)
{
	switch (type) {
		case LISPTYPE_NIL:					return "nil";
		case LISPTYPE_T:					return "t";
		case LISPTYPE_TYPE:					return "type";
		case LISPTYPE_CLOS:					return "clos";
		case LISPTYPE_CONS:					return "cons";
		case LISPTYPE_ARRAY:				return "array";
		case LISPTYPE_VECTOR:				return "vector";
		case LISPTYPE_CHARACTER:			return "character";
		case LISPTYPE_STRING:				return "string";
		case LISPTYPE_HASHTABLE:			return "hashtable";
		case LISPTYPE_READTABLE:			return "readtable";
		case LISPTYPE_SYMBOL:				return "symbol";
		case LISPTYPE_FIXNUM:				return "fixnum";
		case LISPTYPE_BIGNUM:				return "bignum";
		case LISPTYPE_RATIO:				return "ratio";
		case LISPTYPE_SHORT_FLOAT:			return "short-float";
		case LISPTYPE_SINGLE_FLOAT:			return "single-float";
		case LISPTYPE_DOUBLE_FLOAT:			return "double-float";
		case LISPTYPE_LONG_FLOAT:			return "long-float";
		case LISPTYPE_COMPLEX:				return "complex";
		case LISPTYPE_CONTROL:				return "control";
		case LISPTYPE_CODE:					return "code";
		case LISPTYPE_CALLNAME:				return "callname";
		case LISPTYPE_FUNCTION:				return "function";
		case LISPTYPE_INDEX:				return "index";
		case LISPTYPE_SYSTEM:				return "system";
		case LISPTYPE_PACKAGE:				return "package";
		case LISPTYPE_RANDOM_STATE:			return "random-state";
		case LISPTYPE_PATHNAME:				return "pathname";
		case LISPTYPE_STREAM:				return "stream";
		case LISPTYPE_QUOTE:				return "quote";
		case LISPTYPE_RESTART:				return "restart";
		case LISPTYPE_EVAL:					return "eval";
		case LISPTYPE_ENVIRONMENT:			return "environment";
		case LISPTYPE_BITVECTOR:			return "bitvector";
		case LISPTYPE_PPRINT:				return "pprint";
		case LISPTYPE_BYTESPEC:				return "bytespec";

		case LISPSYSTEM_FIXNUM_CACHE:		return "?fixnum-cache";
		case LISPSYSTEM_CHARACTER_CACHE:	return "?character-cache";
		case LISPSYSTEM_BIGDATA:			return "?bigdata";
		case LISPSYSTEM_CHARACTER2:			return "?character2";
		case LISPSYSTEM_CHARQUEUE:			return "?charqueue";
		case LISPSYSTEM_CHARBIT:			return "?charbit";
		case LISPSYSTEM_SYMSTACK:			return "?symstack";
		case LISPSYSTEM_SYMARRAY:			return "?symarray";
		case LISPSYSTEM_BITTYPE:			return "?bittype";
		case LISPSYSTEM_READLABEL:			return "?readlabel";
		case LISPSYSTEM_READINFO:			return "?readinfo";
		case LISPSYSTEM_READTYPE:			return "?readtype";
		case LISPSYSTEM_BITCONS:			return "?bitcons";
		case LISPSYSTEM_BITBUFFER:			return "?bitbuffer";
		case LISPSYSTEM_HASHITERATOR:		return "?hash-iterator";
		case LISPSYSTEM_PACKAGEITERATOR:	return "?package-iterator";
		case LISPSYSTEM_TAGINFO:			return "?taginfo";
		case LISPSYSTEM_ARRAY_DIMENSION:	return "?array-dimension";
		case LISPSYSTEM_ARRAY_GENERAL:		return "?array-general";
		case LISPSYSTEM_ARRAY_SPECIALIZED:	return "?array-specialized";
		case LISPSYSTEM_CODE:				return "?code";
		case LISPSYSTEM_PROMPT:				return "?prompt";
		case LISPSYSTEM_ENVROOT:			return "?envroot";
		case LISPSYSTEM_ENVSTACK:			return "?envstack";
		case LISPSYSTEM_SLOT:				return "?slot";
		case LISPSYSTEM_SLOT_VECTOR:		return "?slot-vector";
		case LISPSYSTEM_CLOS_VALUE:			return "?clos-value";
		case LISPSYSTEM_GENERIC:			return "?generic";
		case LISPSYSTEM_ARGUMENT:			return "?argument";
		case LISPSYSTEM_CHECK:				return "?check";

		case LISPSYSTEM_UNBOUND:			return "unboundtype";
		case LISPSYSTEM_SPACE:				return "space";
		case LISPSYSTEM_SPACE1:				return "space1";
		case LISPSYSTEM_RESERVED:			return "reserved";
		case LISPSYSTEM_END:				return "end";
		default:							return "error";
	}
}

static const char *infochar_size(enum LISPSIZE size)
{
	switch (size) {
		case LISPSIZE_ARRAY2:		return "array2";
		case LISPSIZE_ARRAY4:		return "array4";
		case LISPSIZE_ARRAY8:		return "array8";
		case LISPSIZE_SMALLSIZE:	return "smallsize";
		case LISPSIZE_ARRAYBODY:	return "arraybody";
		case LISPSIZE_BODY2:		return "body2";
		case LISPSIZE_BODY4:		return "body4";
		case LISPSIZE_BODY8:		return "body8";
		default:					return "error";
	}
}

static const char *infochar_decl(enum LISPDECL decl)
{
	switch (decl) {
		case LISPDECL_EMPTY:				return "empty";
		case LISPDECL_OPTIMIZED:			return "optimized";
		case LISPDECL_SUBTYPEP:				return "subtypep";
		case LISPDECL_TYPE:					return "type";
		case LISPDECL_CLOS:					return "clos";
		case LISPDECL_ASTERISK:				return "*";

		case LISPDECL_AND:					return "and";
		case LISPDECL_OR:					return "or";
		case LISPDECL_EQL:					return "eql";
		case LISPDECL_MEMBER:				return "member";
		case LISPDECL_MOD:					return "mod";
		case LISPDECL_NOT:					return "not";
		case LISPDECL_SATISFIES:			return "satisfies";
		case LISPDECL_VALUES:				return "values";

		case LISPDECL_ATOM:					return "atom";
		case LISPDECL_LIST:					return "list";
		case LISPDECL_VECTOR:				return "vector";
		case LISPDECL_SIMPLE_VECTOR:		return "simple-vector";
		case LISPDECL_BIT_VECTOR:			return "bit-vector";
		case LISPDECL_SIMPLE_BIT_VECTOR:	return "simple-bit-vector";
		case LISPDECL_EXTENDED_CHAR:		return "extended-char";
		case LISPDECL_STRING:				return "string";
		case LISPDECL_BASE_STRING:			return "base-string";
		case LISPDECL_SIMPLE_STRING:		return "simple-string";
		case LISPDECL_SIMPLE_BASE_STRING:	return "simple-base-string";
		case LISPDECL_SIGNED_BYTE:			return "signed-byte";
		case LISPDECL_UNSIGNED_BYTE:		return "unsigned-byte";
		case LISPDECL_BIT:					return "bit";
		case LISPDECL_FIXNUM:				return "fixnum";
		case LISPDECL_BIGNUM:				return "bignum";

		case LISPDECL_NIL:					return "nil";
		case LISPDECL_T:					return "t";
		case LISPDECL_NULL:					return "null";
		case LISPDECL_CONS:					return "cons";
		case LISPDECL_HASH_TABLE:			return "hash-table";
		case LISPDECL_SYMBOL:				return "symbol";
		case LISPDECL_KEYWORD:				return "keyword";
		case LISPDECL_PACKAGE:				return "package";
		case LISPDECL_RANDOM_STATE:			return "random-state";
		case LISPDECL_READTABLE:			return "readtable";
		case LISPDECL_FUNCTION:				return "function";
		case LISPDECL_COMPILED_FUNCTION:	return "compiled-function";
		case LISPDECL_PATHNAME:				return "pathname";
		case LISPDECL_LOGICAL_PATHNAME:		return "logical-pathname";
		case LISPDECL_SEQUENCE:				return "sequence";
		case LISPDECL_ARRAY:				return "array";
		case LISPDECL_SIMPLE_ARRAY:			return "simple-array";
		case LISPDECL_CHARACTER:			return "character";
		case LISPDECL_BASE_CHAR:			return "base-char";
		case LISPDECL_STANDARD_CHAR:		return "standard-char";
		case LISPDECL_NUMBER:				return "number";
		case LISPDECL_REAL:					return "real";
		case LISPDECL_RATIONAL:				return "rational";
		case LISPDECL_RATIO:				return "ratio";
		case LISPDECL_INTEGER:				return "integer";
		case LISPDECL_COMPLEX:				return "complex";
		case LISPDECL_FLOAT:				return "float";
		case LISPDECL_SHORT_FLOAT:			return "short-float";
		case LISPDECL_SINGLE_FLOAT:			return "single-float";
		case LISPDECL_DOUBLE_FLOAT:			return "double-float";
		case LISPDECL_LONG_FLOAT:			return "long-float";
		case LISPDECL_RESTART:				return "restart";
		case LISPDECL_ENVIRONMENT:			return "environment";
		case LISPDECL_STREAM:				return "stream";
		case LISPDECL_BROADCAST_STREAM:		return "broadcast-stream";
		case LISPDECL_CONCATENATED_STREAM:	return "concatenated-stream";
		case LISPDECL_ECHO_STREAM:			return "echo-stream";
		case LISPDECL_FILE_STREAM:			return "file-stream";
		case LISPDECL_STRING_STREAM:		return "string-stream";
		case LISPDECL_SYNONYM_STREAM:		return "synonym-stream";
		case LISPDECL_TWO_WAY_STREAM:		return "two-way-stream";
		case LISPDECL_PROMPT_STREAM:		return "prompt-stream";
		default:							return "invalid";
	}
}

static const char *infochar_eval_parse(addr pos)
{
	switch (RefEvalParseType(pos)) {
		case EVAL_PARSE_EMPTY:					return "empty";
		case EVAL_PARSE_NIL:					return "nil";
		case EVAL_PARSE_T:						return "t";
		case EVAL_PARSE_INTEGER:				return "integer";
		case EVAL_PARSE_RATIONAL:				return "rational";
		case EVAL_PARSE_COMPLEX:				return "complex";
		case EVAL_PARSE_CHARACTER:				return "character";
		case EVAL_PARSE_ARRAY:					return "array";
		case EVAL_PARSE_VECTOR:					return "vector";
		case EVAL_PARSE_BITVECTOR:				return "bit-vector";
		case EVAL_PARSE_STRING:					return "string";
		case EVAL_PARSE_SYMBOL:					return "symbol";
		case EVAL_PARSE_FLOAT:					return "float";
		case EVAL_PARSE_DECLAIM:				return "declaim";

		case EVAL_PARSE_PROGN:					return "progn";
		case EVAL_PARSE_LET:					return "let";
		case EVAL_PARSE_LETA:					return "let*";
		case EVAL_PARSE_SETQ:					return "setq";
		case EVAL_PARSE_DEFUN:					return "defun";
		case EVAL_PARSE_DEFMACRO:				return "defmacro";
		case EVAL_PARSE_DESTRUCTURING_BIND:		return "destructuring-bind";
		case EVAL_PARSE_DEFINE_SYMBOL_MACRO:	return "define-symbol-macro";
		case EVAL_PARSE_SYMBOL_MACROLET:		return "symbol-macrolet";
		case EVAL_PARSE_MACRO_LAMBDA:			return "macro-lambda";
		case EVAL_PARSE_QUOTE:					return "quote";
		case EVAL_PARSE_FUNCTION:				return "function";
		case EVAL_PARSE_LAMBDA:					return "lambda";
		case EVAL_PARSE_IF:						return "if";
		case EVAL_PARSE_UNWIND_PROTECT:			return "unwind-protect";
		case EVAL_PARSE_TAGBODY:				return "tagbody";
		case EVAL_PARSE_TAG:					return "tag";
		case EVAL_PARSE_GO:						return "go";
		case EVAL_PARSE_BLOCK:					return "block";
		case EVAL_PARSE_RETURN_FROM:			return "return-from";
		case EVAL_PARSE_CATCH:					return "catch";
		case EVAL_PARSE_THROW:					return "throw";
		case EVAL_PARSE_FLET:					return "flet";
		case EVAL_PARSE_LABELS:					return "labels";
		case EVAL_PARSE_THE:					return "the";
		case EVAL_PARSE_EVAL_WHEN:				return "eval-when";
		case EVAL_PARSE_VALUES:					return "values";
		case EVAL_PARSE_LOCALLY:				return "locally";
		case EVAL_PARSE_CALL:					return "call";
		case EVAL_PARSE_MULTIPLE_VALUE_BIND:	return "multiple-value-bind";
		case EVAL_PARSE_MULTIPLE_VALUE_CALL:	return "multiple-value-call";
		case EVAL_PARSE_MULTIPLE_VALUE_PROG1:	return "multiple-value-prog1";
		case EVAL_PARSE_NTH_VALUE:				return "nth-value";
		case EVAL_PARSE_PROGV:					return "progv";
		default:								return "invalid";
	}
}

static const char *infochar_eval_stack(addr pos)
{
	switch (RefEvalStackType(pos)) {
		case EVAL_STACK_MODE_NIL:		return "nil";
		case EVAL_STACK_MODE_LAMBDA:	return "lambda";
		case EVAL_STACK_MODE_TAGBODY:	return "tagbody";
		case EVAL_STACK_MODE_BLOCK:		return "block";
		default:						return "invalid";
	}
}

static const char *infochar_eval(addr pos)
{
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_DECLARE:			return "declare";
		case EVAL_TYPE_PARSE:			return "parse";
		case EVAL_TYPE_STACK:			return "stack";
		case EVAL_TYPE_SCOPE:			return "scope";
		case EVAL_TYPE_TABLEVALUE:		return "value";
		case EVAL_TYPE_TABLEFUNCTION:	return "function";
		case EVAL_TYPE_TABLECALL:		return "call";
		case EVAL_TYPE_TABLETAGBODY:	return "tagblody";
		case EVAL_TYPE_TABLEBLOCK:		return "block";
		case EVAL_TYPE_CODE:			return "code";
		default:						return "invalid";
	}
}

static const char *infochar_array(enum ARRAY_TYPE type)
{
	switch (type) {
		case ARRAY_TYPE_EMPTY:			return "empty";
		case ARRAY_TYPE_T:				return "t";
		case ARRAY_TYPE_BIT:			return "bit";
		case ARRAY_TYPE_CHARACTER:		return "character";
		case ARRAY_TYPE_SIGNED:			return "signed";
		case ARRAY_TYPE_UNSIGNED:		return "unsigned";
		case ARRAY_TYPE_SINGLE_FLOAT:	return "single-float";
		case ARRAY_TYPE_DOUBLE_FLOAT:	return "double-float";
		case ARRAY_TYPE_LONG_FLOAT:		return "long-float";
		default:						return "error";
	}
}


/*
 *  info stream
 */
static enum LISPTYPE info_gettype(addr pos)
{
	return (pos == Unbound)? LISPSYSTEM_UNBOUND: GetType(pos);
}

static void info_flush(void)
{
	if (InfoStream == NULL)
		InfoStream = INFO_STREAM;
	fflush(InfoStream);
}

static void info_valist(const char *fmt, va_list args)
{
	if (InfoStream == NULL)
		InfoStream = INFO_STREAM;
	vfprintf(InfoStream, fmt, args);
}

static void info_stdarg(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	info_valist(fmt, args);
	va_end(args);
}

static void info_eol(void)
{
	info_stdarg("\n");
	info_flush();
}

static void info_start_valist(const char *fmt, va_list args)
{
	info_stdarg("[INFO] ");
	info_valist(fmt, args);
}

static void info_start(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	info_start_valist(fmt, args);
	va_end(args);
}

void info(const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start_valist(fmt, args);
		va_end(args);
		info_eol();
	}
}

void info_noeol(const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start_valist(fmt, args);
		va_end(args);
		info_flush();
	}
}

void infoerror(const char *first, int line, const char *func, const char *fmt, ...)
{
	va_list args;

	if (lisp_info_enable) {
		va_start(args, fmt);
		info_start("%s(%d) %s: ", first, line, func);
		info_valist(fmt, args);
		info_eol();
		va_end(args);
	}
}

static void infotime(void)
{
	char buffer[32];

	nowtime_string(buffer, 32);
	info("  %s", buffer);
}

void infosystem(void)
{
	info("*** SYSTEM-INFORMATION BEGIN ***");
	infotime();
	info("*** SYSTEM-INFORMATION END ***");
}

static void infobit_dump(addr pos)
{
	int c;
	size_t m, i, len;
	pbyte body;
	char buffer[256];

	if (IsBody(pos)) {
		posbodylen(pos, &body, &len);
		for (i = 0; i < len; i++) {
			c = RdByte(body + i);
			m = i % 16;
			if (i && m == 0) {
				buffer[16] = 0;
				info("%s", buffer);
			}
			buffer[m] = isstandardtype(c)? c: '.';
		}
		info("%s", buffer);
	}
	else {
		info("(nobody)");
	}
}

static int infobit_system(addr pos)
{
	switch (info_gettype(pos)) {
		case LISPSYSTEM_UNBOUND:
			info("  %-10s = %s", "type", "unbound");
			return 0;

		case LISPSYSTEM_SPACE:
			info("  %-10s = %s", "type", "space");
			return 0;

		case LISPSYSTEM_SPACE1:
			info("  %-10s = %s", "type", "space1");
			return 0;

		case LISPSYSTEM_RESERVED:
			info("  %-10s = %s", "type", "reserveed");
			return 0;

		default:
			return 1;
	}
}

#define ONOFF(x) ((x)? '1': '-')
static void infobit_info(addr pos)
{
	enum LISPTYPE type;
	int status, check, user, array, body;
	size_t length;

	type = info_gettype(pos);
	status = GetStatus(pos);
	check = GetCheck(pos);
	user = GetUser(pos);
	array = IsArray(pos);
	body = IsBody(pos);

	info("  %-10s = %d: %s", "type", type, infochar_lisp(type));
	info("  %-10s = %02X: %s [DRSFG:%c%c%c%c%c]", "status",
			status,
			infochar_size(GetStatusSize(pos)),
			ONOFF(GetStatusDynamic(pos)),
			ONOFF(GetStatusReadOnly(pos)),
			ONOFF(GetStatusSystem(pos)),
			ONOFF(GetStatusFixed(pos)),
			ONOFF(GetStatusGc(pos)));
	info("  %-10s = %02X [ABD248:%c%c%c%c%c%c]", "check",
			check,
			ONOFF(GetCheckArray(pos)),
			ONOFF(GetCheckBody(pos)),
			ONOFF(GetCheckArrayBody(pos)),
			ONOFF(GetCheckSize2(pos)),
			ONOFF(GetCheckSize4(pos)),
			ONOFF(GetCheckSize8(pos)));
	info("  %-10s = %02X", "user", user);
	if (array) {
		lenarray(pos, &length);
		info("  %-10s = %" PRIu64, "array", (uint64_t)length);
	}
	else {
		info("  %-10s = 0", "array");
	}
	if (body) {
		lenbody(pos, &length);
		info("  %-10s = %" PRIu64, "body", (uint64_t)length);
	}
	else {
		info("  %-10s = 0", "body");
	}
}

void infobit(addr pos)
{
	info("*** LISPBIT BEGIN ***");
	if (infobit_system(pos)) {
		infobit_info(pos);
		info("---body---");
		infobit_body(pos);
	}
	info("*** LISPBIT END ***");
}

void infoprint(addr pos)
{
	infoprint_stream(pos, 0);
	info_eol();
}

void infoprint_noeol(addr pos)
{
	infoprint_stream(pos, 0);
}


/*
 *  function type
 */
/* nil */
static void infobit_nil(addr pos)
{
	info("nil object: %s", (Nil == pos)? "valid": "INVALID");
}

static void infoprint_nil(void)
{
	info_stdarg("NIL");
}


/* t */
static void infobit_t(addr pos)
{
	info("T object: %s", (T == pos)? "valid": "INVALID");
}

static void infoprint_t(void)
{
	info_stdarg("T");
}


/* type */
static void infobit_type(addr pos)
{
	info("Not: %d", RefNotDecl(pos));
	info("Decl: %s", infochar_decl(RefLispDecl(pos)));
}

static void infoprint_type(addr pos)
{
	info_stdarg("#<TYPE %s%s>",
			(RefNotDecl(pos)? "not.": ""),
			infochar_decl(RefLispDecl(pos)));
}


/* cons */
static void infobit_cons(addr pos)
{
	addr car, cdr;

	GetCons(pos, &car, &cdr);
	info("Car: %s", infochar_lisp(info_gettype(car)));
	info("Cdr: %s", infochar_lisp(info_gettype(cdr)));
}

static void infoprint_cons(addr pos, int depth)
{
	int next;
	addr left;

	if (INFO_DEPTH <= depth) {
		info_stdarg("...");
		return;
	}
	info_stdarg("(");
	for (next = 0; ; next = 1) {
		GetCons(pos, &left, &pos);
		if (next) info_stdarg(" ");
		infoprint_stream(left, depth);
		if (pos == Nil) {
			info_stdarg(")");
			return;
		}
		if (pos == Unbound || GetType(pos) != LISPTYPE_CONS) {
			info_stdarg(" . ");
			infoprint_stream(pos, depth);
			info_stdarg(")");
			return;
		}
	}
}


/* array */
static void infobit_array(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	info("simple      : %s", str->simple? "t": "nil");
	info("adjustable  : %s", str->adjustable? "t": "nil");
	info("fillpointer : %s", str->fillpointer? "t": "nil");
	info("displaced   : %s", str->displaced? "t": "nil");
	info("type        : %s", infochar_array(str->type));
	info("element     : %u", str->element);
	info("bytesize    : %u", str->bytesize);
	info("size        : %zu", str->size);
	info("front       : %zu", str->front);
	info("dimension   : %zu", str->dimension);
	info("offset      : %zu", str->offset);
	info("refer       : %zu", str->refer);
}

static void infoprint_array(addr pos, int depth)
{
	info_stdarg("#<ARRAY>");
}


/* eval */
static void infobit_eval(addr pos)
{
	info("EvalType: %s", infochar_eval(pos));
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_PARSE:
			info("ParseType: %s", infochar_eval_parse(pos));
			break;

		case EVAL_TYPE_STACK:
			info("StackType: %s", infochar_eval_stack(pos));
			break;

		case EVAL_TYPE_SCOPE:
			info_noeol("ScopeThe: ");
			infoprint(RefEvalScopeThe(pos));
			info_noeol("ScopeValue: ");
			infoprint(RefEvalScopeValue(pos));
			break;

		case EVAL_TYPE_DECLARE:
		default:
			break;
	}
}

static void infobit_symbol(addr pos)
{
	int c;
	addr package, name, pack;
	pbyte body;
	size_t i, len, size;
	char buffer[64];

	GetNameSymbol(pos, &name);
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		strcpy(buffer, "(GENSYM)::");
		len = strlen(buffer);
	}
	else {
		getname_package(package, &pack);
		posbodylen(pack, &body, &size);
		len = 0;
		for (i = 0; i < size; i++) {
			c = RdByte(body + i);
			if (c < 0) return;
			buffer[i + len] = c;
			if (20 <= i) {
				buffer[i + len + 1] = '.';
				buffer[i + len + 2] = '.';
				buffer[i + len + 3] = '.';
				i += 4;
				break;
			}
		}
		len += i;
		buffer[len++] = ':';
		buffer[len++] = ':';
	}

	posbodylen(name, &body, &size);
	for (i = 0; i < size; i++) {
		c = RdByte(body + i);
		if (c < 0) return;
		buffer[i + len] = c;
		if (20 <= i) {
			buffer[i + len + 1] = '.';
			buffer[i + len + 2] = '.';
			buffer[i + len + 3] = '.';
			i += 4;
			break;
		}
	}
	len += i;
	buffer[len] = 0;
	info("Symbol-Name: %s", buffer);
}

static void infobit_fixnum(addr pos)
{
	fixnum value;
	GetFixnum(pos, &value);
	info("Fixnum: [#x%08" PRIXF "] %10" PRIdF, value, value);
}

static void infobit_single(addr pos)
{
	single_float value;
	GetSingleFloat(pos, &value);
	info("Fixnum: %e - single", value);
}

static void infobit_double(addr pos)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	info("Fixnum: %e - double", value);
}

static void infoprint_vector(addr array, int depth)
{
	addr pos;
	size_t i, len;

	if (INFO_DEPTH <= depth) {
		info_stdarg("...");
		return;
	}
	info_stdarg("#(");
	lenarray(array, &len);
	for (i = 0; i < len; i++) {
		getarray(array, i, &pos);
		if (i) info_stdarg(" ");
		infoprint_stream(pos, depth);
	}
	info_stdarg(")");
}

static void infoprint_pathname(addr pos, int depth)
{
	info_stdarg("#p:");
	infoprint_vector(pos, depth);
}

static void infostringbody(addr pos)
{
	size_t i, len;
	unicode c;

	string_length(pos, &len);
	for (i = 0; i < len; i++) {
		string_getc(pos, i, &c);
		info_stdarg("%c", isstandardtype(c)? c: '.');
	}
}

static void infoprint_clos(addr pos)
{
	addr check, key, name;

	info_stdarg("#<");
	/* class-of */
	GetClassOfClos(pos, &check);
	GetConst(KEYWORD_NAME, &key);
	if (key == Unbound) {
		info_stdarg("CLOS>");
		return;
	}
	else if (! clos_getp(check, key, &name)) {
		info_stdarg("UNBOUND");
	}
	else if (name == Unbound) {
		info_stdarg("UNBOUND");
	}
	else if (! symbolp(name)) {
		info_stdarg("INVALID");
	}
	else {
		GetNameSymbol(name, &name);
		infostringbody(name);
	}
	info_stdarg(" ");

	/* name */
	if (! clos_getp(pos, key, &name)) {
		info_stdarg("UNBOUND");
	}
	else if (name == Unbound) {
		info_stdarg("UNBOUND");
	}
	else if (! symbolp(name)) {
		info_stdarg("INVALID");
	}
	else {
		GetNameSymbol(name, &name);
		infostringbody(name);
	}
	info_stdarg(">");
}

static void infoprint_fixnum(addr pos)
{
	fixnum value;
	GetFixnum(pos, &value);
	info_stdarg("%" PRIdF, value);
}

static void infoprint_index(addr pos)
{
	info_stdarg("#<index.%zu>", RefIndex(pos));
}

static void infoprint_single(addr pos)
{
	single_float value;
	GetSingleFloat(pos, &value);
	info_stdarg("%g", value);
}

static void infoprint_double(addr pos)
{
	double_float value;
	GetDoubleFloat(pos, &value);
	info_stdarg("%g", value);
}

static void infoprint_character(addr pos)
{
	unicode value;
	GetCharacter(pos, &value);
	if (value < 0x80) {
		if (isstandardtype(value))
			info_stdarg("#\\%c", value);
		else
			info_stdarg("#\\(cannot-printable)");
	}
	else {
		info_stdarg("#\\(unicode)");
	}
}

static void infoprint_string(addr pos)
{
	info_stdarg("\"");
	infostringbody(pos);
	info_stdarg("\"");
}

static void infoprint_symbol(addr pos)
{
	addr package;

	GetPackageSymbol(pos, &package);
	if (package == Nil)
		info_stdarg("#:");
	else {
		getname_package(package, &package);
		if (string_equal_char(package, LISP_KEYWORD)) {
			info_stdarg(":");
		}
		else {
			infostringbody(package);
			info_stdarg("::");
		}
	}
	GetNameSymbol(pos, &pos);
	infostringbody(pos);
}

/* ARGSUSED0 */
static void infoprint_callname(addr pos)
{
	info_stdarg("#<callname:");
	GetCallName(pos, &pos);
	infoprint_symbol(pos);
	info_stdarg(">");
}

static void infoprint_function(addr pos)
{
	info_stdarg("#<function>");
}

static void infoprint_eval(addr pos)
{
	info_stdarg("#<eval.%s", infochar_eval(pos));
	switch (RefEvalType(pos)) {
		case EVAL_TYPE_DECLARE:
			break;

		case EVAL_TYPE_PARSE:
			info_stdarg(".%s", infochar_eval_parse(pos));
			break;

		case EVAL_TYPE_STACK:
			info_stdarg(".%s", infochar_eval_stack(pos));
			break;

		case EVAL_TYPE_SCOPE:
			GetEvalScopeValue(pos, &pos);
			info_stdarg(".");
			infoprint_noeol(pos);
			break;

		default:
			info_stdarg(".invalid");
			break;
	}
	info_stdarg(">");
}

static void infoprint_code(addr pos)
{
	info_stdarg("#<code.");
	switch (gettype_code(pos)) {
		case CodeType_Default: info_stdarg("default"); break;
		case CodeType_Return: info_stdarg("return"); break;
		case CodeType_Argument: info_stdarg("argument"); break;
		case CodeType_Push: info_stdarg("push"); break;
		case CodeType_Remove: info_stdarg("remove"); break;
		case CodeType_Close: info_stdarg("close"); break;
		case CodeType_Protect: info_stdarg("protect"); break;
		case CodeType_TagBody: info_stdarg("tagbody"); break;
		case CodeType_Block: info_stdarg("block"); break;
		case CodeType_Catch: info_stdarg("catch"); break;
		case CodeType_Condition: info_stdarg("condition"); break;
		case CodeType_Restart: info_stdarg("restart"); break;
		default: info_stdarg("invalid"); break;
	}
	info_stdarg(">");
}

static void infoquote_front(addr pos, const char *str)
{
	info_stdarg(str);
	getvalue_quote(pos, &pos);
	infoprint_noeol(pos);
}

static void infoquote_list(addr pos, const char *str)
{
	info_stdarg("#<");
	info_stdarg(str);
	info_stdarg(":");
	getvalue_quote(pos, &pos);
	infoprint_noeol(pos);
	info_stdarg(">");
}

static void infoprint_streamtype(addr pos)
{
	info_stdarg("#<STREAM:");
	switch (getstreamtype(pos)) {
		case StreamType_BinaryInput: info_stdarg("BINARY-INPUT"); break;
		case StreamType_BinaryOutput: info_stdarg("BINARY-OUTPUT"); break;
		case StreamType_BinaryIO: info_stdarg("BINARY-IO"); break;
		case StreamType_CharacterInput: info_stdarg("CHARACTER-INPUT"); break;
		case StreamType_CharacterOutput: info_stdarg("CHARACTER-OUTPUT"); break;
		case StreamType_CharacterIO: info_stdarg("CHARACTER-IO"); break;
		case StreamType_BincharInput: info_stdarg("BINCHAR-INPUT"); break;
		case StreamType_BincharOutput: info_stdarg("BINCHAR-OUTPUT"); break;
		case StreamType_BincharIO: info_stdarg("BINCHAR-IO"); break;
		case StreamType_StringInput: info_stdarg("STRING-INPUT"); break;
		case StreamType_StringOutput: info_stdarg("STRING-OUTPUT"); break;
		case StreamType_Synonym: info_stdarg("SYNONYM"); break;
		case StreamType_BroadCast: info_stdarg("BROADCAST"); break;
		case StreamType_Concatenated: info_stdarg("CONCATENATED"); break;
		case StreamType_TwoWay: info_stdarg("TWOWAY"); break;
		case StreamType_Echo: info_stdarg("ECHO"); break;
		case StreamType_Prompt: info_stdarg("PROMPT"); break;
		default: info_stdarg("ERROR"); break;
	}
	info_stdarg(">");
}

static void infoprint_quote(addr pos)
{
	if (quote_back_p(pos))
		infoquote_front(pos, "`");
	else if (quote_comma_p(pos))
		infoquote_front(pos, ",");
	else if (quote_atsign_p(pos))
		infoquote_front(pos, ",@");
	else if (quote_dot_p(pos))
		infoquote_front(pos, ",.");
	else if (quote_quote_p(pos))
		infoquote_list(pos, "quote.quote");
	else if (quote_append_p(pos))
		infoquote_list(pos, "quote.append");
	else if (quote_nconc_p(pos))
		infoquote_list(pos, "quote.nconc");
	else if (quote_list_p(pos))
		infoquote_list(pos, "quote.list");
	else if (quote_lista_p(pos))
		infoquote_list(pos, "quote.list*");
	else if (quote_clobberable_p(pos))
		infoquote_list(pos, "quote.clobberable");
	else
		infoquote_list(pos, "quote.error");
}

static void infoprint_restart(addr pos)
{
	info_stdarg("#<RESTART ");
	getname_restart(pos, &pos);
	infoprint_noeol(pos);
	info_stdarg(">");
}

static void infoprint_bytespec(addr pos)
{
	struct bytespec_struct *ptr = ByteSpecStruct(pos);
	info_stdarg("#<BYTE size:%zu position:%zu>", ptr->size, ptr->position);
}

static void infoprint_unbound(void)
{
	info_stdarg("#<UNBOUND>");
}

static void infoprint_space(addr pos)
{
	size_t size;
	GetSizeSpace(pos, &size);
	info_stdarg("#<SPACE %lu>", (unsigned long)size);
}

static void infoprint_space1(addr pos)
{
	info_stdarg("#<SPACE %u>", (unsigned)pos[1]);
}

static void infoprint_reserved(addr pos)
{
	size_t size;
	GetSizeReserved(pos, &size);
	info_stdarg("#<RESERVED %lu>", (unsigned long)size);
}

static void infoprint_default(addr pos)
{
	int type;
	const char *ptr;
	
	type = (int)info_gettype(pos);
	ptr = infochar_lisp(type);
	info_stdarg("#<OBJECT %s %d[0x%X]>", ptr, type, type);
}

static void infobit_body(addr pos)
{
	switch (info_gettype(pos)) {
		case LISPTYPE_NIL: infobit_nil(pos); break;
		case LISPTYPE_T: infobit_t(pos); break;
		case LISPTYPE_TYPE: infobit_type(pos); break;
		case LISPTYPE_CONS: infobit_cons(pos); break;
		case LISPTYPE_ARRAY: infobit_array(pos); break;
		case LISPTYPE_EVAL: infobit_eval(pos); break;
		case LISPTYPE_SYMBOL: infobit_symbol(pos); break;
		case LISPTYPE_FIXNUM: infobit_fixnum(pos); break;
		case LISPTYPE_SINGLE_FLOAT: infobit_single(pos); break;
		case LISPTYPE_DOUBLE_FLOAT: infobit_double(pos); break;
		default: infobit_dump(pos); break;
	}
}

static void infoprint_stream(addr pos, int depth)
{
	depth++;
	switch (info_gettype(pos)) {
		case LISPTYPE_NIL: infoprint_nil(); break;
		case LISPTYPE_T: infoprint_t(); break;
		case LISPTYPE_TYPE: infoprint_type(pos); break;
		case LISPTYPE_CONS: infoprint_cons(pos, depth); break;
		case LISPTYPE_ARRAY: infoprint_array(pos, depth); break;
		case LISPTYPE_VECTOR: infoprint_vector(pos, depth); break;
		case LISPTYPE_PATHNAME: infoprint_pathname(pos, depth); break;
		case LISPTYPE_CLOS: infoprint_clos(pos); break;
		case LISPTYPE_CHARACTER: infoprint_character(pos); break;
		case LISPTYPE_STRING: infoprint_string(pos); break;
		case LISPTYPE_FIXNUM: infoprint_fixnum(pos); break;
		case LISPTYPE_INDEX: infoprint_index(pos); break;
		case LISPTYPE_SINGLE_FLOAT: infoprint_single(pos); break;
		case LISPTYPE_DOUBLE_FLOAT: infoprint_double(pos); break;
		case LISPTYPE_SYMBOL: infoprint_symbol(pos); break;
		case LISPTYPE_CALLNAME: infoprint_callname(pos); break;
		case LISPTYPE_FUNCTION: infoprint_function(pos); break;
		case LISPTYPE_EVAL: infoprint_eval(pos); break;
		case LISPTYPE_CODE: infoprint_code(pos); break;
		case LISPTYPE_STREAM: infoprint_streamtype(pos); break;
		case LISPTYPE_QUOTE: infoprint_quote(pos); break;
		case LISPTYPE_RESTART: infoprint_restart(pos); break;
		case LISPTYPE_BYTESPEC: infoprint_bytespec(pos); break;

		case LISPSYSTEM_UNBOUND: infoprint_unbound(); break;
		case LISPSYSTEM_SPACE: infoprint_space(pos); break;
		case LISPSYSTEM_SPACE1: infoprint_space1(pos); break;
		case LISPSYSTEM_RESERVED: infoprint_reserved(pos); break;
		default: infoprint_default(pos); break;
	}
}

