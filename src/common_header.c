#include "common_header.h"
#include "cons.h"
#include "integer.h"

/*
 *  clang function
 */
_g void defconstant_symbol(addr symbol, addr value)
{
	setspecial_symbol(symbol);
	SetValueSymbol(symbol, value);
	SetStatusValue(symbol, LISPSTATUS_READONLY, 1);
}

static void check_function_macro(addr symbol)
{
	addr check;

	GetFunctionSymbol(symbol, &check);
	if (check != Unbound)
		fmte("The function ~S is already exist.", symbol, NULL);
	getmacro_symbol(symbol, &check);
	if (check != Unbound)
		fmte("The macro-function ~S is already exist.", symbol, NULL);
}

_g void setfunction_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	SetFunctionSymbol(symbol, value);
}

_g void setmacro_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	setmacro_symbol(symbol, value);
}

_g void setsetfmacro_common(addr symbol, addr value)
{
	addr check;

	getsetfmacro_symbol(symbol, &check);
	if (check != Unbound)
		fmte("The setf-macro-function ~S is already exist.", symbol, NULL);
	setsetfmacro_symbol(symbol, value);
}

_g void define_special_operator(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	set_special_operator(symbol);
}


/*
 *  :start, :end
 */
static void getsize_keyword_start(addr key, addr rest, addr *reta, size_t *rets)
{
	addr pos;

	if (getplist(rest, key, &pos)) {
		*reta = fixnumh(0);
		*rets = 0;
	}
	else {
		getindex_error(pos, rets);
		*reta = pos;
	}
}

static void getsize_keyword_end(addr key, addr rest, size_t size,
		addr *reta, size_t *rets)
{
	addr pos;

	if (getplist(rest, key, &pos) || pos == Nil) {
		*reta = intsizeh(size);
		*rets = size;
	}
	else {
		getindex_error(pos, rets);
		*reta = pos;
	}
}

static void keyword_start_end_constant(constindex cstart, constindex cend,
		size_t size, addr rest, size_t *pstart, size_t *pend)
{
	addr kstart, kend, astart, aend;
	size_t start, end;

	GetConstant(cstart, &kstart);
	GetConstant(cend, &kend);
	getsize_keyword_start(kstart, rest, &astart, &start);
	getsize_keyword_end(kend, rest, size, &aend, &end);
	if (size < start) {
		fmte("The ~S position ~S must be less than "
				"the sequence length.", kstart, astart, NULL);
	}
	if (size < end) {
		fmte("The ~S position ~S must be less than "
				"equal to the sequence length.", kend, aend, NULL);
	}
	if (end < start) {
		fmte("The ~S position ~S must be less than "
				"equal to the ~S position ~S.", kstart, astart, kend, aend, NULL);
	}
	*pstart = start;
	*pend = end;
}

_g void keyword_start_end(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	keyword_start_end_constant(CONSTANT_KEYWORD_START, CONSTANT_KEYWORD_END,
			size, rest, pstart, pend);
}

_g void keyword_start1_end1(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	keyword_start_end_constant(CONSTANT_KEYWORD_START1, CONSTANT_KEYWORD_END1,
			size, rest, pstart, pend);
}

_g void keyword_start2_end2(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	keyword_start_end_constant(CONSTANT_KEYWORD_START2, CONSTANT_KEYWORD_END2,
			size, rest, pstart, pend);
}


/*
 *  build
 */
_g void build_common_after_settings(void)
{
	addr symbol, value;

	/* (defvar *macroexpand-hook* #'funcall) */
	GetConst(COMMON_FUNCALL, &value);
	GetFunctionSymbol(value, &value);
	GetConst(SPECIAL_MACROEXPAND_HOOK, &symbol);
	SetValueSymbol(symbol, value);
}

