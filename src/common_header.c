#include "common_header.h"
#include "cons.h"
#include "cons_plist.h"
#include "integer.h"

/*
 *  clang function
 */
void defconstant_symbol(addr symbol, addr value)
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
		Abort("COMMON-LISP function error.");
	getmacro_symbol(symbol, &check);
	if (check != Unbound)
		Abort("COMMON-LISP macro-function error.");
}

void setfunction_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	SetFunctionSymbol(symbol, value);
}

void setmacro_common(addr symbol, addr value)
{
	check_function_macro(symbol);
	setmacro_symbol(symbol, value);
}

void setsetfmacro_common(addr symbol, addr value)
{
	addr check;

	getsetfmacro_symbol(symbol, &check);
	if (check != Unbound)
		Abort("COMMON-ILSP setf-macro-function error.");
	setsetfmacro_symbol(symbol, value);
}

void define_special_operator(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	set_special_operator(symbol);
}


/*
 *  :start, :end
 */
static int getsize_keyword_start_(addr pos, addr *reta, size_t *rets)
{
	if (pos == Unbound) {
		fixnum_heap(reta, 0);
		*rets = 0;
	}
	else {
		Return(getindex_integer_(pos, rets));
		*reta = pos;
	}

	return 0;
}

static int getsize_keyword_end_(size_t size, addr pos, addr *reta, size_t *rets)
{
	if (pos == Unbound || pos == Nil) {
		make_index_integer_heap(reta, size);
		*rets = size;
	}
	else {
		Return(getindex_integer_(pos, rets));
		*reta = pos;
	}

	return 0;
}

static int keyword_start_end_addr_(size_t size,
		addr kstart, addr kend,
		addr astart, addr aend,
		size_t *pstart, size_t *pend)
{
	size_t start, end;

	Return(getsize_keyword_start_(astart, &astart, &start));
	Return(getsize_keyword_end_(size, aend, &aend, &end));
	if (size < start) {
		return fmte_("The ~S position ~S must be less than "
				"the sequence length.", kstart, astart, NULL);
	}
	if (size < end) {
		return fmte_("The ~S position ~S must be less than "
				"equal to the sequence length.", kend, aend, NULL);
	}
	if (end < start) {
		return fmte_("The ~S position ~S must be less than "
				"equal to the ~S position ~S.", kstart, astart, kend, aend, NULL);
	}
	*pstart = start;
	*pend = end;
	return 0;
}

static int keyword_start_end_const_(constindex cstart, constindex cend,
		size_t size, addr rest, size_t *pstart, size_t *pend)
{
	addr kstart, kend, astart, aend;

	GetConstant(cstart, &kstart);
	GetConstant(cend, &kend);
	if (getplist_safe(rest, kstart, &astart))
		astart = Unbound;
	if (getplist_safe(rest, kend, &aend))
		aend = Unbound;

	return keyword_start_end_addr_(size,
			kstart, kend, astart, aend, pstart, pend);
}

int keyword_start_end_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START, CONSTANT_KEYWORD_END,
			size, rest, pstart, pend);
}

int keyword_start1_end1_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START1, CONSTANT_KEYWORD_END1,
			size, rest, pstart, pend);
}

int keyword_start2_end2_(size_t size, addr rest, size_t *pstart, size_t *pend)
{
	return keyword_start_end_const_(CONSTANT_KEYWORD_START2, CONSTANT_KEYWORD_END2,
			size, rest, pstart, pend);
}

int keyword_start_end_value_(size_t size,
		addr astart, addr aend, size_t *pstart, size_t *pend)
{
	addr kstart, kend;

	GetConst(KEYWORD_START, &kstart);
	GetConst(KEYWORD_END, &kend);
	return keyword_start_end_addr_(size,
			kstart, kend, astart, aend, pstart, pend);
}

