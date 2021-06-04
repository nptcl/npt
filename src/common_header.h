#ifndef __COMMON_HEADER_HEADER__
#define __COMMON_HEADER_HEADER__

#include "common.h"
#include "condition.h"
#include "constant.h"
#include "control_operator.h"
#include "function.h"
#include "pointer.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

#define setfunction_common _n(setfunction_common)
#define setmacro_common _n(setmacro_common)
#define setsetfmacro_common _n(setsetfmacro_common)
#define defconstant_symbol _n(defconstant_symbol)
#define define_special_operator _n(define_special_operator)
#define keyword_start_end_ _n(keyword_start_end_)
#define keyword_start1_end1_ _n(keyword_start1_end1_)
#define keyword_start2_end2_ _n(keyword_start2_end2_)
#define keyword_start_end_value_ _n(keyword_start_end_value_)
#define eval_when_compile _n(eval_when_compile)

/* helper */
#ifdef LISP_DEBUG
void setfunction_common(addr symbol, addr value);
void setmacro_common(addr symbol, addr value);
void setsetfmacro_common(addr symbol, addr value);
#define SetFunctionCommon	setfunction_common
#define SetMacroCommon		setmacro_common
#define SetSetfMacroCommon	setsetfmacro_common
#else
#define SetFunctionCommon	SetFunctionSymbol
#define SetMacroCommon		setmacro_symbol
#define SetSetfMacroCommon	setsetfmacro_symbol
#endif

void defconstant_symbol(addr symbol, addr value);
void define_special_operator(constindex index);
#define DefineSpecialOperator(x) define_special_operator(CONSTANT_##x)

int keyword_start_end_(size_t size, addr rest, size_t *pstart, size_t *pend);
int keyword_start1_end1_(size_t size, addr rest, size_t *pstart, size_t *pend);
int keyword_start2_end2_(size_t size, addr rest, size_t *pstart, size_t *pend);
int keyword_start_end_value_(size_t size,
		addr astart, addr aend, size_t *pstart, size_t *pend);

void eval_when_compile(addr expr, addr *ret);

#endif

