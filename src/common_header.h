#ifndef __COMMON_HEADER_HEADER__
#define __COMMON_HEADER_HEADER__

#include "condition.h"
#include "constant.h"
#include "control.h"
#include "function.h"
#include "pointer.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

/* helper */
#ifdef LISP_DEBUG
_g void setfunction_common(addr symbol, addr value);
_g void setmacro_common(addr symbol, addr value);
_g void setsetfmacro_common(addr symbol, addr value);
#define SetFunctionCommon	setfunction_common
#define SetMacroCommon		setmacro_common
#define SetSetfMacroCommon	setsetfmacro_common
#else
#define SetFunctionCommon	SetFunctionSymbol
#define SetMacroCommon		setmacro_symbol
#define SetSetfMacroCommon	setsetfmacro_symbol
#endif

_g void defconstant_symbol(addr symbol, addr value);
_g void define_special_operator(constindex index);
#define DefineSpecialOperator(x) define_special_operator(CONSTANT_##x)

_g void keyword_start_end(size_t size, addr rest, size_t *pstart, size_t *pend);
_g void keyword_start1_end1(size_t size, addr rest, size_t *pstart, size_t *pend);
_g void keyword_start2_end2(size_t size, addr rest, size_t *pstart, size_t *pend);

#endif

