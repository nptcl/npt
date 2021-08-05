#ifndef __MOP_GENERIC_HEADER__
#define __MOP_GENERIC_HEADER__

#include "execute.h"
#include "typedef.h"

#define defgeneric_no_applicable_method_mop_ _n(defgeneric_no_applicable_method_mop_)
#define defgeneric_no_next_method_mop_ _n(defgeneric_no_next_method_mop_)

int defgeneric_no_applicable_method_mop_(Execute ptr);
int defgeneric_no_next_method_mop_(Execute ptr);

#endif

