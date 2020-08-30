#include "call.h"
#include "call_conditions.h"
#include "call_printer.h"
#include "typedef.h"

_g void init_call(void)
{
	init_call_conditions();
	init_call_printer();
}

