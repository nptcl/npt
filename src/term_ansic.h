#include "term.h"

int begin_term(int argv)
{
	term_enable = argv;
	return 0;
}

int end_term(void)
{
	return 0;
}

