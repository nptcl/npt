#include "character_queue.h"
#include "constant.h"
#include "define.h"
#include "term.h"
#include "symbol.h"

int term_enable;

void build_term(void)
{
	addr symbol, queue;

	GetConst(SYSTEM_TERM_QUEUE, &symbol);
	charqueue_heap(&queue, 0);
	SetValueSymbol(symbol, queue);
}

