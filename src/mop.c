/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "mop.h"

void intern_mop_class(void);
void intern_mop_reader(void);

/*
 *  intern
 */
void intern_metaobject_protocol(void)
{
	intern_mop_class();
	intern_mop_reader();
}

