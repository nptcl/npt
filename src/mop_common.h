#ifndef __MOP_COMMON_HEADER__
#define __MOP_COMMON_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define init_mop_class _n(init_mop_class)
#define init_mop_generic _n(init_mop_generic)
#define init_mop_protocols _n(init_mop_protocols)
#define build_mop_class_ _n(build_mop_class_)
#define build_mop_generic_ _n(build_mop_generic_)
#define build_mop_protocols_ _n(build_mop_protocols_)
#define init_metaobject_protocol _n(init_metaobject_protocol)
#define build_metaobject_protocol _n(build_metaobject_protocol)

void init_mop_class(void);
void init_mop_generic(void);
void init_mop_protocols(void);

int build_mop_class_(Execute ptr);
int build_mop_generic_(Execute ptr);
int build_mop_protocols_(Execute ptr);

void init_metaobject_protocol(void);
void build_metaobject_protocol(void);

#endif

