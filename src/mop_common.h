#ifndef __MOP_COMMON_HEADER__
#define __MOP_COMMON_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define init_mop_reader _n(init_mop_reader)
#define init_mop_class _n(init_mop_class)
#define init_mop_generic _n(init_mop_generic)
#define init_mop_protocols _n(init_mop_protocols)
#define build_mop_class_ _n(build_mop_class_)
#define build_mop_reader_ _n(build_mop_reader_)
#define build_mop_generic_ _n(build_mop_generic_)
#define build_mop_protocols_ _n(build_mop_protocols_)
#define init_metaobject_protocol _n(init_metaobject_protocol)
#define build_metaobject_protocol _n(build_metaobject_protocol)

_g void init_mop_reader(void);
_g void init_mop_class(void);
_g void init_mop_generic(void);
_g void init_mop_protocols(void);

_g int build_mop_class_(Execute ptr);
_g int build_mop_reader_(Execute ptr);
_g int build_mop_generic_(Execute ptr);
_g int build_mop_protocols_(Execute ptr);

_g void init_metaobject_protocol(void);
_g void build_metaobject_protocol(void);

#endif

