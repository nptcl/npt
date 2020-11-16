#ifndef __PACKAGE_BITTYPE_HEADER__
#define __PACKAGE_BITTYPE_HEADER__

#include "package_object.h"
#include "typedef.h"

#define make_bitpackage_symbol _n(make_bitpackage_symbol)
#define internbitpackage _n(internbitpackage)
#define importbitpackage _n(importbitpackage)
#define inheritedbitpackage _n(inheritedbitpackage)
#define shadowintern_bitpackage _n(shadowintern_bitpackage)
#define shadowimport_bitpackage _n(shadowimport_bitpackage)
#define intern_bitpackage_ _n(intern_bitpackage_)
#define find_bitpackage_ _n(find_bitpackage_)
#define find_char_bitpackage_ _n(find_char_bitpackage_)

struct bittype_struct {
	unsigned base : 1;
	unsigned expt : 1;
	unsigned import : 1;
	unsigned inherit : 1;
	unsigned shadow : 1;
	enum PACKAGE_TYPE intern;
};

#define PtrBitTypeBody(x) PtrBodySSa(x, 1)
#define StructBitType(x) ((struct bittype_struct *)PtrBitTypeBody(x))
#define GetBitTypeSymbol(x,y) GetArraySS((x), 0, (y))
#define SetBitTypeSymbol(x,y) SetArraySS((x), 0, (y))

#define GetBitTypeIntern(x,y) (*(y) = StructBitType(x)->intern)
#define GetBitTypeBase(x,y) (*(y) = StructBitType(x)->base)
#define GetBitTypeExport(x,y) (*(y) = StructBitType(x)->expt)
#define GetBitTypeImport(x,y) (*(y) = StructBitType(x)->import)
#define GetBitTypeInherit(x,y) (*(y) = StructBitType(x)->inherit)
#define GetBitTypeShadow(x,y) (*(y) = StructBitType(x)->shadow)

#define SetBitTypeIntern(x,y) (StructBitType(x)->intern = (y))
#define SetBitTypeBase(x,y) (StructBitType(x)->base = (y))
#define SetBitTypeExport(x,y) (StructBitType(x)->expt = (y))
#define SetBitTypeImport(x,y) (StructBitType(x)->import = (y))
#define SetBitTypeInherit(x,y) (StructBitType(x)->inherit = (y))
#define SetBitTypeShadow(x,y) (StructBitType(x)->shadow = (y))

void make_bitpackage_symbol(addr *ret, addr *symbol, addr name, addr package);
void internbitpackage(addr *ret, addr symbol);
void importbitpackage(addr *ret, addr symbol);
void inheritedbitpackage(addr *ret, addr symbol);
void shadowintern_bitpackage(addr bit, addr name, addr package);
void shadowimport_bitpackage(addr bit, addr symbol);
int intern_bitpackage_(addr package, addr name, addr *value, int *ret);
int find_bitpackage_(addr package, addr name, addr *ret);
int find_char_bitpackage_(addr package, const char *name, addr *ret);

#endif

