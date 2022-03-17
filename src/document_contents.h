#ifndef __DOCUMENT_CONTENTS_HEADER__
#define __DOCUMENT_CONTENTS_HEADER__

#include <stdio.h>
#include "typedef.h"

struct DocumentStruct {
	const char *key, *value;
};

struct DocumentPackage {
	const char *package;
	struct DocumentStruct *list;
	size_t size;
};

#define Document_FUNCTION _n(Document_FUNCTION)
#define Document_VARIABLE _n(Document_VARIABLE)
#define Document_TYPE _n(Document_TYPE)

extern struct DocumentPackage Document_FUNCTION[];
extern struct DocumentPackage Document_VARIABLE[];
extern struct DocumentPackage Document_TYPE[];

#endif

