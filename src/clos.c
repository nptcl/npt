#include "clos.h"
#include "clos_combination.h"
#include "clos_standard.h"
#include "clos_type.h"
#include "constant.h"
#include "hashtable.h"
#include "package.h"

void build_clos_table(Execute ptr)
{
	addr pos;

	/* clos table */
	hashtable_size_heap(&pos, CLOS_TABLE_CLASS_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	Root(LISPINDEX_CLOS) = pos;

	/* method-combination table */
	hashtable_size_heap(&pos, CLOS_TABLE_COMBINATION_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	Root(LISPINDEX_COMBINATION) = pos;

	/* eql-specializer-combination table */
	hashtable_size_heap(&pos, CLOS_TABLE_SPECIALIZER_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	Root(LISPINDEX_SPECIALIZER) = pos;
}

void build_clos(Execute ptr)
{
	build_clos_table(ptr);
	build_clos_standard(ptr);
	build_clos_type(ptr);
	build_clos_combination(ptr);
}

