/*
 *  ANSI COMMON LISP Function
 */
#include "common_header.h"

void intern_common_eval(void);
void intern_common_types(void);
void intern_common_data(void);
void intern_common_iteration(void);
void intern_common_objects(void);
void intern_common_conditions(void);
void intern_common_symbols(void);
void intern_common_packages(void);
void intern_common_numbers(void);
void intern_common_characters(void);
void intern_common_conses(void);
void intern_common_arrays(void);
void intern_common_strings(void);
void intern_common_sequences(void);
void intern_common_hashtables(void);
void intern_common_filenames(void);
void intern_common_files(void);
void intern_common_streams(void);
void intern_common_printer(void);
void intern_common_reader(void);
void intern_common_system(void);
void intern_common_environment(void);

void build_common(void)
{
	/* 3. Evaluation and Compilation */
	intern_common_eval();
	/* 4. Types and Classes */
	intern_common_types();
	/* 5. Data and Control Flow */
	intern_common_data();
	/* 6. Iteration */
	intern_common_iteration();
	/* 7. Objects */
	intern_common_objects();
	/* 9. Conditions */
	intern_common_conditions();
	/* 10. Symbols */
	intern_common_symbols();
	/* 11. Packages */
	intern_common_packages();
	/* 12. Numbers */
	intern_common_numbers();
	/* 13. Characters */
	intern_common_characters();
	/* 14. Conses */
	intern_common_conses();
	/* 15. Arrays */
	intern_common_arrays();
	/* 16. Strings */
	intern_common_strings();
	/* 17. Sequences */
	intern_common_sequences();
	/* 18. Hash Tables */
	intern_common_hashtables();
	/* 19. Filenames */
	intern_common_filenames();
	/* 20. Files */
	intern_common_files();
	/* 21. Streams */
	intern_common_streams();
	/* 22. Printer */
	intern_common_printer();
	/* 23. Reader */
	intern_common_reader();
	/* 24. System Construction */
	intern_common_system();
	/* 25. Environment */
	intern_common_environment();
	/* After settings */
	intern_common_after_settings();
}

