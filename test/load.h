#define loadrt_file(x) { if (loadrt_lisp(x)) return 1; }

static int loadrt_files(void)
{
#if 0
#endif
	/* 3. Evaluation and Compilation */
	loadrt_file("rt-eval.lisp");
	/* 4. Types and Classes */
	loadrt_file("rt-types.lisp");
	/* 5. Data and Control Flow */
	loadrt_file("rt-data.lisp");
	/* 6. Iteration */
	loadrt_file("rt-iteration.lisp");
	/* 7. Objects */
	loadrt_file("rt-objects.lisp");
	/* 8. Structures */
	loadrt_file("rt-structures.lisp");
	/* 9. Conditions */
	loadrt_file("rt-conditions.lisp");
	/* 10. Symbols */
	loadrt_file("rt-symbols.lisp");
	/* 11. Packages */
	loadrt_file("rt-packages.lisp");
	/* 12. Number */
	loadrt_file("rt-numbers.lisp");
	/* 13. Characters */
	loadrt_file("rt-character.lisp");
	/* 14. Conses */
	loadrt_file("rt-conses.lisp");
	/* 15. Arrays */
	loadrt_file("rt-arrays.lisp");
	/* 16. Strings */
	loadrt_file("rt-strings.lisp");
	/* 17. Sequences */
	loadrt_file("rt-sequences.lisp");
	/* 18. Hash Tables */
	loadrt_file("rt-hashtables.lisp");
	/* 19. Filenames */
	loadrt_file("rt-filenames.lisp");
	/* 20. Files */
	loadrt_file("rt-files.lisp");
	/* 21. Streams */
	loadrt_file("rt-streams.lisp");
	/* 22. Printer */
	loadrt_file("rt-printer.lisp");
	/* 23. Reader */
	loadrt_file("rt-reader.lisp");
	/* 24. System Construction */
	loadrt_file("rt-system.lisp");
	/* 25. Environment */
	loadrt_file("rt-environment.lisp");
	/* Error */
	loadrt_file("rt-error.lisp");
	/* Code */
	loadrt_file("rt-code.lisp");
	/* Compile */
	loadrt_file("rt-compile.lisp");
	/* SysCall */
	loadrt_file("rt-syscall.lisp");
	/* division */
	loadrt_file("rt-division.lisp");
	/* subtypep */
	loadrt_file("rt-subtypep.lisp");
	/* MetaObject Protocol */
	loadrt_file("rt-mop.lisp");
#if 0
#endif

	return 0;
}

