;;
;;  npt regression test
;;
(in-package common-lisp-user)

(rename-package 'npt-system 'npt-system '(lisp-system))
(rename-package 'npt-user   'npt-user   '(lisp-user  ))
(rename-package 'npt-clos   'npt-clos   '(lisp-clos  ))
(rename-package 'npt-rt     'npt-rt     '(lisp-rt    ))
(use-package 'lisp-rt)

(defun loadrt (file)
  (format t "~&[~A]~%" file)
  (unless (load (merge-pathnames file #p"test/"))
    (error "loadrt error: ~S" file)))


;;
;;  execute
;;
;; 3. Evaluation and Compilation
(loadrt #p"rt-eval.lisp");
;; 4. Types and Classes
(loadrt #p"rt-types.lisp");
;; 5. Data and Control Flow
(loadrt #p"rt-data.lisp");
;; 6. Iteration
(loadrt #p"rt-iteration.lisp");
;; 9. Conditions
(loadrt #p"rt-conditions.lisp");
;; 11. Packages
(loadrt #p"rt-packages.lisp");
;; 12. Number
(loadrt #p"rt-numbers.lisp");
;; 13. Characters
(loadrt #p"rt-character.lisp");
;; 14. Conses
(loadrt #p"rt-conses.lisp");
;; 15. Arrays
(loadrt #p"rt-arrays.lisp");
;; 16. Strings
(loadrt #p"rt-strings.lisp");
;; 17. Sequences
(loadrt #p"rt-sequences.lisp");
;; 18. Hash Tables
(loadrt #p"rt-hashtables.lisp");
;; 19. Filenames
(loadrt #p"rt-filenames.lisp");
;; 20. Files
(loadrt #p"rt-files.lisp");
;; 21. Streams
(loadrt #p"rt-streams.lisp");
;; 22. Printer
(loadrt #p"rt-printer.lisp");
;; finish
(format t "~&OK.~%")

