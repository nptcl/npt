;;
;;  Regression test
;;
(in-package #:common-lisp-user)

(defun rt-package (symbol &optional (type (lisp-implementation-type)))
  (let ((name (format nil "~A-~A" type symbol))
        (lisp (format nil "~A-~A" 'lisp symbol)))
    (rename-package name name (list lisp))))

(rt-package 'system)
(rt-package 'user)
(rt-package 'clos)
(rt-package 'rt)
(use-package 'lisp-rt)

(defun loadrt (file)
  (format t "~&[~A]~%" file)
  (unless (load (merge-pathnames file #p"test/"))
    (error "loadrt error: ~S" file)))


;;
;;  execute
;;
;; 3. Evaluation and Compilation
(loadrt #p"rt-eval.lisp")
;; 4. Types and Classes
(loadrt #p"rt-types.lisp")
;; 5. Data and Control Flow
(loadrt #p"rt-data.lisp")
;; 6. Iteration
(loadrt #p"rt-iteration.lisp")
;; 7. Objects
(loadrt #p"rt-objects.lisp")
;; 8. Structures
(loadrt #p"rt-structures.lisp")
;; 9. Conditions
(loadrt #p"rt-conditions.lisp")
;; 10. Symbols
(loadrt #p"rt-symbols.lisp")
;; 11. Packages
(loadrt #p"rt-packages.lisp")
;; 12. Number
(loadrt #p"rt-numbers.lisp")
;; 13. Characters
(loadrt #p"rt-character.lisp")
;; 14. Conses
(loadrt #p"rt-conses.lisp")
;; 15. Arrays
(loadrt #p"rt-arrays.lisp")
;; 16. Strings
(loadrt #p"rt-strings.lisp")
;; 17. Sequences
(loadrt #p"rt-sequences.lisp")
;; 18. Hash Tables
(loadrt #p"rt-hashtables.lisp")
;; 19. Filenames
(loadrt #p"rt-filenames.lisp")
;; 20. Files
(loadrt #p"rt-files.lisp")
;; 21. Streams
(loadrt #p"rt-streams.lisp")
;; 22. Printer
(loadrt #p"rt-printer.lisp")
;; 23. Reader
(loadrt #p"rt-reader.lisp")
;; 24. System Construction
(loadrt #p"rt-system.lisp")
;; Error
(loadrt #p"rt-error.lisp")
;; Code
(loadrt #p"rt-code.lisp");
;; finish
(format t "~&OK.~%")

