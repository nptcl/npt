;;
;;  Regression test
;;
(in-package #:common-lisp-user)

(defun rt-package-readonly (&rest args)
  (let* ((type (lisp-implementation-type))
         (name (format nil "~A-SYSTEM" type))
         (call (intern "SYSCTL" name)))
    (apply call 'package 'readonly args)))

(defun rt-rename-package (p x y)
  (let ((value (rt-package-readonly p)))
    (rt-package-readonly p nil)
    (rename-package p x y)
    (rt-package-readonly p value)))

(defun rt-package (symbol)
  (let* ((type (lisp-implementation-type))
         (name (format nil "~A-~A" type symbol))
         (lisp (format nil "~A-~A" 'lisp symbol)))
    (rt-rename-package name name (list lisp))))

(rt-package 'system)
(rt-package 'clos)
(rt-package 'rt)
(use-package 'lisp-rt)

(defun loadrt (file)
  (format t "~&[~A]~%" file)
  (let ((lisp-rt::*result* nil))
    (load (merge-pathnames file #p"test/"))
    (unless lisp-rt::*result*
      (error "loadrt error: ~S" file))))


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
;; 25. Environment
(loadrt #p"rt-environment.lisp")
;; Error
(loadrt #p"rt-error.lisp")
;; Code
(loadrt #p"rt-code.lisp")
;; Compile
(loadrt #p"rt-compile.lisp")
;; SysCall
(loadrt #p"rt-syscall.lisp")
;; division
(loadrt #p"rt-division.lisp")
;; subtypep
(loadrt #p"rt-subtypep.lisp")
;; MetaObject Protocol
(loadrt #p"rt-mop.lisp")
;; finish
(format t "~&OK.~%")

