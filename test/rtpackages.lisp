;;
;;  ANSI COMMON LISP: 11. Packages
;;
(make-package 'test1)
(make-package 'test2)
(make-package 'test3)
(import 'lisp-system:package-export-list 'common-lisp-user)

(defun init-test-package ()
  (delete-package 'test3)
  (delete-package 'test2)
  (delete-package 'test1)
  (make-package 'test1)
  (make-package 'test2)
  (make-package 'test3)
  (values))

(defun find-symbol-list (x &optional (y *package*))
  (multiple-value-bind (symbol status) (find-symbol x y)
    (when status
      (list (package-name
              (symbol-package symbol))
            (symbol-name symbol)
            status))))

(defun package-shadowing-symbols-list (package)
  (mapcar
    (lambda (x)
      (list (package-name (symbol-package x))
            (symbol-name x)))
    (package-shadowing-symbols package)))

(defun count-shadowing-symbols (name package)
  (let ((list (package-shadowing-symbols package)))
    (if (symbolp name)
      (count name list)
      (count name list :key #'symbol-name :test #'equal))))

