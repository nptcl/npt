;;
;;  MetaObject Protocol: Class
;;
(import 'lisp-system::closp)
(import 'lisp-system:sysctl)
(import 'lisp-clos::referenced-class)
(import 'lisp-clos:find-method-combination)
(import 'lisp-clos::method-combination-instance)
(use-package 'lisp-clos)


;;
;;  closget
;;
(deftest clos-variable.1
  (let ((name 'lisp-clos::standard-direct-slot-definition))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.2
  (let ((name 'lisp-clos::standard-effective-slot-definition))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.3
  (let ((name 'standard-class))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.4
  (let ((name 'standard-generic-function))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.5
  (let ((name 'standard-method))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.6
  (let ((name 'lisp-clos::long-method-combination))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.7
  (let ((name 'lisp-clos::short-method-combination))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.8
  (let ((name 'lisp-clos::define-long-method-combination))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.9
  (let ((name 'lisp-clos::define-short-method-combination))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.10
  (let ((name 'lisp-clos::eql-specializer))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

(deftest clos-variable.11
  (let ((name 'structure-class))
    (eq (find-class name)
        (sysctl 'clos 'variable name)))
  t)

