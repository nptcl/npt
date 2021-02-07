;;
;;  Error
;;
(deftest allow-other-keys.1
  (defun allow-other-keys-1 (&key &allow-other-keys)
    nil)
  allow-other-keys-1)

(deftest allow-other-keys.2
  (allow-other-keys-1)
  nil)

(deftest allow-other-keys.3
  (allow-other-keys-1 :hello 10)
  nil)

(deftest allow-other-keys.4
  (funcall #'allow-other-keys-1 :hello 10)
  nil)

(deftest allow-other-keys.5
  (apply #'allow-other-keys-1 :hello 10 nil)
  nil)

(deftest-error allow-other-keys.6
  (eval '(allow-other-keys-1 :hello)))

(deftest-error allow-other-keys.7
  (funcall #'allow-other-keys-1 :hello))

(deftest-error allow-other-keys.8
  (apply #'allow-other-keys-1 :hello nil))

(deftest allow-other-keys-generic.1
  (progn
    (defgeneric allow-other-keys-2 (&key &allow-other-keys))
    (defmethod allow-other-keys-2 (&key &allow-other-keys))
    (values)))

(deftest allow-other-keys-generic.2
  (allow-other-keys-2)
  nil)

(deftest allow-other-keys-generic.3
  (allow-other-keys-2 :hello 10)
  nil)

(deftest-error allow-other-keys-generic.4
  (eval '(allow-other-keys-2 :hello)))

(deftest allow-other-keys-generic.5
  (funcall #'allow-other-keys-2)
  nil)

(deftest allow-other-keys-generic.6
  (funcall #'allow-other-keys-2 :hello 10)
  nil)

(deftest-error allow-other-keys-generic.7
  (eval '(funcall #'allow-other-keys-2 :hello)))

(deftest allow-other-keys-generic.8
  (apply #'allow-other-keys-2 nil)
  nil)

(deftest allow-other-keys-generic.9
  (apply #'allow-other-keys-2 :hello 10 nil)
  nil)

(deftest-error allow-other-keys-generic.10
  (eval '(apply #'allow-other-keys-2 :hello nil)))

