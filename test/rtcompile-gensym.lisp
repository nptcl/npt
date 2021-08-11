;;
;;  compile-define
;;
(defun compile-gensym-p (x)
  (and (symbolp x)
       (null (symbol-package x))))

(deftest compile-gensym-value.1
  (let ((x (value-compile
             '#.(make-symbol "HELLO"))))
    (values
      (symbolp x)
      (null (symbol-package x))
      (symbol-name x)))
  t t "HELLO")

(deftest compile-gensym-cons.1
  (value-compile
    (macrolet ((call () (let ((g (gensym)))
                          `(cons ',g ',g))))
      (let ((x (call)))
        (and (consp x)
             (compile-gensym-p
               (car x))))))
  t)

(deftest compile-gensym-array.1
  (value-compile
    (macrolet ((call () (let ((g (gensym)))
                          `(make-array 3 :initial-contents (list ',g ',g ',g)))))
      (let ((x (call)))
        (and (arrayp x)
             (compile-gensym-p
               (aref x 2))))))
  t)

(deftest compile-gensym-vector.1
  (value-compile
    (macrolet ((call () (let ((g (gensym)))
                          `(vector ',g ',g))))
      (let ((x (call)))
        (and (vectorp x)
             (compile-gensym-p
               (aref x 1))))))
  t)

(deftest compile-gensym-hashtable.1
  (value-compile
    (let ((key (gensym))
          (value (gensym)))
      (macrolet ((call () `(let ((x (make-hash-table :test 'eq)))
                             (setf (gethash key x) value)
                             x)))
        (let ((x (call)))
          (and (hash-table-p x)
               (compile-gensym-p
                 (gethash key x)))))))
  t)

(deftest compile-gensym-callname.1
  (value-compile
    (macrolet ((call () (let ((g (gensym)))
                          `(flet ((,g () :hello))
                             (,g)))))
      (call)))
  :hello)

