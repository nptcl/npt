;;
;;  ANSI COMMON LISP: 17. Sequences
;;
(deftest remove.1
  (remove 10 nil)
  nil)

(deftest remove.2
  (remove 4 '(1 3 4 5 9))
  (1 3 5 9))

(deftest remove.3
  (remove 4 '(1 2 4 1 3 4 5))
  (1 2 1 3 5))

(deftest remove.4
  (remove 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 1 3 4 5))

(deftest remove.5
  (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(deftest remove.6
  (remove 3 '(1 2 4 1 3 4 5) :test #'>)
  (4 3 4 5))

(deftest remove.7
  (remove 10 #())
  #())

(deftest remove.8
  (remove 4 #(1 3 4 5 9))
  #(1 3 5 9))

(deftest remove.9
  (remove 4 #(1 2 4 1 3 4 5))
  #(1 2 1 3 5))

(deftest remove.10
  (remove 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 1 3 4 5))

(deftest remove.11
  (remove 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5))

(deftest remove.12
  (remove 3 #(1 2 4 1 3 4 5) :test #'>)
  #(4 3 4 5))

(deftest remove-if.1
  (remove-if (lambda (x) (eql x 4)) nil)
  nil)

(deftest remove-if.2
  (remove-if (lambda (x) (eql x 4)) '(1 3 4 5 9))
  (1 3 5 9))

(deftest remove-if.3
  (remove-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5))
  (1 2 1 3 5))

(deftest remove-if.4
  (remove-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5) :count 1)
  (1 2 1 3 4 5))

(deftest remove-if.5
  (remove-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(deftest remove-if.6
  (remove-if (lambda (x) (> 3 x))  '(1 2 4 1 3 4 5))
  (4 3 4 5))

(deftest remove-if.7
  (remove-if (lambda (x) (eql x 4)) #())
  #())

(deftest remove-if.8
  (remove-if (lambda (x) (eql x 4)) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest remove-if.9
  (remove-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5))
  #(1 2 1 3 5))

(deftest remove-if.10
  (remove-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5) :count 1)
  #(1 2 1 3 4 5))

(deftest remove-if.11
  (remove-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5))

(deftest remove-if.12
  (remove-if (lambda (x) (> 3 x))  #(1 2 4 1 3 4 5))
  #(4 3 4 5))

(deftest remove-if-not.1
  (remove-if-not #'evenp '(1 2 3 4 5 3 6))
  (2 4 6))

(deftest remove-if-not.2
  (remove-if-not #'evenp #(1 2 3 4 5 3 6))
  #(2 4 6))

(deftest remove-start.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest remove-start.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest remove-start.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest remove-start.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start.6
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest remove-start.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest remove-start.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest remove-start.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest remove-start.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start.11
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start.12
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest remove-start.1e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-start.2e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest remove-start.3e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest remove-start.4e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start.5e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start.6e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-start.7e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-start.8e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest remove-start.9e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest remove-start.10e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest remove-start.11e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-start.12e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest remove-end.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest remove-end.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest remove-end.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end.6
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest remove-end.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest remove-end.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest remove-end.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.11
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end.12
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest remove-end.1e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-end.2e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest remove-end.3e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.4e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.5e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end.6e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-end.7e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-end.8e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest remove-end.9e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.10e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-end.11e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error remove-end.12e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest remove-start-end.1
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest remove-start-end.2
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-start-end.3
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end.4
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end.5
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest remove-start-end.6
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest remove-start-end.7
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-start-end.8
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end.9
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end.10
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest remove-start-end.1e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest remove-start-end.2e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest remove-start-end.3e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end.4e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end.5e
  (remove 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest remove-start-end.6e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest remove-start-end.7e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest remove-start-end.8e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest remove-start-end.9e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error remove-start-end.10e
  (remove 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest delete.1
  (delete 10 nil)
  nil)

(deftest delete.2
  (delete 4 '(1 3 4 5 9))
  (1 3 5 9))

(deftest delete.3
  (delete 4 '(1 2 4 1 3 4 5))
  (1 2 1 3 5))

(deftest delete.4
  (delete 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 1 3 4 5))

(deftest delete.5
  (delete 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(deftest delete.6
  (delete 3 '(1 2 4 1 3 4 5) :test #'>)
  (4 3 4 5))

(deftest delete.7
  (delete 10 #())
  #())

(deftest delete.8
  (delete 4 #(1 3 4 5 9))
  #(1 3 5 9))

(deftest delete.9
  (delete 4 #(1 2 4 1 3 4 5))
  #(1 2 1 3 5))

(deftest delete.10
  (delete 4 #(1 2 4 1 3 4 5) :count 1)
  #(1 2 1 3 4 5))

(deftest delete.11
  (delete 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5))

(deftest delete.12
  (delete 3 #(1 2 4 1 3 4 5) :test #'>)
  #(4 3 4 5))

(deftest delete-if.1
  (delete-if (lambda (x) (eql x 4)) nil)
  nil)

(deftest delete-if.2
  (delete-if (lambda (x) (eql x 4)) '(1 3 4 5 9))
  (1 3 5 9))

(deftest delete-if.3
  (delete-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5))
  (1 2 1 3 5))

(deftest delete-if.4
  (delete-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5) :count 1)
  (1 2 1 3 4 5))

(deftest delete-if.5
  (delete-if (lambda (x) (eql x 4)) '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))

(deftest delete-if.6
  (delete-if (lambda (x) (> 3 x))  '(1 2 4 1 3 4 5))
  (4 3 4 5))

(deftest delete-if.7
  (delete-if (lambda (x) (eql x 4)) #())
  #())

(deftest delete-if.8
  (delete-if (lambda (x) (eql x 4)) #(1 3 4 5 9))
  #(1 3 5 9))

(deftest delete-if.9
  (delete-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5))
  #(1 2 1 3 5))

(deftest delete-if.10
  (delete-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5) :count 1)
  #(1 2 1 3 4 5))

(deftest delete-if.11
  (delete-if (lambda (x) (eql x 4)) #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5))

(deftest delete-if.12
  (delete-if (lambda (x) (> 3 x))  #(1 2 4 1 3 4 5))
  #(4 3 4 5))

(deftest delete-if-not.1
  (delete-if-not #'evenp '(1 2 3 4 5 3 6))
  (2 4 6))

(deftest delete-if-not.2
  (delete-if-not #'evenp #(1 2 3 4 5 3 6))
  #(2 4 6))

(deftest delete-start.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  (4 5 9 8 8 4))

(deftest delete-start.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  (3 4 5 9 8 8 4))

(deftest delete-start.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  (3 3 4 5 9 8 8 4))

(deftest delete-start.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start.6
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest delete-start.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0)
  #(4 5 9 8 8 4))

(deftest delete-start.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1)
  #(3 4 5 9 8 8 4))

(deftest delete-start.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3)
  #(3 3 4 5 9 8 8 4))

(deftest delete-start.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start.11
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start.12
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14))

(deftest delete-start.1e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-start.2e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  (3 4 5 9 8 8 4))

(deftest delete-start.3e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  (3 3 4 5 9 8 8 4))

(deftest delete-start.4e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start.5e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start.6e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-start.7e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-start.8e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :from-end t)
  #(3 4 5 9 8 8 4))

(deftest delete-start.9e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 3 :from-end t)
  #(3 3 4 5 9 8 8 4))

(deftest delete-start.10e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 12 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3))

(deftest delete-start.11e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 13 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-start.12e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 14 :from-end t))

(deftest delete-end.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  (4 5 9 8 8 4))

(deftest delete-end.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  (4 5 9 8 8 4 3))

(deftest delete-end.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end.6
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest delete-end.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13)
  #(4 5 9 8 8 4))

(deftest delete-end.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12)
  #(4 5 9 8 8 4 3))

(deftest delete-end.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.11
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end.12
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14))

(deftest delete-end.1e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-end.2e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  (4 5 9 8 8 4 3))

(deftest delete-end.3e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  (4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.4e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  (3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.5e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end.6e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-end.7e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-end.8e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 12 :from-end t)
  #(4 5 9 8 8 4 3))

(deftest delete-end.9e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 6 :from-end t)
  #(4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.10e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 1 :from-end t)
  #(3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-end.11e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 0 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest-error delete-end.12e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :end 14 :from-end t))

(deftest delete-start-end.1
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  (4 5 9 8 8 4))

(deftest delete-start-end.2
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-start-end.3
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end.4
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end.5
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest delete-start-end.6
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13)
  #(4 5 9 8 8 4))

(deftest delete-start-end.7
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-start-end.8
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end.9
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end.10
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4))

(deftest delete-start-end.1e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  (4 5 9 8 8 4))

(deftest delete-start-end.2e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  (3 4 5 9 8 8 4 3 3))

(deftest delete-start-end.3e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  (3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end.4e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  (3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end.5e
  (delete 3 '(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

(deftest delete-start-end.6e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 0 :end 13 :from-end t)
  #(4 5 9 8 8 4))

(deftest delete-start-end.7e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 11 :from-end t)
  #(3 4 5 9 8 8 4 3 3))

(deftest delete-start-end.8e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 1 :end 1 :from-end t)
  #(3 3 4 5 9 8 3 8 4 3 3 3 3))

(deftest delete-start-end.9e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 4 :end 10 :from-end t)
  #(3 3 4 5 9 8 8 4 3 3 3))

(deftest-error delete-start-end.10e
  (delete 3 #(3 3 4 5 9 8 3 8 4 3 3 3 3) :start 5 :end 4 :from-end t))

