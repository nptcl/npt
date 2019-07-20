;;
;;  ANSI COMMON LISP: 6. Iteration
;;

;;
;;  condition
;;
(deftest loop-condition-if.1
  (loop for i from 1 to 10
        if (evenp i)
        collect i)
  (2 4 6 8 10))

(deftest loop-condition-if.2
  (loop with sum = 0
        for i from 1 to 10
        if (evenp i)
        collect i into x
        else
        do (incf sum i)
        finally (return (values x sum)))
  (2 4 6 8 10) 25)

(deftest loop-condition-when.1
  (loop for i from 1 to 10
        when (evenp i)
        collect i)
  (2 4 6 8 10))

(deftest loop-condition-when.2
  (loop with sum = 0
        for i from 1 to 10
        when (evenp i)
        collect i into x
        else
        do (incf sum i)
        finally (return (values x sum)))
  (2 4 6 8 10) 25)

(deftest loop-condition-unless.1
  (loop for i from 1 to 10
        unless (evenp i)
        collect i)
  (1 3 5 7 9))

(deftest loop-condition-unless.2
  (loop with sum = 0
        for i from 1 to 10
        unless (evenp i)
        collect i into x
        else
        do (incf sum i)
        finally (return (values x sum)))
  (1 3 5 7 9) 30)

(deftest loop-condition-end.1
  (loop with sum = 0
        for i from 1 to 10
        if (evenp i)
        collect i into x
        else
        do (incf sum i)
        end
        finally (return (values x sum)))
  (2 4 6 8 10) 25)

(deftest loop-condition-and.1
  (loop with sum = 0
        for i from 1 to 10
        if (evenp i)
        collect i into x
        and collect i into y
        else
        do (incf sum i)
        and collect i into z
        end
        finally (return (values x y z sum)))
  (2 4 6 8 10)
  (2 4 6 8 10)
  (1 3 5 7 9)
  25)


;;
;;  uncondition
;;
(deftest loop-uncondition-do.1
  (loop do (return 10))
  10)

(deftest loop-uncondition-do.2
  (loop doing (return 10))
  10)

(deftest loop-uncondition-return.1
  (loop return 10)
  10)

(deftest loop-uncondition-return.2
  (loop named hello return 10)
  10)

(deftest loop-uncondition-return.3
  (loop if 100 return it)
  100)


;;
;;  accumulation
;;

;;  collect
(deftest loop-collect.1
  (loop for i from 1 to 5
        collect i)
  (1 2 3 4 5))

(deftest loop-collect.2
  (loop for i from 1 to 5
        collect i into x
        finally (return x))
  (1 2 3 4 5))

(deftest loop-collect.3
  (loop for i from 1 to 10
        if (if (evenp i) i)
        collect it)
  (2 4 6 8 10))

(deftest loop-collect.4
  (loop for i from 1 to 5
        collecting i)
  (1 2 3 4 5))

(deftest loop-collect.5
  (loop for i from 1 to 5
        collecting i into x
        finally (return x))
  (1 2 3 4 5))

(deftest loop-collect.6
  (loop for i from 1 to 10
        if (if (evenp i) i)
        collecting it)
  (2 4 6 8 10))


;;  append
(deftest loop-append.1
  (loop for i from 1 to 5
        append (list i (* 100 i)))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-append.2
  (loop for i from 1 to 5
        append (list i (* 100 i)) into x
        finally (return x))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-append.3
  (loop for i from 1 to 10
        if (if (evenp i) (list i (* 100 i)))
        append it)
  (2 200 4 400 6 600 8 800 10 1000))

(deftest loop-append.4
  (loop for i from 1 to 5
        appending (list i (* 100 i)))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-append.5
  (loop for i from 1 to 5
        appending (list i (* 100 i)) into x
        finally (return x))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-append.6
  (loop for i from 1 to 10
        if (if (evenp i) (list i (* 100 i)))
        appending it)
  (2 200 4 400 6 600 8 800 10 1000))


;;  nconc
(deftest loop-nconc.1
  (loop for i from 1 to 5
        nconc (list i (* 100 i)))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-nconc.2
  (loop for i from 1 to 5
        nconc (list i (* 100 i)) into x
        finally (return x))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-nconc.3
  (loop for i from 1 to 10
        if (if (evenp i) (list i (* 100 i)))
        nconc it)
  (2 200 4 400 6 600 8 800 10 1000))

(deftest loop-nconc.4
  (loop for i from 1 to 5
        nconcing (list i (* 100 i)))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-nconc.5
  (loop for i from 1 to 5
        nconcing (list i (* 100 i)) into x
        finally (return x))
  (1 100 2 200 3 300 4 400 5 500))

(deftest loop-nconc.6
  (loop for i from 1 to 10
        if (if (evenp i) (list i (* 100 i)))
        nconcing it)
  (2 200 4 400 6 600 8 800 10 1000))


;;  count
(deftest loop-count.1
  (loop for i from 0 to 10
        count (evenp i))
  6)

(deftest loop-count.2
  (loop for i from 0 to 10
        count (evenp i) into x
        finally (return x))
  6)

(deftest loop-count.3
  (loop for i from 0 to 10
        count (evenp i) into x integer
        finally (return x))
  6)

(deftest loop-count.4
  (loop for i from 0 to 10
        if (if (evenp i) i)
        count it)
  6)

(deftest loop-count.5
  (loop for i from 0 to 10
        counting (evenp i))
  6)

(deftest loop-count.6
  (loop for i from 0 to 10
        counting (evenp i) into x
        finally (return x))
  6)

(deftest loop-count.7
  (loop for i from 0 to 10
        counting (evenp i) into x integer
        finally (return x))
  6)

(deftest loop-count.8
  (loop for i from 0 to 10
        if (if (evenp i) i)
        count it)
  6)


;;  sum
(deftest loop-sum.1
  (loop for i from 0 to 10
        sum i)
  55)

(deftest loop-sum.2
  (loop for i from 0 to 10
        sum i into x
        finally (return x))
  55)

(deftest loop-sum.3
  (loop for i from 0 to 10
        sum i into x integer
        finally (return x))
  55)

(deftest loop-sum.4
  (loop for i from 0 to 10
        if (if (evenp i) i)
        sum it)
  30)

(deftest loop-sum.5
  (loop for i from 0 to 10
        summing i)
  55)

(deftest loop-sum.6
  (loop for i from 0 to 10
        summing i into x
        finally (return x))
  55)

(deftest loop-sum.7
  (loop for i from 0 to 10
        summing i into x integer
        finally (return x))
  55)

(deftest loop-sum.8
  (loop for i from 0 to 10
        if (if (evenp i) i)
        summing it)
  30)


;;
;;  maximize
;;
(deftest loop-maximize.1
  (loop for i in '(4 5 2 3 9 8 7)
        maximize i)
  9)

(deftest loop-maximize.2
  (loop for i in '(4 5 2 3 9 8 7)
        maximize i into x
        finally (return x))
  9)

(deftest loop-maximize.3
  (loop for i in '(4 5 2 3 9 8 7)
        maximize i into x integer
        finally (return x))
  9)

(deftest loop-maximize.4
  (loop for i in '(4 5 2 3 9 8 7)
        if (if (evenp i) i)
        maximize it)
  8)

(deftest loop-maximize.5
  (loop for i in '(4 5 2 3 9 8 7)
        maximizing i)
  9)

(deftest loop-maximize.6
  (loop for i in '(4 5 2 3 9 8 7)
        maximizing i into x
        finally (return x))
  9)

(deftest loop-maximize.7
  (loop for i in '(4 5 2 3 9 8 7)
        maximizing i into x integer
        finally (return x))
  9)

(deftest loop-maximize.8
  (loop for i in '(4 5 2 3 9 8 7)
        if (if (evenp i) i)
        maximizing it)
  8)


;;
;;  minimize
;;
(deftest loop-minimize.1
  (loop for i in '(4 5 2 3 9 8 7)
        minimize i)
  2)

(deftest loop-minimize.2
  (loop for i in '(4 5 2 3 9 8 7)
        minimize i into x
        finally (return x))
  2)

(deftest loop-minimize.3
  (loop for i in '(4 5 2 3 9 8 7)
        minimize i into x integer
        finally (return x))
  2)

(deftest loop-minimize.4
  (loop for i in '(4 5 2 3 9 8 7)
        if (if (evenp i) i)
        minimize it)
  2)

(deftest loop-minimize.5
  (loop for i in '(4 5 2 3 9 8 7)
        minimizing i)
  2)

(deftest loop-minimize.6
  (loop for i in '(4 5 2 3 9 8 7)
        minimizing i into x
        finally (return x))
  2)

(deftest loop-minimize.7
  (loop for i in '(4 5 2 3 9 8 7)
        minimizing i into x integer
        finally (return x))
  2)

(deftest loop-minimize.8
  (loop for i in '(4 5 2 3 9 8 7)
        if (if (evenp i) i)
        minimizing it)
  2)


;;
;;  termination-test
;;

;;  while
(deftest loop-while.1
  (loop with i = 0
        while (< i 5)
        collect i
        do (incf i))
  (0 1 2 3 4))

;;  until
(deftest loop-until.1
  (loop with i = 0
        until (>= i 5)
        collect i
        do (incf i))
  (0 1 2 3 4))

;; always
(deftest loop-always.1
  (loop for i from 0 to 5
        always i)
  t)

(deftest loop-always.2
  (loop for i in '(10 20 nil 30 40)
        always i
        finally (return 100))
  nil)

;; never
(deftest loop-never.1
  (loop for i in '(nil nil nil nil nil)
        never i)
  t)

(deftest loop-never.2
  (loop for i in '(nil nil 10 nil nil)
        never i
        finally (return 100))
  nil)

;; thereis
(deftest loop-thereis.1
  (loop for i in '(nil nil nil nil)
        thereis i)
  nil)

(deftest loop-thereis.2
  (loop for i in '(nil nil 10 20)
        thereis i)
  10)

;;  repeat
(deftest loop-repeat.1
  (loop repeat 5
        collect 'a)
  (a a a a a))

(deftest loop-repeat.2
  (loop repeat 0
        collect 'a)
  nil)

(deftest loop-repeat.3
  (loop repeat -100
        collect 'a)
  nil)

