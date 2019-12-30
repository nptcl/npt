;;
;;  ANSI COMMON LISP: 22. Printer
;;

;;
;;  GoTo
;;
(deftest format-goto.1
  (format nil "~A~*~A" 10 20 30 40 50 60 70)
  "1030")

(deftest format-goto.2
  (format nil "~A~3*~A" 10 20 30 40 50 60 70)
  "1050")

(deftest format-goto.3
  (format nil "~A~0*~A" 10 20 30 40 50 60 70)
  "1020")

(deftest format-goto.4
  (format nil "~A~A~A~:*~A" 10 20 30 40 50 60 70)
  "10203030")

(deftest format-goto.5
  (format nil "~A~A~A~2:*~A~A" 10 20 30 40 50 60 70)
  "1020302030")

(deftest format-goto.6
  (format nil "~A~A~A~2:*~A~A" 10 20 30 40 50 60 70)
  "1020302030")

(deftest format-goto.7
  (format nil "~A~A~A~0:*~A~A" 10 20 30 40 50 60 70)
  "1020304050")

(deftest format-goto.8
  (format nil "~A~A~A~0@*~A~A" 10 20 30 40 50 60 70)
  "1020301020")

(deftest format-goto.9
  (format nil "~A~A~A~5@*~A~A" 10 20 30 40 50 60 70)
  "1020306070")

(deftest-error format-goto.10
  (format nil "~-1*" 10 20 30 40 50 60 70))

(deftest-error format-goto.11
  (format nil "~-1:*" 10 20 30 40 50 60 70))

(deftest-error format-goto.12
  (format nil "~-1@*" 10 20 30 40 50 60 70))

(deftest-error format-goto.13
  (format nil "~,1*" 10 20 30 40 50 60 70))

(deftest-error format-goto.14
  (format nil "~,1:*" 10 20 30 40 50 60 70))

(deftest-error format-goto.15
  (format nil "~,1:@*" 10 20 30 40 50 60 70))

(deftest-error format-goto.16
  (format nil "~100*" 10 20 30 40 50 60 70))

(deftest-error format-goto.17
  (format nil "~100:*" 10 20 30 40 50 60 70))

(deftest-error format-goto.18
  (format nil "~100@*" 10 20 30 40 50 60 70))

(deftest-error format-goto.19
  (format nil "~:@*" 10 20 30 40 50 60 70))


;;
;;  Recursive
;;
(deftest format-recursive.1
  (format nil "~A ~? ~A" 10 "<~S-~D>" '(#\A 20 30) 40 50)
  "10 <#\\A-20> 40")

(deftest format-recursive.2
  (format nil "~A ~? ~A" 10 (formatter "<~S-~D>") '(#\A 20 30) 40 50)
  "10 <#\\A-20> 40")

(deftest format-recursive.3
  (format nil "~A ~@? ~A" 10 "<~S-~D>" #\A 20 30 40 50)
  "10 <#\\A-20> 30")

(deftest format-recursive.4
  (format nil "~A ~@? ~A" 10 (formatter "<~S-~D>") #\A 20 30 40 50)
  "10 <#\\A-20> 30")

(deftest-error format-recursive.5
  (format nil "~A ~:? ~A" 10 "<~S-~D>" '(#\A 20 30) 40 50))

(deftest-error format-recursive.6
  (format nil "~A ~10? ~A" 10 "<~S-~D>" '(#\A 20 30) 40 50))

(deftest format-recursive.7
  (format nil "~A ~? ~A" 10 "<~S-~0^~D>" '(#\A 20 30) 40 50)
  "10 <#\\A- 40")

(deftest format-recursive.8
  (format nil "~A ~? ~A" 10 (formatter "<~S-~0^~D>") '(#\A 20 30) 40 50)
  "10 <#\\A- 40")

(deftest format-recursive.9
  (format nil "~A ~@? ~A" 10 "<~S-~0^~D>" #\A 20 30 40 50)
  "10 <#\\A- 20")

(deftest format-recursive.10
  (format nil "~A ~@? ~A" 10 (formatter "<~S-~0^~D>") #\A 20 30 40 50)
  "10 <#\\A- 20")


;;
;;  Case
;;
(deftest format-case.1
  (format nil "~(Hello100-ABC~)")
  "hello100-abc")

(deftest format-case.2
  (format nil "~(heLLo100-abc~)")
  "hello100-abc")

(deftest format-case.3
  (format nil "zzz~(heLLo100-abc~)yyy")
  "zzzhello100-abcyyy")

(deftest format-case.4
  (format nil "~:(Hello100-ABC~)")
  "Hello100-Abc")

(deftest format-case.5
  (format nil "~:(heLLo100-abc~)")
  "Hello100-Abc")

(deftest format-case.6
  (format nil "zzz~:(heLLo100-abc~)yyy")
  "zzzHello100-Abcyyy")

(deftest format-case.7
  (format nil "~@(Hello100-ABC~)")
  "Hello100-abc")

(deftest format-case.8
  (format nil "~@(heLLo100-abc~)")
  "Hello100-abc")

(deftest format-case.9
  (format nil "zzz~@(heLLo100-abc~)yyy")
  "zzzHello100-abcyyy")

(deftest format-case.10
  (format nil "~:@(Hello100-ABC~)")
  "HELLO100-ABC")

(deftest format-case.11
  (format nil "~:@(heLLo100-abc~)")
  "HELLO100-ABC")

(deftest format-case.12
  (format nil "zzz~:@(heLLo100-abc~)yyy")
  "zzzHELLO100-ABCyyy")

(deftest format-case.13
  (format nil "~:(~R~)" 1000)
  "One Thousand")

(deftest format-case.14
  (format nil "~@(~R~)" 1000)
  "One thousand")

(deftest format-case.15
  (format nil "~(ABC~^DEF~)")
  "abc")

(deftest format-case.16
  (format nil "~(AB~(C~^D~)EF~)")
  "abc")


;;
;;  Condition
;;
(deftest format-condition-select.1
  (format nil "~[10~;20~;30~]" 0)
  "10")

(deftest format-condition-select.2
  (format nil "~[10~;20~;30~]" 1)
  "20")

(deftest format-condition-select.3
  (format nil "~[10~;20~;30~]" 2)
  "30")

(deftest format-condition-select.4
  (format nil "~[10~;20~;30~]" 4)
  "")

(deftest format-condition-select.5
  (format nil "~0[10~;20~;30~]" 1)
  "10")

(deftest format-condition-select.6
  (format nil "~1[10~;20~;30~]" 1)
  "20")

(deftest format-condition-select.7
  (format nil "~2[10~;20~;30~]" 1)
  "30")

(deftest format-condition-select.8
  (format nil "~3[10~;20~;30~]" 1)
  "")

(deftest format-condition-select.9
  (format nil "~[10~;20~;30~:;40~]" 1)
  "20")

(deftest format-condition-select.10
  (format nil "~[10~;20~;30~:;40~]" 10)
  "40")

(deftest format-condition-select.11
  (format nil "~-1[10~;20~;30~:;40~]" 10)
  "40")

(deftest-error format-condition-select.12
  (format nil "~'=[10~;20~;30~:;40~]" 10))

(deftest-error format-condition-select.13
  (format nil "~1,2[10~;20~;30~:;40~]" 10))

(deftest-error format-condition-select.14
  (format nil "~[10~;20~;30~@:;40~]" 2))

(deftest-error format-condition-select.15
  (format nil "~[10~;20~;30~10:;40~]" 2))

(deftest-error format-condition-select.16
  (format nil "~[10~;20~:;30~;40~]" 2))

(deftest-error format-condition-select.17
  (format nil "~[111~]"))

(deftest format-condition-select.18
  (format nil "~[111~]" 0)
  "111")

(deftest format-condition-select.19
  (format nil "~[~]" 0)
  "")

(deftest format-condition-select.20
  (format nil "~[AAA~;BBB~0^CCC~;DDD~]" 1)
  "BBB")

(deftest-error format-condition-select.21
  (format nil "~[10~10;20~;30~:;40~]" 10))

(deftest format-condition-boolean.1
  (format nil "~:[AAA~;BBB~]" nil)
  "AAA")

(deftest format-condition-boolean.2
  (format nil "~:[AAA~;BBB~]" 'hello)
  "BBB")

(deftest-error format-condition-boolean.3
  (format nil "~:[AAA~;BBB~]"))

(deftest-error format-condition-boolean.4
  (format nil "~10:[AAA~;BBB~]" 10))

(deftest-error format-condition-boolean.5
  (format nil "~:[AAA~;BBB~;CCC~]" 10))

(deftest-error format-condition-boolean.6
  (format nil "~:[AAA~:;BBB~]" 10))

(deftest-error format-condition-boolean.7
  (format nil "~:[AAA~10;BBB~]" 10))

(deftest-error format-condition-boolean.8
  (format nil "~:[AAA~]" 10))

(deftest-error format-condition-boolean.9
  (format nil "~1:[AAA~;BBB~]" 10))

(deftest format-condition-true.1
  (format nil "~@[AAA~]~A" nil 20 30 40)
  "20")

(deftest format-condition-true.2
  (format nil "~@[AAA~]~A" 10 20 30 40)
  "AAA10")

(deftest-error format-condition-true.3
  (format nil "~:@[AAA~]~A" 10 20 30 40))

(deftest-error format-condition-true.4
  (format nil "~10@[AAA~]~A" 10 20 30 40))

(deftest-error format-condition-true.5
  (format nil "~@[AAA~;BBB~]~A" 10 20 30 40))

(deftest-error format-condition-true.6
  (format nil "~@[AAA~]~A"))

(deftest format-condition-true.7
  (format nil "~@[~]~A" 10 20 30)
  "10")

(deftest format-condition-true.8
  (format nil "~@[~]~A" nil 20 30)
  "20")


;;
;;  Iteration
;;
(deftest format-iteration-list.1
  (format nil "~{<~A>~}~A" '(10 20 30) 40)
  "<10><20><30>40")

(deftest format-iteration-list.2
  (format nil "~2{<~A>~}~A" '(10 20 30) 40)
  "<10><20>40")

(deftest format-iteration-list.3
  (format nil "~{<~A,~A>~}~A" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration-list.4
  (format nil "~6{<~A,~A>~}~A" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration-list.5
  (format nil "~{<~A,~A>~:}~A" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration-list.6
  (format nil "~{<~A,~A>~}~A" nil 50)
  "50")

(deftest-error format-iteration-list.7
  (format nil "~{<~A,~A>~:}~A" nil 50))

(deftest-error format-iteration-list.8
  (format nil "~-1{<~A,~A>~}~A" '(10 20 30 40) 50))

(deftest-error format-iteration-list.9
  (format nil "~,1{<~A,~A>~}~A" '(10 20 30 40) 50))

(deftest-error format-iteration-list.10
  (format nil "~{<~A,~A>~@}~A" '(10 20 30 40) 50))

(deftest-error format-iteration-list.11
  (format nil "~{<~A,~A>~2}~A" '(10 20 30 40) 50))

(deftest format-iteration-list.12
  (format nil "~{<~A~^,~A>~}~A" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration-list.13
  (format nil "~{<~A~^,~A>~}~A" '(10 20 30) 50)
  "<10,20><3050")

(deftest format-iteration-list.14
  (format nil "~{<~A~0^,~A>~}~A" '(10 20 30 40) 50)
  "<1050")

(deftest format-iteration-rest.1
  (format nil "~@{<~A,~A>~}" 10 20 30 40)
  "<10,20><30,40>")

(deftest format-iteration-rest.2
  (format nil "~2@{<~A,~A>~}~A" 10 20 30 40 50 60)
  "<10,20><30,40>50")

(deftest format-iteration-rest.3
  (format nil "~10@{<~A,~A>~}" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration-rest.4
  (format nil "~@{<~A,~A>~:}" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration-rest.5
  (format nil "~@{<~A,~A>~}")
  "")

(deftest-error format-iteration-rest.6
  (format nil "~@{<~A,~A>~:}"))

(deftest-error format-iteration-rest.7
  (format nil "~-1@{<~A,~A>~}" 10 20 30 40 50 60))

(deftest-error format-iteration-rest.8
  (format nil "~,1@{<~A,~A>~}" 10 20 30 40 50 60))

(deftest-error format-iteration-rest.9
  (format nil "~@{<~A,~A>~@}" 10 20 30 40 50 60))

(deftest-error format-iteration-rest.10
  (format nil "~@{<~A,~A>~10}" 10 20 30 40 50 60))

(deftest format-iteration-rest.11
  (format nil "~@{<~A~^,~A>~}" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration-rest.12
  (format nil "~@{<~A~^,~A>~}" 10 20 30 40 50)
  "<10,20><30,40><50")

(deftest format-iteration-rest.13
  (format nil "~@{<~A~0^,~A>~}~A" 10 20 30 40 50 60)
  "<1020")

(deftest format-iteration-listargs.1
  (format nil "~:{<~A,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration-listargs.2
  (format nil "~2:{<~A,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50>80")

(deftest format-iteration-listargs.3
  (format nil "~10:{<~A,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration-listargs.4
  (format nil "~:{<~A,~A>~:}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration-listargs.5
  (format nil "~:{<~A,~A>~}~A" nil 80)
  "80")

(deftest-error format-iteration-listargs.6
  (format nil "~:{<~A,~A>~:}~A" nil 80))

(deftest-error format-iteration-listargs.7
  (format nil "~-1:{<~A,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration-listargs.8
  (format nil "~,1:{<~A,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration-listargs.9
  (format nil "~:{<~A,~A>~@}~A" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration-listargs.10
  (format nil "~:{<~A,~A>~10}~A" '((10 20 30) (40 50) (60 70)) 80))

(deftest format-iteration-listargs.11
  (format nil "~:{<~A~^,~A>~}~A" '((10 20 30) (40) (60 70)) 80)
  "<10,20><40<60,70>80")

(deftest format-iteration-listargs.12
  (format nil "~:{<~A~0^,~A>~}~A" '((10 20 30) (40) (60 70)) 80)
  "<10<40<6080")

(deftest format-iteration-listargs.13
  (format nil "~:{<~A~:^,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><6080")

(deftest format-iteration-listargs.14
  (format nil "~:{<~A~0:^,~A>~}~A" '((10 20 30) (40 50) (60 70)) 80)
  "<1080")

(deftest format-iteration-restargs.1
  (format nil "~:@{<~A,~A>~}" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration-restargs.2
  (format nil "~2:@{<~A,~A>~}~A" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50>(60 70)")

(deftest format-iteration-restargs.3
  (format nil "~10:@{<~A,~A>~}" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration-restargs.4
  (format nil "~:@{<~A,~A>~:}" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration-restargs.5
  (format nil "~:@{<~A,~A>~}")
  "")

(deftest-error format-iteration-restargs.6
  (format nil "~:@{<~A,~A>~:}"))

(deftest-error format-iteration-restargs.7
  (format nil "~-1:@{<~A,~A>~}" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration-restargs.8
  (format nil "~,1:@{<~A,~A>~}" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration-restargs.9
  (format nil "~:@{<~A,~A>~@}" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration-restargs.10
  (format nil "~:@{<~A,~A>~10}" '(10 20 30) '(40 50) '(60 70)))

(deftest format-iteration-restargs.11
  (format nil "~:@{<~A~^,~A>~}" '(10 20 30) '(40) '(60 70))
  "<10,20><40<60,70>")

(deftest format-iteration-restargs.12
  (format nil "~:@{<~A~0^,~A>~}" '(10 20 30) '(40) '(60 70))
  "<10<40<60")

(deftest format-iteration-restargs.13
  (format nil "~:@{<~A~:^,~A>~}" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60")

(deftest format-iteration-restargs.14
  (format nil "~:@{<~A~0:^,~A>~}" '(10 20 30) '(40 50) '(60 70))
  "<10")

(deftest format-iteration-restargs.15
  (format nil "~:@{<~A~0:^,~A>~}~A" '(10 20 30) '(40 50) '(60 70))
  "<10(40 50)")

(deftest format-iteration2-list2.1
  (format nil "~{~}~A" "<~S~S>" '(10 20) 30)
  "<1020>30")

(deftest format-iteration2-list2.2
  (format nil "~{~}~A" (formatter "<~S~S>") '(10 20) 30)
  "<1020>30")

(deftest format-iteration2-list2.3
  (format nil "~{~}~A" "<~S>" '(10 20 30 40) 50)
  "<10><20><30><40>50")

(deftest format-iteration2-list2.4
  (format nil "~{~}~A" (formatter "<~S>") '(10 20 30 40) 50)
  "<10><20><30><40>50")

(deftest format-iteration2-list2.5
  (format nil "~2{~}~A" "<~S>" '(10 20 30 40) 50)
  "<10><20>50")

(deftest format-iteration2-list2.6
  (format nil "~2{~}~A" (formatter "<~S>") '(10 20 30 40) 50)
  "<10><20>50")

(deftest format-iteration2-list2.7
  (format nil "~2{~}~A" "<~S>" '() 50)
  "50")

(deftest format-iteration2-list2.8
  (format nil "~2{~}~A" (formatter "<~S>") '() 50)
  "50")

(deftest format-iteration2-list2.9
  (format nil "~2{~}~A" "<~S>" '(10) 50)
  "<10>50")

(deftest format-iteration2-list2.10
  (format nil "~2{~}~A" (formatter "<~S>") '(10) 50)
  "<10>50")

(deftest format-iteration2-list2.11
  (format nil "~2{~:}~A" "<~S>" '(10) 50)
  "<10>50")

(deftest format-iteration2-list2.12
  (format nil "~2{~:}~A" (formatter "<~S>") '(10) 50)
  "<10>50")

(deftest format-iteration2-list2.13
  (format nil "~2{~:}~A" "<~S>" '(10 20 30) 50)
  "<10><20>50")

(deftest format-iteration2-list2.14
  (format nil "~2{~:}~A" (formatter "<~S>") '(10 20 30) 50)
  "<10><20>50")

(deftest-error format-iteration2-list2.15
  (format nil "~{~:}~A" "<~S>" nil 50))

(deftest-error format-iteration2-list2.16
  (format nil "~{~:}~A" (formatter "<~S>") nil 50))

(deftest-error format-iteration2-list2.17
  (format nil "~-1{~}~A" "<~S>" '(10 20) 50))

(deftest-error format-iteration2-list2.18
  (format nil "~-1{~}~A" (formatter "<~S>") '(10 20) 50))

(deftest-error format-iteration2-list2.19
  (format nil "~,1{~}~A" "<~S>" '(10 20) 50))

(deftest-error format-iteration2-list2.20
  (format nil "~,1{~}~A" (formatter "<~S>") '(10 20) 50))

(deftest-error format-iteration2-list2.21
  (format nil "~{~@}~A" "<~S>" '(10 20) 50))

(deftest-error format-iteration2-list2.22
  (format nil "~{~@}~A" (formatter "<~S>") '(10 20) 50))

(deftest-error format-iteration2-list2.23
  (format nil "~{~1:}~A" "<~S>" '(10 20) 50))

(deftest-error format-iteration2-list2.24
  (format nil "~{~1:}~A" (formatter "<~S>") '(10 20) 50))

(deftest format-iteration2-list2.25
  (format nil "~{~}~A" "<~A~^,~A>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list2.26
  (format nil "~{~}~A" (formatter "<~A~^,~A>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list2.27
  (format nil "~{~}~A" "<~A~^,~A>" '(10 20 30) 50)
  "<10,20><3050")

(deftest format-iteration2-list2.28
  (format nil "~{~}~A" (formatter "<~A~^,~A>") '(10 20 30) 50)
  "<10,20><3050")

(deftest format-iteration2-list2.29
  (format nil "~10{~}" "<~A>" '(10 20 30) 50)
  "<10><20><30>")

(deftest format-iteration2-list2.30
  (format nil "~10{~}" (formatter "<~A>") '(10 20 30) 50)
  "<10><20><30>")

(deftest format-iteration2-list.1
  (format nil "~{~}~A" "<~A>" '(10 20 30) 40)
  "<10><20><30>40")

(deftest format-iteration2-list.2
  (format nil "~{~}~A" (formatter "<~A>") '(10 20 30) 40)
  "<10><20><30>40")

(deftest format-iteration2-list.3
  (format nil "~2{~}~A" "<~A>" '(10 20 30) 40)
  "<10><20>40")

(deftest format-iteration2-list.4
  (format nil "~2{~}~A" (formatter "<~A>") '(10 20 30) 40)
  "<10><20>40")

(deftest format-iteration2-list.5
  (format nil "~{~}~A" "<~A,~A>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.6
  (format nil "~{~}~A" (formatter "<~A,~A>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.7
  (format nil "~6{~}~A" "<~A,~A>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.8
  (format nil "~6{~}~A" (formatter "<~A,~A>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.9
  (format nil "~{~:}~A" "<~A,~A>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.10
  (format nil "~{~:}~A" (formatter "<~A,~A>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.11
  (format nil "~{~}~A" "<~A,~A>" nil 50)
  "50")

(deftest format-iteration2-list.12
  (format nil "~{~}~A" (formatter "<~A,~A>") nil 50)
  "50")

(deftest-error format-iteration2-list.13
  (format nil "~{~:}~A" "<~A,~A>" nil 50))

(deftest-error format-iteration2-list.14
  (format nil "~{~:}~A" (formatter "<~A,~A>") nil 50))

(deftest-error format-iteration2-list.15
  (format nil "~-1{~}~A" "<~A,~A>" '(10 20 30 40) 50))

(deftest-error format-iteration2-list.16
  (format nil "~-1{~}~A" (formatter "<~A,~A>") '(10 20 30 40) 50))

(deftest-error format-iteration2-list.17
  (format nil "~,1{~}~A" "<~A,~A>" '(10 20 30 40) 50))

(deftest-error format-iteration2-list.18
  (format nil "~,1{~}~A" (formatter "<~A,~A>") '(10 20 30 40) 50))

(deftest-error format-iteration2-list.19
  (format nil "~{~@}~A" "<~A,~A>" '(10 20 30 40) 50))

(deftest-error format-iteration2-list.20
  (format nil "~{~@}~A" (formatter "<~A,~A>") '(10 20 30 40) 50))

(deftest-error format-iteration2-list.21
  (format nil "~{~2}~A" "<~A,~A>" '(10 20 30 40) 50))

(deftest-error format-iteration2-list.22
  (format nil "~{~2}~A" (formatter "<~A,~A>") '(10 20 30 40) 50))

(deftest format-iteration2-list.23
  (format nil "~{~}~A" "<~A~^,~A>" '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.24
  (format nil "~{~}~A" (formatter "<~A~^,~A>") '(10 20 30 40) 50)
  "<10,20><30,40>50")

(deftest format-iteration2-list.25
  (format nil "~{~}~A" "<~A~^,~A>" '(10 20 30) 50)
  "<10,20><3050")

(deftest format-iteration2-list.26
  (format nil "~{~}~A" (formatter "<~A~^,~A>") '(10 20 30) 50)
  "<10,20><3050")

(deftest format-iteration2-list.27
  (format nil "~{~}~A" "<~A~0^,~A>" '(10 20 30 40) 50)
  "<1050")

(deftest format-iteration2-list.28
  (format nil "~{~}~A" (formatter "<~A~0^,~A>") '(10 20 30 40) 50)
  "<10<20<30<4050")

(deftest format-iteration2-rest.1
  (format nil "~@{~}" "<~A,~A>" 10 20 30 40)
  "<10,20><30,40>")

(deftest format-iteration2-rest.2
  (format nil "~@{~}" (formatter "<~A,~A>") 10 20 30 40)
  "<10,20><30,40>")

(deftest format-iteration2-rest.3
  (format nil "~2@{~}~A" "<~A,~A>" 10 20 30 40 50 60)
  "<10,20><30,40>50")

(deftest format-iteration2-rest.4
  (format nil "~2@{~}~A" (formatter "<~A,~A>") 10 20 30 40 50 60)
  "<10,20><30,40>50")

(deftest format-iteration2-rest.5
  (format nil "~10@{~}" "<~A,~A>" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.6
  (format nil "~10@{~}" (formatter "<~A,~A>") 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.7
  (format nil "~@{~:}" "<~A,~A>" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.8
  (format nil "~@{~:}" (formatter "<~A,~A>") 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.9
  (format nil "~@{~}" "<~A,~A>")
  "")

(deftest format-iteration2-rest.10
  (format nil "~@{~}" (formatter "<~A,~A>"))
  "")

(deftest-error format-iteration2-rest.11
  (format nil "~@{~:}" "<~A,~A>"))

(deftest-error format-iteration2-rest.12
  (format nil "~@{~:}" (formatter "<~A,~A>")))

(deftest-error format-iteration2-rest.13
  (format nil "~-1@{~}" "<~A,~A>" 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.14
  (format nil "~-1@{~}" (formatter "<~A,~A>") 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.15
  (format nil "~,1@{~}" "<~A,~A>" 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.16
  (format nil "~,1@{~}" (formatter "<~A,~A>") 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.17
  (format nil "~@{~@}" "<~A,~A>" 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.18
  (format nil "~@{~@}" (formatter "<~A,~A>") 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.19
  (format nil "~@{~10}" "<~A,~A>" 10 20 30 40 50 60))

(deftest-error format-iteration2-rest.20
  (format nil "~@{~10}" (formatter "<~A,~A>") 10 20 30 40 50 60))

(deftest format-iteration2-rest.21
  (format nil "~@{~}" "<~A~^,~A>" 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.22
  (format nil "~@{~}" (formatter "<~A~^,~A>") 10 20 30 40 50 60)
  "<10,20><30,40><50,60>")

(deftest format-iteration2-rest.23
  (format nil "~@{~}" "<~A~^,~A>" 10 20 30 40 50)
  "<10,20><30,40><50")

(deftest format-iteration2-rest.24
  (format nil "~@{~}" (formatter "<~A~^,~A>") 10 20 30 40 50)
  "<10,20><30,40><50")

(deftest format-iteration2-rest.25
  (format nil "~@{~}~A" "<~A~0^,~A>" 10 20 30 40 50 60)
  "<1020")

(deftest-error format-iteration2-rest.26
  (format nil "~@{~}~A" (formatter "<~A~0^,~A>") 10 20 30 40 50 60))

(deftest format-iteration2-listargs.1
  (format nil "~:{~}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.2
  (format nil "~:{~}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.3
  (format nil "~2:{~}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50>80")

(deftest format-iteration2-listargs.4
  (format nil "~2:{~}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50>80")

(deftest format-iteration2-listargs.5
  (format nil "~10:{~}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.6
  (format nil "~10:{~}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.7
  (format nil "~:{~:}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.8
  (format nil "~:{~:}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")

(deftest format-iteration2-listargs.9
  (format nil "~:{~}~A" "<~A,~A>" nil 80)
  "80")

(deftest format-iteration2-listargs.10
  (format nil "~:{~}~A" (formatter "<~A,~A>") nil 80)
  "80")

(deftest-error format-iteration2-listargs.11
  (format nil "~:{~:}~A" "<~A,~A>" nil 80))

(deftest-error format-iteration2-listargs.12
  (format nil "~:{~:}~A" (formatter "<~A,~A>") nil 80))

(deftest-error format-iteration2-listargs.13
  (format nil "~-1:{~}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.14
  (format nil "~-1:{~}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.15
  (format nil "~,1:{~}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.16
  (format nil "~,1:{~}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.17
  (format nil "~:{~@}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.18
  (format nil "~:{~@}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.19
  (format nil "~:{~10}~A" "<~A,~A>" '((10 20 30) (40 50) (60 70)) 80))

(deftest-error format-iteration2-listargs.20
  (format nil "~:{~10}~A" (formatter "<~A,~A>") '((10 20 30) (40 50) (60 70)) 80))

(deftest format-iteration2-listargs.21
  (format nil "~:{~}~A" "<~A~^,~A>" '((10 20 30) (40) (60 70)) 80)
  "<10,20><40<60,70>80")

(deftest format-iteration2-listargs.22
  (format nil "~:{~}~A" (formatter "<~A~^,~A>") '((10 20 30) (40) (60 70)) 80)
  "<10,20><40<60,70>80")

(deftest format-iteration2-listargs.23
  (format nil "~:{~}~A" "<~A~0^,~A>" '((10 20 30) (40) (60 70)) 80)
  "<10<40<6080")

(deftest format-iteration2-listargs.24
  (format nil "~:{~}~A" (formatter "<~A~0^,~A>") '((10 20 30) (40) (60 70)) 80)
  "<10<40<6080")

(deftest format-iteration2-listargs.25
  (format nil "~:{~}~A" "<~A~:^,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><6080")

(deftest format-iteration2-listargs.26
  (format nil "~:{~}~A" (formatter "<~A~:^,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10,20><40,50><60,70>80")  ;; ERROR

(deftest format-iteration2-listargs.27
  (format nil "~:{~}~A" "<~A~0:^,~A>" '((10 20 30) (40 50) (60 70)) 80)
  "<1080")

(deftest format-iteration2-listargs.28
  (format nil "~:{~}~A" (formatter "<~A~0:^,~A>") '((10 20 30) (40 50) (60 70)) 80)
  "<10<40<6080")  ;; ERROR

(deftest format-iteration2-restargs.1
  (format nil "~:@{~}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.2
  (format nil "~:@{~}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.3
  (format nil "~2:@{~}~A" "<~A,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50>(60 70)")

(deftest format-iteration2-restargs.4
  (format nil "~2:@{~}~A" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50>(60 70)")

(deftest format-iteration2-restargs.5
  (format nil "~10:@{~}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.6
  (format nil "~10:@{~}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.7
  (format nil "~:@{~:}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.8
  (format nil "~:@{~:}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")

(deftest format-iteration2-restargs.9
  (format nil "~:@{~}" "<~A,~A>")
  "")

(deftest format-iteration2-restargs.10
  (format nil "~:@{~}" (formatter "<~A,~A>"))
  "")

(deftest-error format-iteration2-restargs.11
  (format nil "~:@{~:}" "<~A,~A>"))

(deftest-error format-iteration2-restargs.12
  (format nil "~:@{~:}" (formatter "<~A,~A>")))

(deftest-error format-iteration2-restargs.13
  (format nil "~-1:@{~}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.14
  (format nil "~-1:@{~}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.15
  (format nil "~,1:@{~}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.16
  (format nil "~,1:@{~}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.17
  (format nil "~:@{~@}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.18
  (format nil "~:@{~@}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.19
  (format nil "~:@{~10}" "<~A,~A>" '(10 20 30) '(40 50) '(60 70)))

(deftest-error format-iteration2-restargs.20
  (format nil "~:@{~10}" (formatter "<~A,~A>") '(10 20 30) '(40 50) '(60 70)))

(deftest format-iteration2-restargs.21
  (format nil "~:@{~}" "<~A~^,~A>" '(10 20 30) '(40) '(60 70))
  "<10,20><40<60,70>")

(deftest format-iteration2-restargs.22
  (format nil "~:@{~}" (formatter "<~A~^,~A>") '(10 20 30) '(40) '(60 70))
  "<10,20><40<60,70>")

(deftest format-iteration2-restargs.23
  (format nil "~:@{~}" "<~A~0^,~A>" '(10 20 30) '(40) '(60 70))
  "<10<40<60")

(deftest format-iteration2-restargs.24
  (format nil "~:@{~}" (formatter "<~A~0^,~A>") '(10 20 30) '(40) '(60 70))
  "<10<40<60")

(deftest format-iteration2-restargs.25
  (format nil "~:@{~}" "<~A~:^,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60")

(deftest format-iteration2-restargs.26
  (format nil "~:@{~}" (formatter "<~A~:^,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10,20><40,50><60,70>")  ;; ERROR

(deftest format-iteration2-restargs.27
  (format nil "~:@{~}" "<~A~0:^,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10")

(deftest format-iteration2-restargs.28
  (format nil "~:@{~}" (formatter "<~A~0:^,~A>") '(10 20 30) '(40 50) '(60 70))
  "<10<40<60")  ;; ERROR

(deftest format-iteration2-restargs.29
  (format nil "~:@{~}~A" "<~A~0:^,~A>" '(10 20 30) '(40 50) '(60 70))
  "<10(40 50)")

(deftest-error format-iteration2-restargs.30
  (format nil "~:@{~}~A" (formatter "<~A~0:^,~A>") '(10 20 30) '(40 50) '(60 70)))


;;
;;  Justification
;;
(deftest format-justification-normal.1
  (format nil "~<~>")
  "")

(deftest format-justification-normal.2
  (format nil "~<AAA~>")
  "AAA")

(deftest format-justification-normal.3
  (format nil "~<AAA~;BBB~>")
  "AAABBB")

(deftest format-justification-normal.4
  (format nil "~10<AAA~;BBB~>")
  "AAA    BBB")

(deftest format-justification-normal.5
  (format nil "~10<AAA~;BBB~;CCC~>")
  "AAABBB CCC")

(deftest format-justification-normal.6
  (format nil "~18<AA~;BB~;CC~;DD~;EE~;FF~;GG~>")
  "AABB CC DDEE FF GG")

(deftest format-justification-normal.7
  (format nil "~37<AAA~;BBB~;CCC~>")
  "AAA              BBB              CCC")

(deftest format-justification-normal.8
  (format nil "++~30<AAA~;BBB~;CCC~>++")
  "++AAA          BBB           CCC++")

(deftest format-justification-normal.9
  (format nil "++~30<AA~;BBBBBBBBB~;C~;DD~;EEE~>++")
  "++AA   BBBBBBBBB   C   DD    EEE++")

(deftest format-justification-normal.10
  (format nil "++~30:<AAA~;BBB~;CCC~>++")
  "++       AAA       BBB       CCC++")

(deftest format-justification-normal.11
  (format nil "++~30@<AAA~;BBB~;CCC~>++")
  "++AAA       BBB       CCC       ++")

(deftest format-justification-normal.12
  (format nil "++~30:@<AAA~;BBB~;CCC~>++")
  "++     AAA     BBB     CCC      ++")

(deftest format-justification-normal.13
  (format nil "++~30<AAA~>++")
  "++                           AAA++")

(deftest format-justification-normal.14
  (format nil "++~30:<AAA~>++")
  "++                           AAA++")

(deftest format-justification-normal.15
  (format nil "++~30@<AAA~>++")
  "++AAA                           ++")

(deftest format-justification-normal.16
  (format nil "++~30:@<AAA~>++")
  "++             AAA              ++")

(deftest format-justification-normal.17
  (format nil "++~40<AAA~;BBB~;CCC~>++")
  "++AAA               BBB                CCC++")

(deftest format-justification-normal.18
  (format nil "++~<AAA~;BBB~;CCC~>++")
  "++AAABBBCCC++")

(deftest format-justification-normal.19
  (format nil "++~,20<AAA~;BBB~;CCC~>++")
  "++AAA     BBB      CCC++")

(deftest format-justification-normal.20
  (format nil "++~5,20<AAA~;BBB~;CCC~>++")
  "++AAA        BBB        CCC++")

(deftest format-justification-normal.21
  (format nil "++~,,5<AAA~;BBB~;CCC~>++")
  "++AAA     BBB     CCC++")

(deftest format-justification-normal.22
  (format nil "++~30,,5<AAA~;BBB~;CCC~>++")
  "++AAA          BBB           CCC++")

(deftest format-justification-normal.23
  (format nil "++~15<AAA~;BBB~;CCC~>++")
  "++AAA   BBB   CCC++")

(deftest format-justification-normal.24
  (format nil "++~15,20,5<AAA~;BBB~;CCC~>++")
  "++AAA             BBB             CCC++")

(deftest format-justification-normal.25
  (format nil "++~30,,,'*<AAA~;BBB~;CCC~>++")
  "++AAA**********BBB***********CCC++")

(deftest format-justification-normal.26
  (format nil "++~30<AA~;BB~;CC~;DD~;~^EE~>++")
  "++AA       BB       CC        DD++")

(deftest format-justification-normal.27
  (format nil "++~30<AA~;BB~;CC~;DD~;~^EE~>++~A" 11)
  "++AA     BB     CC     DD     EE++11")

(deftest format-justification-normal.28
  (format nil "++~30<AA~;BB~;CC~;DD~;~0^EE~>++~A" 11)
  "++AA       BB       CC        DD++11")

(deftest format-justification-normal.29
  (format nil "++~30<AA~;BB~;CC~;~^DD~;EE~>++")
  "++AA            BB            CC++")

(deftest format-justification-normal.30
  (format nil "++~30<AA~;BB~;CC~;DD~^dd~;EE~>++")
  "++AA            BB            CC++")

(deftest format-justification-normal.31
  (format nil "++~30<A~^AA~;BBB~;CCC~>++")
  "++                              ++")

(deftest format-justification-width.1
  (format nil "++~30<AAA~:;BBB~;CCC~>++")
  "++BBB                        CCC++")

(deftest format-justification-width.2
  (format nil "++~30<AAA~,10:;BBB~;CCC~>++")
  "++AAABBB                        CCC++")

(deftest format-justification-width.3
  (format nil "++~30<AAA~40:;BBB~;CCC~>++")
  "++BBB                        CCC++")

(deftest format-justification-width.4
  (format nil "~%;; ~{~<~%;; ~1,50:; ~S~>~^,~}.~%"
              '(100000000 2000000000 hellohellohello 4000000 599999999))
  #.(mkstr #\newline
           ";;  100000000, 2000000000, HELLOHELLOHELLO," #\newline
           ";;  4000000, 599999999." #\newline))

(deftest format-justification-width.5
  (format nil "++~30<A~^AA~:;BBB~;CCC~>++")
  "++                              ++")

(deftest format-justification-width.6
  (format nil "++~30<AAA~:;B~^BB~;CCC~>++")
  "++                              ++")

(deftest format-justification-width.7
  (format nil "++~30<AAA~,10:;B~^BB~;CCC~>++")
  "++AAA                              ++")

(deftest-error format-justification.1
  (format nil "~<~W~>" 100))

(deftest-error format-justification.2
  (format nil "~<~_~>"))

(deftest-error format-justification.3
  (format nil "~<~10I~>"))

(deftest-error format-justification.4
  (format nil "~<~5:T~>"))

(deftest format-justification.5
  (format nil "~<~5T~>")
  "     ")

(deftest-error format-justification.6
  (format nil "~<~<HELLO~:>~>" nil))

(deftest format-justification.7
  (format nil "~37<~A~;~A~;CCC~>" "AAA" "BBB")
  "AAA              BBB              CCC")

(deftest format-justification.8
  (format nil "++~30<~A~,10:;~A~;CCC~>++" "AAA" "BBB")
  "++AAABBB                        CCC++")


;;
;;  EscapeUpward
;;
(deftest format-escape-upward.1
  (format nil "AAA~^BBB")
  "AAA")

(deftest format-escape-upward.2
  (format nil "AAA~^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.3
  (format nil "AAA~#^BBB")
  "AAA")

(deftest format-escape-upward.4
  (format nil "AAA~#^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.5
  (format nil "AAA~4,4^BBB" 100)
  "AAA")

(deftest format-escape-upward.6
  (format nil "AAA~4,2^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.7
  (format nil "AAA~4,10^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.8
  (format nil "AAA~5,5,5^BBB" 100)
  "AAA")

(deftest format-escape-upward.9
  (format nil "AAA~5,5,7^BBB" 100)
  "AAA")

(deftest format-escape-upward.10
  (format nil "AAA~2,5,5^BBB" 100)
  "AAA")

(deftest format-escape-upward.11
  (format nil "AAA~2,5,10^BBB" 100)
  "AAA")

(deftest format-escape-upward.12
  (format nil "AAA~8,5,5^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.13
  (format nil "AAA~5,5,-2^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.14
  (format nil "AAA~:^BBB" 100)
  "AAABBB")

(deftest-error format-escape-upward.15
  (format nil "AAA~@^BBB" 100))

(deftest-error format-escape-upward.16
  (format nil "AAA~'a^BBB" 100))

(deftest-error format-escape-upward.17
  (format nil "AAA~1,'a^BBB" 100))

(deftest-error format-escape-upward.18
  (format nil "AAA~1,1,'a^BBB" 100))

(deftest-error format-escape-upward.19
  (format nil "AAA~1,1,1,1^BBB" 100))

(deftest format-escape-upward.20
  (format nil "AAA~,1^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.21
  (format nil "AAA~,1^BBB")
  "AAA")  ;; ERROR

(deftest format-escape-upward.22
  (format nil "AAA~,0^BBB" 100)
  "AAABBB")

(deftest format-escape-upward.23
  (format nil "AAA~,0^BBB")
  "AAA")  ;; ERROR

