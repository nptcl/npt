;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(setf (logical-pathname-translations "test")
      '(("path;*.*.*" "/path/to/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))

(setf (logical-pathname-translations "name")
      '(("*.*.*" "/name/")
        ("name;to;*.*" "/name/to/")))

(setf (logical-pathname-translations "logical-name")
      '(("*.*.*" "/var/")
        ("path;to;*.*" "/usr/local/")))

(defun parse-namestring-unix (name)
  (parse-namestring name 'lisp-system::unix))

(defun parse-namestring-windows (name)
  (parse-namestring name 'lisp-system::windows))

