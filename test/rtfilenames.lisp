;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(setf (logical-pathname-translations "test")
      '(("path;*.*.*" "/path/to/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))

(setf (logical-pathname-translations "name")
      '(("name;to;*.*" "/name/to/")
        ("*.*.*" "/name/")))

(defun parse-namestring-unix (name)
  (parse-namestring name 'lisp-system::unix))

(defun parse-namestring-windows (name)
  (parse-namestring name 'lisp-system::windows))

(defun pathname-values (x)
  (values
    (pathname-host x)
    (pathname-device x)
    (pathname-directory x)
    (pathname-name x)
    (pathname-type x)
    (pathname-version x)))

(defun pathname-values5 (x)
  (values
    (pathname-host x)
    (pathname-device x)
    (pathname-directory x)
    (pathname-name x)
    (pathname-type x)))

(defun pathname-values3 (x)
  (values
    (pathname-directory x)
    (pathname-name x)
    (pathname-type x)))

