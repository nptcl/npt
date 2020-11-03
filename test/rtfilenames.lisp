;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(setf (logical-pathname-translations "test")
      '(("path;*.*.*" "/path/to/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))

(setf (logical-pathname-translations "logical-name")
      '(("*.*.*" "/var/")
        ("path;to;*.*" "/usr/local/")))

(defvar *pathname-unix*
  (make-pathname :host 'lisp-system::unix))

(defvar *pathname-windows*
  (make-pathname :host 'lisp-system::windows))

(defun parse-namestring-unix (name)
  (let ((*default-pathname-defaults* *pathname-unix*))
    (parse-namestring name)))

(defun parse-namestring-windows (name)
  (let ((*default-pathname-defaults* *pathname-windows*))
    (parse-namestring name)))

