;;
;;  ANSI COMMON LISP: 19. Filenames
;;
(setf (logical-pathname-translations "test")
      '(("path;*.*.*" "/path/to/")
        ("aaa;bbb;*.lisp" "/usr/local/lisp/*.l")))

