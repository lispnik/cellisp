;;;; CI entry point: load Quicklisp, ensure bordeaux-threads, run the suite,
;;;; and exit non-zero on any failure. Portable across SBCL / CCL / ECL.
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(handler-case
    (progn
      #+quicklisp (ql:quickload "bordeaux-threads" :silent t)
      (asdf:test-system "cellisp")
      (uiop:quit 0))
  (error (e)
    (format t "~&CI FAILURE: ~A~%" e)
    (uiop:quit 1)))
