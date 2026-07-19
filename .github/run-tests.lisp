;;;; CI entry point: load Quicklisp, ensure bordeaux-threads, run the suite,
;;;; and exit non-zero on any failure. Portable across SBCL / CCL / ECL.

;;; Register the project directly on ASDF's central registry, computed from this
;;; file's own location (../ from .github/). This is more portable than relying
;;; on the ~/quicklisp/local-projects symlink — ECL's local-projects scan does
;;; not reliably follow a symlinked directory, so find-system would miss it.
(pushnew (uiop:pathname-parent-directory-pathname
          (uiop:pathname-directory-pathname *load-truename*))
         asdf:*central-registry* :test #'equal)

(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(handler-case
    (progn
      #+quicklisp (ql:quickload "bordeaux-threads" :silent t)
      (asdf:test-system "cellisp")
      (asdf:test-system "cellisp/display")
      (uiop:quit 0))
  (error (e)
    (format t "~&CI FAILURE: ~A~%" e)
    (uiop:quit 1)))
