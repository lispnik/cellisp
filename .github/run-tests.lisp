;;;; CI entry point: run both suites, exit non-zero on any failure.  Portable
;;;; across SBCL / CCL / ECL; no quicklisp.
;;;;
;;;; Register this project tree (computed from this file's location, ../ from
;;;; .github/) on the ASDF source registry.  `ocicl install' has placed the deps
;;;; (bordeaux-threads + transitive) under ./ocicl, which the :tree scan finds —
;;;; so the run is self-contained, needing no global Lisp configuration.

(require :asdf)
(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:initialize-source-registry
   (list :source-registry (list :tree root) :inherit-configuration)))

(handler-case
    (progn
      (asdf:test-system "cellisp")
      (asdf:test-system "cellisp/display")
      (uiop:quit 0))
  (error (e)
    (format t "~&CI FAILURE: ~A~%" e)
    (uiop:quit 1)))
