;;;; csv-spill-live.lisp — spill a REAL public CSV/HTTPS endpoint.
;;;;
;;;;   sbcl --script examples/csv-spill-live.lisp
;;;;   ecl  --load   examples/csv-spill-live.lisp
;;;;
;;;; Same idea as csv-spill.lisp, but instead of a local server it fetches a
;;;; live HTTPS endpoint — Microsoft's public Office 365 IP/URL feed
;;;; (endpoints.office.com, format=CSV) — with dexador, and an ASYNC cell spills
;;;; one cell per field, sized to however many rows the response returned.
;;;; Switching the ServiceArea re-fetches a different-sized feed and re-spills.
;;;;
;;;; Two upgrades over the local-server demo, because this is real-world CSV:
;;;;   * dexador for the HTTPS GET (pulls cl+ssl);
;;;;   * a quote-aware (RFC-4180) CSV parser, since fields like the IP list are
;;;;     double-quoted and packed with commas.
;;;; The dump truncates each cell so the very wide columns stay readable.

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload '("bordeaux-threads" "dexador") :silent t)
(asdf:load-system "cellisp")
(asdf:load-system "cellisp/display")

(in-package #:cellisp)
(use-package '#:cellisp/display)
(load (merge-pathnames "csv-util.lisp"       ; shared quote-aware PARSE-CSV
                       (or *load-truename* *default-pathname-defaults*)))

(defparameter *url-template*
  "https://endpoints.office.com/endpoints/Worldwide?ServiceAreas=~A~
   &format=CSV&ClientRequestId=d6bc355c-51ff-48f5-acb0-dd42baf76b88")

;;;; --- async fetch + dynamic spill (as in csv-spill.lisp) -------------------

(defvar *generation* 0)

(defun make-fetcher (workbook)
  (let ((raw  (find-sheet workbook "_raw"))
        (data (find-sheet workbook "Data")))
    (lambda (deliver)
      (bt:make-thread
       (lambda ()
         (let ((rows (handler-case
                         (parse-csv (dexador:get (format nil *url-template*
                                                         (get-value raw "B1"))))
                       (error (e) (list (list "ERROR" (princ-to-string e)))))))
           (funcall deliver rows)
           (respill data "A1" '(cell "_raw!A1"))    ; self-clearing dynamic spill
           (incf *generation*)))
       :name "csv-fetch"))))

(defun refresh-and-wait (workbook &key (timeout 15.0))
  (let ((before *generation*)
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (refresh-async (find-sheet workbook "_raw") "A1")
    (loop while (and (= *generation* before)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.05))))

;;;; --- demo -----------------------------------------------------------------

(defun run ()
  (let ((wb (make-workbook)))
    (add-sheet wb "_raw")
    (add-sheet wb "Data")
    (set-async (find-sheet wb "_raw") "A1" (make-fetcher wb) :initial nil)
    (flet ((show (area)
             (set-cell (find-sheet wb "_raw") "B1" area)
             (refresh-and-wait wb)
             (multiple-value-bind (rows cols)
                 (sheet-dimensions (find-sheet wb "Data"))
               (format t "~2&===== GET …ServiceAreas=~A  ->  ~D rows x ~D cols spilled =====~%"
                       area rows cols))
             ;; :max-col-width keeps the very wide IP-list cells from blowing up
             (print-sheet (find-sheet wb "Data") :name nil :max-col-width 16)))
      (show "Exchange")
      (show "SharePoint")))       ; a different-sized feed -> re-spills dynamically
  (values))

(run)
#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
