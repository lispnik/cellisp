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

(defparameter *url-template*
  "https://endpoints.office.com/endpoints/Worldwide?ServiceAreas=~A~
   &format=CSV&ClientRequestId=d6bc355c-51ff-48f5-acb0-dd42baf76b88")

;;;; --- quote-aware CSV parsing ---------------------------------------------

(defun parse-field (s)
  "Coerce a CSV field to a number when the whole field is numeric, else a string."
  (if (and (plusp (length s))
           (let ((c (char s 0)))
             (or (digit-char-p c) (member c '(#\- #\+ #\.)))))
      (multiple-value-bind (v pos)
          (let ((*read-eval* nil)) (ignore-errors (read-from-string s nil nil)))
        (if (and (numberp v) (eql pos (length s))) v s))
      s))

(defun parse-csv (text)
  "RFC-4180-ish parse: fields may be double-quoted and then contain commas,
newlines, and doubled quotes (\"\"). Returns a list of rows of coerced fields."
  (let ((rows '()) (row '()) (field '()) (in-quotes nil) (any nil)
        (i 0) (n (length text)))
    (labels ((end-field () (push (parse-field (coerce (nreverse field) 'string)) row)
                           (setf field '()))
             (end-row () (push (nreverse row) rows) (setf row '() any nil)))
      (loop while (< i n) for c = (char text i) do
        (setf any t)
        (if in-quotes
            (cond ((char= c #\")
                   (if (and (< (1+ i) n) (char= (char text (1+ i)) #\"))
                       (progn (push #\" field) (incf i))   ; "" -> literal "
                       (setf in-quotes nil)))
                  (t (push c field)))
            (cond ((char= c #\") (setf in-quotes t))
                  ((char= c #\,) (end-field))
                  ((char= c #\Return))                     ; ignore CR
                  ((char= c #\Newline) (end-field) (end-row))
                  (t (push c field))))
        (incf i))
      (when any (end-field) (end-row)))            ; flush a final unterminated row
    (nreverse rows)))

;;;; --- async fetch + dynamic spill (as in csv-spill.lisp) -------------------

(defparameter *extent* (cons 0 0))
(defvar *generation* 0)

(defun clear-spill (data anchor extent)
  (with-sheet-lock (data)
    (let ((a (parse-ref anchor)))
      (dotimes (i (car extent))
        (dotimes (j (cdr extent))
          (clear-cell data (make-ref (+ (ref-row a) i) (+ (ref-col a) j))))))))

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
           (clear-spill data "A1" *extent*)
           (funcall deliver rows)
           (setf *extent* (spill data "A1" '(cell "_raw!A1")))
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

;;;; --- readable dump: truncate wide cells ----------------------------------

(defun clip-formats (width)
  "A registry whose one rule truncates every cell's text to WIDTH chars."
  (let ((f (make-formats)))
    (add-conditional f (constantly t)
                     (lambda (v)
                       (let ((s (as-text v)))
                         (if (> (length s) width)
                             (concatenate 'string (subseq s 0 (- width 1)) "…")
                             s))))
    f))

;;;; --- demo -----------------------------------------------------------------

(defun run ()
  (let ((wb (make-workbook)))
    (add-sheet wb "_raw")
    (add-sheet wb "Data")
    (set-async (find-sheet wb "_raw") "A1" (make-fetcher wb) :initial nil)
    (flet ((show (area)
             (set-cell (find-sheet wb "_raw") "B1" area)
             (refresh-and-wait wb)
             (format t "~2&===== GET …ServiceAreas=~A  ->  ~D rows x ~D cols spilled =====~%"
                     area (car *extent*) (cdr *extent*))
             (print-sheet (find-sheet wb "Data") :name nil :formats (clip-formats 16))))
      (show "Exchange")
      (show "SharePoint")))       ; a different-sized feed -> re-spills dynamically
  (values))

(run)
#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
