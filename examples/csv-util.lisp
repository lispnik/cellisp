;;;; csv-util.lisp — a small RFC-4180 CSV parser shared by the csv-spill demos.
;;;;
;;;; Load it after cellisp is loaded (it uses CELLISP:TO-NUMBER to coerce fields):
;;;;   (load (merge-pathnames "csv-util.lisp"
;;;;                          (or *load-truename* *default-pathname-defaults*)))

(in-package #:cellisp)

(defun parse-csv (text)
  "Parse CSV TEXT into a list of rows, each a list of fields. RFC-4180-ish:
double-quoted fields may contain commas, newlines, and doubled quotes (\"\").
Numeric-looking fields are coerced with TO-NUMBER; everything else stays a string."
  (let ((rows '()) (row '()) (field '()) (in-quotes nil) (any nil)
        (i 0) (n (length text)))
    (labels ((end-field ()
               (let ((s (coerce (nreverse field) 'string)))
                 (push (to-number s s) row))   ; number, or the string as-is
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
      (when any (end-field) (end-row)))       ; flush a final unterminated row
    (nreverse rows)))
