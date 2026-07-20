;;;; csv-spill.lisp — a cell that fetches CSV over HTTP and spills the rows.
;;;;
;;;;   sbcl --script examples/csv-spill.lisp
;;;;   ecl  --load   examples/csv-spill.lisp
;;;;
;;;; Self-contained: the script starts a tiny local HTTP server that serves CSV,
;;;; then an ASYNC cell fetches it on a worker thread and SPILLS one cell per
;;;; field — sized to however many rows the response returned. Switching the
;;;; requested dataset re-fetches and re-spills, growing/shrinking dynamically.
;;;;
;;;; Design (all with the existing public API — no core changes):
;;;;   * a workbook with two sheets: "Data" (where rows spill) and "_raw" (holds
;;;;     the fetched array off to the side, plus a control cell for the path);
;;;;   * "_raw!A1" is an ASYNC cell; its fetcher does the HTTP GET + CSV parse on
;;;;     a bt:make-thread worker, delivers the parsed rows, then spills the
;;;;     "Data" sheet from (cell "_raw!A1") — a cross-sheet array formula;
;;;;   * spill's shape is fixed at spill time and it does NOT clear a shrunk
;;;;     block, so the driver clears the previous rectangle before each re-spill.
;;;;
;;;; Only usocket is added (server + client, both), keeping core dependency-free.

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload '("bordeaux-threads" "usocket") :silent t)
(asdf:load-system "cellisp")
(asdf:load-system "cellisp/display")

(in-package #:cellisp)
(use-package '#:cellisp/display)

(defparameter *port* 8765)

;;;; --- tiny local HTTP server serving CSV by path ---------------------------

(defparameter *datasets*
  '(("/q1" . "product,units,price
Widget,120,25
Gadget,80,40
Gizmo,150,15")
    ("/q2" . "product,units,price
Widget,300,25
Gadget,90,40
Gizmo,220,15
Sprocket,40,60
Cog,500,5
Flange,75,32")))

(defun dataset-for (path)
  (or (cdr (assoc path *datasets* :test #'string=))
      "error,message
404,no such dataset"))

(defvar *listener* nil)
(defvar *server-thread* nil)

(defun start-server (port)
  (setf *listener* (usocket:socket-listen "127.0.0.1" port :reuse-address t))
  (setf *server-thread*
        (bt:make-thread
         (lambda ()
           ;; closing the listener makes SOCKET-ACCEPT error, ending the loop.
           (ignore-errors
            (loop
              (let* ((conn (usocket:socket-accept *listener*))
                     (stream (usocket:socket-stream conn)))
                (ignore-errors
                 (let* ((req  (string-right-trim '(#\Return) (read-line stream nil "")))
                        (path (second (split-on #\Space req)))     ; "GET /q1 HTTP/1.0"
                        (body (dataset-for path)))
                   (format stream "HTTP/1.0 200 OK~C~CContent-Type: text/csv~C~C~
                                   Content-Length: ~D~C~C~C~C~A"
                           #\Return #\Linefeed #\Return #\Linefeed
                           (length body) #\Return #\Linefeed #\Return #\Linefeed body)
                   (force-output stream)))
                (ignore-errors (usocket:socket-close conn))))))
         :name "csv-http-server")))

(defun stop-server ()
  (ignore-errors (usocket:socket-close *listener*))   ; ends the accept loop
  (ignore-errors (bt:join-thread *server-thread*)))

;;;; --- tiny HTTP client + CSV parsing --------------------------------------

(defun split-on (char string)
  "STRING split on CHAR into a list of substrings."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start (or pos (length string)))
        while pos do (setf start (1+ pos))))

(defun http-get (host port path)
  "GET PATH from HOST:PORT and return the response body (headers stripped)."
  (let ((socket (usocket:socket-connect host port)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~C~C~C"
                   path #\Return #\Linefeed host #\Return #\Linefeed
                   #\Return #\Linefeed)
           (force-output stream)
           (let* ((lines (loop for line = (read-line stream nil nil)
                               while line
                               collect (string-right-trim '(#\Return) line)))
                  (blank (position "" lines :test #'string=)))   ; header/body split
             (format nil "~{~A~^~%~}" (if blank (nthcdr (1+ blank) lines) lines))))
      (usocket:socket-close socket))))

(defun parse-field (s)
  "Coerce a CSV field to a number when the whole field is numeric, else a string."
  (let ((s (string-trim '(#\Space #\Tab #\Return) s)))
    (if (and (plusp (length s))
             (let ((c (char s 0)))
               (or (digit-char-p c) (member c '(#\- #\+ #\.)))))
        (multiple-value-bind (v pos)
            (let ((*read-eval* nil)) (ignore-errors (read-from-string s nil nil)))
          (if (and (numberp v) (eql pos (length s))) v s))
        s)))

(defun parse-csv (text)
  "TEXT to a list of rows, each a list of coerced fields. Blank lines dropped."
  (loop for line in (split-on #\Newline text)
        when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
          collect (mapcar #'parse-field (split-on #\, line))))

;;;; --- the async fetch + dynamic spill -------------------------------------

(defparameter *extent* (cons 0 0))     ; last (rows . cols) spilled onto Data
(defvar *generation* 0)                ; bumped after each spill completes

(defun clear-spill (data anchor extent)
  "Empty the previously-spilled rectangle so a shrunk result leaves no leftovers."
  (with-sheet-lock (data)
    (let ((a (parse-ref anchor)))
      (dotimes (i (car extent))
        (dotimes (j (cdr extent))
          (clear-cell data (make-ref (+ (ref-row a) i) (+ (ref-col a) j))))))))

(defun make-csv-fetcher (workbook)
  "A fetcher for SET-ASYNC: on a worker thread, GET the CSV named by _raw!B1,
parse it, then clear the old block, deliver the rows, and re-spill Data."
  (let ((raw  (find-sheet workbook "_raw"))
        (data (find-sheet workbook "Data")))
    (lambda (deliver)
      (bt:make-thread
       (lambda ()
         (let ((rows (handler-case
                         (parse-csv (http-get "127.0.0.1" *port* (get-value raw "B1")))
                       (error (e) (list (list "ERROR" (princ-to-string e)))))))
           (clear-spill data "A1" *extent*)              ; clear the previous block
           (funcall deliver rows)                        ; _raw!A1 := rows (async)
           (setf *extent* (spill data "A1" '(cell "_raw!A1")))  ; dynamic re-spill
           (incf *generation*)))
       :name "csv-fetch"))))

(defun refresh-and-wait (workbook &key (timeout 3.0))
  "Trigger the async cell and block until its fetch+spill finishes (or times out)."
  (let ((before *generation*)
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (refresh-async (find-sheet workbook "_raw") "A1")
    (loop while (and (= *generation* before)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.02))))

;;;; --- demo -----------------------------------------------------------------

(defun run ()
  (start-server *port*)
  (unwind-protect
       (let ((wb (make-workbook)))
         (add-sheet wb "_raw")
         (add-sheet wb "Data")
         (set-cell (find-sheet wb "_raw") "B1" "/q1")         ; the requested path
         (set-async (find-sheet wb "_raw") "A1" (make-csv-fetcher wb) :initial nil)

         (flet ((show (path note)
                  (set-cell (find-sheet wb "_raw") "B1" path)
                  (refresh-and-wait wb)
                  (format t "~2&===== GET ~A  ->  ~D rows spilled ~A =====~%"
                          path (car *extent*) note)
                  (print-sheet (find-sheet wb "Data") :name nil)))
           (show "/q1" "")
           (show "/q2" "(grew)")
           (show "/q1" "(shrank — old rows cleared)")))
    (stop-server))
  (values))

(run)
#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
