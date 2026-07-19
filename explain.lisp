(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Explain — introspect how a cell's value (or error) arises
;;;;
;;;; EXPLAIN-TREE walks a cell's precedents into a nested plist; EXPLAIN
;;;; prints that as an indented tree. Because a precedent's error propagates
;;;; up to the cells that read it, the tree pinpoints the *root cause* — the
;;;; deepest cell that actually errored. Cycles and already-shown cells are
;;;; collapsed so the walk always terminates.
;;;; ------------------------------------------------------------------

(defun explain-tree (sheet designator)
  "Return a nested plist describing DESIGNATOR and its precedents. Each node is
(:ref S …): a normal node also has :formula, :value, :error, and :precedents;
an empty referenced cell is (:ref S :state :empty); a cell already on the path
is :state :cycle; one already expanded elsewhere is :state :seen."
  (with-sheet-lock (sheet)
    (let ((done (make-hash-table :test 'equal)))
      (labels ((walk (ref path)
                 (let ((cell (find-cell sheet ref)))
                   (cond
                     ((null cell)
                      (list :ref (ref-string ref) :state :empty))
                     ((member ref path :test 'equal)
                      (list :ref (ref-string ref) :state :cycle))
                     ((gethash ref done)
                      (list :ref (ref-string ref) :state :seen
                            :value (cell-value cell) :error (cell-err cell)))
                     (t
                      (prog1
                          (list :ref (ref-string ref)
                                :formula (cell-formula cell)
                                :value (cell-value cell)
                                :error (cell-err cell)
                                :precedents
                                ;; sort by ref so the tree is deterministic and
                                ;; readable (cell-precedents order is a hash
                                ;; iteration order, which varies by impl)
                                (mapcar (lambda (p) (walk p (cons ref path)))
                                        (sort (copy-list (cell-precedents cell))
                                              #'string< :key #'ref-string)))
                        (setf (gethash ref done) t)))))))
        (walk (parse-ref designator) '())))))

(defun %explain-node-label (node)
  (let ((ref (getf node :ref)))
    (case (getf node :state)
      (:empty (format nil "~A  <empty>" ref))
      (:cycle (format nil "~A  <cycle>" ref))
      (:seen  (format nil "~A = ~A  (shown above)" ref
                      (if (getf node :error) "<error>" (prin1-to-string (getf node :value)))))
      (t (cond
           ((getf node :error)
            (format nil "~A = <error: ~A>   ~S" ref (getf node :error) (getf node :formula)))
           ((null (getf node :formula))
            (format nil "~A  <empty>" ref))         ; a blank backing cell
           (t (format nil "~A = ~S   ~S" ref (getf node :value) (getf node :formula))))))))

(defun explain (sheet designator &optional (stream *standard-output*))
  "Print an indented tree explaining how DESIGNATOR's value (or error) arises,
walking its precedents. For an errored cell, follow the branch down to the
deepest cell that actually failed — the root cause."
  (labels ((pr (node prefix child-prefix)
             (format stream "~A~A~%" prefix (%explain-node-label node))
             (loop for (kid . rest) on (getf node :precedents) do
               (if rest
                   (pr kid (concatenate 'string child-prefix "├─ ")
                           (concatenate 'string child-prefix "│  "))
                   (pr kid (concatenate 'string child-prefix "└─ ")
                           (concatenate 'string child-prefix "   "))))))
    (pr (explain-tree sheet designator) "" ""))
  (values))
