;;; Copyright (c) 2025 Carnegie Mellon University

(ql:quickload '(:alexandria :iterate :cl-interpol :cl-ppcre :usocket-server :babel
                :cl-json :bordeaux-threads :local-time :uiop :vom :trivial-backtrace))

(interpol:enable-interpol-syntax :modify-*readtable* t)

(defpackage :expert-mind
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:us :usocket)
                    (:bb :babel)
                    (:js :json)
                    (:re :ppcre)
                    (:lt :local-time)
                    (:ui :uiop)
                    (:tb :trivial-backtrace)
                    (:v :vom))
  (:export #:run-model))

(in-package :expert-mind)

(vom:config :expert-mind :info)

(load (merge-pathnames #P"act-up-v1_3_3" *load-truename*))
(load (merge-pathnames #P"http-server" *load-truename*))
(load (merge-pathnames #P"ExpertMind-helpers" *load-truename*))

(defparameter *history* nil)
(defparameter *clear-past-chunks* nil)
(defparameter *clear-future-chunks* nil)
(defparameter *activation-fn* (symbol-function 'activation))
(defparameter *capture-activations* nil)

(defvar *activations* nil)

(fmakunbound 'activation)               ; suppress the redefinition warning

(defun activation (chunk &optional (trace *verbose*))
  (let ((result (funcall *activation-fn* chunk trace)))
    (when *capture-activations*
      (push (cons (chunk-content chunk) result) *activations*))
    result))

(defmacro with-activations (&body body)
  `(%with-activations (lambda () ,@body)))

(defun %with-activations (thunk)
  (let* ((*activations* nil)
         (*capture-activations* t)
         (result (funcall thunk)))
    (values result (nreverse *activations*))))

(defparameter *init-file* (merge-pathnames #P"init_data.lisp" *load-truename*))

(defparameter *using-numeric-ids* nil)
(defparameter *last-click* 0)
(defparameter *time-origin* nil)
(defparameter *initial-data* nil)
(defparameter *current-task* nil)
(defparameter *history* (list nil nil))

(defparameter *cycle* 0)

(defun reset (&key params (init-file *init-file*))
  (setf *using-numeric-ids* nil)
  (setf *last-click* 0)
  (setf *time-origin* nil)
  (init-memory)
  (init-similarities)
  (parameter :ol nil)
  (parameter :ans 0)
  (parameter :v nil)
  (iter (for (key val) :on params :by #'cddr)
        (parameter key val))
  (setf *current-task* nil)
  (with-open-file (in init-file)
    (setf *initial-data* 
			   (read in)))
  (actr-time 1))


(defun future-model (id time)
  (let ((*package* (find-package :expert-mind)))
    (push id *history*)
    (labels ((lags (&optional include-current)
               (let ((tags '(current lag1 lag2)))
                 (unless include-current
                   (pop tags))
                 (mapcar #'list tags *history*))))
      (multiple-value-bind (_best-chunk _best-value blend-alist)
          (blend-vote (lags) 'current)
        (declare (ignore _best-chunk _best-value))
        ;; If BLEND-VOTE failed (no chunk / no values), bail out cleanly.
        (when (null blend-alist)
          (learn (lags t))
          (actr-time time)
          (pop (cddr *history*))
          (return-from future-model nil))
        ;; Normal path: BLEND-ALIST is a proper alist of (key . activation)
        (let* ((future
                 (alist-probabilities
                  (mapcar (curry #'apply #'cons) blend-alist))))
          ;; Enforce numeric probabilities; replace NIL with 0
          (setf future
                (mapcar (lambda (entry)
                          (destructuring-bind (dst-id . p) entry
                            (cons dst-id (if (numberp p) p 0.0d0))))
                        future))
          (learn (lags t))
          (actr-time time)
          (pop (cddr *history*))
          future)))))

(defun safe-future-model (id time)
  ;; Defensively normalize TIME.
  (unless (numberp time)
    (return-from safe-future-model nil))

  ;; Call the original function in a protected way.
  (handler-case
      (let ((result (future-model id time)))
        ;; Normalize the *shape* of RESULT:
        ;; - NIL       => no prediction
        ;; - non-list  => treat as unusable
        ;; - list of (dst-id . prob): repair NIL probs to 0.0d0.
        (cond
          ((null result) nil)
          ((not (listp result)) nil)
          (t
           (mapcar (lambda (entry)
                     (destructuring-bind (dst-id . p) entry
                       (cons dst-id (if (numberp p) p 0.0d0))))
                   result))))
    ;; If any numeric type error escapes ACT-R, degrade to NIL.
    (type-error (c)
      (declare (ignore c))
      nil)))

(defun time-offset (timestamp)
  (check-type timestamp real)
  (when (< timestamp *last-click*)
    (error "Clicks appear to be arriving out of time sequence (~A, ~A)"
           *last-click* timestamp))
  (setf *last-click* timestamp)
  (unless *time-origin*
    (setf *time-origin* (- timestamp 2)))
  (let ((result (- timestamp *time-origin*)))
    (assert (> result 0))
    result))

;; Basic structs
(defun run-model (json-plist)
    (let* ((*package* (find-package :expert-mind))
           (cg (make-code-graph :code-id (node-code-id (make-node-from-json json-plist)) 
                                :nodes (make-node-from-json json-plist) 
                                :workflow (make-workflow)))
           )
                (let* ((nodes (code-graph-nodes cg))
                       (node  (if (typep nodes 'sequence) (elt nodes 0) nodes))                      
                       (task (node-code-id node))
                       (embeddings (node-embeddings node)))
                      (vom:info "~S" (code-graph-nodes cg))
                      (reset)
                      (actr-time -1.0)
                      (dolist (chunk-desc 
                               (cdr (assoc task *initial-data* :test #'equalp)))
                          (learn (iter (for (key val) :in chunk-desc)
                               (collect (list key val)))));(and val (intern val)))))))
                      (actr-time 1.0)
                      (setf *current-task* task)
                      (setf *history* (list nil nil))             
                     ;; main loop over embeddings
      (loop for embedding across embeddings do
            (let* ((vec (embedding-vector embedding)))
              ;; guard against bad / empty vectors
              (when (and vec (typep vec 'sequence)
                         (> (length vec) 0)
                         (numberp (elt vec 0)))
                (let* ((ts     (coerce (elt vec 0) 'double-float))
                       (offset (time-offset ts)))
                  (multiple-value-bind (future activations)
                      (with-activations
                          (safe-future-model (embedding-code-element-id embedding)
                                        offset))
                    (declare (ignore activations))
                    ;; skip if future-model returned NIL (no chunk / no blend)
                    (when future
                      (let* ((wf-node
                               (make-workflow-node
                                :src-id  (embedding-code-element-id embedding)
                                 :targets (map 'vector
                                              (lambda (entry)
                                                (destructuring-bind
                                                   (dst-id . probability)
                                                    entry
                                                  (make-target-edge
                                                   :dst-id dst-id
                                                   :probability probability)))
                                              future))))
                        (add-workflow-node (code-graph-workflow cg) wf-node)
                      )
                      )
                  )))
                  ))
                      )

    ;; final result
    (code-graph-to-plist cg)))

;(defun run-model (json-plist)
;   "Decode JSON-plist, build NODES from its :NODES field, and
;    return a new CODE-GRAPH reusing OLD-GRAPH's workflow."
;    (let* ((nodes-plist-vec (getf json-plist :NODES)) ; vector of node plists
          ;(wf-plist (getf json-plist :WORKFLOW)) ; plist
;           (nodes (map 'vector
;                       #'make-node-from-json
;                       nodes-plist-vec))
           ;(workflow (make-workflow-node-from-json wf-plist)))
          ;(make-code-graph
          ; :nodes nodes
          ; :workflow workflow)
;           )
;           (nodes-to-plist-vector nodes))
          ;TODO: Do using act-up model output
;  )
         



(jh:run-standalone)