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
    (setf *initial-data* ;(let ((*package* (find-package :expert-mind)))
			   (read in)));)
  (actr-time 1))

(defun place-into-bins (alist limit &optional (bins 5))
                     (with-open-file (out #P"data.log"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
                        (format out "~S~% ~S~%" alist limit))
  (setf alist (stable-sort alist #'> :key #'cdr))
  (setf alist (subseq alist 0 (min limit (length alist))))
  (iter (with min-val := (cdar (last alist)))
        (with delta := (/ (- (cdr (first alist)) min-val) (- bins 1)))
        (for (key . val) :in alist)
        (collect (cons key
                       (if (<= delta 0)
                           1
                           (1+ (floor (/ (- val min-val) delta))))))))

(defun alist-probabilities (alist &optional (limit 3))
  "Given ALIST of (key . activation), optionally limited to LIMIT leading
   entries, compute probabilities from the activations and return an
   alist (key . probability)."
  (let* ((slice (if limit
                   (subseq alist 0 (min limit (length alist)))
                   alist))
         (activations (mapcar #'cdr slice))
         (probs (probabilities-from-activations activations)))
    (mapcar #'cons (mapcar #'car slice) probs)))


(defun future-model (id time)
 (let ((*package* (find-package :expert-mind)))
      (v:debug "Calling future-model on ~S, ~S" id time)
      (push id *history*)
      (labels ((lags (&optional include-current)
                     (let ((tags '(current lag1 lag2)))
                          (unless include-current
                              (pop tags))
                          (mapcar #'list tags *history*))))
              (vom:debug "Context is ~S" (lags))
              (prog1
                  (and (first *history*)
                       (alist-probabilities (mapcar (curry #'apply #'cons)
                                      (third (multiple-value-list
                                              (blend-vote (lags) 'current))))))
                   (learn (lags t))
                   (actr-time time)
                   (pop (cddr *history*))
                )
              )
      )
    )

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
           (cg-plist-vec (getf json-plist :NODES)) ; vector of node plists from a code-graph
           (cg (make-code-graph :nodes (map 'vector #'make-node-from-json cg-plist-vec) :workflow (make-workflow)))
           )
          ;iterate over node
          (loop for node across (code-graph-nodes cg) do ;within a single node, get the embeddings.
                (let ((task (node-code-id node))
                      (embeddings (node-embeddings node)))
                     (reset)
                     (actr-time -1.0)
                     (dolist (chunk-desc 
                               (cdr (assoc task *initial-data* :test #'equalp)))
                          (learn (iter (for (key val) :in chunk-desc)
                               (collect (list key val)))));(and val (intern val)))))))
                     (actr-time 1.0)
                     (setf *current-task* task)
                     (setf *history* (list nil nil))             
                     (loop for embedding across embeddings do 
                           ;for each embedding, run the model and collect the ouput. 
                           (let* ((ts     (coerce (with-input-from-string 
                                                     (s (embedding-embedding-id embedding)) 
                                                     (read s)) 'double-float))
                                  (offset (time-offset ts)))
                                (multiple-value-bind (future)
                                    (with-activations (future-model (embedding-code-element-id embedding) offset))
                               ;do something to keep track of this embedding
                                    (let*((wf-node (make-workflow-node :src-id (embedding-code-element-id embedding)                                                                            :targets (map 'vector (lambda (entry)
                                                     (destructuring-bind (dst-id . probability) entry
                                                         (make-target-edge :dst-id dst-id
                                                                           :probability probability)))
                                     future))))
                                     (add-workflow-node (code-graph-workflow cg) wf-node)
                                    )
                                    )           
                                )                  
                           )
                     ;TBD Place to do something to keep track of this node
                ))
          ;output final result
          (code-graph-to-plist cg)
          
          )
     ;'(:COMPLETED-WITHOUT-CRASHING t)
    )

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
         



(jh:start-server)