(in-package :expert-mind)

(defstruct embedding
  code-element-id   ; string
  embedding-id      ; string
  address-begin     ; string or integer
  address-end       ; string or integer
  labels            ; vector of strings
  vector)           ; vector of floats

(defstruct node
  code-id           ; string
  embeddings)       ; vector of EMBEDDING

(defstruct target-edge
  dst-id            ; string
  probability)      ; float

(defstruct workflow-node
  src-id            ; string
  targets)          ; vector of TARGET-EDGE

(defstruct (workflow
             (:constructor make-workflow
                 (&key (graph (make-array 0
                                               :adjustable t
                                               :fill-pointer 0)))))
  graph)            ; vector of WORKFLOW-NODE

(defstruct code-graph
  nodes             ; vector of NODES
  workflow)         ; WORKFLOW

(defun make-embedding-from-json (emb-plist)
    (make-embedding
     :code-element-id (getf emb-plist :CODE-ELEMENT-ID)   
     :embedding-id (getf emb-plist :EMBEDDING-ID)
     :address-begin (getf emb-plist :ADDRESS-BEGIN)
     :address-end (getf emb-plist :ADDRESS-END)
     :labels (getf emb-plist :LABELS) ; already vector of strings
     :vector (getf emb-plist :VECTOR))) ; vector of floats

(defun make-node-from-json (node-plist)
    (let* ((emb-vec (getf node-plist :EMBEDDINGS)) ; vector of plists
        (embeddings (map 'vector #'make-embedding-from-json emb-vec)))
    (make-node
        :code-id (getf node-plist :CODE-ID)
        :embeddings embeddings)))

(defun make-target-edge-from-json (edge-plist)
    (make-target-edge
        :dst-id (getf edge-plist :DST-ID)
        :probability (getf edge-plist :PROBABILITY)))

(defun make-workflow-node-from-json (wf-node-plist)
  (let* ((targets-plist-vec (getf wf-node-plist :TARGETS)) ; vector of plists
         (targets (map 'vector
                       #'make-target-edge-from-json
                       targets-plist-vec)))
    (make-workflow-node
     :src-id  (getf wf-node-plist :SRC-ID)
     :targets targets)))

(defun make-workflow-from-json (wf-plist)
  (let* ((graph-plist-vec (getf wf-plist :GRAPH)) ; vector of wf-node plists
         (graph (map 'vector
                     #'make-workflow-node-from-json
                     graph-plist-vec)))
    (make-workflow
     :graph graph)))

(defun make-code-graph-from-json (json-plist)
"JSON-PLIST is the decoded top-level JSON."
    (let* ((nodes-plist-vec (getf json-plist :NODES)) ; vector of node plists
           (wf-plist (getf json-plist :WORKFLOW)) ; plist
           (nodes (map 'vector
                       #'make-node-from-json
                       nodes-plist-vec))
           (workflow (make-workflow-node-from-json wf-plist)))
          (make-code-graph
           :nodes nodes
           :workflow workflow)))

(defun embedding-to-plist (emb)
  (list :CODE-ELEMENT-ID (embedding-code-element-id emb)
        :EMBEDDING-ID    (embedding-embedding-id emb)
        :ADDRESS-BEGIN   (embedding-address-begin emb)
        :ADDRESS-END     (embedding-address-end emb)
        :LABELS          (embedding-labels emb)   ; vector
        :VECTOR          (embedding-vector emb))) ; vector

(defun node-to-plist (nds)
  (list :CODE-ID     (node-code-id nds)
        :EMBEDDINGS  (map 'vector #'embedding-to-plist
                          (node-embeddings nds))))

(defun target-edge-to-plist (edge)
  (list :DST-ID      (target-edge-dst-id edge)
        :PROBABILITY (target-edge-probability edge)))

(defun workflow-node-to-plist (node)
  (list :SRC-ID  (workflow-node-src-id node)
        :TARGETS (map 'vector #'target-edge-to-plist
                      (workflow-node-targets node))))

(defun workflow-to-plist (wf)
  (list :GRAPH
        (map 'vector #'workflow-node-to-plist
             (workflow-graph wf))))

(defun code-graph-to-plist (cg)
  "Return a plist shaped like the JSON: 
   (:NODES #(<node-plist> ...) :WORKFLOW <workflow-plist>)."
  (list :NODES
        (map 'vector #'node-to-plist
             (code-graph-nodes cg))
        :WORKFLOW
        (workflow-to-plist (code-graph-workflow cg))))

(defun nodes-to-plist-vector (nodes-vector)
  "NODES-VECTOR is a vector of NODE structs. Return a vector of node plists."
  (map 'vector #'node-to-plist nodes-vector))

(defun first-n-nodes (nodes n)
  "Return a new vector with the first N nodes of NODES."
  (let ((limit (min n (length nodes))))
    (subseq nodes 0 limit)))

; Helper to convert a string of numbers with spaces between them to a vector of double-floats amenable to further analyses by the cognitive model
(defun string-to-vector (string)
  "Converts a string of space-separated numbers into a vector of double-floats."
  (with-input-from-string (s string)
    (coerce (loop for num = (read s nil nil)
                  while num
                  collect (float num 1.0d0))
            '(vector double-float))))

(defun probabilities-from-activations (log-scores)
  "Given LOG-SCORES (a list of real numbers, e.g. log-odds or logits),
return a list of probabilities that sum to 1 using the softmax transform."
  (when (null log-scores)
    (return-from probabilities-from-activations nil))
  (let* ((max-x (reduce #'max log-scores))  ; for numerical stability
         (exp-list (mapcar (lambda (x)
                             (exp (- x max-x)))
                           log-scores))
         (sum-exp (reduce #'+ exp-list)))
    (mapcar (lambda (e) (/ e sum-exp))
            exp-list)))

(defun add-workflow-node (wf node)
  "Append WORKFLOW-NODE to WF's graph vector."
  (vector-push-extend node (workflow-graph wf))
  wf)

; Similarity Functions for various list structures including Jaccard similarity for unranked and both Rank-Bias Overlap and Sequential Rank Agreement for ordered lists, and cosine-similarity for numeric lists.
(defun multiset-jaccard (seq1 seq2 &key (test #'eql))
  "Jaccard-style similarity for multisets (bags) defined by SEQ1 and SEQ2.
Duplicates are respected via counts. Returns a float in [0,1]."
  ;; build hash tables of element -> count
  (flet ((counts (seq)
           (let ((table (make-hash-table :test test)))
             (dolist (x seq table)
               (incf (gethash x table 0))))))
    (let* ((c1 (counts seq1))
           (c2 (counts seq2))
           (intersection 0)
           (union 0))
      ;; iterate over all keys in the union of supports
      (flet ((update-for (key)
               (let* ((a (gethash key c1 0))
                      (b (gethash key c2 0)))
                 (incf intersection (min a b))
                 (incf union        (max a b)))))
        (maphash (lambda (k v) (declare (ignore v)) (update-for k)) c1)
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (unless (gethash k c1) (update-for k)))
                 c2))
      (if (zerop union)
          0.0
          (/ (float intersection) union)))))

(defun sequential-rank-agreement (rank-lists)
  "Compute sequential rank agreement (sra_d) for fully observed lists.
RANK-LISTS is a list of L ranked lists, each a permutation of the same
P items. Returns a vector of length P whose element at index (d-1) is
the sequential rank agreement at depth d, as defined in Ekstrøm et al.
\"Sequential rank agreement methods for comparison of ranked lists\"."
  (assert rank-lists)
  (let* ((l (length rank-lists))
         (p (length (first rank-lists))))
    ;; sanity: all lists same length and same item set (weak check)
    (dolist (lst (rest rank-lists))
      (assert (= (length lst) p)))
    ;; Build rank maps: item -> rank (1..P) for each list
    (let* ((rank-maps
             (mapcar (lambda (lst)
                       (let ((ht (make-hash-table :test #'equal)))
                         (loop for item in lst
                               for r from 1 do
                                 (setf (gethash item ht) r))
                         ht))
                     rank-lists))
           ;; global set of all items
           (all-items (first rank-lists))
           ;; For each item X_p: A(X_p) = sample std.dev of its L ranks
           (item-ax (make-hash-table :test #'equal)))
      (dolist (item all-items)
        ;; collect ranks across lists
        (let* ((ranks (map 'vector
                           (lambda (rm)
                             (gethash item rm))
                           rank-maps))
               (mean-r (/ (loop for r across ranks sum r) (float l 1.0d0)))
               (sum-sq 0.0d0))
          (loop for r across ranks do
                (incf sum-sq (expt (- r mean-r) 2)))
          (setf (gethash item item-ax)
                (sqrt (/ sum-sq (1- l))))))
      ;; Precompute inverse ranking R_l^{-1}(r) for fast S_d construction
      (let* ((inv-lists
               ;; for each list: vector of length P, inv[r-1] = item at rank r
               (mapcar (lambda (lst)
                         (coerce lst 'vector))
                       rank-lists))
             (seen (make-hash-table :test #'equal))
             (sra (make-array p :element-type 'double-float)))
        ;; incrementally build S_d and compute pooled SD over A(X_p)
        (loop for d from 1 to p do
              ;; update S_d: add items at rank d in each list
              (dolist (inv inv-lists)
                (let ((item (aref inv (1- d))))
                  (setf (gethash item seen) t)))
              (let ((n (hash-table-count seen)))
                (let ((sum-ax 0.0d0)
                      (sum-ax2 0.0d0))
                  (maphash (lambda (item _)
                             (declare (ignore _))
                             (let ((ax (gethash item item-ax)))
                               (incf sum-ax ax)
                               (incf sum-ax2 (* ax ax))))
                           seen)
                  (let* ((n-f (float n 1.0d0))
                         (mean-ax (/ sum-ax n-f))
                         (var (if (> n 1)
                                  (/ (- sum-ax2 (* n-f mean-ax mean-ax))
                                     (1- n-f))
                                  0.0d0)))
                    (setf (aref sra (1- d)) (sqrt var))))))
        sra))))
;(let* ((l1 '(a b c d e))
;       (l2 '(b a c e d))
;       (l3 '(a c b d e)))
;  (sequential-rank-agreement (list l1 l2 l3)))       

(defun rank-biased-overlap (list-a list-b &key (p 0.9))
  "Compute extrapolated Rank Biased Overlap (RBO) between two
ranked lists LIST-A and LIST-B, using persistence parameter P in (0,1).
Lists are treated as rankings without ties."
  (assert (and (> p 0.0) (< p 1.0)) (p) "P must be in (0,1).")
  (let* ((len-a (length list-a))
         (len-b (length list-b))
         (k (max len-a len-b))
         ;; sets seen so far
         (seen-a (make-hash-table :test #'equal))
         (seen-b (make-hash-table :test #'equal))
         (overlap 0)
         (sum-weights 0.0d0)
         (sum-overlaps 0.0d0))
    (labels ((incr-hash (table key)
               (setf (gethash key table) t)))
      (loop for d from 1 to k do
           (when (<= d len-a)
             (let ((item (nth (1- d) list-a)))
               (when (gethash item seen-b)
                 (incf overlap))
               (incr-hash seen-a item)))
           (when (<= d len-b)
             (let ((item (nth (1- d) list-b)))
               (when (and (not (gethash item seen-a))
                          (gethash item seen-b))
                 ;; already counted via list-a branch
                 )
               (when (and (gethash item seen-a)
                          (not (gethash item seen-b)))
                 (incf overlap))
               (incr-hash seen-b item)))
           (let* ((a-d (if (or (= d 0)
                               (= (+ (hash-table-count seen-a)
                                     (hash-table-count seen-b))
                                  0))
                           0.0d0
                           (/ (float overlap 0.0d0)
                              (float d 0.0d0))))
                  (weight (expt p (1- d))))
             (incf sum-weights weight)
             (incf sum-overlaps (* weight a-d))))
      ;; extrapolated RBO assumes agreement at depth k continues
      (let* ((rbo-base (/ sum-overlaps sum-weights))
             (a-k (if (zerop k) 0.0d0
                      (/ (float overlap 0.0d0)
                         (float k 0.0d0))))
             (residual (* (expt p k) a-k)))
        (+ rbo-base residual)))))

(defun cosine-similarity (seq1 seq2)
  "Cosine similarity between two numeric sequences.
If lengths differ, pad the shorter one with zeros and signal a warning."
  (let* ((len1 (length seq1))
         (len2 (length seq2)))
    (when (/= len1 len2)
      (warn "cosine-similarity: length mismatch ~A vs ~A; padding shorter sequence with zeros."
            len1 len2))
    (let* ((n (max len1 len2))
           (v1 (make-array n :element-type 'double-float :initial-element 0.0d0))
           (v2 (make-array n :element-type 'double-float :initial-element 0.0d0)))
      ;; copy existing elements
      (dotimes (i len1)
        (setf (aref v1 i) (coerce (elt seq1 i) 'double-float)))
      (dotimes (i len2)
        (setf (aref v2 i) (coerce (elt seq2 i) 'double-float)))
      ;; compute cosine similarity
      (let ((dot 0.0d0)
            (n1  0.0d0)
            (n2  0.0d0))
        (dotimes (i n)
          (let ((x (aref v1 i))
                (y (aref v2 i)))
            (incf dot (* x y))
            (incf n1 (* x x))
            (incf n2 (* y y))))
        (if (or (zerop n1) (zerop n2))
            0.0d0
            (/ dot (sqrt (* n1 n2))))))))