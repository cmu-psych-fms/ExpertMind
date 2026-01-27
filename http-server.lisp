 ;;;; Copyright 2025 Carnegie Mellon University

;;; A simple HTTP server that reads JSON data from a POST request, runs a Lisp function
;;; on a Lisp representation of the JSON, converts the return value from that function
;;; back to JSON and returns it. If there is difficulty assembling the POST data into
;;; a UTF-8 string or parsing that string as JSON it returns HTTP status 400, and if it
;;; subsequently encounters and error it returns HTTP status 500.

#-(and bordeaux-threads hunchentoot)
(ql:quickload '(:cl-interpol :alexandria :iterate :hunchentoot :babel
                :com.inuoe.jzon :cl-change-case :uiop :vom))

(defpackage :json-http
  (:nicknames :jh)
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:ht :hunchentoot) (:b babel) (:jzon :com.inuoe.jzon) (:v :vom))
  (:import-from :cl-change-case #:param-case #:snake-case)
  (:export #:start-server #:stop-server #:run-standalone))

(in-package :json-http)

(interpol:enable-interpol-syntax :modify-*readtable* t)

(defparameter *port* 9899)
(defparameter *debug* t)
(defparameter *access-log* "json-http-access.log")

(defun canonicalize-jzon (value)
  ;; Walks over the results of a jzon:parse (a) replacing hash tables with plists,
  ;; and converting object keys to Lisp keywords with hyphens.
  (etypecase value
    ((or symbol integer string) value)
    (float (coerce value *read-default-float-format*))
    (vector (map 'vector #'canonicalize-jzon value))
    (hash-table (iter (for (k v) :in-hashtable value)
                      (nconcing (list (make-keyword (string-upcase (param-case k)))
                                      (canonicalize-jzon v)))))))

(defun decanonicalize-jzon (value)
  ;; Performs the reverse transformation to canonicalize-jzon, while making object
  ;; keys all lower case snake_case strings.
  (etypecase value
    ((or (member t nil null) real string) value)
    (vector (map 'vector #'decanonicalize-jzon value))
    (list (iter (with result := (make-hash-table :test 'equal))
                (for (k v) :on value :by #'cddr)
                (setf (gethash (snake-case (string k)) result) (decanonicalize-jzon v))
                (finally (return result))))))

(defun default-run-model (json)
  ;; The function run instead of cl-user::run-model if the latter is not defined. This
  ;; function is just a stub that always returns a constant value, while also logging
  ;; some information.
  (v:info "No run-model function was available so using a default stub")
  (let ((result '(:ACTIONS
                   #((:ACTION-ID "d44cc237-9e09-4cb9-97aa-6f32831df844" :PROBABILITY 1.0)
                     (:ACTION-ID "bf93c4cc-6063-458e-afa4-b024f5c9abb6" :PROBABILITY 0.0)))))
    (format v:*log-stream* "~2%argument to default-run-model:~%~W~2%return value:~%~W~2%"
            json result)
    result))

(ht:define-easy-handler (decision :uri "/predict") ()
  (setf (ht:header-out "Content-Type") "application/json")
  (handler-case
      (let ((json (ht:raw-post-data)))
        (v:debug1 "received raw request ~S" json)
        (unless (stringp json)
          (setf json (b:octets-to-string json :encoding :utf-8)))
        (v:debug "received request ~S" json)
        (setf json (jzon:parse json))
        (handler-case
            (progn
              (setf json (canonicalize-jzon json))
              (v:debug1 "JSON converted to Lisp ~S" json)
              (let* (;(sym (find-symbol "RUN-MODEL" 'common-lisp-user))
                     (sym (find-symbol "RUN-MODEL" :expert-mind))
                     (fn (or (and sym (symbol-function sym)) #'default-run-model))
                     (result (funcall fn json)))
                (v:debug "run-model returned ~S" result)
                (setf result (jzon:stringify (decanonicalize-jzon result)))
                (v:debug "returning ~S" result)
                result))
          (error (e)
            (v:error "Error processing message: ~A" e)
            (setf (ht:return-code*) 500)
            (format nil "~A" e))))
    (error (e)
      (v:error "Error reading or parsing message: ~A" e)
      (setf (ht:return-code*) 400)
      (format nil "~A" e))))

(defvar *server* nil)

(defun stop-server (&optional (soft t))
  (let ((result *server*))
    (cond (*server*
           (v:info "Stopping ~A" *server*)
           (ht:stop *server* :soft soft)
           (v:info "~A stopped" *server*)
           (setf *server* nil))
          (t (v:warn "No server was running")))
    result))

(defun enable-debug (&optional (debug t))
  (cond ((null debug) (setf *debug* nil))
        ((not (realp debug)) (setf *debug* t))
        (t (setf debug (clamp (round debug) 0 4))
           (setf *debug* (if (zerop debug) t debug))))
  (v:config t (cond ((null *debug*) :info)
                      ((integerp *debug*) (make-keyword #?"DEBUG${*debug*}"))
                      (t :debug))))

(defun start-server (&key (port *port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
  (setf *server* (ht:start (make-instance 'ht:easy-acceptor
                                          :access-log-destination *access-log*
                                          :port port)))
  (v:info "Started ~A" *server*)
  *server*)

(defun run-standalone ()
  (handler-case (progn
                  (start-server)
                  (sleep 1e10))
    (error (e)
      (vom:crit "top level error ~A" e)
      #+SBCL (sb-debug:print-backtrace)
      (uiop:quit 1))
    #+SBCL
    (sb-sys:interactive-interrupt ()
      (stop-server)
      (vom:info "Quitting")
      (uiop:quit 0))))
