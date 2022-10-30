;;;; snippets.lisp

(in-package #:snippets)

;;; ENV ;;;

(defparameter *env* (make-hash-table :test #'equal))

(defun load-env (line) 
  "Save key=value in the environment."
  (let* ((index (position #\= line))
         (key (subseq line 0 index))
         (value (subseq line (1+ index))))
    (set-env key value)))

(defun get-env (name) 
  "Get value for uppercased NAME in env if it exists."
  (gethash (string-upcase name) *env*))

(defun set-env (name value)
  "Set value for uppercased NAME in env to VALUE."
  (setf (gethash (string-upcase name) *env*) value))

#+nil
(with-open-file (env-file (asdf:system-relative-pathname :snippets ".env"))
  (loop for line =  (read-line env-file nil nil)
        while line
        do (load-env line)))

;;; ACCEPTOR ;;; 

(defvar *acceptor* nil "hunchentoot acceptor")

(defun start (&optional (port 4242))
  "Start server if it isn't already running"
  (unless (and *acceptor* (hunchentoot:started-p *acceptor*))
    (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
    (hunchentoot:start *acceptor*)))

(defun stop () 
  "Stop server if it is running."
  (when (and *acceptor* (hunchentoot:started-p *acceptor*))
    (hunchentoot:stop *acceptor*)))

;;; TEMPLATES ;;; 

(djula:add-template-directory (asdf:system-relative-pathname :snippets #p"templates/"))

(defparameter +index.html+ (djula:compile-template* "index.html"))

;;; ROUTES ;;; 

(hunchentoot:define-easy-handler (index :uri "/") () 
  (djula:render-template* +index.html+ nil))

;;; DATABASE ;;;
