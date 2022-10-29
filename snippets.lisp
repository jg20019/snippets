;;;; snippets.lisp

(in-package #:snippets)

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
