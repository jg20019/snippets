;;;; snippets.lisp

(in-package #:snippets)

;;; ENV ;;;

(defparameter *env* (make-hash-table :test #'equal))

(defun get-env (name) 
  "Get value for name in env."
  (gethash name *env*))

(defun set-env (name value)
  "Set value for name in env to value"
  (setf (gethash name *env*) value))

(defun load-env ()
  "Load .env file"
  (labels ((load-env* (line)
             "Save line of form key=value into the environment"
             (let* ((index (position #\= line))
                    (key (subseq line 0 index))
                    (value (subseq line (1+ index))))
               (set-env key value))))
    (with-open-file (env-file (asdf:system-relative-pathname :snippets ".env"))
      (loop for line =  (read-line env-file nil nil)
        while line
        do (load-env* line)))))

(defun list-env () 
  "Print key=value pairs that are in the *env*"
  (loop for key being the hash-keys of *env* 
        using (hash-value value)
        do (format t "~a=~a~%" key value)))

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

(defun connect-db ()
  (postmodern:connect-toplevel 
    (get-env "POSTGRES_DB")
    (get-env "POSTGRES_USER")
    (get-env "POSTGRES_PASSWORD")
    (get-env "POSTGRES_HOST")
    :port (parse-integer  (get-env "POSTGRES_PORT"))))

;;; DAO CLASSES ;;;

#+nil
(defclass user () 
  ((id :col-type integer 
       :col-identity t
       :accessor user-id)
   (email :col-type string 
          :initarg :email
          :reader user-email)
   (password :col-type string
             :initarg :password
             :reader user-password)
   (:metaclass postmodern:dao-class)
   (:table-name users)))

;;; TABLES ;;;

(defun create-user-table () 
  (postmodern:query (:create-table 'users
                     ((id :type integer :primary-key t :identity-always t)
                      (email :type :text :unique t :check (:<> 'email ""))
                      (password :type :text :check (:<> 'password ""))))))


;;; USERS & AUTHENTICATION ;;;

(defun save-user (email password) 
  "Save user in database"
  (let ((password-hash (cl-pass:hash password)))
    (postmodern:query (:insert-into 'users 
                       :set 'email email 'password password-hash))))

(defun get-user (email) 
  "Get user by the given email"
  (first (postmodern:query (:select 'email 'password :from 'users :where (:= 'email email)))))

(defun authenticatep (email password) 
  "Check if user with email and password exists"
  (let ((user (get-user email)))
    (when user 
      (cl-pass:check-password password (second user)))))

