;;;; snippets.asd

(asdf:defsystem #:snippets
  :description "Describe snippets here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:djula #:cl-pass #:postmodern)
  :components ((:file "package")
               (:file "snippets")))
