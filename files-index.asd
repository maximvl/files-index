;;;; files-index.asd

(asdf:defsystem #:files-index
  :serial t
  :description "Indexes files and makes searches"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-fad
               #:split-sequence
               #:alexandria)
  :components ((:file "package")
               (:file "files-index")))

