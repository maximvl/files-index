;;;; package.lisp

(defpackage #:files-index
  (:use #:cl)
  (:export #:index-dir
           #:lookup
           #:to-bytes
           #:dump-db
           #:restore-db
           #:clear-db
           #:show-props))

