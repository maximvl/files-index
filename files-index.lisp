;;;; files-index.lisp

(in-package #:files-index)

;;; "files-index" goes here. Hacks and glory await!

(defparameter *db* (make-hash-table :test 'equal))
(defparameter *index-properties* (make-hash-table))

;; db stuff

(defun dump-db (&key (stream t))
  (mapc #'(lambda (e)
            (format stream "~s~%" 
                    (cons (car e)
                          (alexandria:hash-table-alist (cdr e)))))
        (alexandria:hash-table-alist *db*)))

(defun restore-db (&key (stream t))
  (handler-case
      (loop for obj = (read stream)
            do (setf (gethash (car obj) *db*)
                     (alexandria:alist-hash-table (cdr obj))))
    (end-of-file () t)))

(defun clear-db ()
  (setf *db* (make-hash-table :test 'equal)))

;; indexing

(defun index-dir (dir)
  (cl-fad:walk-directory dir #'indexer
                         :directories t
                         :test #'not-skip-path
                         :follow-symlinks nil))

(defun not-skip-path (path)
  (not (skip-path path)))

(defun skip-path (path)
  (or (cl-fad:directory-pathname-p path)
   (find-if #'hidden-path-p (split-path path))))

(defun hidden-path-p (path)
  (and (> (length path) 1)
       (equal (aref path 0) #\.)))

(defun split-path (path)
  (remove-empty (split-sequence:split-sequence #\/ (namestring path))))

(defun indexer (path)
  (loop for k being the hash-keys in *index-properties*
          using (hash-value v)
        do (when v (add-property (namestring path) k (funcall v path)))))

;; files properties

(defun add-property (path prop-name prop-value)
  (multiple-value-bind (props exists) (gethash path *db*)
    (let ((props (if exists props
                     (let ((hash (make-hash-table)))
                       (setf (gethash path *db*) hash)
                       hash))))
      (setf (gethash prop-name props) prop-value))))

(defun get-tags (string)
  (mapcar #'string-downcase
          (remove-empty
           (split-sequence:split-sequence-if
            #'(lambda (c) (find c "-_ .\"',;!?[]{}()+<>#%="))
            string))))

(defmacro defproperty (name body)
  `(setf (gethash ',name *index-properties*) (lambda (path) ,body)))

(defproperty size
    (handler-case
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (file-length in))
      (error (e) (format t "~a~%" e) 0)))

(defproperty type
    (let ((type (pathname-type path)))
      (when type (string-downcase type))))

(defproperty path-tags (reduce #'(lambda (acc n)
                                   (append acc (get-tags n)))
                               (split-path 
                                (cl-fad:pathname-directory-pathname
                                 path)) :initial-value nil))

(defproperty name-tags (get-tags (pathname-name path)))

;; lookups

(defun to-bytes (val &optional (spec nil))
  (cond
    ((typep val 'cons)
     (reduce #'+ (mapcar #'(lambda (v) (apply #'to-bytes v)) val)))
    ((equal spec :k) (* val 1024))
    ((equal spec :m) (* val 1024 1024))
    ((equal spec :g) (* val 1024 1024 1024))
    (t val)))

(defmacro lookup (body)
  (let ((prop-names (alexandria:hash-table-keys *index-properties*)))
    `(loop for k being the hash-keys in *db*
             using (hash-value props)
           when (let ,(loop for name in prop-names
                            collect (list name `(gethash ',name props)))
                  (declare (ignorable ,@prop-names))
                  ,body)
             collect k)))


;; utils

(defun remove-empty (seq)
  (remove-if #'(lambda (x) (zerop (length x))) seq))

(defun show-props (path)
  (alexandria:hash-table-alist (gethash path *db*)))

