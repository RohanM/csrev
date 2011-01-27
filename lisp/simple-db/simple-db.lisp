;; Simple db!

(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


;; where

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))


;; update

(defun make-update-expr (field value)
  `(setf (getf row ,field) ,value))

(defun make-updates-list (fields)
  (loop while fields
     collecting (make-update-expr (pop fields) (pop fields))))

(defmacro update (selector-fn &rest clauses)
  `(setf *db*
	 (mapcar
	  #'(lambda (row)
	      (when (funcall ,selector-fn row)
		,@(make-updates-list clauses))
	      row) *db*)))
