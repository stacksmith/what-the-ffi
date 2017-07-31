(in-package :wtf)
;; accounting for items in project

(defparameter *pulled* (make-hash-table :test 'equal))
(defparameter *pullseq* nil)

(defun pull ()
  (clrhash *pulled*)
  (setf *pullseq* nil)
  (mapc #'pull-one *selected*)
  (hash-table-count *pulled*))

(defun pull-one (name)
  "pull a name and its dependencies..."
  (let ((item (gethash name *names*)))
    (unless item (error "Name ~A not found" name))
    (pull-  item :fiat))
  )


(defun pull-prim (item by &key (key (slot-value item 'name) ))
  (let ((value (gethash key *pulled*)))
    (unless value (push key *pullseq*))
    (setf (gethash key *pulled*)
	  (cons by value)))

  key)
;; types are often represented by a string name...
;; just keep it going
(defmethod pull- ((item string) by)
  (pull- (resolve item) by))
;; generally, pull routines pull in dependencies and
;; themselves only after.. This macro simplifies that...
;; Actually because of hashtables, sequence does not matter...
(defmacro pull-deps (item by &body body)
  `(let ((myname (slot-value ,item 'name)))
     ,@body
     (pull-prim ,item ,by)))

(defmethod pull- ((item vstruct) by)
  (pull-deps item by
    (loop for field in (vstruct-fields item)
       do (pull- (field-type field) myname))))

(defmethod pull- ((item vunion) by)
  (pull-deps item by
    (loop for field in (vunion-fields item)
       do (pull- (field-type field) myname))))

(defmethod pull- ((item vfunction) by)
  (pull-deps item by
    (loop for parameter in (vfunction-parameters item)
       do (pull- (parameter-type parameter) myname))
    (pull- (vfunction-rtype item) myname)))

(defmethod pull- ((item vtypedef) by)
  (pull-deps item by
    (pull- (vtypedef-type item) myname)))

(defmethod pull- ((item pointer) by)
  (pull- (pointer-type item) by))

(defmethod pull- ((item varray) by)
  (pull- (varray-type item) by))

(defmethod pull- ((item vbase) by)
  )
(defmethod pull- ((item venum) by)
  (pull-prim item by)
)
