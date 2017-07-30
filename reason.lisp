(in-package :wtf)
;; accounting for items in project

(defparameter *included* (make-hash-table :test 'equal))

(defun include (name)
  "include a name"
  (let ((item (gethash name *names*)))
    (unless item (error "Name ~A not found" name))
    (include-  item :fiat))
  *included* 
  )

(defun include-prim (item by &key (key (slot-value item 'name) ))
  (setf (gethash key *included*) by)
  key)


(defmethod include- ((item vstruct) by)
  (let ((includer (include-prim item by)))
    (loop for field in (vstruct-fields item)
       do (include- (field-type field) includer))))

(defmethod include- ((item vunion) by)
  (let ((includer  (include-prim item by)))
    (loop for field in (vunion-fields item)
       do (include- (field-type field) includer))))

(defmethod include- ((item vfunction) by)
  (let ((includer (include-prim item by)))
    (loop for parameter in (vfunction-parameters item)
       do (include- (parameter-type parameter) includer))
    (include- (vfunction-rtype item) includer)))

(defmethod include- ((item vtypedef) by)
  (let ((includer (include-prim item by)))
    (include- (vtypedef-type item) includer)))

(defmethod include- ((item pointer) by)
  (include- (pointer-type item) by))

(defmethod include- ((item varray) by)
  (include- (varray-type item) by))

(defmethod include- ((item vbase) by)
)


