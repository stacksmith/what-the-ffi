(in-package :wtf)
;; accounting for items in project

(defparameter *included* (make-hash-table :test 'equal))

(defun show (hashtable &optional key)
  (if key
      (format t "~A~%" (gethash key hashtable))
      (maphash (lambda (key value)
		 (format t "~A . ~A~%"key value))
	       hashtable)))

(defun include (name)
  "include a name and its dependencies..."
  (let ((item (gethash name *names*)))
    (unless item (error "Name ~A not found" name))
    (include-  item :fiat))
  *included* 
  )

(defun include-resolve-tag (tagname)
  "resolve t")

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
(defmethod include- ((item venum) by)
  (include-prim item by)
)


(defun tt ()
  (let ((exclude-sources '("/usr"))
	(include-sources '("vc_dispmanx"))
	(exclude-definitions '("^vc" "^_vc" "^pthread_" "single_get_func_table"))
	(include-definitions '("^vc_dispmanx")))
    (ttt exclude-definitions exclude-sources
	 include-definitions include-sources)))


(defun select-auto (+files -files +names -names )
  (let ((items (hash-table-values *names*)))
    (loop for item in items
       as location = (slot-value item 'location)
       as name = (slot-value item 'name)
       unless (or (not (vfunction-p item))
		  (and  (or (included-p name -names)
			    (and (included-p location -files)
				 (not (included-p name +names))))
			(not (or (included-p name +names)
				 (and (included-p location +files)
				      (not (included-p name -names)))))))
      
       collect name
	 )))
