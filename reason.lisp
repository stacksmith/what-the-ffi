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
  "include a name"
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

;;===============================
(defstruct project
  include-sources include-definitions
  exclude-sources exclude-definitions)

(defparameter *include-sources* nil)
(defparameter *exclude-sources* nil)
(defparameter *include-definitions* nil)
(defparameter *exclude-definitions* nil)
#||
((exclude-sources '("usr" "vmcs_host" "vc_cec"  "vhci" "vc_dispservice_x_defs"))
	(include-sources '("stdint" "vc_dispmanx"  "vc_image_types" "vc_display_types"))
	(exclude-definitions '("^_" "DISPMANX_DISPLAY_FUNCS_T" "vc_vchi_"))
	(include-definitions '("VCHI_MEM_HANDLE_T" "__eglMustCastToProperFunctionPointerType")))
||#
(defun tt ()
  (let ((exclude-sources '("/usr"))
	(include-sources '("vc_dispmanx"))
	(exclude-definitions '("^vc" "^_vc" "^pthread_" "single_get_func_table"))
	(include-definitions '("^vc_dispmanx")))
    (ttt exclude-definitions exclude-sources
	 include-definitions include-sources)))


(defun ttt (exclude-definitions exclude-sources
	    include-definitions include-sources)
  (let ((items (hash-table-values *names*)))
    (loop for item in items
       as location = (slot-value item 'location)
       as name = (slot-value item 'name)
       unless (or (not (vfunction-p item))
		  (and  (or (included-p name exclude-definitions)
			    (and (included-p location exclude-sources)
				 (not (included-p name include-definitions))))
			(not (or (included-p name include-definitions)
				 (and (included-p location include-sources)
				      (not (included-p name exclude-definitions)))))))
      
       collect name
	 )))
