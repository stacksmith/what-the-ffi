(in-package :wtf)
;;(ql:quickload :what-the-ffi)(in-package :wtf)
;;==============================================================================

(defparameter *names* (make-hash-table :test 'equal)) ;string to <name>

(defclass name ()
  ((cname  :accessor cname :initarg :cname :initform nil)
   (loc :accessor loc :initarg :loc :initform nil)
   (obj :accessor obj :initarg :obj :initform nil)))



;; parse a name from form, optionally as a tag
(defun parse-name (form &optional istag)
  (let ((cname (aval :NAME form)))
    (if istag
	(if (zerop (length cname))
	    (format nil "VTG_~A" (aval :ID form))
	    (format nil "VTG_~A" cname))
	cname)))

;; Since C allows definitions overdefining declarations, we shall check if the
;; name is already defined.
(defun maybe-new-p (cname location &optional objtype)
  "make sure we have a name and object, (values obj oldp)"
  (multiple-value-bind (name exists)
      (ensure-gethash cname *names* (make-instance 'name :cname cname))
    (with-slots (loc obj) name
      (push location loc)
      (values (if exists
		  obj
		  (setf obj
			(make-instance objtype :prefname name)))
	      exists))))

(defun maybe-new (form istag &optional objtype)
  "make sure we have a name and object, (values name obj oldp)"
  (maybe-new-p (parse-name form istag)
	       (aval :LOCATION form)
	       objtype))

;;-------------------------------------------------------------------------------
;; Parse top sexp, maybe creating a new one, collecting data if keys requested 
(defun parse-top-prim (type istag form &rest keys)
  (let ((obj (maybe-new form istag type)))
    (loop for key in keys do
	 (let ((val (aval key form)))
	   (setf (slot-value obj (find-symbol (symbol-name key)))
		 (parse-slot obj key val))))
    obj))

;; used for non-top, always makes instance...?
(defun parse-prim (type form &rest keys)
  (let ((obj (make-instance type)))
    (loop for key in keys do
	 (let ((val (aval key form)))
	   (setf (slot-value obj (find-symbol (symbol-name key)))
		 (parse-slot obj key val))))
    obj))

(defmethod parse-slot ((obj t) (key t) form)
    form)
;; generic type parser
(defmethod parse-slot ((obj t) (key (eql :type)) form)
  (parse-type form)
  )

(defgeneric parse+ (type form))

(defun parse-top (form)
  (parse+ (intern (aval :TAG form)) form )
	;(gethash tag *names*)
;  (error "Unable to parse ~A" form)
  )
;; A 
(defun parse-type (form)
  (let ((str (aval :TAG form)))
    (or (when-let ((sym (find-symbol str))) ; a builtin type, such as "union"
	  (parse+ sym form ))
	(when-let ((name (gethash str *names*)))
	  (if (obj name)
	      (obj name))
	  (obj name))

	(error "Unable to parse type ~A ~A" form (find-symbol str))
	)))

;;==============================================================================
;;==============================================================================
(defclass efield ()
  ((name :accessor name :initarg :name :initform nil)
   (value :accessor value :initarg :value :initform nil)))

(defun parse-efield (form)
  (parse-prim 'efield form :name :value)
#||  (make-instance 'efield
		 :name (aval :name form)
		 :value (aval :value form))
||#)

;;==============================================================================
(defclass field ()
  ((name :accessor name :initarg :name :initform nil)
   (bit-offset :accessor bit-offset :initarg :bit-offset :initform nil)
   (bit-alignment :accessor bit-alignment :initarg :bit-alignment :initform nil)
   (bit-size :accessor bit-size :initarg :bit-size :initform nil)
   (type :accessor :type :initarg :type)))

(defun parse-field (form)
  (parse-prim 'field form :name :bit-offset :bit-alignment :bit-size :type))
;;==============================================================================
(defclass parameter()
  ((name :accessor name :initarg :name :initform nil)
   (type :accessor :type :initarg :type)))

(defun parse-parameter (form)
  (parse-prim 'parameter form :name :type))

(defclass cl-named ()
  ((prefname :accessor prefname :initarg :prefname )))

;;==============================================================================
(defclass pointer ()
  ((type :accessor :type :initarg :type)))

(defmethod parse+ ((type (eql '|:pointer|)) form)
  (make-instance 'pointer :type (parse-type (aval :TYPE form))))

;;==============================================================================
(defclass bitfield ()
  ((type :accessor :type :initarg :type)
   (width :accessor width :initarg :width :initform nil)))

(defmethod parse+ ((type (eql '|:bitfield|)) form)
  (parse-prim 'bitfield form :TYPE :WIDTH))

;;==============================================================================
(defclass vbase (cl-named)
 ((type :accessor :type :initarg :type :initform nil)))

(defun new-vbase (cname basetype)
  (let ((obj (maybe-new-p cname nil 'vbase)))
    (setf (:type obj) basetype))
  )

(defmethod parse+ ((o vbase) form)
  (:type o))

;;==============================================================================
;;
;; typedef
;;
(defclass |typedef| (cl-named)
  ((type :accessor :type :initarg :type)))

(defmethod parse+ ((type (eql '|typedef|)) form)
  (parse-top-prim type nil form :TYPE))
;;==============================================================================
;;
;; extern
;;
(defclass |extern| (cl-named)
  ((type :accessor :type :initarg :type)))

(defmethod parse+ ((type (eql '|extern|)) form)
  (parse-top-prim type nil form :TYPE))

;;==============================================================================
;;
;; unhandled
;;
(defclass |unhandled| (cl-named)
  ((kind :accessor kind :initarg :kind)))

(defmethod parse+ ((type (eql '|unhandled|)) form)
  (parse-top-prim type nil form :KIND))


;;==============================================================================
(defclass |enum| (cl-named)
  ((fields :accessor :fields :initarg :fields)))

(defmethod parse+ ((type (eql '|enum|)) form)
  (parse-top-prim type t form :FIELDS))

(defmethod parse+ ((type (eql '|:enum|)) form)
  (obj (gethash (parse-name form t) *names*)))

(defmethod parse-slot ((obj |enum|) (key (eql :fields)) form)
  (mapcar #'parse-efield form)
)
;;==============================================================================
;;
;; struct
;;
(defclass |struct| (cl-named)
  ((bit-size :accessor bit-size :initarg :bit-size :initform nil)
   (fields :accessor :fields :initarg :fields)))

(defmethod parse+ ((type (eql '|struct|)) form)
  (parse-top-prim type t form :BIT-SIZE :FIELDS))

;; get obj, maybe creating an empty struct
(defmethod parse+ ((type (eql '|:struct|)) form)
  (maybe-new form t '|struct|))

(defmethod parse-slot ((obj |struct|) (key (eql :fields)) fields)
  (mapcar #'parse-field fields))

;;==============================================================================
;;
;; union
;;
(defclass |union| (cl-named)
  ((bit-size :accessor bit-size :initarg :bit-size :initform nil)
   (fields :accessor :fields :initarg :fields)))

(defmethod parse+ ((type (eql '|union|)) form)
  (parse-top-prim type t form :BIT-SIZE :FIELDS))

;; get obj, maybe creating an empty struct
(defmethod parse+ ((type (eql '|:union|)) form)
  (maybe-new form t '|struct|))

(defmethod parse-slot ((obj |union|) (key (eql :fields)) form)
  (mapcar #'parse-field form))

;;==============================================================================
;;
;; function
;;
(defclass |function| (cl-named)
  ((variadic :accessor variadic :initarg :variadic :initform nil)
   (inline   :accessor :inline  :initarg :inline :initform nil)
   (storage--class :accessor storage--class :initarg :storage--class :initform nil)
   (parameters :accessor :parameters :initarg :parameters :initform nil)))

(defmethod parse+ ((type (eql '|function|)) form)
  (parse-top-prim type nil form :VARIADIC :INLINE :STORAGE--CLASS :PARAMETERS))
#|
(defmethod parse+ ((type (eql '|:function|)) form)
  (multiple-value-bind (obj existed)
      (maybe-new form nil '|function|)
    (unless existed
      (format t "Warning: reference to undeclared function ~A"
	      (cname  (prefname obj))))))
||#
(defmethod parse-slot ((obj |function|) (key (eql :parameters)) form)
  (mapcar #'parse-parameter form)
)
;;==============================================================================
;;
;; array  (never top)
;;
(defclass |array| ()
  ((type :accessor :type :initarg :type)
   (size :accessor :size :initarg :size :initform nil)) )

;;weird
(defmethod parse+ ((type (eql '|:array|)) form)
  (parse-prim '|array| form :TYPE :SIZE))
;;==============================================================================

(defun init ()
  "Wipe symbol tables, and create base types"
  (clrhash *names*)

  (loop for (cname . vtype) in
       '((":char" . :char)
	 (":unsigned-char" . :uchar)
	 (":short" . :short)
	 (":unsigned-short" . :ushort)
	 (":int" . :int)
	 (":unsigned-int" . :uint)
	 (":long" . :long)
	 (":unsigned-long" . :ulong)
	 (":long-long" . :long)
	 (":unsigned-long-long" . :ullong)
	 (":float" . :float)
	 (":double" . :double)
	 (":void" . :void)
	 ;;
	 (":signed-char" . :char)
	 (":function-pointer" . :pointer)
	 (":function" . :pointer)
	 (":long-double" . :long-double)
	 ("__builtin_va_list" . :pointer)
;;
	 )
     do (new-vbase cname vtype)))

(defun parse-all ()
  (init)
  (mapc #'parse-top *sexps*)
  nil)
