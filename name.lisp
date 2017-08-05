(in-package :wtf)
;;(ql:quickload :what-the-ffi)(in-package :wtf)

;;==============================================================================
;; Make c2ffi slots match the spec names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *c2ffi-slots*  (make-hash-table :test 'equal))

  (mapc (lambda (slotname)
	  (setf (gethash slotname *c2ffi-slots*)
		`(,slotname :accessor
			    ,(intern (concatenate
				      'string "-" (symbol-name slotname)))
			    :initform nil)))
	'(name value type fields width bit-offset bit-alignment 
	  bit-size kind variadic inline storage--class parameters size)))
;; A defclass that uses the c2ffi slots
(defmacro defc2ffi (name direct-superclasses  c2ffi-slots)
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (slot)
		(gethash slot *c2ffi-slots*))
	      c2ffi-slots)))



;;==============================================================================
(defparameter *names* (make-hash-table :test 'equal)) ;string to <name>

(defclass name ()
  ((cname  :accessor cname :initarg :cname :initform nil)
   (loc :accessor loc :initarg :loc :initform nil)
   (obj :accessor obj :initarg :obj :initform nil)))


;;==============================================================================
;; parse a name from form, optionally as a tag.  Anon tags use ID as name.
(defun parse-name (form &optional istag)
  (let ((cname (aval :NAME form)))
    (if istag
	(format nil "VTG_~A" (if (zerop (length cname))
				 (aval :ID form)
				 cname))
	cname)))

;; Handle top-level name creation, including forward references.
;; We know the cname.
(defun maybe-new-p (cname location &optional objtype)
  "make sure we have a name and object, (values obj oldp)"
  (multiple-value-bind (name exists); get or create name, setting 'exists'
      (ensure-gethash cname *names* (make-instance 'name :cname cname))
    (with-slots (loc obj) name
      (push location loc)
      (values (if exists 
		  obj ;existing names return their existing object;
		  (setf obj ;new names get a new object.
			(make-instance objtype :prefname name)))
	      exists))))
;;-------------------------------------------------------------------------------
;; Top-level name parsing and find or create name and object.
(defun maybe-new (form istag &optional objtype)
  "make sure we have a name and object, (values name obj oldp)"
  (maybe-new-p (parse-name form istag)
	       (aval :LOCATION form)
	       objtype))
;;==============================================================================
;;==============================================================================
;;==============================================================================
;; The generic function allows us to specialize the subform parsers
(defgeneric parse-key-form (obj key subform))
;;-------------------------------------------------------------------------------
;; Normally, we just return the subform for the key, but some
;; keys such as pointers, fields, etc. require more parsing.
(defmethod parse-key-form ((obj t) (key t) subform)
  subform)
;;-------------------------------------------------------------------------------
;; generic type parser
(defmethod parse-key-form ((obj t) (key (eql :type)) form)
  (parse-type form))
;;-------------------------------------------------------------------------------
;;
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
;;
;; The other key-form parsers are specialized by type. See the types below.
;;
;;-------------------------------------------------------------------------------;; The actual key parse dispatcher.
(defun parse-named-keys (obj form keys)
  (loop for key in keys do
       (let ((val (aval key form))
	     (slot (find-symbol (symbol-name key))))
	 (unless slot
	   (error "parse-named-keys: no (slot) symbol ~A in form ~A"
		  slot form))
	   (setf (slot-value obj slot)
		 (parse-key-form obj key val))))
  obj)
;;==============================================================================
;;-------------------------------------------------------------------------------
;; Parse top form, handling name and parse named keys
;;
(defun parse-top-prim (type istag form &rest keys)
  (parse-named-keys (maybe-new form istag type) form keys))
;;-------------------------------------------------------------------------------
;; Parse internal form, such as a pointer or a field,
;; creating a specific type (no name, of course), and parse named keys.
(defun parse-internal (type form &rest keys)
  (parse-named-keys (make-instance type) form keys))

;;==============================================================================
;;
;; Dispatching parsers on type...
;; 
(defgeneric parse+ (type form))
;;
;;
(defun parse-top (form)
  (parse+ (intern (aval :TAG form)) form ))


;;==============================================================================
;;==============================================================================
(defc2ffi efield () (name value))

(defun parse-efield (form)
  (parse-internal 'efield form :name :value))

;;==============================================================================
(defc2ffi field () (name bit-offset bit-alignment bit-size type))

(defun parse-field (form)
  (parse-internal 'field form :name :bit-offset :bit-alignment :bit-size :type))
;;==============================================================================
(defc2ffi parameter () (name type))

(defun parse-parameter (form)
  (parse-internal 'parameter form :name :type))


;;==============================================================================
(defc2ffi pointer () (type))

(defmethod parse+ ((type (eql '|:pointer|)) form)
  (parse-internal 'pointer form :TYPE))

;;==============================================================================
(defc2ffi bitfield () (type width))

(defmethod parse+ ((type (eql '|:bitfield|)) form)
  (parse-internal 'bitfield form :TYPE :WIDTH))

;;==============================================================================
;;==============================================================================
;;==============================================================================
(defclass cl-named ()
  ((prefname :accessor prefname :initarg :prefname )))

;;==============================================================================
(defc2ffi vbase (cl-named) (type))

;; Fake type, we initialize it explicitly...
(defun new-vbase (cname basetype)
  (let ((obj (maybe-new-p cname nil 'vbase)))
    (setf (-type obj) basetype))
  )

(defmethod parse+ ((o vbase) form)
  (:type o))

;;==============================================================================
;;
;; typedef
;;
(defc2ffi |typedef| (cl-named) (type))

(defmethod parse+ ((type (eql '|typedef|)) form)
  (parse-top-prim type nil form :TYPE))
;;==============================================================================
;;
;; extern
;;
(defc2ffi |extern| (cl-named) (type))

(defmethod parse+ ((type (eql '|extern|)) form)
  (parse-top-prim type nil form :TYPE))

;;==============================================================================
;;
;; unhandled
;;
(defc2ffi |unhandled| (cl-named) (kind))

(defmethod parse+ ((type (eql '|unhandled|)) form)
  (parse-top-prim type nil form :KIND))


;;==============================================================================
;;
;; enum
;;
(defc2ffi |enum| (cl-named) (fields))

(defmethod parse+ ((type (eql '|enum|)) form)
  (parse-top-prim type t form :FIELDS))

(defmethod parse+ ((type (eql '|:enum|)) form)
  (maybe-new form t '|enum|))

(defmethod parse-key-form ((obj |enum|) (key (eql :fields)) form)
  (mapcar #'parse-efield form)
)
;;==============================================================================
;;
;; struct
;;
(defc2ffi |struct| (cl-named) (bit-size fields))

(defmethod parse+ ((type (eql '|struct|)) form)
  (parse-top-prim type t form :BIT-SIZE :FIELDS))

;; get obj, maybe creating an empty struct
(defmethod parse+ ((type (eql '|:struct|)) form)
  (maybe-new form t '|struct|))

(defmethod parse-key-form ((obj |struct|) (key (eql :fields)) fields)
  (mapcar #'parse-field fields))

;;==============================================================================
;;
;; union
;;
(defc2ffi |union| (cl-named) (bit-size fields))

(defmethod parse+ ((type (eql '|union|)) form)
  (parse-top-prim type t form :BIT-SIZE :FIELDS))

;; get obj, maybe creating an empty struct
(defmethod parse+ ((type (eql '|:union|)) form)
  (maybe-new form t '|union|))

(defmethod parse-key-form ((obj |union|) (key (eql :fields)) form)
  (mapcar #'parse-field form))

;;==============================================================================
;;
;; function
;;
(defc2ffi |function| (cl-named) (variadic inline storage--class parameters))

(defmethod parse+ ((type (eql '|function|)) form)
  (parse-top-prim type nil form :VARIADIC :INLINE :STORAGE--CLASS :PARAMETERS))

(defmethod parse-key-form ((obj |function|) (key (eql :parameters)) form)
  (mapcar #'parse-parameter form))

;;==============================================================================
;;
;; array  (never top)
;;
(defc2ffi |array| () (type size))

;;weird
(defmethod parse+ ((type (eql '|:array|)) form)
  (parse-internal '|array| form :TYPE :SIZE))
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
  (hash-table-count *names*))
