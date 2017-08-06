(in-package :wtf)
;;(ql:quickload :what-the-ffi)(in-package :wtf)
;;
;; This could probably be more automated to parse right into slots, but
;;==============================================================================
;; Make c2ffi slots match the spec names.  Just a convenience to avoid
;; declaring classes with same slots over and over.
;;
;; Must be done before compilation takes place, as this is a compiler aid.
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *c2ffi-slots*  (make-hash-table :test 'equal))

  (mapc (lambda (slotname)
	  (setf (gethash slotname *c2ffi-slots*)
		`(,slotname :accessor
			    ,(intern (concatenate
				      'string "-" (symbol-name slotname)))
			    :initform nil)))
	'(name value type fields width bit-offset bit-alignment 
	  bit-size kind variadic inline storage--class parameters size
	  return-type)))
;; A defclass that uses the c2ffi slots
(defmacro defc2ffi (name direct-superclasses  c2ffi-slots)
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (slot)
		(gethash slot *c2ffi-slots*))
	      c2ffi-slots)))

;;==============================================================================
;; Names are kept separately from objects, to maintain maximum flexibility
;; in renaming.  Names are also associated with location information containing
;; the source file references.
;;
;; The *names* hashtable maps strings to name objects.
(defparameter *names* (make-hash-table :test 'equal)) ;string to <name>

(defclass name ()
  ((cname  :accessor cname :initarg :cname :initform nil)
   (loc :accessor loc :initarg :loc :initform nil)
   (obj :accessor obj :initarg :obj :initform nil)))

(defmethod print-object ((o name) s)
  ;;(print-unreadable-object (o s :type t))   ; :identity t
  (format s "#<~A>" (cname o)))

(defmethod show ((o name) &key)
  ;;(print-unreadable-object (o s :type t))   ; :identity t
  (format t "~A~%"  (loc o) )
  (show (obj o)))
;;==============================================================================
;; parse a name from form, optionally as a tag.  Anonymous struct etc. are
;; assigned a VGT_xxx name, where xxx is c2ffi-assigned id number.
;;
(defun parse-name (form &optional istag)
  (let ((cname (aval :NAME form)))
    (if istag
	(format nil "VTG_~A" (if (zerop (length cname))
				 (aval :ID form)
				 cname))
	cname)))

;;==============================================================================
;; Handle top-level name creation, including forward references.
;; When we know the cname in advance (useful for fake vbase type), this works.
(defun maybe-new-p (cname location &optional objtype)
  "make sure we have a name and object, (values obj oldp)"
  (multiple-value-bind (name exists); get or create name, setting 'exists'
      (ensure-gethash cname *names* (make-instance 'name :cname cname))
    (with-slots (loc obj) name
      (setf loc location);; (push location loc)
      (values (if exists 
		  obj ;existing names return their existing object;
		  (setf obj ;new names get a new object.
			(make-instance objtype :prefname name)))
	      exists))))
;;-------------------------------------------------------------------------------
;; Top-level name parsing and find or create name and object.  This one gets
;; the name from the form, although requires a hint about whether it's a topname
;; or a tag.
(defun maybe-new (form istag &optional objtype)
  "make sure we have a name and object, (values name obj oldp)"
  (maybe-new-p (parse-name form istag)
	       (aval :LOCATION form)
	       objtype))
;;==============================================================================
;;==============================================================================
;;==============================================================================
;; The generic function allows us to specialize the subform parsers
;;
(defgeneric parse-key-form (obj key subform))
;;-------------------------------------------------------------------------------
;; Normally, we just return the subform for the key, but some
;; keys such as pointers, fields, etc. require more parsing.
(defmethod parse-key-form ((obj t) (key t) subform)
  subform)
;;-------------------------------------------------------------------------------
;; generic type parser, 
(defmethod parse-key-form ((obj t) (key (eql :type)) form)
  (parse-type form))
(defmethod parse-key-form ((obj t) (key (eql :return-type)) form)
  (parse-type form))

;;-------------------------------------------------------------------------------
;; and function typ parse type from a form
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

;;-------------------------------------------------------------------------------
;; The actual key parse dispatcher.  Processes all keys, matching eponymous
;; slots with values from form.
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

(defun parse-all ()
  (init)
  (mapc #'parse-top *sexps*)
  (hash-table-count *names*))

