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

(defmethod cname ((obj null))
  "")

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
  ;; temp names are VTG_NAME_ID
  (let* ((cname (aval :NAME form))
	;; (anon (and cname   (zerop(length cname))))
	 (id    (aval :ID   form)))
    (let ((ret
	   (if istag
	       (format nil "$~A$~A" cname id )
	       cname)))
      ret)))

;;==============================================================================
;; Handle top-level name creation, including forward references.
;; When we know the cname in advance (useful for fake vbase type), this works.
(defun maybe-new-p (cname location &optional objtype)
  "make sure we have a name and object, (values obj oldp)"
  (multiple-value-bind (name exists); get or create name, setting 'exists'
      (ensure-gethash cname *names* (make-instance 'name :cname cname))
    (with-slots (loc obj) name
      (when exists
	(unless (eq (type-of obj) objtype)
	  (if (eq objtype '|typedef|)
	      (warn "Redefining ~A from ~A to typedef" cname (type-of obj))
	      (error "Incompatible redefinition of ~A from ~A to ~A"
		     cname (type-of obj) objtype))))
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
	  (parse+ sym form )) ;;parse using that type...
	(when-let ((name (gethash str *names*))) ;;a a declared type
	  (obj name)
	  )
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
;; called by parse+ handlers
(defparameter *current-obj* nil)
(defun parse-top-prim (type istag form &rest keys)
  (let ((*current-obj* (maybe-new form istag type)))
    (parse-named-keys *current-obj* form keys)))
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

(defun names-rehash ()
  (let ((names (vals *names*)))
    (init)
    (dolist (name names)
      (setf (gethash (cname name) *names*) name))
    (hash-table-count *names*))
  )

;; typedefs referring to anonymous compound types should give them their names...

;;
(defun typedef-p (obj)
  "if typedef, return type"
  (and (typep obj '|typedef|)
       (-type obj)))

(defun pointer-p (obj)
  (and (typep obj 'pointer)
       (-type obj)))

(defun named-p (obj)
  (and (typep obj 'cl-named)
       (prefname obj)))


;; rename
(defun rename-typedef-targets ()
  (loop for typedef in (select :ctype '|typedef|)
     for targetname = (prefname (-type typedef))
     when targetname
     when (cl-ppcre:scan "\\$\\$" (cname targetname))
  ;;   collect (cname targetname)
     do (setf (cname targetname)
	      (concatenate 'string "@" (cname (prefname typedef)))))
  (names-rehash))
(defun objects ()
  (loop for name being the hash-values in *names*
     collecting (obj name)))
;;
;; go across a list of objects, selecting by type and or regex
(defun select ( &key ctype regex (list (objects)))
  "select objects that satisfy regex exp on a specific slot from list"
  (let ((result
	 (loop for obj in list
	    when (or (not ctype)
		     (eq (type-of obj) ctype))
	    when (or (not regex)
		     (and (prefname obj)
			  (cl-ppcre:scan regex (cname (prefname obj)))))
					;     when (and slotval    
	    collect obj)))
    (values result (length result))))


