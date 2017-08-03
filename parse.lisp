(in-package :wtf)


;;==============================================================================
;; There are many namespaces (global, tag, and one for each function, struct or
;; union).  Global names must be unique when read multiple times; local ones are
;; unique by definition, and are lists to preserve sequence.
;;
;; Here namespace is either a global namespace or owner stucture/union/enum.
(defclass name ()
  ((c         :accessor c         :initarg :c)
   (lisp      :accessor lisp                          :initform nil)
   (location  :accessor location  :initarg :location  :initform nil)
   (namespace :accessor namespace :initarg :namespace )
   (needs     :accessor needs     :initform nil))
  )

;;------------------------------------------------------------------------------
(defmethod print-object ((obj name) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (c obj))))

;;------------------------------------------------------------------------------
(defmethod initialize-instance :after ((name name) &key)
  (with-slots (namespace c lisp) name
    (when (eq (type-of namespace) 'hash-table)
      (setf  (gethash c namespace) name))
    (setf lisp (default-c-to-lisp c)))
  
  )

;;==============================================================================
(defparameter *sexps* nil)

;; global namespaces...
(defparameter *tags*  (make-hash-table :test 'equal))
(defparameter *names* (make-hash-table :test 'equal))

(defun resolve (c)
  (gethash c (if (string= "VTG_" c :end2 4)
		 *tags*
		 *names*)))
;; canonicalize type, removing indirections imposed by pointer or typedef.
(defun can-type (obj)
  (typecase obj
    (vpointer (can-type (vtype obj)))
;;    (vtypedef (can-type (vtype obj))) ;we want typedefs!!!
    (t  obj)))

(defun parse-read-json (file)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json file)))

;;
;; 
(defun parse-spec-prim (specfname sexpfname)
  "Read the JSON specfile, and convert to sexps."
  (with-open-file (spec specfname)
    (setf *sexps* (parse-read-json spec))
    (with-open-file (out sexpfname :direction :output :if-exists :supersede )
      (write *sexps* :stream out)))
  (length *sexps*))

(defun parse-load-prim (sexpfname)
  (with-open-file (in sexpfname)
    (setf *sexps* (read in))
    (length *sexps*)))
(defparameter *print-full* t)
;; Top-level types are described with these structures.
(defmethod c ((obj null))
  "()")

;;==============================================================================
;;
;; field (union or struct
;;
(defclass field  (name)
  ((offset  :accessor offset :initarg :offset)
   (width   :accessor width  :initarg :width)
   (align   :accessor align  :initarg :align)
   (vtype       :accessor vtype       :initarg :vtype)))

;;------------------------------------------------------------------------------
#|(defmethod print-object ((obj field) out)
  (if *print-full*
      (format out "<~a ~a>" (vtype obj) (c obj))
      (format out "<~a ~a>" (c  (vtype obj)) (c obj))))
|#
;;------------------------------------------------------------------------------
(defun parse-field (form owner)
  (make-instance 'field
		 :c (aval :name form)
		 :namespace owner
		 :offset (aval :bit-offset form)
		 :width (aval :bit-size form)
		 :align (aval :bit-alignment form )
		 :vtype (parse-type (aval :type form))
		 ))



;;==============================================================================
;;
;; struct
;;
(defclass vstruct   (name)
  ((fields  :accessor fields :initform nil)
   (width   :accessor width  :initform 0    :initarg :width)
   (align   :accessor align  :initform 0    :initarg :align)))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj vstruct) out)
  (if *print-full*
      (let ((*print-full* nil))
	(format out "struct ~a~a"  (c obj) (fields obj)))
      (format out "struct ~a;" (c obj) )))
||#
;;------------------------------------------------------------------------------
(defun parse-struct (form)
  (let ((item
	 (make-instance 'vstruct 
			:c (parse-tagname-or-id form)
			:namespace *tags*
			:location (aval :location form)
			:width (aval :bit-size form)
			:align (aval :bit-alignment form))))
    (with-slots (fields needs) item
      (setf fields (mapcar (lambda (field)
			     (parse-field field item))
			   (aval :fields form)))
      (setf needs
	    (remove-duplicates (mapcar (lambda (element)
					 (can-type (vtype element)))
				       fields))))
    item))
;;==============================================================================
;;
;; union
;;
(defclass vunion   (name)
  ((fields  :accessor fields :initform nil)
   (width   :accessor width  :initform 0    :initarg :width)
   (align   :accessor align  :initform 0    :initarg :align)))

;;------------------------------------------------------------------------------
(defun parse-union (form)
  (let ((item
	 (make-instance 'vunion
			:c (parse-tagname-or-id form)
			:namespace *tags*
			:location (aval :location form)
			:width (aval :bit-size form)
			:align (aval :bit-alignment form))))
    (with-slots (fields needs) item
      (setf fields  (mapcar (lambda (field)
			      (parse-field field item))
			    (aval :fields form))
	    needs (remove-duplicates
		   (mapcar (lambda (element)
			     (can-type (vtype element)))
			   fields))))
    item))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj vstruct) out)
  (if *print-full*
      (let ((*print-full* nil))
	(format out "union ~a~a;"  (c obj) (fields obj)))
      (format out "union ~a;" (c obj) )))
||#
;;==============================================================================
;;
;; enum
;;
(defclass venum   (name)
  ((fields  :accessor fields :initform nil)
   (width   :accessor width  :initform 0    :initarg :width)
   (align   :accessor align  :initform 0    :initarg :align)))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj venum) out)
  (if *print-full*
g      (let ((*print-full* nil))
	(format out "venum ~A ~:A;"   (c obj) (fields obj)))
      (format out "venum ~A;"   (c obj) ))  )
||#
  ;;------------------------------------------------------------------------------

(defun parse-enum (form )
  (let ((item
	 (make-instance 'venum
			:c (parse-tagname-or-id form)
			:namespace *tags*
			:location (aval :location form))))
    (with-slots(fields needs) item
      (setf fields
	    (mapcar (lambda (efield)
		      (parse-efield efield item))
		    (aval :fields form))))
    item))

;;==============================================================================
;;
;; efield (enum only)
;;

(defclass efield  (name)
  ((value      :accessor value      :initarg :value)))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj efield) out)
  (format out "~s ~a, "  (c obj) (value obj) ))
||#
;;------------------------------------------------------------------------------
(defun parse-efield (form owner)
  (make-instance 'efield
		 :c (aval :name form)
		 :namespace owner
		 :value (aval :value form)))

;;==============================================================================
;;
;; function
;;
(defclass vfunction  (name)
  ((parameters :accessor parameters :initform nil)
   (rtype      :accessor rtype      :initarg :rtype)
   (variadic   :accessor variadic   :initform nil  :initarg :variadic)
   (storage    :accessor storage    :initform nil  :initarg :storage)
   (vinline     :accessor vinline     :initform nil  :initarg :inline)))

(defun vfunction-p (obj)
  (eq 'vfunction (type-of obj)))
;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj vfunction) out)
  (print-unreadable-object (obj out :type t)
    (format out "LEVEL ~A" *print-level*))
;  (let ((*print-full* nil))    (format out "~A ~A~:A"  (rtype obj) (c obj) (parameters obj)))
  )

||#
;;------------------------------------------------------------------------------
(defun parse-function (form)
  (let ((item (make-instance 'vfunction
			     :c (aval :name form)
			     :namespace *names*
			     :location (aval :location form)
			     :variadic (aval :variadic form)
			     :storage  (aval :storage--class form)
			     :inline   (aval :inline form)
			     :rtype (parse-type (aval :return-type form)))))
    (with-slots (parameters needs rtype) item
      (setf parameters (mapcar (lambda (parameter)
				 (parse-parameter parameter item))
			       (aval :parameters form))
	    needs (remove-duplicates
		   (cons (can-type rtype)
			 (mapcar (lambda (parameter)
				   (can-type (vtype parameter)))
				 parameters)))))
    item))

;;==============================================================================
;;
;; parameter
;;
(defclass vparameter  (name)
  ((vtype       :accessor vtype       :initarg :vtype)))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj vparameter) out)
  (format out "~s ~a, "  (vtype obj) (c obj)))
||#
;;------------------------------------------------------------------------------
(defun parse-parameter (form owner)
  (make-instance 'vparameter
		 :c (aval :name form)
		 :namespace owner
		 :vtype (parse-type (aval :type form))))

;;==============================================================================
;;
;; typedef
;; 
(defclass vtypedef  (name)
  ((vtype       :accessor vtype       :initarg :vtype)))

;;------------------------------------------------------------------------------

#||
(defmethod print-object ((obj vtypedef) out)
  (print-unreadable-object (obj out :type t)
    (format t "print-level ~A" *print-level*))
  (if *print-full*
      (let ((*print-full* nil))
	(format out "typedef ~a ~a" (c obj)  (vtype obj)))
      (format out "~A"(c (vtype obj)))
      )
)
||#


;;------------------------------------------------------------------------------
(defun parse-typedef (form )
  (let ((item
	 (make-instance 'vtypedef
			:c     (aval :name form)
			:namespace *names*
			:location (aval :location form)
			:vtype (parse-type (aval :type form)))))
    (setf (needs item) (list (can-type (vtype item))))
    item))

;;==============================================================================
;;
;; base (built-in cffi classes)
;;
(defclass vbase  (name)
  ((value      :accessor value      :initarg :value)))

;;------------------------------------------------------------------------------
#||
(defmethod print-object ((obj vbase) out)
  (if *print-full*
      (format out "<~s>"  (value obj))
      (format out "~s" (value obj))))
||#
;;==============================================================================
;;
;; pointer  (anon, type-parser)
;;
(defclass vpointer  ()
  ((vtype       :accessor vtype       :initarg :vtype)))

;;------------------------------------------------------------------------------
#|#
(defmethod print-object ((obj vpointer) out)
  (format out "~a*"  (vtype obj)))
||#
;;------------------------------------------------------------------------------
(defmethod c ((obj vpointer))
  ":pointer")


;;==============================================================================
;;
;; array
;;
(defclass varray  ()
  ((vtype       :accessor vtype       :initarg :vtype)
   (size        :accessor size        :initarg :size)))

;;------------------------------------------------------------------------------
(defmethod c ((obj varray))
  "varray")
(defmethod needs ((obj varray))
  (list (can-type (vtype obj))))
;;------------------------------------------------------------------------------
(defun parse-type-array (form)
  (make-instance 'varray
		 :vtype (parse-type (aval :type form))
		 :size (aval :size form)))

;;==============================================================================
;;
;; bitfield
;;
(defclass bitfield  ()
  ((vtype       :accessor vtype       :initarg :vtype)
   (width   :accessor width  :initarg :width)))

;;------------------------------------------------------------------------------
(defmethod c ((obj bitfield))
  "bitfield")

;;------------------------------------------------------------------------------
(defun parse-type-bitfield (form)
  (make-instance 'bitfield
		 :vtype (parse-type (aval :type form))
		 :width (aval :width form)))

;;==============================================================================
;;
;; extern
;;
(defun parse-extern (form)
  (format t "Not implemented: EXTERN ~A~%~A~%"
	  (aval :name form)
	  (aval :location form)))
;;

;;==============================================================================
;;==============================================================================
;;==============================================================================
;;==============================================================================
;; Parse a form for a tagname or id if there is no name, in tag space
;;
(defun parse-tagname-or-id (form)
  (let ((name (aval :name form)))
    (if (zerop (length name))
	(format nil "VTG_~A" (aval :id form))
	(format nil "VTG_~A" name))))
;;
;; for tags only, from struct,enum,union
(defun type-from-tagname-or-id (form)
  "Return type of item; if anonymous, a numeric id"
					;(gethash (parse-tagname-or-id form) *tags*)
  (gethash (parse-tagname-or-id form) *tags*))
 

(defun init ()
  "Wipe symbol tables, and create base types"
  (clrhash *tags*)
  (clrhash *names*)
  (loop for (key . value) in
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
	 (":long-double" :long-double)
	 ("__builtin_va_list" :pointer)
;;
	 )
     do (make-instance 'vbase  :c key :namespace *names* :value value )))


(defun parse ()
  (init)
  (loop for form in *sexps*
     for i from 1
     do
       ;;(print i)
       (parse-top form))
  (values (hash-table-count *names*) (hash-table-count *tags*)))

(defun parse-type (form)
  "Porcess :TYPE sexps"
  (let ((formtag (aval :tag form)))
    (switch (formtag :test 'equal)
      (":enum" (type-from-tagname-or-id form))
      (":pointer" (make-instance 'vpointer :vtype (parse-type (aval :type form))))
      (":struct" (type-from-tagname-or-id form))
      (":union" (type-from-tagname-or-id form))
      (":array" (parse-type-array form))
      (":bitfield" (parse-type-bitfield form))
      (":function" (gethash ":function-pointer" *names*));; should be :pointer in *tags*?
      ("struct"  (parse-struct form));; inline definition
      ("union"  (parse-union form))  ;; inline definition
;;
      (t (or (gethash formtag *names*)
	     ;;formtag ;;just a name
	     (error "No type handler for ~A" form))))))


(defun parse-top (form)
  "Process toplevel sexps"
  (switch ((aval :tag form) :test 'equal)
    ("typedef" (parse-typedef form))
    ("enum" (parse-enum form))
    ("function" (parse-function form))
    ("struct" (parse-struct form))
    ("union" (parse-union form))
    ("extern" (parse-extern form))
    ("unhandled" (format t "UNHANDLED by c2ffi: ~A~%" form))
    (t (error "No top handler for ~A" form))))

;;----------------------------------------------------------------

