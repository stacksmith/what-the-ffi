(in-package :wtf)

(defparameter *source* nil)


(defparameter *tags*  (make-hash-table :test 'equal))
(defparameter *names* (make-hash-table :test 'equal))

(defun read-json (file)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json file)))

(defun convert ()
  (with-open-file (spec "~/local/test/egl/spec")
    (with-open-file (out "~/local/test/egl/spec.sexp" :direction :output :if-exists :supersede )
      (let ((forms (read-json  spec)))
	(write forms :stream out)
	)))
  nil)

(defun load-sexp ()
  (with-open-file (in "~/local/test/egl/spec.sexp")
    (setf *source* (read in))
    nil))

;; Top-level types are described with these structures.
(defstruct vstruct   name location  fields width align)
(defstruct vunion    name location  fields width align)
(defstruct venum     name location  fields )
(defstruct vfunction name location parameters rtype variadic storage inline )
(defstruct vtypedef  name location type)
(defstruct vbase     name value)
;; inside, fields are described as follows:
(defstruct field     name offset width align type)
(defstruct efield    name value)
(defstruct parameter name type)
(defstruct pointer   type)
(defstruct varray    type size)
(defstruct bitfield  type width)


(defun parse-tagname-or-id (form)
  (let ((name (aval :name form)))
    (if (zerop (length name))
	(aval :id form)
	name)))
(defun type-from-tagname-or-id (form)
  "Return type of item; if anonymous, a numeric id"
  (let ((id (aval :id form))
	(name (aval :name form)))
    (if (zerop (length name))
	(gethash id *tags*)
	(or   (gethash name *names*)
	      (gethash name *tags*)))))

(defun set-item (item key hashtable from)
  "add an item to the specified hashtable by key"
  (let ((old (gethash key hashtable)))
;;    (when old (format t "~A overwriting ~A with ~A" from old item))
    (setf (gethash key hashtable) item))
  )

(defun init ()
  "Wipe symbol tables, and create base types"
  (clrhash *tags*)
  (clrhash *names*)
  (loop for (key . value) in
       '((":char" . :char)
	 (":unsigned-char" . :unsigned-char)
	 (":short" . :short)
	 (":unsigned-short" . :unsigned-short)
	 (":int" . :int)
	 (":unsigned-int" . :unsigned-int)
	 (":long" . :long)
	 (":unsigned-long" . :unsigned-long)
	 (":long-long" . :long)
	 (":unsigned-long-long" . :unsigned-long-long)
	 (":float" . :float)
	 (":double" . :double)
	 (":void" . :void)
	 ;;
	 (":signed-char" . :char)
	 (":function-pointer" . :pointer)
	 (":long-double" :long-double)
	 ("__builtin_va_list" :pointer)
	 )
     do (setf (gethash key *names*)
	      (make-vbase  :name key :value value))))


(defun parse ()
  (init)
  (loop for form in *source*
     for i from 1
     do
       ;;(print i)
       (parse-top form))
  nil)

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

(defun parse-type (form)
  "Porcess :TYPE sexps"
  (let ((formtag (aval :tag form)))
    (switch (formtag :test 'equal)
      (":enum" (parse-type-enum form))
      (":pointer" (parse-type-pointer form ))
      (":struct" (parse-type-struct form))
      (":union" (parse-type-union form))
      (":array" (parse-type-array form))
      (":bitfield" (parse-type-bitfield form))
      (":function" (parse-type-function form))
      ("struct" (parse-struct form))
      ("union" (parse-union form))      
      (t (or (gethash formtag *names*)
	     (error "No handler for ~A" form))))))


;;==============================================================================
;;(:TYPE (:TAG . ":enum") (:NAME . "") (:ID . 1))
;; type should exist...
(defun parse-type-enum (form)

  (type-from-tagname-or-id form))

(defun parse-type-struct (form) ;;assuming that struct is named!
  (type-from-tagname-or-id form)
)

(defun parse-type-union (form) ;;assuming that struct is named!
  (type-from-tagname-or-id form)
)

(defun parse-type-pointer (form)
  (make-pointer :type (parse-type (aval :type form))))

(defun parse-type-array (form)
  (make-varray :type (parse-type (aval :type form))
	       :size (aval :size form)))

(defun parse-type-bitfield (form)
  (make-bitfield
   :type (parse-type (aval :type form))
   :width (aval :width form)))

(defun parse-type-function (form)
  :pointer)
;;----------------------------------------------------------------
;; typedef  (defstruct typedef name location type)
;;
(defun parse-typedef (form )
  (let* ((name (aval :name form))
	 (item
	  (make-vtypedef
	   :name     name
	   :location (aval :location form)
	   :type (parse-type (aval :type form)))))
    (set-item item name  *names* "parse-typedef")))
;;----------------------------------------------------------------
;; enum    (defstruct venum   tag location  fields )
;;

(defun parse-enum (form )
  (let* ((designator (parse-tagname-or-id form))
	 (fields (aval :fields form)))
    (let ((item
	   (make-venum
	    :name designator
	    :location (aval :location form)
	    :fields (mapcar #'parse-efield fields))))
      (set-item item designator *tags* "parse-enum")

      )))
;;----------------------------------------------------------------
;; efield   (defstruct efield name value);
(defun parse-efield (form)
  (make-efield
   :name (aval :name form)
   :value (aval :value form)))



;;==============================================================================
;; struct    (defstruct vstruct tag location  fields width align)
(defun parse-struct (form)
  (let* ((designator (parse-tagname-or-id form))
	 (item
	    (make-vstruct
	     :name designator
	     :location (aval :location form)
	     :width (aval :bit-size form)
	     :align (aval :bit-alignment form)
	     :fields (mapcar #'parse-field (aval :fields form)))))
    (set-item item designator *tags* "parse-structs"))
  )

(defun parse-field (form)
  (make-field
   :name (aval :name form)
   :offset (aval :bit-offset form)
   :width (aval :bit-size form)
   :align (aval :bit-alignment form )
   :type (parse-type (aval :type form))
))

;;==============================================================================
;; union    (defstruct vunion  tag location  fields width align)
(defun parse-union (form)
  (let* ((designator (parse-tagname-or-id form))
	 (item
	    (make-vunion
	     :name designator
	     :location (aval :location form)
	     :width (aval :bit-size form)
	     :align (aval :bit-alignment form)
	     :fields (mapcar #'parse-field (aval :fields form)))))
    (set-item item designator *tags* "parse-union"))
  )
;;==============================================================================
;;   (defstruct vfunction name location parameter rtype variadic storage inline )
(defun parse-function (form)
  (let* ((designator (aval :name form))
	 (item
	  (make-vfunction
	   :name designator
	   :location (aval :location form)
	   :variadic (aval :variadic form)
	   :storage  (aval :storage--class form)
	   :inline   (aval :inline form)
	   :parameters (mapcar #'parse-parameter (aval :parameters form))
	   :rtype (parse-type (aval :return-type form)))))
    (set-item item designator *names* "parse-function"))
  )

(defun parse-parameter (form)
  (make-parameter
   :name (aval :name form)
   :type (parse-type (aval :type form)))
  )


(defun parse-extern (form)
  (format t "Not implemented: EXTERN ~A~%~A~%"
	  (aval :name form)
	  (aval :location form)))
;;(defstruct field name offset width align type)
(defun ? (name)
  (gethash name *names*))

