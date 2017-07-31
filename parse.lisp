(in-package :wtf)

(defparameter *sexps* nil)


(defparameter *tags*  (make-hash-table :test 'equal))
(defparameter *names* (make-hash-table :test 'equal))

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

;; Top-level types are described with these structures.
(defstruct vstruct   name location  fields width align)
(defstruct vunion    name location  fields width align)
(defstruct venum     name location  fields )
(defstruct vfunction name location parameters rtype variadic storage inline )
(defstruct vtypedef  name location type)
(defstruct vbase     name location value)
;; inside, fields are described as follows:
(defstruct field     name offset width align type)
(defstruct efield    name value)
(defstruct parameter name type)
(defstruct pointer   type)
(defstruct varray    type size)
(defstruct bitfield  type width)

;;
;; Parse a form for a tagname or id if there is no name
(defun parse-tagname-or-id (form)
  (let ((name (aval :name form)))
    (if (zerop (length name))
	(format nil "VTG_~A" (aval :id form))
	name)))
;;
;; for tags only, from struct,enum,union
(defun type-from-tagname-or-id (form)
  "Return type of item; if anonymous, a numeric id"
  (gethash (parse-tagname-or-id form) *tags*))

(defun set-named-item (item key hashtable from )
  "add an item to the specified hashtable by key"
  (declare (ignore from))
  (setf (gethash key hashtable) item)
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
  (loop for form in *sexps*
     for i from 1
     do
       ;;(print i)
       (parse-top form))
  (values (hash-table-count *names*) (hash-table-count *tags*)))

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
      (":enum" (type-from-tagname-or-id form))
      (":pointer" (make-pointer :type (parse-type (aval :type form))))
      (":struct" (type-from-tagname-or-id form))
      (":union" (type-from-tagname-or-id form))
      (":array" (parse-type-array form))
      (":bitfield" (parse-type-bitfield form))
      (":function" :pointer)
      ("struct" (parse-struct form))
      ("union" (parse-union form))      
      (t (or (gethash formtag *names*)
	     (error "No handler for ~A" form))))))


;;==============================================================================
;;(:TYPE (:TAG . ":enum") (:NAME . "") (:ID . 1))
;; type should exist...
(defun parse-type-array (form)
  (make-varray :type (parse-type (aval :type form))
	       :size (aval :size form)))

(defun parse-type-bitfield (form)
  (make-bitfield
   :type (parse-type (aval :type form))
   :width (aval :width form)))

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
    (set-named-item item name  *names* "parse-typedef")))
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
      (set-named-item item designator *tags* "parse-enum")

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
    (set-named-item item designator *tags* "parse-structs"))
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
    (set-named-item item designator *tags* "parse-union"))
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
    (set-named-item item designator *names* "parse-function"))
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

;;=====================================
;; Handle source file storage efficiently:
;; Keep a hashtable of names;
;; Encode a location as a filename, line and column.
(defstruct srcloc path line column)

(defun parse-location (locstr)
  (let ((parts (split-seq locstr ":")))
    (make-srcloc
     :path (first parts)
     :line (parse-integer (second parts))
     :column (parse-integer (third parts)))))
