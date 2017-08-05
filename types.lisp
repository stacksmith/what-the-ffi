(in-package :wtf)
;;(ql:quickload :what-the-ffi)(in-package :wtf)

;;==============================================================================
;;==============================================================================
;;==============================================================================
;; +            +
;;    CLASSES
;; +            +
;;==============================================================================
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
;; named classes
;;
(defclass cl-named ()
  ((prefname :accessor prefname :initarg :prefname )))

;;==============================================================================
;; vbase   a fake class for the bottommost types
;;
(defc2ffi vbase (cl-named) (type))

;; Fake type, we initialize it explicitly...
(defun new-vbase (cname basetype)
  (setf (-type (maybe-new-p cname nil 'vbase))
	basetype))

(defmethod parse+ ((o vbase) form)
  (-type o))

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
  (mapcar #'parse-efield form))

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

