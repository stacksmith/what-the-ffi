(in-package :wtf)

(defparameter *pulls* nil) ;maps obj - who needs them?
(defparameter *pullseq* nil)
(defparameter *topull* nil)

(defun pull (items)
  (setf *pulls* (make-hash-table :test 'equal)
	*pullseq* nil)
  (dolist (item items) (pullp item :fiat))
  (setf *pullseq* (reverse *pullseq*))
 )

(defun pullp (item by)
  (let ((oldpull (gethash item *pulls*)))
    (unless oldpull
      (mapc (lambda (dep)
	      (pullp dep item))
	    (needs item ))
      (push item *pullseq*); register item as new pull
      ) 
    (setf (gethash item *pulls*) (cons by oldpull))))




(defun gen ()
  (mapc #'gendef- *pullseq*))

(defmethod gen- ((item vbase) )
  (format t "~S"(value item)))
(defmethod gen- ((item vtypedef))
  (gen- (vtype item)))
(defmethod gen- ((item venum))
  (format t "~A" (lisp item)))
(defmethod gen- ((item vstruct))
  (format t "~A" (lisp item)))

(defmethod gen- ((item vunion))
  (format t "~A" (lisp item)))


(defmethod gendef- ((item vbase)))
(defmethod gendef- ((item vtypedef) )
  (format t "~%(defctype ~A " (c item))
  (gen- (vtype item))
  (format t ")"))
(defmethod gendef- ((item vfunction) )
  (format t "~%(defcfun (\"~A\" ~A)  " (c item) (lisp item))
  (gen- (rtype item))
;;  (gen- (vtype item))
  (format t ")"))
(defmethod gendef- ((item venum) )
  (format t "~%(defcenum ~A" (lisp item))
  (dolist (efield (fields item))
    (format t "~%  (~A ~A)" (lisp efield) (value efield)))
  (format t ")"))
(defmethod gendef- ((item vstruct) )
  (format t "~%(defcstruct ~A" (lisp item))
  (dolist (field (fields item))
    (format t "~%  (~A " (lisp field))
    (gen- (vtype field))
    (format t ")"))
  (format t ")"))

(defmethod gendef- ((item vunion) )
  (format t "~%(defcunion ~A" (lisp item))
  (dolist (field (fields item))
    (format t "~%  (~A " (lisp field))
    (gen- (vtype field))
    (format t ")"))
  (format t ")"))
