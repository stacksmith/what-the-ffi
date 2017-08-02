(in-package :wtf)


(defun generate-one (identifier)
  (generate- (resolve identifier)))

(defmethod generate- ((item vbase))
  (print (value item)))

(defmethod generate- ((item vtypedef))
  (generate- (resolve (vtypedef-type item)))
  )

(defmethod generate- ((item vfunction))
  (with-slots (c location parameters rtype variadic storage inline) item
    (format t "~%(defcfun (\"~A\" ~A) ~A" c c  (c rtype))
    (loop for parameter in parameters do
	 (with-slots (c vtype) parameter
	   (format t "~%  (~A ~A)" c (c vtype))))
    (format t ")~%"))
  )


