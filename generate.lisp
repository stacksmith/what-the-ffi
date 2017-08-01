(in-package :wtf)


(defun generate-one (identifier)
  (generate- (resolve identifier)))

(defmethod generate- ((item vbase))
  (print (vbase-value item)))

(defmethod generate- ((item vtypedef))
  (generate- (resolve (vtypedef-type item)))
  )

(defmethod generate- ((item vfunction))
  (with-slots (name location parameters rtype variadic storage inline) item
    (format t "(defcfun (\"~A\" ~A) ~A~%" name name  (resolve rtype))
    (format t ")~%"))
  )


