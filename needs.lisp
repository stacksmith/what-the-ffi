(in-package :wtf)
;;
;; Track dependencies of named

;;
(defun depends (named-obj)
	 (depends- named-obj)
	 )

(defmethod depends- ((obj vbase))
  nil)

(defmethod depends- ((obj vtypedef))
  (list ))
