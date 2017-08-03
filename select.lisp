(in-package :wtf)
;; accounting for items in project

(defparameter *selected* nil)


(defun select-auto (+files -files +names -names )
  (let ((items (hash-table-values *names*)))
    (setf *selected*
	  (loop for item in items
	     as location = (slot-value item 'location)
	     as c = (slot-value item 'c)
	     unless (or (not (vfunction-p item))
			(and  (or (included-p c -names)
				  (and (included-p location -files)
				       (not (included-p c +names))))
			      (not (or (included-p c +names)
				       (and (included-p location +files)
					    (not (included-p c -names)))))))
	     collect item
	       ))))
