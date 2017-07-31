(in-package :wtf)
;; accounting for items in project

(defparameter *selected* (make-hash-table :test 'equal))


(defun select-auto (+files -files +names -names )
  (let ((items (hash-table-values *names*)))
    (setf *selected*
	  (loop for item in items
	     as location = (slot-value item 'location)
	     as name = (slot-value item 'name)
	     unless (or (not (vfunction-p item))
			(and  (or (included-p name -names)
				  (and (included-p location -files)
				       (not (included-p name +names))))
			      (not (or (included-p name +names)
				       (and (included-p location +files)
					    (not (included-p name -names)))))))
	     collect name
	       ))))
