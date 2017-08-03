(in-package :wtf)
;; After pulling, we may need some help..

(defun listify-parameter (in)
  (typecase in
    (list in)
    (hash-table (hash-table-keys in))
    (string (list in))
    (t (error "~A cannot be listified"in )))
  )

(defun what-files (in)
  (remove-duplicates
   (loop for identifier in (listify-parameter in)
      collect ;(slot-value (resolve identifier) 'location   )
	(first (split-seq (slot-value (resolve identifier) 'location) ":"))
	)
   :test 'equal) )

;; look for promising defines
(defun what-defines (fpath)
  (loop for fname in (listify-parameter fpath) do
       (with-open-file (in fname)
	 (format t "~a~%" fname )
	 (loop for line = (read-line in nil)
	    for lineno from 1
	    while line 
	    when (and (search "#define" line)
		      (< 2 (length (split-seq line " ")))
		      )
	    do (format t "~A:~A~%" lineno line)))))
  
(defun e (location)
  (let ((fspec (split-seq location ":")))
    (uiop:run-program (format nil "leafpad --jump=~A ~A &" (or (second fspec) 0)(first fspec)
			      ))))


