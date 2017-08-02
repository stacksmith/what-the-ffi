(in-package :wtf)
(defun default-c-to-lisp (string)
     (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
       (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
	 (if (ppcre:all-matches "^(:_|_)" string)
	     (nstring-upcase string)
	     (nstring-upcase (nsubstitute #\- #\_ string))))))


(defstruct name-rule
  (function ))
