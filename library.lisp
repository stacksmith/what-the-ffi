;;;; test.lisp

(in-package #:wtf)

;;; "test" goes here. Hacks and glory await!

(cffi:define-foreign-library lib-dispmanx
  (t (:default "libbcm_host")) )

(cffi:use-foreign-library lib-dispmanx)

