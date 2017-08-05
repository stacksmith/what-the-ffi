;;;; test.asd

(asdf:defsystem #:what-the-ffi
  :description "Generate and reason about ffi's"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (#:cffi #:alexandria #:cl-ppcre #:cl-json)
  :components ((:file "package")
	       (:file "util")
	       (:file "json-sexp")
	       (:file "name")
;;	       (:file "parse")
	;;       (:file "select")
;;	       (:file "project")
;;	       (:file "library")
;;               (:file "test")
	       ))

