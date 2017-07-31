(in-package :wtf)

;;=============================================
;; A project is a repeatable configuration that
;; describes transformations from the initial spec
;; to the final ffi
(defstruct project
  specfile
  sexpfile 
  ;; inclusion parameters
  -files +files
  -names +names)


#||
(defparameter *project*
  (make-project
   ;; intake
   :specfile "~/local/test/egl/spec"
   :sexpfile "~/local/test/egl/spec.sexp"
   ;; inclusion
   :-files  '("/usr")
   :+files '("vc_dispmanx")
   :-names  '("^vc" "^_vc" "^pthread_" "single_get_func_table")
   :+names '("^vc_dispmanx")
   ))
||#
(defparameter *project*
  (make-project
   ;; intake
   :specfile "~/local/test/egl/spec"
   :sexpfile "~/local/test/egl/spec.sexp"
   ;; selection
   :-files  '("/usr")
   :+files '()
   :-names  '(".*")
   :+names '("bcm_host" "graphics")
   ;; pull dependencies
   ))

(defun spec ()
  (parse-spec-prim (project-specfile *project*)
		   (project-sexpfile *project*)))

(defun reload ()
  (parse-load-prim (project-sexpfile *project*)))

;; parse
(defun select ()
  (with-slots (+files -files +names -names) *project*
    (select-auto +files -files +names -names))
)

(defun)
