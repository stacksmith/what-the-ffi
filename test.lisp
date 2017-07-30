;;;; test.lisp

(in-package #:wtf)





(defparameter %bcm-host-init (foreign-symbol-pointer "bcm_host_init"))
(defun bcm-host-init ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((ptr %bcm-host-init))
    (declare (type SB-SYS:SYSTEM-AREA-POINTER ptr))
    (foreign-funcall-pointer ptr () :void))  )

(defparameter %graphics-get-display-size (foreign-symbol-pointer "graphics_get_display_size"))
(defmacro %%ggg (w h)
  `(let ((ptr %graphics-get-display-size ))
      (declare (type SB-SYS:SYSTEM-AREA-POINTER ptr))
      (foreign-funcall-pointer ptr () :uint32 0 :pointer ,w :pointer ,h :void )))
(defun nullcall ())

(defun graphics-get-display-size ()
 ; (declare (optimize (speed 3) (safety 0) (debug 0)))
   ;;NOT PORTABLE, BUT EASY!
  (cffi:with-foreign-objects ((width :uint32) (height :uint32))
    (%%ggg width height)
       (values (the fixnum (cffi:mem-ref width :uint32))
	    (the fixnum (cffi:mem-ref height :uint32))  )))
    

(defcfun ("graphics_get_display_size" xxx) :int32
  (display_number  :uint16)
  (width  (:pointer :uint32))
  (height (:pointer :uint32)))

(defun shit ()
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cffi:with-foreign-objects ((width :uint32) (height :uint32))
    (xxx 0 width height)
    (values (the fixnum (cffi:mem-ref width :uint32))
	    (the fixnum (cffi:mem-ref height :uint32))  )))

(defctype fuck :uint)

(defcstruct re
  (x :uint)
  (y :uint)
  (width fuck)
  (height :uint))

;; 64 bytes
(defun x1 ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-foreign-object (q 're)
    (setf (the fixnum (foreign-slot-value q '(:struct re) 'width)) 9)
    (the fixnum (foreign-slot-value q '(:struct re) 'width))
    ))
;; 392 bytes!
(defun x2 ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((q (foreign-alloc :uint :count 4)))
    (mem-aref q :uint 2)
    (foreign-free q)))
