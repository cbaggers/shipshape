(in-package #:shipshape)


(defun wrap-main-function (manifest main-function-name)
  (let ((func-name (read-from-string main-function-name)))
    (unless (fboundp func-name)
      (error "shipshape: Entrypoint function ~s not found"
	     main-function-name))
    (let ((func (symbol-function func-name)))
      (lambda ()
	(run manifest func)))))


(defun run (manifest entrypoint-func)
  ;; setup so implementations specific stuff
  #+sbcl(%run-sbcl)

  ;; re-attach all the c libraries we need
  (cl-fad:walk-directory (local-c-library-path manifest)
			 #'cffi:load-foreign-library)
  ;; kick off main func
  (funcall entrypoint-func)

  ;; return success code for the os
  0)


;;----------------------------------------------------
;; Implementation Specific Initialization

#+sbcl
(defun %run-sbcl ()
  #+unix
  (sb-posix:putenv
   (format nil "SBCL_HOME=~A"
	   #.(sb-ext:posix-getenv "SBCL_HOME"))))
