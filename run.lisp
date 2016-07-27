(in-package #:shipshape)


(defun wrap-main-function (manifest)
  (let ((func-name (main-function-name manifest)))
    (unless (fboundp func-name)
      (error "shipshape: Entrypoint function ~s not found" func-name))
    (lambda () (run manifest))))


(defun run (manifest)
  ;; setup so implementations specific stuff
  #+sbcl(%run-sbcl)

  ;; re-attach all the c libraries we need
  (cl-fad:walk-directory (local-c-library-path manifest)
                         #'cffi:load-foreign-library)
  ;; kick off main func
  (funcall (symbol-function (main-function-name manifest)))

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
