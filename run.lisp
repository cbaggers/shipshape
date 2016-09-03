(in-package #:shipshape)


(defun wrap-main-function (manifest)
  (let ((func-name (main-function-name manifest)))
    (unless (fboundp func-name)
      (error "shipshape: Entrypoint function ~s not found" func-name))
    (lambda () (run manifest))))


(defun load-c-library (path)
  (if (char= (aref (pathname-name path) 0) #\.)
      (when (equal (pathname-name path) *expected-libs-file*)
        (with-open-file (s path)
          (loop :for line := (read-line s nil) :while line :do
             (format t "~%---LOADING SYSTEM LIB ~s" line)
             (cffi:load-foreign-library line))))
      (cffi:load-foreign-library path)))


(defun run (manifest)
  ;; setup so implementations specific stuff
  #+sbcl(%run-sbcl)

  ;; re-attach all the c libraries we need
  (cl-fad:walk-directory (local-c-library-path manifest) #'load-c-library)

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
