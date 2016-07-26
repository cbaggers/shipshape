;; usage at shell prompt:
;;
;; cd ~/myproject
;; sbcl --load "build.lisp" --name myproject

#-quicklisp (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*) asdf:*central-registry*)
(require 'sb-posix)
(setf sb-impl::*default-external-format* :utf-8)

(defun argument (name)
  (let* ((args sb-ext:*posix-argv*)
	 (index (position name args :test 'equal))
	 (value (when (and (numberp index)
			   (< index (length args)))
		  (nth (1+ index) args))))
    value))

(defparameter *name* (argument "--name"))
(defparameter *binary*
  (or (argument "--binary")
      #+win32 (concatenate 'string *name* ".exe")
      #+linux (concatenate 'string *name* ".bin")))
(defparameter *system*
  (or (argument "--system")
      (intern *name* :keyword)))

(asdf:load-system :cffi :force t)

(defvar *old* cffi:*foreign-library-directories*)
(push "/opt/local/lib/" cffi:*foreign-library-directories*)

(asdf:load-system *system* :force t)

(setf cffi:*foreign-library-directories* *old*)

(defparameter *main*
  (or (argument "--main")
      (concatenate 'string (string-upcase *name*) "::" (string-upcase *name*))))

(defun close-lib (library)
  "Closes a foreign library."
  (let* ((library (cffi::filter-pathname library))
         (lib (cffi::get-foreign-library library))
         (handle (cffi::foreign-library-handle lib)))
    (when handle
      (cffi::%close-foreign-library handle)
      t)))

(mapcar #'close-lib (alexandria:hash-table-values cffi::*foreign-libraries*))

(sb-ext:save-lisp-and-die *binary*
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A"
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (funcall (read-from-string *main*))
				      0)
			  :executable t)

;;----------------------------------------------------
;; Ideas
;;
;; Use http://www.cliki.net/trivial-dump-core instead of save-and-die
;; Then we can make the claim that this is cross implementation and get
;; it in quicklisp
;;

;; Read these! They pretty much show our technique, except a little less hacky
;; https://github.com/search?utf8=%E2%9C%93&q=close-foreign-library+save-lisp-and-die&type=Code&ref=searchresults



;;----------------------------------------------------
;; failed
;; (defvar *shipping* nil)
;; (defun jam ()
;;   (let ((r (if *shipping*
;;                ;;(asdf:system-relative-pathname *name* (format nil "media/~a" x))
;;                (merge-pathnames
;;                 "c-libs/libSOIL.dylib"
;;                 (directory-namestring (first sb-ext:*posix-argv*)))
;;                "/opt/local/lib/")))
;;     (format t "~%Using ~s" r)
;;     r))
;; (push '(jam) cffi:*foreign-library-directories*)
;; (setf *shipping* t)
;; (ql:quickload :alexandria)
;; (mapcar #'cffi:close-foreign-library (alexandria:hash-table-values cffi::*foreign-libraries*))
;;
;;
