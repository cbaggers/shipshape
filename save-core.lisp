(in-package :shipshape)


(defun save-core (binary-path manifest)
  ;; This seems redundent but save trivial-dump-core doesnt
  ;; allow us to specify the compression for sbcl, so we have
  ;; to duplicate some functionality
  ;;
  #+sbcl (%save-executable-sbcl binary-path manifest)
  #-sbcl
  (trivial-dump-core:save-executable
   binary-path (wrap-main-function manifest)))

#+sbcl
(defun %save-executable-sbcl (filename manifest)
  (let ((init-function (wrap-main-function manifest)))
    (if (trivial-dump-core::is-slime-running)
        (trivial-dump-core::print-save-slime-and-die-help
         filename init-function)
        (sb-ext:save-lisp-and-die filename
                                  :toplevel init-function
                                  :executable t
                                  :compression (compression manifest)))))
