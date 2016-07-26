(in-package :shipshape)

(defun ship-it (system-name)
  (unless (asdf:component-loaded-p system-name)
    (error "No system named ~s was found, has it been loaded yet?"
	   system-name))
  (let ((src (asdf:system-relative-pathname :shipshape "build-it.lisp"))
	(dst (asdf:system-relative-pathname system-name "build-it.lisp")))
    (cl-fad:copy-file src dst :overwrite t)

    ;; If we can then lets run the script for the user..
    #+(or linux darwin)
    (asdf:run-shell-command dst)

    ;; ..Otherwise let them know how to run it
    #+windows
    (format t "Please run your implementation's equivalent of the following: sbcl --load "build-it.lisp" --name ~s"
	    system-name)))


(defun set-sail (system-name
		 main-function-name
		 &optional (profile +default-profile+))
  ;; force load to catch errors
  (unless (asdf:component-loaded-p system-name)
    (asdf:load-system system-name :force t))

  ;; manifest should now be available
  (let ((manifest (find-manifest system-name profile)))
    (unless manifest
      (error "No shipping manifest found for ~s with profile ~s"
	     system-name profile))

    ;; delete any existing build
    (ensure-no-directory (local-system-media-path manifest))
    (ensure-no-directory (local-c-library-path manifest))

    ;; Copy all files
    (copy-all-media manifest)
    (copy-all-c-libs manifest)

    ;; disconnect all c-libraries
    (disconnect-all-c-libs)

    ;; we have picked a profile so the other information is redundent
    (transform-manifest-store-for-shipped profile)

    ;; and now we can save
    (setf *shipped* t)
    (trivial-dump-core:save-executable
     (binary-name manifest)
     (wrap-main-function manifest main-function-name))))


(defun dock ()
  ;; Only used during development of shipshape
  (setf *shipped* nil)
  (transform-manifest-store-for-dev))


(defun command-line-invoke ()
  (labels ((argument (name)
	     (let* ((args sb-ext:*posix-argv*)
		    (index (position name args :test 'equal))
		    (value (when (and (numberp index)
				      (< index (length args)))
			     (nth (1+ index) args))))
	       value)))
    (let* ((name (argument "--name"))
	   (system (or (argument "--system") (intern name :keyword)))
	   (profile (or (argument "--profile") +default-profile+))
	   (main (or (argument "--main")
		     (concatenate 'string (string-upcase name) "::"
				  (string-upcase name)))))
      (set-sail system main profile))))
