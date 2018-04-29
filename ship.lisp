(in-package :shipshape)

(defun ship-it (system-name)
  (let ((system-name
         (or (asdf:coerce-name system-name)
             (error "Could not coerce "))))
    (unless (asdf:find-component system-name nil)
      (error "No system named ~s was found, has it been loaded yet?"
             system-name))
    (let ((src (asdf:system-relative-pathname :shipshape "build-it.lisp"))
          (dst (asdf:system-relative-pathname system-name "build-it.lisp")))
      (cl-fad:copy-file src dst :overwrite t)

      ;; If we can then lets run the script for the user..
      #+(and sbcl (or linux darwin))
      (let ((task (format nil "sbcl --load ~s --system ~s" (namestring dst) system-name)))
        (format t "will now try to run: ~a" task)
        (asdf/run-program:run-program task :output *standard-output*))

      ;; ..Otherwise let them know how to run it
      #+(or windows (not sbcl))
      (format t "Please run your implementation's equivalent of the following: sbcl --load \"build-it.lisp\" --system ~s"
              system-name))))


(defun set-sail (system-name &optional (profile +default-profile+))
  ;; force load to catch errors
  (unless (asdf:component-loaded-p system-name)
    (asdf:load-system system-name :force t))

  ;; manifest should now be available
  (let ((manifest (find-manifest system-name profile)))
    (unless manifest
      (error "No shipping manifest found for ~s with profile ~s"
             system-name profile))

    ;; delete any existing build
    (ensure-no-directory (local-media-path manifest))
    (ensure-no-directory (local-c-library-path manifest))

    ;; Copy all files
    (copy-all-media manifest)
    (copy-all-c-libs manifest)

    ;; disconnect all c-libraries
    (disconnect-all-c-libs)

    ;; we have picked a profile so the other information is redundent
    (transform-manifest-store-for-shipped profile)

    ;; and now we can save
    (let ((binary-path (merge-pathnames
                        (binary-name manifest)
                        (local-path (build-path manifest)
                                    (system manifest)))))
      (setf *shipped* t)
      (save-core binary-path manifest)
      (format t "~%Binary written to ~a" binary-path))))


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
    (let* ((system (argument "--system"))
           (profile (or (argument "--profile") +default-profile+)))
      (set-sail system profile))))
