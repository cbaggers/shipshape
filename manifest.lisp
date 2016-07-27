(in-package :shipshape)


(defvar *manifests* (make-hash-table :test #'equal))


(defun key (system profile)
  (if *shipped*
      (asdf:coerce-name system)
      (progn
	(assert (keywordp profile))
	(cons (asdf:coerce-name system) profile))))


(defclass shipping-manifest ()
  ((system :initform nil :initarg :system
	   :accessor system)
   (profile :initform +default-profile+ :initarg :profile
            :accessor profile)
   (main-function-name :initform nil :initarg :main-function-name
		       :accessor main-function-name)
   (binary-name :initform nil :initarg :binary-name
            :accessor binary-name)
   (c-library-path :initform "" :initarg :c-library-path
                   :accessor c-library-path)
   (system-media-path :initform "media/" :initarg :system-media-path
                      :accessor system-media-path)
   (copy-paths :initform nil :initarg :copy-paths
               :accessor copy-paths)))


(defun find-manifest (system &optional (profile +default-profile+)
                               error-if-missingp)
  (or (gethash (key system profile) *manifests*)
      (when error-if-missingp
        (error "Manifest could not be found for system ~s with profile ~s"
               system profile))))


(defun add-manifest (manifest)
  (with-slots (system profile) manifest
    (let ((key (key system profile)))
      (when (gethash key *manifests*)
        (warn "A manifest for system ~s with build profile ~s already existed.
 Replacing" system (profile manifest)))
      (setf (gethash key *manifests*) manifest))))



(defun ensure-dir-name (path)
  (if (char= (elt path (1- (length path))) #\/)
      path
      (format nil "~a/" path)))


(defmethod initialize-instance :after ((manifest shipping-manifest) &key)
  (with-slots (system profile system-media-path c-library-path copy-paths
		      binary-name)
      manifest
    (setf system (asdf:coerce-name system)
          system-media-path (ensure-dir-name system-media-path)
          c-library-path (ensure-dir-name c-library-path)
	  binary-name (pathname-file-name
		       (etypecase binary-name
			 (symbol (string-downcase binary-name))
			 ((or pathname string) binary-name))))
    ;; we only validate the path can be a valid pathname here, we leave
    ;; checking if the file/dir exists until later as technically the target
    ;; could be generated at compile-time. We only need to check they exist
    ;; when setting sail
    (let* ((paths (mapcar #'cons (mapcar #'pathname copy-paths) copy-paths))
	   (problem-paths (mapcar #'cdr (remove-if-not #'car paths))))
      (unless (every #'identity paths)
	(error "Could not make manifest ~s for profile ~s as the following
could not make into valid pathnames:~{~%~s~}"
	       system profile problem-paths))))
  manifest)


(defun find-dependent-manifests (system &key (profile +default-profile+) flat)
  (walk-dependencies system (lambda (x) (find-manifest x profile)) :flat flat))


(defun transform-manifest-store-for-shipped (profile)
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
	       (when (eq (cdr k) profile)
		 (setf (gethash (car k) new-table) v)))
	     *manifests*)
    (setf *manifests* new-table )))


(defun transform-manifest-store-for-dev ()
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
	       (setf (gethash (key k (profile v)) new-table) v))
	     *manifests*)
    (setf *manifests* new-table )))


;;----------------------------------------------------------------------
;; Macro

(defmacro def-shipping-manifest (system main-function-name &body args)
  (assert (symbolp main-function-name))
  (destructuring-bind (profile binary-name c-library-path
			       system-media-path paths)
      (check-macro-args (parse-macro-args system args))
    `(add-manifest
      (make-instance 'shipping-manifest
		     :system ',system
		     :profile ',profile
		     :binary-name ',binary-name
		     :main-function-name ',main-function-name
		     :c-library-path ',c-library-path
		     :system-media-path ',system-media-path
		     :copy-paths ',paths))))

(defun check-macro-args (args)
  (destructuring-bind (profile binary-name c-library-path
			       system-media-path paths) args
    (assert (symbolp profile))
    (assert (stringp binary-name))
    (assert (or (stringp c-library-path)
		(pathnamep c-library-path)))
    (assert (or (stringp system-media-path)
		(pathnamep system-media-path)))
    (assert (every #'string paths))
    args))

(defun default-binary-name (system)
  #+windows
  (format nil "~a.exe" system)
  #-windows
  system)

(defun parse-macro-args (system args)
  (let* ((system (asdf:coerce-name system))
	 (profile :ship)
	 (binary-name (default-binary-name system))
	 (c-library-path "c-deps")
	 (system-media-path "sys-media")
	 (paths nil))
    (labels ((eat-something (e)
	       (etypecase (first e)
		 ((or pathname string) (eat-paths e))
		 (keyword (eat-pair e))))
	     (eat-paths (e)
	       (setf paths e))
	     (eat-pair (e)
	       (destructuring-bind (k v . rest) e
		 (set-pair k v)
		 (eat-something rest)))
	     (set-pair (k v)
	       (ecase k
		 (:profile (setf profile v))
		 (:binary-name (setf binary-name v))
		 (:c-library-path (setf c-library-path v))
		 (:system-media-path (setf system-media-path v)))))
      (eat-something args)
      (list profile
	    binary-name
	    c-library-path
	    system-media-path
	    paths))))
