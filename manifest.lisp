(in-package :shipshape)


(defvar *manifests* (make-hash-table :test #'equal))


(defclass shipping-manifest ()
  ((system :initform nil :initarg :system
	   :accessor system)
   (profile :initform :ship :initarg :profile
	    :accessor profile)
   (c-library-path :initform "" :initarg :c-library-path
		   :accessor c-library-path)
   (copy-paths :initform nil :initarg :copy-paths
	       :accessor copy-paths)))


(defun add-manifest (manifest)
  (with-slots (system profile) manifest
    (let ((key (cons system profile)))
      (when (gethash key *manifests*)
	(warn "A manifest for system ~s with build profile ~s already existed.
 Replacing" system (profile manifest)))
      (setf (gethash key *manifests*) manifest))))


(defmacro def-shipping-manifest
    (system &key (profile :ship) c-library-path copy-paths)
  `(add-manifest
    (make-instance 'shipping-manifest
		   :system ',system
		   :profile ',profile
		   :c-library-path ',c-library-path
		   :copy-paths ',copy-paths)))


(defmethod initialize-instance :after ((manifest shipping-manifest) &key)
  (print "IMPLEMENT ME! #'(initialize-instance shipping-manifest)"))
