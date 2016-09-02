(in-package :shipshape)

(defun walk-dependencies (system function &key flat)
  (let* ((system (etypecase system
                   (asdf:system system)
                   ((or string symbol) (asdf:find-system system))))
         (depends-on (asdf:system-depends-on system)))
    (remove nil
            (funcall (if flat #'mapcan #'mapcar)
                     (lambda (x)
                       (remove nil
                               (cons (funcall function x)
                                     (walk-dependencies x function
                                                        :flat flat))))
                     depends-on))))

(defun find-dependencies (system &key flat)
  (remove-duplicates (walk-dependencies system #'identity :flat flat)
                     :test #'equal))
