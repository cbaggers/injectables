;;;; injectables - Like global variables.. only worse.

(in-package #:injectables)

(defparameter *injvars* (make-hash-table))

(defun injectable (name &optional (default nil default?))
  (multiple-value-bind (val exists?) (gethash name *injvars*)
    (if exists? val (when default? 
                      (setf (gethash name *injvars*) default)))))

(defun (setf injectable) (value name &optional ignore)
  (declare (ignore ignore))
  (setf (gethash name *injvars*) value))

(defun flush (name)
  (remhash name *injvars*))

(defun flush-all ()
  (setf *injvars* (make-hash-table))
  t)

(set-dispatch-macro-character #\# #\@ #'injector)
(set-dispatch-macro-character #\# #\! #'injector)

(defun injector (stream char1 char2)
  (declare (ignorable char1 char2))
  (let ((target (read stream t nil t)))
    (if (symbolp target)
        (append (list 'injectable (list 'quote target))
                (when (equal char1 #\!) (list target)))
        (error "Target must be a symbol"))))
