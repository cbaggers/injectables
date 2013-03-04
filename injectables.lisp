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

(set-dispatch-macro-character #\# #\@ 
   (lambda (stream char1 char2)
     (declare (ignorable char1 char2))
     (let ((target (read stream nil (values) t)))
       (if (symbolp target)
           (list 'injectable (list 'quote target))
           (error "Target must be a symbol")))))

(set-dispatch-macro-character #\# #\^
   (lambda (stream char1 char2)
     (declare (ignorable char1 char2))
     (let ((target (read stream nil (values) t)))
       (if (symbolp target)
           (list 'injectable (list 'quote target) target)
           (error "Target must be a symbol")))))
