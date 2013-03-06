;;;; injectables - Like global variables.. only worse.

(in-package #:injectables)

(defparameter *inj* (make-hash-table))

(defun injectable (name &optional (default nil default?))
  (multiple-value-bind (val exists?) (gethash name *inj*)
    (if exists? val (when default? 
                      (setf (gethash name *inj*) default)))))

(defun (setf injectable) (value name &optional ignore)
  (if ignore
      (error "Cannot setf ^ injector")
      (setf (gethash name *inj*) value)))

(defun flush (name)
  (remhash name *inj*))

(defun flush-all ()
  (setf *inj* (make-hash-table))
  t)

(set-dispatch-macro-character #\# #\@ 
   (lambda (stream char1 char2)
     (declare (ignorable char1 char2))
     (let ((target (read stream nil (values) t)))
       (if (symbolp target)
           (list 'injectable (list 'quote (intern (write-to-string target) 
                                                  (quote injectables))))
           (error "Target must be a symbol")))))

;;[TODO] I'm thinking that this should probably produce macros 
;;       which fully exapnd to check the sanity of the form
;;       for now this will do.
(set-dispatch-macro-character #\# #\^
   (lambda (stream char1 char2)
     (declare (ignorable char1 char2))
     (let* ((peek (peek-char t stream nil nil nil))
            (func? (equal peek #\'))
            (peek (if func?
                      (progn (read-char stream nil nil nil)
                             (peek-char t stream nil nil nil))
                      peek)))
       (if (equal peek #\( )
           (progn (read-char stream nil nil nil)
                  (let* ((target (read-delimited-list #\) stream t))
                         (name (first target))
                         (value (second target)))
                    (if (eql 2 (length target))
                        `(injectable ',(intern (write-to-string name) 
                                               'injectables)
                                     ,(if func? `(function ,value) value))
                        (error "Injectables overiding lambdas must be given a name"))))
           (let* ((target (read stream nil (values) t))
                  (target (if func? 
                              (if (consp target) (cadr target) target) 
                              target)))
             (if (symbolp target)
                 `(injectable ',(intern (write-to-string target) 'injectables)
                              ,(if func? `(function ,target) target))
                 (error "Injectables overiding values or lambdas must be given a name")))))))

;; #^a
;; #^(a 1.0)
;; #^(a '(1 2 3))

;; #^'+
;; #^'(b (lambda (x) x))

;; #@a
;; (setf #@a 2.0)
;; (setf #@b #'(lambda (a) (print a)))
