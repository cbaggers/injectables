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


(set-macro-character #\> (get-macro-character #\)))

(set-dispatch-macro-character #\# #\^
   (lambda (stream char1 char2)
     (declare (ignorable char1 char2))
     (let* ((peek (peek-char t stream nil nil nil))
            (func? (equal peek #\'))
            (peek (if func?
                      (peek-char t stream nil nil nil)
                      peek)))
       (if (equal peek #\<)
           (progn (read-char stream nil nil nil)
                  (let* ((target (read-delimited-list #\> stream t))
                         (name (first target))
                         (value (second target)))
                    `(injectable ',(intern (write-to-string name) 
                                           'injectables)
                                 ,value)))
           (let ((target (read stream nil (values) t)))
             (if (or (symbolp target)
                     (when (consp target)
                       (equal (car target) 'quote)))
                 `(injectable ',(intern (write-to-string 
                                         (if func?
                                             (cadr target)
                                             target)) 
                                        'injectables)
                              ,(if func?
                                   `(function ,(cadr target))
                                   `(quote ,target)))                     
                 (error "Injectables overiding values must be given a name")))))))

;; Working example - REMEMBER MACROEXPAND IN REPL CAN CRASH WITH EOF
;; (set-dispatch-macro-character #\# #\<
;;    (lambda (stream char1 char2)
;;      (declare (ignorable char1 char2))
;;      (let ((target (read-delimited-list #\> stream t)))
;;        target)))

;; #^a
;; #^<a 1.0>
;; #^<a '(1 2 3)>

;; #^'+
;; #^'<b (lambda (x) x)>

;; #@a
;; (setf #@a 2.0)
;; (setf #@b #'(lambda (a) (print a)))
