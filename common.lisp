(in-package :x.let-star)

(defvar *binder-specs* '())
(defvar *declaration-specs* '())

(defparameter *lambda-list-markers* '(&key &body &rest &aux &optional))
(defparameter *lambda-list-markers-with-initializer* '(&optional &key &aux))

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defun map-lambda-list (list leaf-fn
                        &optional
                        (cons-fn (lambda (x) (values t x))))
  (let ((nest-deeper t)
        (marker nil))
    (labels ((do-elem (x)
               (cond ((and nest-deeper (consp x))
                      (multiple-value-bind (nest-deeper x)
                          (funcall cons-fn x)
                        (if nest-deeper
                            (map-lambda-list x leaf-fn cons-fn)
                            (leaf x))))
                     (t (leaf x))))
             (leaf (x)
               (cond ((member x *lambda-list-markers*)
                      (setf nest-deeper nil
                            marker x)
                      x)
                     ((consp x)
                      (if (member marker *lambda-list-markers-with-initializer*)
                          (cons (funcall leaf-fn (car x)) (cdr x))
                          (error "~A after ~A in lambda-list" x marker)))
                     (t
                      (funcall leaf-fn x)))))
      (let ((before-list (butlast list))
            (last-cons (last list)))
        (let ((last-elem (cdr last-cons)))
          (if (null last-elem)
              (mapcar #'do-elem list)
              (append (mapcar #'do-elem before-list)
                      (cons (do-elem (car last-cons))
                            (do-elem last-elem)))))))))

(defun lambda-list-vars (list)
  (let ((result '()))
    (map-lambda-list list (lambda (x) (push x result)))
    (nreverse result)))

(defun merge-hash-tables (main-table other-table)
  (maphash (lambda (key val0)
             (let ((val (gethash key main-table)))
               (setf (gethash key main-table)
                     (nconc val (list val0)))))
           other-table))

(defun parse-binding (form)
  (if (atom form)
      (values nil form nil nil)
      (ecase (length form)
        (0 (values nil nil nil))
        (1 (values nil (first form) nil))
        (2 (destructuring-bind (var val) form
             (if (consp var)
                 (if (member (car var) *binder-specs*)
                     (values (car var) (cdr var) val)
                     (values nil var val))
                 (values nil var val)))))))

(defun ignore-varname-p (symbol)
  (if (and (null (symbol-package symbol))
           (string= (subseq (symbol-name symbol) 0 6)
                    "IGNORE"))
      t
      nil))

(defun ignore-symbol-p (symbol &optional (ignore-sym '_))
  (and (symbolp symbol)
       (string= (symbol-name symbol)
                (symbol-name ignore-sym))))

(defun valid-varname-p (symbol)
  (and (symbolp symbol)
       (not (null symbol))
       (not (keywordp symbol))))

(defun extract-nested-binding-specs (vars decls &key extract-plain-lists)
  (let ((bindings nil))
    (values
     (map-lambda-list
      vars
      (lambda (elem)
        (cond ((ignore-symbol-p elem)
               (let ((var-name (gensym "IGNORE-")))
                 (setf (gethash var-name decls) `((ignore ,var-name)))
                 var-name))
              ((arrayp elem)
               (let ((var-name (gensym "ARRAY-")))
                 (push (cons elem var-name) bindings)
                 var-name))
              ((valid-varname-p elem) elem)
              (t (error "invalid variable name: ~A" elem))))
      (lambda (list)
        (cond ((eql (car list) :mval)
               (error "nested :MVAL in ~A are meaningless" vars))
              ((or extract-plain-lists (member (car list) *binder-specs*))
               (let ((var-name (gensym (if extract-plain-lists
                                           "LIST-"
                                           (format nil "~A-" (car list))))))
                 (push (cons list var-name) bindings)
                 (values nil var-name)))
              (t (values t list)))))
     bindings)))



