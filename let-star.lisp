(in-package :x.let-star)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *binder-specs* '())
  (defgeneric expand-binding (spec var val decls body))
  
  (flet ((parse-binding (form)
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
                          (values nil var val))))))))
    
    (defmacro let* ((&rest forms) &body body)
      (multiple-value-bind (body variable-decls)
          (process-declarations body)
        (labels ((rec (forms)
                   (destructuring-bind (form . forms) forms
                     (multiple-value-bind (spec var val)
                         (parse-binding form)
                       (expand-binding spec
                                       var
                                       val
                                       variable-decls
                                       (if forms
                                           (list (rec forms))
                                           body))))))
          (rec forms))))

    (defmacro define-binder ((spec var val decls body) &body binder-body)
      (let ((spec-sym (gensym "SPEC")))
        `(progn
           (pushnew ',spec *binder-specs*)
           (defmethod expand-binding ((,spec-sym (eql ,spec)) ,var ,val ,decls ,body)
             ,@binder-body)))))

  (defun process-lambda-list-with-ignore-markers (lambda-list declarations ignore-sym)
    (let ((result-declarations '())
          (ignore-sym (symbol-name ignore-sym)))
      (flet ((store-decl (decls)
               (setf result-declarations (nconc result-declarations decls))))
        (values (map-lambda-list
                 (lambda (x)
                   (cond ((string= (symbol-name x) ignore-sym)
                          (let ((ignore-var (gensym "IGNORE-")))
                            (store-decl `((ignore ,ignore-var)))
                            ignore-var))
                         (t
                          (store-decl (use-declaration x declarations))
                          x)))
                 lambda-list)
                result-declarations)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binder (nil (var (eql nil)) (val (eql nil)) decls body)
  `(let ()
     ,@body))

(define-binder (nil var (val (eql nil)) decls body)
  (let ((decl (use-declaration var decls)))
    `(let (,var)
       ,@(when decl `((declare ,@decl)))
       ,@body)))

(define-binder (nil (var list) val decls body)
  (multiple-value-bind (vars decl)
      (process-lambda-list-with-ignore-markers var decls '_)
    `(destructuring-bind ,vars ,val
       ,@(when decl `((declare ,@decl)))
       ,@body)))
  
(define-binder (nil var val decls body)
  (let ((decl (use-declaration var decls)))
    `(let ((,var ,val))
       ,@(when decl `((declare ,@decl)))
       ,@body)))

(define-binder (:mval (var list) val decls body)
  (multiple-value-bind (vars decl)
      (process-lambda-list-with-ignore-markers var decls '_)
    `(multiple-value-bind ,vars ,val
       ,@(when decl `((declare ,@decl)))
       ,@body)))

(define-binder (:slot (var list) val decls body)
  (let ((decl (mapcan (lambda (x) (use-declaration x decls))
                      (lambda-list-vars var))))
    `(with-slots ,var ,val
       ,@(when decl `((declare ,@decl)))
       ,@body)))

(define-binder (:slotval (var list) val decls body)
  (let ((val-sym (gensym "VAL")))
    (labels ((rec (vars)
               (destructuring-bind (var . rest) vars
                 (multiple-value-bind (var-name slot-name)
                     (cond ((atom var)
                            (values var var))
                           ((and (consp var) (eql (length var) 2))
                            (values (car var) (cadr var)))
                           (t
                            (error "~A is invalid, expected VAR-NAME or (VAR-NAME SLOT-NAME)" var)))
                   (let ((decl (use-declaration var-name decls)))
                     `(let ((,var-name (slot-value ,val-sym ',slot-name)))
                        ,@(when decl `((declare ,@decl)))
                        ,@(if rest
                              (list (rec rest))
                              body)))))))
      `(let ((,val-sym ,val))
         ,(rec var)))))

