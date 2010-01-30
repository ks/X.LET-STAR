(in-package :x.let-star)

(defvar *binder-specs* '())

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defgeneric expand-binding (spec var val decls body))
  
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

(defun extract-nested-binding-specs (vars decls)
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
              ((member (car list) *binder-specs*)
               (let ((var-name (gensym (format nil "~A-" (car list)))))
                 (push (cons list var-name) bindings)
                 (values nil var-name)))
              (t (values t list)))))
     bindings)))

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binder (nil var val decls body)
  (when (ignore-symbol-p var)
    (setf var (gensym "IGNORE-")))
  `(let ((,var ,val))
     ,@(if (ignore-varname-p var)
           `((declare (ignore ,var)))
           (when-let (decl (use-declaration var decls))
             `((declare ,@decl))))
     ,@body))

(define-binder (nil (var (eql nil)) (val (eql nil)) decls body)
  `(let ()
     ,@body))

(define-binder (nil var (val (eql nil)) decls body)
  (let ((decl (use-declaration var decls)))
    `(let (,var)
       ,@(when decl `((declare ,@decl)))
       ,@body)))

(define-binder (nil (var vector) val decls body)
  (cl:let* ((length (length var))
            (rest-idx (position '&rest var))
            (val-name (gensym))
            (rest-name (if rest-idx
                           (cond ((eql rest-idx (- length 2))
                                  (let ((name (aref var (1- length))))
                                    (unless (ignore-symbol-p name)
                                      (if (valid-varname-p name)
                                          name
                                          (error "invalid variable name: ~A" name)))))
                                 ((eql rest-idx (1- length)) nil)
                                 (t (error "misplaced &rest in ~A" var)))
                           nil))
            (body (if (and rest-idx rest-name)
                      `((let ((,rest-name
                               (make-array (- (length ,val-name) ,rest-idx)
                                           :displaced-to ,val-name
                                           :displaced-index-offset ,rest-idx)))
                          ,@(when-let (decl (use-declaration rest-name decls))
                                      `((declare ,@decl)))
                          ,@body))
                      body)))
    `(let ((,val-name ,val))
       ,@(unless rest-idx
                 `((assert (eql (length ,val-name) ,length) nil
                           "expected vector of length ~A" ,length)))
       ,@(reduce (lambda (binding body)
                   (destructuring-bind (var-name . idx) binding
                     (cond ((ignore-symbol-p var-name)
                            body)
                           ((valid-varname-p var-name)
                            `((let ((,var-name (aref ,val-name ,idx)))
                                ,@(when-let (decl (use-declaration var-name decls))
                                            `((declare ,@decl)))
                                ,@body)))
                           (t
                            (let ((tmp-var (gensym (format nil "ARRAY-NESTED-VAL-~A" idx))))
                              (multiple-value-bind (spec var)
                                  (parse-binding `(,var-name ,tmp-var))
                                `((let ((,tmp-var (aref ,val-name ,idx)))
                                    ,(expand-binding spec
                                                     var
                                                     tmp-var
                                                     decls
                                                     body)))))))))
                 (loop :for idx :from 0 :below (or rest-idx length)
                    :for var-name :across var
                    :collect (cons var-name idx))
                 :from-end t
                 :initial-value body))))

(flet ((expand-bindings-form (bindings body decls)
         (reduce (lambda (binding body)
                   (destructuring-bind (var-form . val) binding
                     (multiple-value-bind (spec var)
                         (parse-binding `(,var-form ,val))
                       (list (expand-binding spec var val decls body)))))
                 bindings
                 :from-end t
                 :initial-value body)))
        
  (define-binder (nil (var list) val decls body)
    (multiple-value-bind (vars bindings)
        (extract-nested-binding-specs var decls)
      `(destructuring-bind ,vars ,val
         ,@(when-let (decl (mapcan (lambda (x) (use-declaration x decls)) vars))
                     `((declare ,@decl)))
         ,@(expand-bindings-form bindings body decls))))

  (define-binder (:mval (var list) val decls body)
    (multiple-value-bind (vars bindings)
        (extract-nested-binding-specs var decls)
      `(multiple-value-bind ,vars ,val
         ,@(when-let (decl (mapcan (lambda (x) (use-declaration x decls)) vars))
                     `((declare ,@decl)))
         ,@(expand-bindings-form bindings body decls)))))

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

(define-binder (:all (var list) val decls body)
  (let ((decl (mapcan (lambda (x) (use-declaration x decls)) var))
        (val-sym (gensym "VAL")))
    `(let ((,val-sym ,val))
       (let (,@(mapcar (lambda (var) `(,var ,val-sym)) var))
         ,@(when decl `((declare ,@decl)))
         ,@body))))
    
(define-binder (:complex (var list) val decls body)
  (let ((val-sym (gensym "COMPLEX-")))
    (let ((bindings 
           (mapcan (lambda (var function)
                     (unless (ignore-symbol-p var)
                       (list `(,var (,function ,val-sym)))))
                   (if (or (null var) (> (length var) 2))
                       (error "expected REALPART or REALPART IMAGPART variable names, got ~A" var)
                       var)
                   '(realpart imagpart))))
      `(let ((,val-sym ,val))
         ,@(if bindings
               `((let (,@bindings)
                   ,@(when-let (decl (mapcan (lambda (x)
                                               (use-declaration (car x) decls))
                                             bindings))
                               `((declare ,@decl)))
                   ,@body))
              body)))))
