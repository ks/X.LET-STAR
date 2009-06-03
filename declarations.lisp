(in-package :x.let-star)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *declaration-specs* '())

  ;; returns: hash-table var-name -> canonic declarations
  ;;          not related body-declarations (optimize, ftype)
  (defgeneric process-declaration (spec form))
  
  (defmacro define-declaration-processing ((spec form) &body body)
    (let ((spec-sym (gensym "SPEC")))
      (when (member spec *declaration-specs*)
        (error "~A declaration processing is already defined" spec))
      `(progn
         (push ',spec *declaration-specs*)
         (defmethod process-declaration ((,spec-sym (eql ',spec)) ,form)
           ,@body)))))

(defparameter *lambda-list-markers* '(&key &body &rest &args &optional))

(defun lambda-list-vars (list)
  (let ((optional-or-key nil))
    (mapcan (lambda (x)
              (cond ((null x) nil)
                    ((atom x) (list x))
                    (t (lambda-list-vars x))))
            (mapcar (lambda (x)
                      (if (or (eq x '&optional) (eq x '&key))
                          (prog1 nil (setf optional-or-key t))
                          (unless (member x *lambda-list-markers*)
                            (if optional-or-key
                                (prog1 (if (consp x) (car x) x)
                                  (setf optional-or-key nil))
                                x))))
                    (let ((last (last list)))
                      (if (cdr last)
                          (nconc (butlast list) (list (car last) (cdr last)))
                          list))))))

(defun strip-declarations (body &optional decls)
  (if (and (consp (car body))
           (eq (caar body) 'declare))
      (strip-declarations (cdr body) (append decls (cdar body)))
      (values body decls)))

(defun merge-hash-tables (main-table other-table)
  (maphash (lambda (key val0)
             (let ((val (gethash key main-table)))
               (setf (gethash key main-table)
                     (nconc val (list val0)))))
           other-table))

(defun process-declarations (body)
  (multiple-value-bind (body declarations)
      (strip-declarations body)
    (let ((variable-decls (make-hash-table :test #'eq)))
      (if declarations
          (let ((all-body-decls '()))
            (dolist (declaration declarations)
              (destructuring-bind (spec &rest form) declaration
                (multiple-value-bind (canonic body-decls)
                    (if (member spec *declaration-specs*)
                        (process-declaration spec form)
                        (process-declaration 'type declaration))
                  (merge-hash-tables variable-decls canonic)
                  (setf all-body-decls (nconc all-body-decls body-decls)))))
            (values (if all-body-decls
                        `((declare ,@all-body-decls) ,@body)
                        body)
                    variable-decls))
          (values body variable-decls)))))

(defun use-declaration (var variable-decls)
  (prog1 (gethash var variable-decls)
    (remhash var variable-decls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-common-declaration (spec form &optional decl-form-fn)
  (let ((canonic (make-hash-table :test #'eq))
        (body-declarations '()))
    (dolist (var form)
      (let ((decl-form (or (and decl-form-fn
                                (funcall decl-form-fn var))
                           `(,spec ,var))))
        (if (atom var)
            (setf (gethash var canonic) decl-form)
            (push decl-form body-declarations))))
    (values canonic body-declarations)))

(defun skip-declaration (spec form)
  (values (make-hash-table :test #'eq)
          (mapcar (lambda (var) `(,spec ,var)) form)))

(define-declaration-processing (ignore form)
  (process-common-declaration 'ignore form))

(define-declaration-processing (ignorable form)
  (process-common-declaration 'ignorable form))

(define-declaration-processing (special form)
  (process-common-declaration 'special form))

(define-declaration-processing (dynamic-extent form)
  (process-common-declaration 'dynamic-extent form))

(define-declaration-processing (type form)
  (destructuring-bind (type &rest vars) form
    (process-common-declaration 'type vars (lambda (var) `(type ,type ,var)))))

(define-declaration-processing (optimize form)
  (skip-declaration 'optimize form))

(define-declaration-processing (ftype form)
  (skip-declaration 'ftype form))

(define-declaration-processing (inline form)
  (skip-declaration 'inline form))

(define-declaration-processing (notinline form)
  (skip-declaration 'notinline form))









