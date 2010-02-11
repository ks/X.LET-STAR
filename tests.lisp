(in-package :cl-user)

(defpackage :x.let-star-test
  (:use :cl :x.let-star)
  (:import-from x.let-star when-let ignore-varname-p)
  (:shadowing-import-from x.let-star let*))

(in-package :x.let-star-test)

(defparameter *tests* '())
    
(defmacro deftest (name &body body)
  `(let ((elem (cons ',name
                     (lambda ()
                       (multiple-value-bind (result error)
                           (ignore-errors (progn ,@body))
                         (let ((passed (and result (not error))))
                           (format t "test ~A ~:[FAILED~;ok~].~%" ',name passed)
                           passed))))))
     (let ((pos (member ',name *tests* :key #'car)))
       (if pos
           (rplaca pos elem)
           (push elem *tests*)))))

(defun run-tests ()
  (let ((passed 0)
        (failed 0))
    (loop :for (name . test) :in (reverse *tests*)
       :do (if (funcall test)
               (incf passed)
               (incf failed)))
    (format t "/// X.LET-STAR testing done. ~A passed, ~A failed." passed failed)
    (zerop failed)))

(defun flatten-filter (fn tree)
  (let ((result nil))
    (labels ((rec (elem)
               (when elem
                 (if (consp elem)
                     (progn
                       (rec (car elem))
                       (rec (cdr elem)))
                     (when-let (elem (funcall fn elem))
                       (push elem result))))))
      (rec tree))
    (nreverse result)))

(defun find-gensyms (form)
  (let ((seen '()))
    (flet ((seen (x)
             (member x seen
                     :test (lambda (s1 s2)
                             (string= (symbol-name s1)
                                      (symbol-name s2))))))
      (flatten-filter (lambda (x)
                        (when (and (symbolp x)
                                   (null (symbol-package x))
                                   (not (seen x)))
                          (push x seen)
                          x))
                      form))))

(defstruct xxx x y z)

;;;;;;;;;;

(deftest let*-simple
  (equalp (macroexpand-1 `(let* ((a 10))
                            :body))
          `(let ((a 10))
             :body)))

(deftest let*-simple-ignore
  (let ((exp (macroexpand-1 `(let* ((_ 10)) :body))))
    (destructuring-bind (ignore-var) (find-gensyms exp)
      (and (ignore-varname-p ignore-var)
           (equalp exp
                   `(let ((,ignore-var 10))
                      (declare (ignore ,ignore-var))
                      :body))))))

(deftest let*-simple-decl
  (equalp (macroexpand-1
           `(let* ((a 10)) (declare (fixnum a))))
          `(let ((a 10))
             (declare (type fixnum a)))))
    
(deftest let*-empty
  (equalp (macroexpand-1 `(let* () :body))
          `(let () :body)))

(deftest let*-varname-only
  (equalp (macroexpand-1 `(let* (xxx) xxx))
          `(let (xxx) xxx)))

(deftest let*-varname-only-ignore
  (equalp (macroexpand-1 `(let* (_) :body))
          `(progn :body)))

(deftest let*-varname-only-decl
  (equalp (macroexpand-1 `(let* (xxx)
                            (declare ((or cons null) xxx))
                            xxx))
          `(let (xxx) 
             (declare (type (or cons null) xxx))
             xxx)))

(deftest let*-vector-simple-destructuring
  (let ((exp (macroexpand-1 `(let* ((#(a) #(1))) a))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val #(1)))
                 (assert (eql (length ,val) 1) nil
                         "expected vector of length ~a" 1)
                 (let ((a (aref ,val 0)))
                   a))))))

(deftest let*-vector-simple-destructuring-decl
  (let ((exp (macroexpand-1 `(let* ((#(a) #(1)))
                               (declare (fixnum a))
                               a))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val #(1)))
                 (assert (eql (length ,val) 1) nil
                         "expected vector of length ~a" 1)
                 (let ((a (aref ,val 0)))
                   (declare (type fixnum a))
                   a))))))

(deftest let*-vector-length-check
  (handler-case
      (let* ((#(a) #(1 2)))
        nil)
    (error ()
      t)))

(deftest let*-vector-simple-destructuring-rest-unbound
  (let ((exp (macroexpand-1 `(let* ((#(a &rest) #(1 2))))))) ;; or #(... &rest _) 
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val #(1 2)))
                 (let ((a (aref ,val 0)))))))))

(deftest let*-vector-simple-destructuring-rest-bound
  (let ((exp (macroexpand-1 `(let* ((#(a &rest b) #(1 2)))))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val #(1 2)))
                 (let ((a (aref ,val 0)))
                   (let ((b (make-array (- (length ,val) 1)
                                        :displaced-to ,val
                                        :displaced-index-offset 1))))))))))

(deftest let*-vector-simple-destructuring-rest-bound-decl
  (let ((exp (macroexpand-1 `(let* ((#(a &rest b) #(1 2)))
                               (declare (fixnum a) (vector b))
                               :body))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val #(1 2)))
                 (let ((a (aref ,val 0)))
                   (declare (type fixnum a))
                   (let ((b (make-array (- (length ,val) 1)
                                        :displaced-to ,val
                                        :displaced-index-offset 1)))
                     (declare (type vector b))
                     :body)))))))

(deftest let*-vector-nested-destructuring-rest-bound-decl
  (let ((exp (macroexpand-1 `(let* ((#((b . c) &rest rest)
                                     (vector (list 2 3 4) 7 8 9)))
                               (declare (fixnum b) (list c) (vector rest))
                               :body))))
    (destructuring-bind (val array-nested-val) (find-gensyms exp)
      (equalp exp
              `(let ((,val (vector (list 2 3 4) 7 8 9)))
                 (let ((,array-nested-val (aref ,val 0)))
                   (destructuring-bind (b . c) ,array-nested-val
                     (declare (type fixnum b) (type list c))
                     (let ((rest (make-array (- (length ,val) 1)
                                             :displaced-to ,val
                                             :displaced-index-offset 1)))
                       (declare (type vector rest))
                       :body))))))))

(deftest let*-list-nested-destructuring-decl
  (let ((exp (macroexpand-1 `(let* (((a (#(b) . c) (:slotval x) &rest rest)
                                     (list 1 (cons #(2) 3) (make-xxx :x 4) 5 6)))
                               (declare (fixnum a b c x) (list rest))
                               :body))))
    (destructuring-bind (array slot-val val1 val2) (find-gensyms exp)
      (equalp exp
              `(destructuring-bind
                     (a (,array . c) ,slot-val &rest rest)
                   (list 1 (cons #(2) 3) (make-xxx :x 4) 5 6)
                 (declare (type fixnum a)
                          (type fixnum c)
                          (type list rest))
                 (let ((,val1 ,slot-val))
                   (let ((x (slot-value ,val1 'x)))
                     (declare (type fixnum x))
                     (let ((,val2 ,array))
                       (assert (eql (length ,val2) 1) nil "expected vector of length ~a" 1)
                       (let ((b (aref ,val2 0)))
                         (declare (type fixnum b))
                         :body)))))))))

(deftest let*-mval-simple
  (let ((exp (macroexpand-1 `(let* (((:mval a b) (values 1 2))) :body))))
    (equalp exp `(multiple-value-bind (a b) (values 1 2) :body))))

(deftest let*-mval-nested-destructuring-decl
  (let ((exp (macroexpand-1 `(let* (((:mval _ #((a . b) (:slotval x)))
                                     (ignored (vector (list 1 2)) (make-xxx :x 3))))
                               (declare (fixnum a x) (list b))
                               :body))))
    (destructuring-bind
          (ignore array val1 array-nested-val1 array-nested-val2 val2)
        (find-gensyms exp)
      (equalp exp
              `(multiple-value-bind (,ignore ,array)
                   (ignored (vector (list 1 2)) (make-xxx :x 3))
                 (declare (ignore ,ignore))
                 (let ((,val1 ,array))
                   (assert (eql (length ,val1) 2) nil "expected vector of length ~a" 2)
                   (let ((,array-nested-val1 (aref ,val1 0)))
                     (destructuring-bind (a . b) ,array-nested-val1
                       (declare (type fixnum a) (type list b))
                       (let ((,array-nested-val2 (aref ,val1 1)))
                         (let ((,val2 ,array-nested-val2))
                           (let ((x (slot-value ,val2 'x)))
                             (declare (type fixnum x))
                             :body)))))))))))
    
(deftest let*-slotval-decl
  (let ((exp (macroexpand-1 `(let* (((:slotval (xxx x) y) (make-xxx :x 10 :y 20)))
                               (declare (fixnum xxx y))
                               :body))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val (make-xxx :x 10 :y 20)))
                 (let ((xxx (slot-value ,val 'x)))
                   (declare (type fixnum xxx))
                   (let ((y (slot-value ,val 'y)))
                     (declare (type fixnum y))
                     :body)))))))

(deftest let*-slotval-decl-multiple
  (let ((exp (macroexpand-1 `(let* (((:mval (:slotval (xxx x) y)
                                            (:slotval (yy y) z)) 
                                     (values (make-xxx :x 10 :y 20)
                                             (make-xxx :y 30 :z 40))))
                               (declare (fixnum xxx y yy z))
                               :body))))
    (destructuring-bind (list1 list2 val1 val2) (find-gensyms exp)
      (equalp exp
              `(multiple-value-bind (,list1 ,list2)
                   (values (make-xxx :x 10 :y 20) (make-xxx :y 30 :z 40))
                 (let ((,val1 ,list2))
                   (let ((yy (slot-value ,val1 'y)))
                     (declare (type fixnum yy))
                     (let ((z (slot-value ,val1 'z)))
                       (declare (type fixnum z))
                       (let ((,val2 ,list1))
                         (let ((xxx (slot-value ,val2 'x)))
                           (declare (type fixnum xxx))
                           (let ((y (slot-value ,val2 'y)))
                             (declare (type fixnum y))
                             :body)))))))))))

(deftest let*-slot-decl
  (let ((exp (macroexpand-1 `(let* (((:slot (xxx x) y) (make-xxx :x 10 :y 20)))
                               (declare (fixnum xxx y))
                               :body))))
    (equalp exp
            `(with-slots ((xxx x) y)
                 (make-xxx :x 10 :y 20)
               (declare (type fixnum xxx) (type fixnum y))
               :body))))

(deftest let*-all-decl
  (let ((exp (macroexpand-1 `(let* (((:all x y z) 0))
                               (declare (fixnum x y z))
                               :body))))
    (destructuring-bind (val) (find-gensyms exp)
      (equalp exp
              `(let ((,val 0))
                 (let ((x ,val)
                       (y ,val)
                       (z ,val))
                   (declare (type fixnum x)
                            (type fixnum y)
                            (type fixnum z))
                   :body))))))

(deftest let*-complex-decl
  (let ((exp (macroexpand-1 `(let* (((:complex x y) (complex 10 20)))
                               (declare (fixnum x y))
                               :body))))
    (destructuring-bind (complex) (find-gensyms exp)
      (equalp exp
              `(let ((,complex (complex 10 20)))
                 (let ((x (realpart ,complex))
                       (y (imagpart ,complex)))
                   (declare (type fixnum x) (type fixnum y))
                   :body))))))

(unless (run-tests)
  (warn "!!!!!!!!!! some X.LET-STAR tests failed, please report that to 'karol <dot> skocik <at> gmail'"))