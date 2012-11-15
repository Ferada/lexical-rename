(in-package #:cl-user)

(defpackage #:lexical-rename
  (:use #:cl #:fiveam)
  (:export
   ;; actual API
   #:lexical-rename
   #:lexical-rename-1 #:lexical-rename-n
   #:lexical-rename-1* #:lexical-rename-n*

   ;; helpers
   #:lexically-bound-p
   #:free-variables
   #:map-variable-references))

(in-package #:lexical-rename)

(defun lexically-bound-p (variable environment)
  (sb-walker:var-lexical-p variable environment))

(defun map-variable-references (function form &optional environment)
  (sb-walker:walk-form
   form environment
   (lambda (form context environment)
     (declare (ignore context))
     (when (symbolp form)
       (funcall function form environment))
     form)))

(defun free-variables (form &optional environment)
  "Returns a LIST of free variables"
  (let (variables)
    (map-variable-references
     (lambda (variable environment)
       (unless (lexically-bound-p variable environment)
         (push variable variables)))
     form environment)
    variables))

(defun expand-lexical-rename-bindings (occurs-check macrobindings environment form
                                       &optional (noted-bindings NIL noted-bindings-p))
  "Expands one round of LEXICAL-RENAME and returns two VALUES:  the
expanded FORM and a BOOLEAN flag indicating whether an expansion was
actually performed."
  (let (seen changed)
    (values
     (sb-walker::walker-environment-bind (new-environment environment)
       ;; record *our* bindings
       (let ((noted-bindings
               (if noted-bindings-p
                   noted-bindings
                   (mapcar (lambda (macrobinding)
                             (let ((name (car macrobinding)))
                               (cons name (sb-walker::note-lexical-binding name new-environment))))
                           macrobindings))))
         ;; detect changed bindings, i.e. new ones by comparing them to
         ;; our records
         (flet ((replacep (form walk-environment)
                  "Returns T if the FORM should be replaced with an expansion."
                  (eq (lexically-bound-p form walk-environment)
                      (cdr (assoc form noted-bindings)))))
           (sb-walker:walk-form
            form new-environment
            (lambda (walk-form context environment)
              (declare (ignore context))
              (typecase walk-form
                (symbol
                 (let ((member (member walk-form macrobindings :key #'car :test #'eq)))
                   (cond
                     ((and member (replacep walk-form environment))
                      (setf changed T)
                      ;; only expand once
                      (let ((expansion (second (car member))))
                        ;; occurs check to safeguard us against a stack overflow
                        (when occurs-check
                          (if (atom expansion)
                              (when (eq expansion walk-form)
                                (error "Occurs-check failed:  FORM and EXPANSION are the same: ~A."
                                       expansion))
                              ;; TODO: occurs as free lexical variable,
                              ;; i.e. we would be called again
                              (let ((free-variables (free-variables expansion)))
                                (when (member walk-form free-variables :test #'eq)
                                  (error "Occurs-check failed:  EXPANSION ~A contains ~A as a free variable."
                                         expansion walk-form)))))
                        (values expansion T)))
                     (T walk-form))))
                (list
                 (let ((car (car walk-form)))
                   (case car
                     ;; handle our own expansion
                     ;; the body is expanded _before_ the inner macro sees its body
                     (lexical-rename-n
                         (if (member walk-form seen :test #'eq)
                             walk-form
                             (let ((expanded-inner
                                     (sb-walker::walker-environment-bind
                                         (inner-environment environment
                                                            :lexical-vars
                                                            (append (mapcar (lambda (binding)
                                                                              (list (car binding) :lexical-var '#:foo))
                                                                            (third walk-form))
                                                                    (sb-walker::env-lexical-variables environment)))
                                       (expand-lexical-rename-bindings
                                        occurs-check macrobindings inner-environment
                                        `(progn ,@(nthcdr 3 walk-form))
                                        noted-bindings)))
                                   (expanded-bindings
                                     (mapcar (lambda (binding)
                                               (list (first binding)
                                                     (expand-lexical-rename-bindings
                                                      occurs-check macrobindings environment
                                                      (second binding)
                                                      noted-bindings)))
                                             (third walk-form))))
                               (pushnew walk-form seen :test #'eq)
                               (let ((result `(,car ,(second walk-form) ,expanded-bindings ,@(cdr expanded-inner))))
                                 (pushnew result seen :test #'eq)
                                 (values result T)))))
                     (T walk-form))))
                (T walk-form)))))))
     changed)))

(defun expand-lexical-rename-bindings-times (times occurs-check macrobindings environment body)
  "Expand the BODY repeatedly.  TIMES specifies the number of expansions:
Either a positive INTEGER (expands with at most TIMES iterations) or T
\(expands until no further expansions are necessary, or infinitely often)."
  (flet ((expand ()
           (let (changed)
             (multiple-value-setq (body changed)
               (expand-lexical-rename-bindings occurs-check macrobindings environment body))
             changed)))
    (cond
      ((eq times T)
       (loop while (expand)))
      ((and (integerp times) (> times 0))
       (loop for i from 1 to times while (expand)))
      (T
       (error "TIMES parameter should be either a positive INTEGER or T (was ~A instead)" times))))
  body)

(defmacro lexical-rename-1 (macrobindings &body body)
  "Almost like LEXICAL-RENAME, but only expands one time."
  `(lexical-rename-n 1 ,macrobindings ,@body))

(defmacro lexical-rename-n (times macrobindings &body body &environment environment)
  "Expands like LEXICAL-RENAME-1 but with up to TIMES iterations (a
positive INTEGER) or infinitely often if TIMES is T."
  (expand-lexical-rename-bindings-times
   times (eq times T) macrobindings environment `(progn ,@body)))

(defmacro lexical-rename-1* (macrobindings &body body)
  "Same as LEXICAL-RENAME-1, but performs expansion sequentially."
  `(lexical-rename-n* 1 ,macrobindings ,@body))

(defmacro lexical-rename-n* (times macrobindings &body body)
  "Same as LEXICAL-RENAME-N, but performs expansion sequentially."
  (if macrobindings
      `(lexical-rename-n ,times (,(first macrobindings))
         (lexical-rename-n* ,times ,(rest macrobindings)
           ,@body))
      `(progn ,@body)))

(defmacro lexical-rename (macrobindings &body body)
  "Same as (LEXICAL-RENAME-N T ...), i.e. expand infinitely often."
  `(lexical-rename-n T ,macrobindings
     ,@body))
