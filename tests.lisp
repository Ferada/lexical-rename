(in-package #:lexical-rename)

(def-test test.1 ()
  "Simple renaming of variables."
  (let ((foo (gensym)))
    (is (eq foo (lexical-rename-1 ((foo foo)) foo)))
    (is (eq foo (lexical-rename-1 ((bar foo)) bar)))))

(def-test test.2 ()
  "Failing occurs check."
  ;; yes, EVAL, because the macroexpansion has to happen where
  ;; we can catch the error
  (signals error (eval `(lexical-rename-n T ((foo foo)) foo)))
  (signals error (eval `(lexical-rename-n T ((foo (list foo))) foo))))

(def-test test.3 ()
  "Nested bindings."
  (let ((foo 1) (bar 2) (baz 3) (qux 4))
    (declare (ignore foo))
    (is (equal
         (list bar baz qux qux)
         (lexical-rename-1 ((foo bar)
                            (bar baz)
                            (baz qux))
           (list foo bar baz qux))))
    (is (equal
         #1=(list qux qux qux qux)
         (lexical-rename-1 ((foo bar))
           (lexical-rename-1 ((bar baz))
             (lexical-rename-1 ((baz qux))
               (list foo bar baz qux))))))
    (is (equal
         #1#
         (lexical-rename-1* ((foo bar)
                             (bar baz)
                             (baz qux))
           (list foo bar baz qux))))))

(def-test test.4 ()
  "More nested bindings."
  (let ((foo 2) (bar 3) (baz 5) (qux 23) (quux 42))
    (declare (ignore foo))
    (macrolet ((aux (symbol arity)
                 `(,symbol ,arity ((foo bar)
                                   (bar baz)
                                   (baz qux)
                                   (qux quux))
                           (list foo bar baz qux quux)))
               (test (form test)
                 (let ((sym (gensym)))
                   `(let ((,sym ,form))
                      (list ,sym (equal ,sym ,test))))))
      (let ((five-quuxes (list qux qux qux qux qux)))
        (is (equal (list bar baz qux quux quux)
                   (aux lexical-rename-n 1)))
        (is (equal (list baz qux quux quux quux)
                   (aux lexical-rename-n 2)))
        (is (equal five-quuxes
                   (aux lexical-rename-n* 1)))
        (is (equal five-quuxes
                   (lexical-rename-1 ((foo bar))
                     (lexical-rename-1 ((bar baz))
                       (lexical-rename-1 ((baz qux))
                         (lexical-rename-1 ((qux quux))
                           (list foo bar baz qux quux)))))))
        (is (equal five-quuxes
                   (aux lexical-rename-n* 2)))))))

(def-test test.5 ()
  "No-ops."
  (is (equal 1 (lexical-rename ((foo bar))
                 (let ((foo 1))
                   foo))))
  (is (equal 1 (lexical-rename ((foo bar))
                 ((lambda (foo) foo) 1)))))
