LEXICAL-RENAME - Less than SYMBOL-MACROLET, alpha-conversion for CL.

Copyright (C) 2012 Olof-Joachim Frahm

Released under a Simplified BSD license.

Implementations currently running on: SBCL (because of SB-WALKER).  For
everything else a portable walker would be necessary.

The API isn't particularly fixed at the moment.

Uses fiveam for defining tests.


# WRITEUP

While working with `CL-TUPLES` I came across the annoying problem that by
using certain variable names as arguments to the various macros the
running Lisp instance crashed, respectively looped indefinitely.

Investigating this, I found `SYMBOL-MACROLET` used as a rename tool like
in the following snippet (my definition):

    (def-tuple-op scaling-matrix44*
        ((sx cl-tuples::fast-float)
         (sy cl-tuples::fast-float)
         (sz cl-tuples::fast-float))
      (:return matrix44
               (matrix44-values*
                sx    0.0f0 0.0f0 0.0f0
                0.0f0 sy    0.0f0 0.0f0
                0.0f0 0.0f0 sz    0.0f0
                0.0f0 0.0f0 0.0f0 1.0f0)))

This creates as its expansion a macro by the same name:

    (DEFMACRO SCALING-MATRIX44* (SX SY SZ)
      "DEF-TUPLE-OP SCALING-MATRIX44* (FAST-FLOAT FAST-FLOAT FAST-FLOAT)"
      `(THE
        (VALUES CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT
                CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT
                CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT
                CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT
                CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT CL-TUPLES::FAST-FLOAT
                CL-TUPLES::FAST-FLOAT)
        (SYMBOL-MACROLET ((SX (THE CL-TUPLES::FAST-FLOAT ,SX)))
          (SYMBOL-MACROLET ((SY (THE CL-TUPLES::FAST-FLOAT ,SY)))
            (SYMBOL-MACROLET ((SZ (THE CL-TUPLES::FAST-FLOAT ,SZ)))
              (PROGN
               (MATRIX44-VALUES* SX 0.0 0.0 0.0
                                 0.0 SY 0.0 0.0
                                 0.0 0.0 SZ 0.0
                                 0.0 0.0 0.0 1.0)))))))

The arguments of the macro are used in the `SYMBOL-MACROLETS` to
generate bindings without actually binding variables in that moment.
The intent is to not evaluate those expressions until necessary,
i.e. inlining them.

E.g. expanding this:

    (with-vector3d scale (sx sy sz) (scaling-matrix44* sx sy sz))

results in:

    (MULTIPLE-VALUE-BIND (SX SY SZ)
        (VALUES (AREF (THE (VECTOR * *) SCALE) 0)
                (AREF (THE (VECTOR * *) SCALE) 1)
                (AREF (THE (VECTOR * *) SCALE) 2))
      (DECLARE (IGNORABLE SX SY SZ)
               (TYPE CL-TUPLES::FAST-FLOAT SX SY SZ))
      (PROGN (THE
              (VALUES CL-TUPLES::FAST-FLOAT ...)
              (SYMBOL-MACROLET ((SX (THE CL-TUPLES::FAST-FLOAT SX)))
                (SYMBOL-MACROLET ((SY (THE CL-TUPLES::FAST-FLOAT SY)))
                  (SYMBOL-MACROLET ((SZ (THE CL-TUPLES::FAST-FLOAT SZ)))
                    (MATRIX44-VALUES* SX  0.0 0.0 0.0
                                      0.0 SY  0.0 0.0
                                      0.0 0.0 SZ  0.0
                                      0.0 0.0 0.0 1.0)))))))

This poses a problem however, as the argument `SX` can easily be the
symbol `SX`.  Thus, after expanding the macro, one of those bindings
becomes:

    (symbol-macrolet ((sx (the ... sx))) ...)

Which is bad.  Now, none of the standard CL utilities help me with this.
Replacing the symbol macro with a regular macro (i.e. use `(SX)` instead
of `SX`) doesn't help, since the problem stays the same, because (not
so) pathological cases still trigger the same stack blowing behaviour
(e.g. supplying the form `(SX)` as argument, then we get the same
infinite expansion using `MACROLET` instead).

So what I really want is a lexical rename:  All free variable references
in the body of this new macro should be renamed and it should be able to
handle the somehow useless rename case from above:

    (lexical-rename-1 ((sx sx))
      sx)

would then expand to:

    (PROGN SX)

But something like this:

    (lexical-rename-1 ((foo bar))
      (let ((foo 42))
        foo))

would still return 42.

However, unlike `SYMBOL-MACROLET` this is only expanded once (and is not
kept for further expansions).  Also, it is intentionally named like
this, because a number of variants of this macro are possible, like
varying the number of expansions (from one to unbounded, in which case
it resembles `SYMBOL-MACROLET`) and with parallel, or sequential binding
strategy.

Using this approach, the example above expands now to something like this:

    (MULTIPLE-VALUE-BIND (SX SY SZ)
        (VALUES (AREF (THE (VECTOR * *) SCALE) 0)
                (AREF (THE (VECTOR * *) SCALE) 1)
                (AREF (THE (VECTOR * *) SCALE) 2))
      (DECLARE (IGNORABLE SX SY SZ)
               (TYPE CL-TUPLES::FAST-FLOAT SX SY SZ))
      (PROGN (THE
              (VALUES CL-TUPLES::FAST-FLOAT ...)
              (LEXICAL-RENAME:LEXICAL-RENAME-1 ((SX (THE CL-TUPLES::FAST-FLOAT SX)))
                (LEXICAL-RENAME:LEXICAL-RENAME-1 ((SY (THE CL-TUPLES::FAST-FLOAT SY)))
                  (LEXICAL-RENAME:LEXICAL-RENAME-1 ((SZ (THE CL-TUPLES::FAST-FLOAT SZ)))
                    (PROGN
                     (MATRIX44-VALUES* SX  0.0 0.0 0.0
                                       0.0 SY  0.0 0.0
                                       0.0 0.0 SZ  0.0
                                       0.0 0.0 0.0 1.0))))))))

which in turn expands to:

    (MULTIPLE-VALUE-BIND (SX SY SZ)
        (VALUES (AREF (THE (VECTOR * *) SCALE) 0)
                (AREF (THE (VECTOR * *) SCALE) 1)
                (AREF (THE (VECTOR * *) SCALE) 2))
      (DECLARE (IGNORABLE SX SY SZ)
               (TYPE CL-TUPLES::FAST-FLOAT SX SY SZ))
      (PROGN (THE
              (VALUES CL-TUPLES::FAST-FLOAT ...)
              (PROGN
               (PROGN
                (PROGN
                 (PROGN
                  (THE
                   (VALUES CL-TUPLES::FAST-FLOAT ...)
                   (VALUES (THE CL-TUPLES::FAST-FLOAT SX) 0.0 0.0 0.0
                           0.0 (THE CL-TUPLES::FAST-FLOAT SY) 0.0 0.0
                           0.0 0.0 (THE CL-TUPLES::FAST-FLOAT SZ) 0.0
                           0.0 0.0 0.0 1.0)))))))))

All the macro calls are fully expanded and only `PROGNS` remain.


# API

This section describes all exported symbols.

## LEXICAL-RENAME

`LEXICAL-RENAME-1 MACROBINDINGS &BODY BODY` replaces all occurences of
free variables in the given `BODY` in parallel.  `MACROBINDINGS` are the
replacements to perform, e.g. `((A B) (C D))` would replace `A` with `B`
and `C` with `D`.

`LEXICAL-RENAME-N TIMES MACROBINDINGS &BODY BODY` performs at most
`TIMES` expansions, or infinitely many if `TIMES` is `T`.

`LEXICAL-RENAME-1*` and `LEXICAL-RENAME-N*` are similar, but replace
sequentially, i.e. `(LEXICAL-RENAME-1* ((A B) (C D)) ...)` is equivalent
to `(LEXICAL-RENAME-1 ((A B)) (LEXICAL-RENAME ((C D)) ...))`.

`LEXICAL-RENAME` is the same as `(LEXICAL-RENAME-N T ...)`.

## HELPER FUNCTIONS

At the moment three additional helper functions are exposed, although
I'm not sure whether they should stay there, since they only provide a
very thin wrapper around `SB-WALKER`.

`LEXICALLY-BOUND-P VARIABLE ENVIRONMENT` returns true, if that `SYMBOL`
is lexically bound in the given environment.  Might be useful in writing
similar macros.

`FREE-VARIABLES FORM &OPTIONAL ENVIRONMENT` returns a `LIST` of free
variables for a given form.  Optionally accepts an environment in which
they are looked up in.

`MAP-VARIABLE-REFERENCES FUNCTION FORM &OPTIONAL ENVIRONMENT` calls the
given `FUNCTION` on all variable references in a given form.
`FREE-VARIABLES` is implemented in terms of this.
