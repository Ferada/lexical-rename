;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

;; Copyright (c) 2012, Olof-Joachim Frahm
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-user)

(asdf:defsystem #:lexical-rename
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :description "Less then SYMBOL-MACROLET."
  :long-description "Less then SYMBOL-MACROLET, alpha-conversion for CL."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :licence "Simplified BSD License"
  :components ((:file "lexical-rename"))
  :in-order-to ((asdf:test-op (asdf:test-op #:lexical-rename-tests)))
  :perform (asdf:test-op :after (operation component)
             (funcall (intern (symbol-name '#:run!) '#:fiveam)
                      (intern (symbol-name '#:lexical-rename-tests)
                              '#:lexical-rename-tests))))

(asdf:defsystem #:lexical-rename-tests
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :depends-on (#:lexical-rename #:fiveam)
  :components ((:file "tests")))
