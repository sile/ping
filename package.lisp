(defpackage ping
  (:use :common-lisp :sb-alien)
  (:export ping))
(in-package :ping)

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))

