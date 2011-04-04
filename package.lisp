(defpackage ping
  (:use :common-lisp :sb-alien)
  (:export ping
           
           sockaddr-in
           icmp-echo-header
           ip-header
           done))
(in-package :ping)

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))

