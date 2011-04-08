(defpackage ping
  (:use :common-lisp :sb-alien)
  (:export ping
           
           sockaddr-in
           icmp-echo-header
           ip-header
           done))
(in-package :ping)

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))

(defvar *native-endian* 
  (if (eq sb-c:*backend-byte-order* :big-endian) :big :little))