(in-package :ping)

;; REF: http://www.networksorcery.com/enp/rfc/rfc792.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IP header
(eval-when (:load-toplevel :compile-toplevel :execute)
(define-alien-type nil
  (struct ip-header
    (version         (unsigned 04))
    (ihl             (unsigned 04) :alignment 4)
    (tos             (unsigned 08))              ; type of service
    (total-length    (unsigned 16))
    (id              (unsigned 16))
    (flags           (unsigned 03))
    (fragment-offset (unsigned 13) :alignment 3)
    (ttl             (unsigned 8))
    (protocol        (unsigned 8))
    (checksum        (unsigned 16))
    (src-addr        (unsigned 32))
    (dst-addr        (unsigned 32)))))

(defun ip-header.version (h)
  (ldb (byte 4 4) (alien-coerce (slot h 'version) (unsigned 8))))

(defsetf ip-header.version (h) (value)
  `(setf (ldb (byte 4 4) (alien-coerce (slot ,h 'version) (unsigned 8)))
         ,value))

(defun ip-header.ihl (h)
  (ldb (byte 4 0) (alien-coerce (slot h 'version) (unsigned 8))))

(defsetf ip-header.ihl (h) (value)
  `(setf (ldb (byte 4 0) (alien-coerce (slot ,h 'version) (unsigned 8)))
         ,value))

(defun ip-header.tos (h)
  (slot h 'tos))

(defsetf ip-header.tos (h) (value)
  `(setf (slot ,h 'tos) ,value))

(defun ip-header.total-length (h)
  (to-native-order (slot h 'total-length) 2))
(defsetf ip-header.total-length (h) (value)
  `(progn (setf (slot ,h 'total-length) (to-network-order ,value 2))
          ,value))

(defun ip-header.id (h)
  (to-native-order (slot h 'id) 2))

(defsetf ip-header.id (h) (value)
  `(progn (setf (slot ,h 'id) (to-network-order ,value 2))
          ,value))

(defun ip-header.flags (h)
  (ldb (byte 3 5) (alien-coerce (slot h 'flags) (unsigned 8))))

(defsetf ip-header.flags (h) (value)
  `(setf (ldb (byte 3 5) (alien-coerce (slot ,h 'flags) (unsigned 8)))
         ,value))

(defun ip-header.fragment-offset (h)
  (ldb (byte 13 0) (to-native-order
                    (alien-coerce (slot h 'flags) (unsigned 16))
                    2)))

(defsetf ip-header.fragment-offset (h) (value)
  (let ((tmp (gensym)))
    `(let ((,tmp ,value))
       (setf (ldb (byte 3 13) ,value) (ip-header.flags ,h))
       (setf (alien-coerce (slot ,h 'flags) (unsigned 16)) (to-network-order ,value 2))
       ,tmp)))

(defun ip-header.ttl (h)
  (slot h 'ttl))
(defsetf ip-header.ttl (h) (value)
  `(setf (slot ,h 'ttl) ,value))

(defun ip-header.protocol (h)
  (slot h 'protocol))
(defsetf ip-header.protocol (h) (value)
  `(setf (slot ,h 'protocol) ,value))

(defun ip-header.checksum (h)
  (to-native-order (slot h 'checksum) 2))
(defsetf ip-header.checksum (h) (value)
  `(progn (setf (slot ,h 'checksum) (to-network-order ,value 2))
          ,value))

(defun ip-header.src-addr (h)
  (to-native-order (slot h 'src-addr) 4))
(defsetf ip-header.src-addr (h) (value)
  `(progn (setf (slot ,h 'src-addr) (to-network-order ,value 4))
          ,value))

(defun ip-header.dst-addr (h)
  (to-native-order (slot h 'dst-addr) 4))
(defsetf ip-header.dst-addr (h) (value)
  `(progn (setf (slot ,h 'dst-addr) (to-network-order ,value 4))
          ,value))

(set-pprint-dispatch '(alien (struct ip-header))
  (lambda (stream obj)
    (print-unreadable-object (obj stream)
      (format stream "~s#X~x ~s" 
              '(alien (struct ip-header))
              (sb-sys:sap-int (alien-sap obj))
              (list :version (ip-header.version obj)
                    :ihl (ip-header.ihl obj)
                    :tos (ip-header.tos obj)
                    :total-length (ip-header.total-length obj)
                    :id (ip-header.id obj)
                    :flags (ip-header.flags obj)
                    :fragment-offset (ip-header.fragment-offset obj)
                    :ttl (ip-header.ttl obj)
                    :protocol (ip-header.protocol obj)
                    :checksum (ip-header.checksum obj)
                    :src-addr (addr-to-ip (ip-header.src-addr obj))
                    :dst-addr (addr-to-ip (ip-header.dst-addr obj))))))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icmp header
(eval-when (:load-toplevel :compile-toplevel :execute)
(defconstant +ICMP_ECHO+ 8)
(defconstant +ICMP_ECHO_REPLY+ 0)
(defconstant +ICMP_UNREACH+ 3))

(defparameter *icmp-unreach-types*
  (list
    (defconstant +ICMP_UNREACH_NET+ 0 "bad net")
    (defconstant +ICMP_UNREACH_HOST+ 1 "bad host")
    (defconstant +ICMP_UNREACH_PROTOCOL+ 2 "bad protocol")
    (defconstant +ICMP_UNREACH_PORT+ 3 "bad port")
    (defconstant +ICMP_UNREACH_NEEDFRAG+ 4 "IP_DF caused drop")
    (defconstant +ICMP_UNREACH_SRCFAIL+ 5 "src route failed")
    (defconstant +ICMP_UNREACH_NET_UNKNOWN+ 6 "unknown net")
    (defconstant +ICMP_UNREACH_HOST_UNKNOWN+ 7 "unknown host")
    (defconstant +ICMP_UNREACH_ISOLATED+ 8 "src host isolated")
    (defconstant +ICMP_UNREACH_NET_PROHIB+ 9 "net denied")
    (defconstant +ICMP_UNREACH_HOST_PROHIB+ 10 "host denied")
    (defconstant +ICMP_UNREACH_TOSNET+ 11 "bad tos for net")
    (defconstant +ICMP_UNREACH_TOSHOST+ 12 "bad tos for host")
    (defconstant +ICMP_UNREACH_FILTER_PROHIB+ 13 "admin prohib")
    (defconstant +ICMP_UNREACH_HOST_PRECEDENCE+ 14 "host prec vio.")
    (defconstant +ICMP_UNREACH_PRECEDENCE_CUTOFF+ 15 "prec cutoff")))


(eval-when (:load-toplevel :compile-toplevel :execute)
(define-alien-type nil
  (struct icmp-echo-header
    (type     (unsigned 08))
    (code     (unsigned 08))
    (checksum (unsigned 16))
    (id       (unsigned 16))
    (seq-num  (unsigned 16)))))

(defun icmp-echo-header.type (h)
  (slot h 'type))

(defsetf icmp-echo-header.type (h) (value)
  `(setf (slot ,h 'type) ,value))

(defun icmp-echo-header.code (h)
  (slot h 'code))

(defsetf icmp-echo-header.code (h) (value)
  `(setf (slot ,h 'code) ,value))

(defun icmp-echo-header.checksum (h)
  (to-native-order (slot h 'checksum) 2))

(defsetf icmp-echo-header.checksum (h) (value)
  `(progn (setf (slot ,h 'checksum) (to-network-order ,value 2))
          ,value))

(defun icmp-echo-header.id (h)
  (to-native-order (slot h 'id) 2))

(defsetf icmp-echo-header.id (h) (value)
  `(progn (setf (slot ,h 'id) (to-network-order ,value 2))
          ,value))

(defun icmp-echo-header.seq-num (h)
  (to-native-order (slot h 'seq-num) 2))

(defsetf icmp-echo-header.seq-num (h) (value)
  `(progn (setf (slot ,h 'seq-num) (to-network-order ,value 2))
          ,value))

(set-pprint-dispatch '(alien (struct icmp-echo-header))
  (lambda (stream obj)
    (print-unreadable-object (obj stream)
      (format stream "~s#X~x ~s" 
              '(alien (struct icmp-echo-header))
              (sb-sys:sap-int (alien-sap obj))
              (list :type (icmp-echo-header.type obj)
                    :code (icmp-echo-header.code obj)
                    :checksum (icmp-echo-header.checksum obj)
                    :id (icmp-echo-header.id obj)
                    :seq-num (icmp-echo-header.seq-num obj)))))
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; sockaddr
(eval-when (:load-toplevel :compile-toplevel :execute)
(define-alien-type nil
  (struct sockaddr-in
    (family (unsigned 16))
    (port   (unsigned 16))
    (addr   (unsigned 32))
    (__pad  (array char 8)))))

(defun sockaddr-in.family (o)
  (slot o 'family) 2)

(defsetf sockaddr-in.family (o) (value)
  `(setf (slot ,o 'family) ,value))

(defun sockaddr-in.port (o)
  (slot o 'port))

(defsetf sockaddr-in.port (o) (value)
  `(setf (slot ,o 'port) ,value))

(defun sockaddr-in.addr (o)
  (slot o 'addr))

(defsetf sockaddr-in.addr (o) (value)
  `(setf (slot ,o 'addr) ,value))

(defun sockaddr-in.ip (o)
  (addr-to-ip (to-network-order (slot o 'addr) 4))) ; XXX:

(defsetf sockaddr-in.ip (o) (value)
  `(progn (setf (slot ,o 'addr) (to-network-order (ip-to-addr ,value) 4)) ; XXX:
          ,value))

(set-pprint-dispatch '(alien (struct sockaddr-in))
  (lambda (stream obj)
    (print-unreadable-object (obj stream)
      (format stream "~s#X~x ~s" 
              '(alien (struct sockaddr-in))
              (sb-sys:sap-int (alien-sap obj))
              (list :family (sockaddr-in.family obj)
                    :port (sockaddr-in.port obj)
                    :addr (sockaddr-in.ip obj)))))
  0)

(defconstant +AF_INET+ 2)
(defconstant +SOCK_RAW+ 3)
(defconstant +IPPROTO_ICMP+ 1)
(defconstant +SOL_SOCKET+ 1)
(defconstant +SOL_RAW+ 255)
(defconstant +SOL_PACKET+ 263)
(defconstant +SO_BROADCAST+ 6)

;;;;;;;;;;;
;; echo
(eval-when (:load-toplevel :compile-toplevel :execute)
(define-alien-type nil
  (struct ip-echo 
    (ip-header (struct ip-header))
    (icmp-echo (struct icmp-echo-header)))))


(set-pprint-dispatch '(alien (struct ip-echo))
  (lambda (stream obj)
    (print-unreadable-object (obj stream)
      (format stream "~s#X~x ~s" 
              '(alien (struct sockaddr-in))
              (sb-sys:sap-int (alien-sap obj))
              (list :ip-header (slot obj 'ip-header)
                    :icmp-echo (slot obj 'icmp-echo)))))
  0)

(defconstant icmp-echo-header.size (alien-size (struct icmp-echo-header) :bytes))
(defconstant sockaddr-in.size (alien-size (struct sockaddr-in) :bytes))
(defconstant ip-echo.size (alien-size (struct ip-echo) :bytes))

;; see: http://linux.die.net/include/bits/socket.h
(defconstant +MSG_DONTROUTE+ #x04)
