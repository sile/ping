(in-package :ping)

(declaim #.*muffle-compiler-note*)

;; ICMP RFC: http://www.networksorcery.com/enp/rfc/rfc792.txt

;;;;;;;;;;;;
;; constants
(defconstant +ICMP_ECHO+ 8)
(defconstant +ICMP_ECHO_REPLY+ 0)
(defconstant +ICMP_UNREACH+ 3)

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

(defconstant +AF_INET+ 2)
(defconstant +SOCK_RAW+ 3)
(defconstant +IPPROTO_ICMP+ 1)
(defconstant +SOL_SOCKET+ 1)
(defconstant +SOL_RAW+ 255)
(defconstant +SOL_PACKET+ 263)
(defconstant +SO_BROADCAST+ 6)
(defconstant +SO_RCVBUF+ 8)

(defconstant +MSG_DONTROUTE+ #x04)
(defconstant +MSG_DONTWAIT+ #x40)

(defconstant +EAGAIN+ 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IP header
(define-alien-struct ip-header (:endian :big)
  (version   (unsigned 04))
  (ihl       (unsigned 04))
  (tos       (unsigned 08))
  (total-len (unsigned 16))
  (id        (unsigned 16))
  (frag-off  (unsigned 16))
  (ttl       (unsigned 08))
  (protocol  (unsigned 08))
  (checksum  (unsigned 16))
  (src-addr  (unsigned 32) :show dotted-ip)
  (dst-addr  (unsigned 32) :show dotted-ip))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icmp header (for echo)
(define-alien-struct icmp-header (:endian :big)
  (type     (unsigned 08))
  (code     (unsigned 08))
  (checksum (unsigned 16))
  (id       (unsigned 16))
  (seq-num  (unsigned 16)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; sockaddr
(define-alien-struct sockaddr-in (:endian #.*native-endian*)
  (family (unsigned 16))
  (port   (unsigned 16))
  (addr   (unsigned 32) :show dotted-ip)
  (__pad  (array char 8)))

;;;;;;;;;;;;;;;;;;;;;;;
;; packet (for echo)
(define-alien-struct packet (:endian :big)
  (ip ip-header)
  (icmp icmp-header))

;; 
(define-alien-type size_t unsigned-long)
(define-alien-type socklen_t (unsigned 32))
