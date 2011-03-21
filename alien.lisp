(in-package :ping)

(eval-when (:load-toplevel)
  (define-alien-routine socket int (domain int) (type int) (protocol int)))

(defconstant +AF_INET+ 2)
(defconstant +SOCK_RAW+ 3)
(defconstant +IPPROTO_ICMP+ 1)
(defconstant +ICMP_ECHO+ 8)

(define-alien-type __u8 (unsigned 8))
(define-alien-type __sum16 (unsigned 16))
(define-alien-type __be16 (unsigned 16))
(define-alien-type __be32 (unsigned 32))
(define-alien-type sa_family_t (signed 16))

(define-alien-type nil
  (struct icmphdr
    (type __u8)
    (code __u8)
    (checksum __sum16)
    (un (union nil 
          (echo (struct nil
                  (id       __be16)
                  (sequence __be16)))
          (gateway __be32)
          (frag (struct nil
                  (__unused __be16)
                  (mtu      __be16)))))))

(define-alien-type nil
  (struct in_addr
   (s_addr __be32)))

(define-alien-type nil
  (struct sockaddr_in
    (sin_family sa_family_t)
    (sin_port   __be16)
    (sin_addr   (struct in_addr))
    (__pad      (array char 8))))

(defun make-socket-fd (domain type protocol)
  (let ((fd (socket domain type protocol)))
    (if (= fd -1)
        (values nil (get-errno))
      (values fd nil))))

(defun make-icmp-socket-fd ()
  (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+))

;; ip-vector: #(0 0 0 0)
(defun ip-vector-to-inet-addr (ip-vector)
  (loop FOR n ACROSS ip-vector
        FOR i FROM 3 DOWNTO 0
        SUM (ash n (* i 8))))

;; XXX:
(defun inet-int (n size)
  (loop FOR l FROM 0 BELOW size
        FOR h FROM (1- size) DOWNTO 0
        WHILE (< l h)
        DO (rotatef (ldb (byte 8 (* l 8)) n) 
                    (ldb (byte 8 (* h 8)) n)))
  n)

(defun memset (sap value size)
  (declare ((alien (unsigned 8)) value))
  (let ((*p (cast sap (* (unsigned 8)))))
    (dotimes (i size sap)
      (setf (deref *p i) value))))

;; ip: #(0 0 0 0)
(defun make-sockaddr-in (sa-family ip &key port)
  (declare ((alien sa_family_t) sa-family)
           ((vector t 4) ip)
           ((or null (alien __be16)) port)
           #.*muffle-compiler-note*)
  (let* ((*sa (make-alien (struct sockaddr_in)))
         (sa (deref *sa)))
    (memset *sa 0 (alien-size (struct sockaddr_in) :bytes))
    
    (setf (slot sa 'sin_family) (inet-int sa-family 2))
    (setf (slot (slot sa 'sin_addr) 's_addr) (inet-int (ip-vector-to-inet-addr ip) 4))
    (when port
      (setf (slot sa 'sin_port) (inet-int port 2)))
    *sa))

