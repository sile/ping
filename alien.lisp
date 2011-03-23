(in-package :ping)

(defun inet-addr-to-ip-vector (ip &aux (ip (inet-int ip 4)))
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) ip) INTO list
        FINALLY (return (coerce list 'vector))))

(define-alien-type __u8 (unsigned 8))
(define-alien-type __sum16 (unsigned 16))
(define-alien-type __be16 (unsigned 16))
(define-alien-type __be32 (unsigned 32))
(define-alien-type sa_family_t (signed 16))
(define-alien-type size_t unsigned-long)
(define-alien-type socklen_t (unsigned 32))

(defconstant +AF_INET+ 2)
(defconstant +SOCK_RAW+ 3)
(defconstant +IPPROTO_ICMP+ 1)
(defconstant +ICMP_ECHO+ 8)
(defconstant +ICMP_ECHOREPLY+ 0)

(defmacro slots (obj &rest fields)
  (if (null fields)
      obj
    `(slots (slot ,obj ,(car fields)) ,@(cdr fields))))

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

(defun icmphr-to-list (obj)
  `(:struct :icmphdr
            :type ,(slot obj 'type)
            :code ,(slot obj 'code)
            :checksum ,(slot obj 'checksum)
            :un (:union nil
                        :echo (:struct nil 
                                       :id ,(slots obj 'un 'echo 'id)
                                       :sequence ,(slots obj 'un 'echo 'sequence))
                        :gateway ,(slots obj 'un 'gateway)
                        :frag (:struct nil
                                       :__unused ,(slots obj 'un 'frag '__unused)
                                       :mtu ,(slots obj 'un 'frag 'mtu)))))

(define-alien-type nil
  (struct iphdr
    (ver-ihl __u8)
    (tos __u8)
    (tot_len __be16)
    (id __be16)
    (frag_off __be16)
    (ttl __u8)
    (protocol __u8)
    (check __sum16)
    (saddr __be32)
    (daddr __be32)))

(defun iphdr.ihl (iphdr)
  (ldb (byte 4 0) (slot iphdr 'ver-ihl)))

(defun iphdr.version (iphdr)
  (ldb (byte 4 4) (slot iphdr 'ver-ihl)))

(defun iphdr-to-list (o)
  `(:struct :iphdr
            :version ,(iphdr.version o)
            :ihl ,(iphdr.ihl o)
            :tos ,(slot o 'tos)
            :tot_len ,(slot o 'tot_len)
            :id ,(slot o 'id)
            :frag_off ,(slot o 'frag_off)
            :ttl ,(slot o 'ttl)
            :protocol ,(slot o 'protocol)
            :check ,(slot o 'check)
            :saddr ,(inet-addr-to-ip-vector (slot o 'saddr))
            :daddr ,(inet-addr-to-ip-vector (slot o 'daddr))))
    
(define-alien-type nil
  (struct in_addr
   (s_addr __be32)))

(define-alien-type nil
  (struct sockaddr_in
    (sin_family sa_family_t)
    (sin_port   __be16)
    (sin_addr   (struct in_addr))
    (__pad      (array char 8))))

(defun sockaddr_in-to-list (o)
  `(:struct :sockaddr_in
            :sin_family ,(slot o 'sin_family)
            :sin_port ,(slot o 'sin_port)
            :sin_addr (:struct :in_addr :s_addr ,(inet-addr-to-ip-vector (slots o 'sin_addr 's_addr)))))

(eval-when (:load-toplevel)
  (define-alien-routine socket int (domain int) (type int) (protocol int))
  (define-alien-routine sendto int (sockfd int) (buf (* t))
                                   (len size_t) (flags int)
                                   (dest_addr (* (struct sockaddr_in))) (addrlen socklen_t))
  (define-alien-routine recv size_t (socketfd int) (buf (* t)) (len size_t) (flags int))
  )

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
    
    (setf (slot sa 'sin_family) sa-family)
    (setf (slot (slot sa 'sin_addr) 's_addr) (inet-int (ip-vector-to-inet-addr ip) 4))
    (when port
      (setf (slot sa 'sin_port) (inet-int port 2)))
    *sa))

(defun checksum (*obj size)
  (declare #.*muffle-compiler-note*)
  (let ((ptr (cast *obj (* (unsigned 8))))
        (sum 0))
    (loop FOR i FROM 0 BELOW (1- size) BY 2
          DO
          (incf sum (+ (ash (deref ptr (+ i 0)) 8)
                       (ash (deref ptr (+ i 1)) 0))))
    (when (oddp size)
      (incf sum (deref ptr (1- size))))
    (setf sum (+ (ldb (byte 16 0) sum) (ash sum -16))
          sum (+ (ldb (byte 16 0) sum) (ash sum -16)))
    (logxor #xFFFF (ldb (byte 16 0) sum))))

(defun make-icmp-header (&key type)
  (declare #.*muffle-compiler-note*)
  (let* ((*h (make-alien (struct icmphdr)))
         (h  (deref *h)))
    (memset *h 0 (alien-size (struct icmphdr) :bytes))
    
    (setf (slot h 'type) type
          (slot h 'checksum) (inet-int (checksum *h (alien-size (struct icmphdr) :bytes)) 2))
    *h))

(defun ping (ip)
  (declare #.*muffle-compiler-note*)
  (let ((*addr (make-sockaddr-in +AF_INET+ ip))
        (*hdr (make-icmp-header :type +ICMP_ECHO+))
        (socket (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+)))
    (unless socket
      (return-from ping (sb-int:strerror (get-errno))))
    (print (sockaddr_in-to-list (deref *addr)))
    (print (icmphr-to-list (deref *hdr)))    

    (let ((ret (sendto socket
                       *hdr (alien-size (struct icmphdr) :bytes) 0
                       *addr (alien-size (struct sockaddr_in) :bytes))))
      (when (= ret -1)
        (return-from ping (sb-int:strerror (get-errno))))
      
      (let* ((buf (make-alien (array (unsigned 8) 2000)))
             (ret (recv socket buf 2000 0)))
        (when (= ret -1)
          (return-from ping (sb-int:strerror (get-errno))))
        
        (let* ((*iphdr (cast buf (* (struct iphdr))))
               (*icmphdr (cast (deref buf (* (iphdr.ihl (deref *iphdr)) 4))
                               (* (struct icmphdr)))))
          (print (iphdr-to-list (deref *iphdr)))
          (print (icmphr-to-list (deref *icmphdr)))
          (if (= (slot (deref *icmphdr) 'type) +ICMP_ECHOREPLY+)
              :reply
            (slot (deref *icmphdr) 'type)))
        ))))

            
#|
struct iphdr {
#if defined(__LITTLE_ENDIAN_BITFIELD)
        __u8    ihl:4,
                version:4;
#elif defined (__BIG_ENDIAN_BITFIELD)
        __u8    version:4,
                ihl:4;
#else
#error  "Please fix <asm/byteorder.h>"
#endif
        __u8    tos;
        __be16  tot_len;
        __be16  id;
        __be16  frag_off;
        __u8    ttl;
        __u8    protocol;
        __sum16 check;
        __be32  saddr;
        __be32  daddr;
        /*The options start here. */
};
|#
