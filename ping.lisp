(in-package :ping)

;;; XXX:
(define-alien-type size_t unsigned-long)
(define-alien-type socklen_t (unsigned 32))

(eval-when (:load-toplevel)
  (define-alien-routine socket int (domain int) (type int) (protocol int))
  (define-alien-routine sendto int (sockfd int) (buf (* t))
                                   (len size_t) (flags int)
                                   (dest_addr (* (struct sockaddr-in))) (addrlen socklen_t))
  (define-alien-routine recv size_t (socketfd int) (buf (* t)) (len size_t) (flags int)))


(defun make-socket-fd (domain type protocol)
  (let ((fd (socket domain type protocol)))
    (if (= fd -1)
        (values nil (get-errno) (sb-int:strerror (get-errno)))
      (values fd nil nil))))

(defun init-sockaddr-in (sa ip)
  (setf (sockaddr-in.family sa) +AF_INET+
        (sockaddr-in.port sa) 0
        (sockaddr-in.ip sa) ip)
  sa)

(defun init-icmp-echo-header (icmp &key type sequence)
  (memset icmp 0 icmp-echo-header.size)
  (setf (icmp-echo-header.type icmp) type
        (icmp-echo-header.seq-num icmp) sequence
        (icmp-echo-header.checksum icmp) (checksum icmp icmp-echo-header.size))
  icmp)

(defun summarize (results)
  results)

(defun log-msg (fmt &rest args)
  (format *error-output* "; ~?~%" fmt args))

(defun ip-fmt (stream ip &rest ignore)
  (declare (ignore ignore))
  (format stream "~d.~d.~d.~d" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defun icmp-sendto (socket icmp sa flags)
  (declare #.*muffle-compiler-note*)
  (/= -1 (sendto socket
                 (cast icmp (* t)) icmp-echo-header.size flags
                 (cast sa (* (struct sockaddr-in))) sockaddr-in.size)))

(defun icmp-recv (socket buf flags)
  (declare #.*muffle-compiler-note*)
  (/= -1 (recv socket (cast buf (* t)) ip-echo.size flags)))

(defun ping-impl (sa seq-num loop-count)
  (declare #.*muffle-compiler-note*)
  (if (>= seq-num loop-count)
      'done
    (with-alien ((icmp (struct icmp-echo-header))
                 (echo (struct ip-echo)))
      (init-icmp-echo-header icmp :type +ICMP_ECHO+ :sequence seq-num)
      (multiple-value-bind (sock errcode reason) 
                           (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+)
        (unless sock
          (return-from ping-impl (values nil `(:make-socket-error ,errcode ,reason))))
        
        (let ((took
               (unwind-protect
                   (timing
                    (unless (icmp-sendto sock icmp sa 0)
                      (return-from ping-impl (values nil `(:sendto-error ,(get-errno) ,(sb-int:strerror (get-errno))))))
                    
                    (unless (icmp-recv sock echo 0)
                      (return-from ping-impl (values nil `(:recv-error ,(get-errno) ,(sb-int:strerror (get-errno)))))))
                 (sb-unix:unix-close sock))))
          (log-msg "reply from ~/ping::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms" 
                   (sockaddr-in.ip sa) seq-num (ip-header.ttl (slot echo 'ip-header)) (round took))
          
          (ping-impl sa (1+ seq-num) loop-count))))))
     
(defun ping (target &key (loop-count 10))
  (declare #.*muffle-compiler-note*)
  (log-msg "ping to ~a~@[ (~/ping::ip-fmt/)~]" target (resolve-address target))
  (with-alien ((sa (struct sockaddr-in)))
    (n.if (ip (resolve-address target))
        (ping-impl (init-sockaddr-in sa ip) 0 loop-count)
      (values nil `(:unknown-host ,target)))))

