(in-package :ping)

;;; XXX:
(define-alien-type size_t unsigned-long)
(define-alien-type socklen_t (unsigned 32))

(eval-when (:load-toplevel)
  (define-alien-routine setsockopt int (sockfd int) (level int) (optname int) 
                                       (optval (* t)) (optlen socklen_t))
  (define-alien-routine socket int (domain int) (type int) (protocol int))
  (define-alien-routine sendto int (sockfd int) (buf (* t))
                                   (len size_t) (flags int)
                                   (dest_addr (* (struct sockaddr-in))) (addrlen socklen_t))
  (define-alien-routine recv int (socketfd int) (buf (* t)) (len size_t) (flags int)))

(defun set-broadcast-option (socket)
  (declare #.*muffle-compiler-note*)
  (with-alien ((yes (array int 1)))
    (setf (deref yes 1) 1)
    (or (= 0 (setsockopt socket +SOL_SOCKET+ +SO_BROADCAST+
                        (cast yes (* t)) (alien-size int :bytes)))
        (progn (sb-unix:unix-close socket) nil))))

(defun make-socket-fd (domain type protocol &key broadcast)
  (let ((fd (socket domain type protocol)))
    (if (= fd -1)
        (values nil (get-errno) (sb-int:strerror (get-errno)))
      (if (or (not broadcast)
              (set-broadcast-option fd))
          (values fd nil nil)
        (values nil (get-errno) (sb-int:strerror (get-errno)))))))

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

;; XXX: 
(defun icmp-recv (socket buf flags)
  (declare #.*muffle-compiler-note*)
  (/= 2 (recv socket (cast buf (* t)) ip-echo.size flags)))
(defun icmp-recv2 (socket buf flags)
  (declare #.*muffle-compiler-note*)
  (recv socket (cast buf (* t)) ip-echo.size flags))

(defun extract-message (icmp)
  (case (icmp-echo-header.type icmp)
    (#.+ICMP_ECHO+ "LOOPBACK")
    (#.+ICMP_ECHO_REPLY+ "REPLY")
    (#.+ICMP_UNREACH+
      ;; TODO: out of range check
      (format nil "UNREACHED - ~a"
        (documentation (nth (icmp-echo-header.code icmp) *icmp-unreach-types*)
                       'variable)))
    (otherwise
      (list :unknown (icmp-echo-header.type icmp)))))

(defmacro return-error (block method)
   ;; TODO: throw and catch
  `(return-from ,block (values nil (print (list ,method (get-errno) (sb-int:strerror (get-errno)))))))

(defun echo-and-reply (sock icmp sa flags echo seq-num)
  (let ((took 
          (timing
            (unless (icmp-sendto sock icmp sa flags)
              (return-error echo-and-reply :sendto-error))
            (unless (icmp-recv sock echo 0) 
              (return-error echo-and-reply :recv-error))))
        (reply-ip (slot echo 'ip-header))
        (reply-icmp (slot echo 'icmp-echo)))
    (log-msg "From ~/ping::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms: ~a"
             (addr-to-ip (ip-header.src-addr reply-ip)) seq-num (ip-header.ttl reply-ip) (round took)
             (extract-message reply-icmp))))

(defun bcecho-and-reply (sock icmp sa flags echo seq-num &aux (beg-t (now-ms)))
  (unless (icmp-sendto sock icmp sa flags)
    (return-error bcecho-and-reply :sendto-error))
  (loop WITH wait-limit = 5
        WITH wait-time = 0.1
        FOR ret = (icmp-recv2 sock echo +MSG_DONTWAIT+)
        WHILE (plusp wait-limit)
     DO
     (cond ((/= -1 ret)
             (sleep wait-time)
             (let ((reply-ip (slot echo 'ip-header))
                   (reply-icmp (slot echo 'icmp-echo)))
               (log-msg "From ~/ping::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms: ~a"
                        (addr-to-ip (ip-header.src-addr reply-ip)) seq-num (ip-header.ttl reply-ip) (- (now-ms) beg-t)
                        (extract-message reply-icmp)))
             (when (= ret 0)
               (return)))
          ((= +EAGAIN+ (get-errno))
            (sleep wait-time)
            (decf wait-limit))
          (t (return-error bcecho-and-reply :recv-error)))))

(defun ping-impl (sa seq-num loop-count flags broadcast sleep)
  (declare #.*muffle-compiler-note*)
  (if (>= seq-num loop-count)
      'done
    (with-alien ((icmp (struct icmp-echo-header))
                 (echo (struct ip-echo)))
      (init-icmp-echo-header icmp :type +ICMP_ECHO+ :sequence seq-num)
      (multiple-value-bind (sock errcode reason) 
                           (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+
                                           :broadcast broadcast)
        (unless sock
          (return-from ping-impl (values nil `(:make-socket-error ,errcode ,reason))))
        
        (unwind-protect
            (if (not broadcast)
                (echo-and-reply sock icmp sa flags echo seq-num)
              (bcecho-and-reply sock icmp sa flags echo seq-num))
          (sb-unix:unix-close sock))
        (sleep sleep)
        (ping-impl sa (1+ seq-num) loop-count flags broadcast sleep)))))

(defun make-flags (&key dont-route)
  (let ((flags 0))
    (when dont-route
      (setf flags (logior flags +MSG_DONTROUTE+)))
    flags))
 
(defun ping (target &key (loop-count 10) dont-route broadcast (sleep 0.4))
  (declare #.*muffle-compiler-note*)
  (log-msg "ping to ~a~@[ (~/ping::ip-fmt/)~]" target (resolve-address target))
  (let ((flags (make-flags :dont-route dont-route)))
    (with-alien ((sa (struct sockaddr-in)))
      (n.if (ip (resolve-address target))
          (ping-impl (init-sockaddr-in sa ip) 0 loop-count flags broadcast sleep)
        (values nil `(:unknown-host ,target))))))
