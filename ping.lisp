(in-package :ping)

(declaim #.*muffle-compiler-note*)

(eval-when (:load-toplevel)
  (define-alien-routine setsockopt int (sockfd int) (level int) (optname int) 
                                       (optval (* t)) (optlen socklen_t))
  (define-alien-routine socket int (domain int) (type int) (protocol int))
  (define-alien-routine sendto int (sockfd int) (buf (* t))
                                   (len size_t) (flags int)
                                   (dest_addr (* sockaddr-in)) (addrlen socklen_t))
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
          fd
        nil))))

(defun init-sockaddr-in (sa ip)
  (setf (sockaddr-in.family sa) +AF_INET+
        (sockaddr-in.port sa) 0
        (sockaddr-in.addr sa) (ip-to-addr ip))
  sa)

(defun init-icmp-header (icmp &key type sequence)
  (memset icmp 0 icmp-header.size)
  (setf (icmp-header.type icmp) type
        (icmp-header.seq-num icmp) sequence
        (icmp-header.checksum icmp) (checksum icmp icmp-header.size))
  icmp)

(defun icmp-sendto (socket icmp sa flags)
  (/= -1 (sendto socket
                 (cast icmp (* t)) icmp-header.size flags
                 (cast sa (* sockaddr-in)) sockaddr-in.size)))

(defun icmp-recv (socket packet flags)
  (case (recv socket (cast packet (* t)) packet.size flags)
    (-1 :error)
    (0  :shutdown)
    (t  :ok)))

(defun get-echo-message (icmp)
  (case (icmp-header.type icmp)
    (#.+ICMP_ECHO+ "LOOPBACK")
    (#.+ICMP_ECHO_REPLY+ "REPLY")
    (#.+ICMP_UNREACH+
      (format nil "UNREACHED - ~a"
        (documentation (nth (icmp-header.code icmp) *icmp-unreach-types*)
                       'variable)))
    (otherwise
     (format nil "UNKNOWN TYPE: ~d" (icmp-header.type icmp)))))

(defmacro throw-error (tag &key (alien-error t))
  `(throw :error
          (list ,tag 
                (when ,alien-error (format nil "~A[~D]" (sb-int:strerror (get-errno)) (get-errno))))))

(defun echo-and-reply (sock icmp sa flags packet seq-num)
  (let ((took 
          (timing
            (unless (icmp-sendto sock icmp sa flags)
              (throw-error :sendto-error))
            (case (icmp-recv sock packet 0)
              (:error (throw-error :recv-error))
              (:shutdown (throw-error :recv-peer-closed :alien-error nil)))))
        (reply-ip (slot packet 'ip))
        (reply-icmp (slot packet 'icmp)))
    (log-msg "From ~/ping::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms: ~a"
             (ip-header.src-addr reply-ip) seq-num (ip-header.ttl reply-ip) (round took)
             (get-echo-message reply-icmp))))

;; http://linux.die.net/man/7/socket
(defun bcecho-and-reply (sock icmp sa flags packet seq-num &aux acc (beg-t (now-ms)))
  (unless (icmp-sendto sock icmp sa flags)
    (throw-error :sendto-error))
  (loop WITH wait-limit = 100
        WITH wait-time = 0.01
        FOR ret = (icmp-recv sock packet +MSG_DONTWAIT+)
        WHILE (plusp wait-limit)
     DO
     (case ret
       (:ok 
        (let ((reply-ip (slot packet 'ip))
              (reply-icmp (slot packet 'icmp)))
          (push (list (ip-header.src-addr reply-ip) seq-num 
                      (ip-header.ttl reply-ip) (- (now-ms) beg-t)
                      (get-echo-message reply-icmp)) acc)))
       (:shutdown 
        (return))
       (:error 
        (if (= (get-errno) +EAGAIN+)
            (progn (sleep wait-time)
                   (decf wait-limit))
          (throw-error :recv-error)))))
   (loop FOR (a b c d e) IN (reverse acc)
     DO
     (log-msg "From ~/ping::ip-fmt/: icmp_seq=~d ttl=~d time=~d ms: ~a" a b c d e)))

(defun ping-impl (sa seq-num loop-count flags broadcast sleep)
  (if (>= seq-num loop-count)
      'done
    (with-alien ((icmp icmp-header)
                 (packet packet))
      (init-icmp-header icmp :type +ICMP_ECHO+ :sequence seq-num)
      (let ((sock (make-socket-fd +AF_INET+ +SOCK_RAW+ +IPPROTO_ICMP+ :broadcast broadcast)))
        (unless sock
          (throw-error :make-socket-error))
        
        (unwind-protect
            (if (not broadcast)
                (echo-and-reply sock icmp sa flags packet seq-num)
              (bcecho-and-reply sock icmp sa flags packet seq-num))
          (sb-unix:unix-close sock))
        (sleep sleep)
        (ping-impl sa (1+ seq-num) loop-count flags broadcast sleep)))))

(defun make-flags (&key dont-route)
  (let ((flags 0))
    (when dont-route
      (setf flags (logior flags +MSG_DONTROUTE+)))
    flags))

(defun ping (target &key (loop-count 10) dont-route broadcast (sleep 0.4))
  (log-msg "ping to ~a~@[ (~/ping::ip-fmt/)~]" target 
           (to-network-order (ip-to-addr (resolve-address target)) 4))
  (let ((flags (make-flags :dont-route dont-route)))
    (with-alien ((sa sockaddr-in))
      (n.if (ip (resolve-address target))
           (catch :error 
             (ping-impl (init-sockaddr-in sa ip) 0 loop-count flags broadcast sleep))
        `(:unknown-host ,target)))))
