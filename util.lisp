(in-package :ping)

(defun reverse-order (int size)
  (loop FOR i FROM 0 TO size
        FOR j FROM (1- size) DOWNTO 0
        WHILE (< i j)
    DO
    (rotatef (ldb (byte 8 (* i 8)) int)
             (ldb (byte 8 (* j 8)) int)))
  int)

(defun resolve-address (hostname-or-ipaddress &aux (host hostname-or-ipaddress))
  (handler-case
   (if (stringp host)
       (first (sb-bsd-sockets:host-ent-addresses
               (sb-bsd-sockets:get-host-by-name host)))
     host)
   (sb-bsd-sockets:name-service-error ()
     nil)))

(defun to-network-order (int size)
  (declare (ignorable size))
  #.(if (eq *native-endian* :big)
        'int
      '(reverse-order int size)))

(defun addr-to-ip (ip)
  (loop FOR i FROM 3 DOWNTO 0
        COLLECT (ldb (byte 8 (* i 8)) ip) INTO list
        FINALLY (return (coerce list 'vector))))

(defun ip-to-addr (ip-vector)
  (loop FOR n ACROSS ip-vector
        FOR i FROM 0 TO 3
        SUM (ash n (* i 8))))

(defun memset (sap value size)
  (declare ((alien (unsigned 8)) value))
  (let ((*p (cast sap (* (unsigned 8)))))
    (dotimes (i size sap)
      (setf (deref *p i) value))))

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

(defun now-ms ()
  (* (/ (get-internal-real-time) INTERNAL-TIME-UNITS-PER-SECOND) 1000))

(defmacro timing (&body exps)
  (let ((begin (gensym)))
    `(let ((,begin (get-internal-real-time)))
       ,@exps
       (* (/ (- (get-internal-real-time) ,begin) INTERNAL-TIME-UNITS-PER-SECOND) 1000))))

(defun dotted-ip (addr)
  (format nil "~d.~d.~d.~d" 
          (ldb (byte 8 24) addr) (ldb (byte 8 16) addr) 
          (ldb (byte 8 08) addr) (ldb (byte 8 00) addr)))

(defun mksym (&rest args)
  (intern (format nil "~{~:@(~a~)~}" args)))

(defmacro n.if ((var exp) then else)
  `(let ((,var ,exp))
     (if ,var ,then ,else)))

(defun log-msg (fmt &rest args)
  (format *error-output* "; ~?~%" fmt args))

(defun ip-fmt (stream ip &rest ignore)
  (declare (ignore ignore))
  (write-string (dotted-ip ip) stream))