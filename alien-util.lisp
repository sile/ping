(in-package :ping)

(deftype unsigned-type-spec () '(satisfies unsigned-type-spec-p))

(defun unsigned-type-spec-p (type)
  (and (listp type)
       (eq (car type) 'unsigned)
       (integerp (second type))))

(defmacro alien-coerce (obj new-type)
  `(deref (cast (addr ,obj) (* ,new-type))))

(defun eval-alien-size (type &optional (units :bits))
  (eval `(sb-alien:alien-size ,type ,units)))

(defun gen-struct-fields (specs)
  (loop WITH bit-align = 0
        FOR (name type) IN specs
    COLLECT
    (if (/= bit-align 0)
        `(,name ,type :alignment ,bit-align)
      `(,name ,type))
    WHEN (unsigned-type-spec-p type)
    DO
    (setf bit-align (mod (+ bit-align (second type)) 8))))

(defun make-getter (struct-name endian base bit-align name type)
  (let ((val `(slot ,struct-name ',base))
        (size (eval-alien-size type))
        (aligned-size (and (unsigned-type-spec-p type)
                           (* 8 (ceiling (second type) 8)))) )
    (when (unsigned-type-spec-p type)
      (unless (and (eq base name)
                   (= size aligned-size))
        (setf val `(alien-coerce ,val (unsigned ,aligned-size))))
      
      (unless (or (eq endian *native-endian*)
                  (<= aligned-size 8))
        (setf val `(reverse-order ,val ,(/ aligned-size 8))))
      
      (unless (= size aligned-size)
        (setf val 
              `(ldb (byte ,size
                          ,(case endian
                                 (:big    (- aligned-size size bit-align))
                                 (:little bit-align)))
                    ,val))))

    `(defun ,(mksym struct-name"."name) (,struct-name)
       ,val)))

(defun make-setter (struct-name endian base bit-align name type)
  (let* ((pos `(slot ,struct-name ',base))
         (valname (gensym))
         (val valname)
         (size (eval-alien-size type))
         (aligned-size (and (unsigned-type-spec-p type)
                            (* 8 (ceiling (second type) 8)))))
    (when (unsigned-type-spec-p type)
      (unless (and (eq base name)
                   (= size aligned-size))
        (setf pos `(alien-coerce ,pos (unsigned ,aligned-size))))
      
      (unless (or (eq endian *native-endian*)
                  (<= aligned-size 8))
        (setf val `(reverse-order ,val ,(/ aligned-size 8))))
      
      (unless (= size aligned-size)
        (let ((offset (case endian 
                            (:big    (- aligned-size size bit-align))
                            (:little bit-align))))
          (setf pos `(ldb (byte ,size ,offset) ,pos)))))
      
    `(progn
       (defun ,(mksym "%set-"struct-name"."name) (,struct-name ,valname)
         (setf ,pos ,val)
         ,valname)
       
       (defsetf ,(mksym struct-name"."name) ,(mksym "%set-"struct-name"."name)))))

(defun make-accessor (struct-name endian base bit-align name type)
  (list (make-getter struct-name endian base bit-align name type)
        (make-setter struct-name endian base bit-align name type)))

(defun make-accessors (struct-name endian specs)
  (loop WITH bit-align = 0
        WITH base = nil
        FOR (name type) IN specs
    APPEND
    (prog2 
        (when (= bit-align 0)
          (setf base name))
        (make-accessor struct-name endian base bit-align name type)
      (when (unsigned-type-spec-p type)
        (setf bit-align (mod (+ bit-align (second type)) 8))))))

(defun gen-alien-struct-printer (struct-name specs)
  (let ((stream (gensym))
        (object (gensym)))
    `(lambda (,stream ,object)
       (print-unreadable-object (,object ,stream)
         (format ,stream "~a ~s ~s #X~x" 
                 '(alien ,struct-name)
                 (list ,@(loop FOR (field _ . keys) IN specs
                               FOR getter = (mksym struct-name"."field)
                               FOR name = (intern (symbol-name field) :keyword)
                               COLLECT 
                               (destructuring-bind (&key (show 'identity)) keys
                                 `(list ,name (,show (,getter ,object))))))
                 :sap
                 (sb-sys:sap-int (sb-alien:alien-sap ,object)))))))

(defmacro define-alien-struct (name (&key (endian *native-endian*)) &rest field-specs)
  `(progn
     (sb-alien:define-alien-type ,name
       (struct nil ,@(gen-struct-fields field-specs)))

     ,@(make-accessors name endian field-specs)

     (set-pprint-dispatch '(sb-alien:alien ,name)
       ,(gen-alien-struct-printer name field-specs)
       0)

     (define-symbol-macro ,(mksym name ".size") (sb-alien:alien-size ,name :bytes))
     (define-symbol-macro ,(mksym name ".bit-size") (sb-alien:alien-size ,name))
     ',name))
