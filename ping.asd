(require :asdf)

(defsystem ping 
  :name "ping"
  :author "Takeru Ohta"
  :version "0.0.2"
  :description "icmp ping"
  
  :depends-on (:sb-bsd-sockets)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "type")
               (:file "ping")))
