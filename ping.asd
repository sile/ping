(require :asdf)

(defsystem ping 
  :name "ping"
  :author "Takeru Ohta"
  :version "0.0.1"
  :description "icmp ping"
  :serial t
  :components ((:file "package")
               (:file "alien")
               (:file "ping")))