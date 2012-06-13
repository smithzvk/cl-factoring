
(asdf:defsystem #:cl-factoring
  :name "Integer Factoring"
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL or more permissive (Ulimy's code is BSD-ish)"
  :components ((:file "ulimyhmpqs-1.0")
               (:file "cl-factoring"))
  :serial t
  :depends-on (iterate cl-primality))
