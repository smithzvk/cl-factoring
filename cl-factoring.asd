
(asdf:defsystem #:cl-factoring
  :name "Integer Factoring"
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :components ((:file "cl-factoring"))
  :serial t
  :depends-on (iterate cl-primality))
