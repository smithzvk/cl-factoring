
(asdf:defsystem #:cl-factoring
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL (http://opensource.franz.com/preamble.html) except for the
  file \"ulimyhmpqs-1.0.lisp\" which is 3 Clause BSD"
  :description "Integer Factoring"
  :components ((:file "ulimyhmpqs-1.0")
               (:file "cl-factoring"))
  :serial t
  :depends-on (:iterate :cl-primality))
