
(asdf:defsystem #:cl-factoring-test
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL (http://opensource.franz.com/preamble.html)"
  :description "CL-Factoring test suite"
  :components ((:file "test"))
  :serial t
  :depends-on (:cl-primality :cl-factoring :stefil :iterate))
