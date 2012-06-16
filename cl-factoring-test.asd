
(asdf:defsystem #:cl-factoring-test
  :name "Integer factorization testing"
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :components ((:file "test"))
  :serial t
  :depends-on (cl-primality cl-factoring stefil iterate))
