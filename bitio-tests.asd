(in-package :cl-user)

(asdf:defsystem #:bitio-tests
  :description "Unit tests for bitio."
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:bitio
               #:prove)
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "tests")))
