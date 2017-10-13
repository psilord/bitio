;; Copyright 2010 Peter K. Keller (psilord@cs.wisc.edu)
;;
;; Licensed under MIT License found in the LICENSE file.

(in-package :cl-user)

(asdf:defsystem #:bitio
  :description "A wrapper for octet streams that enable bit level streams."
  :version "0.1"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License"

  :serial t
  :depends-on (#:fast-io)
  :components ((:file "package")
	       (:file "utils")
               (:file "bitio")
	       (:file "bitio-read")
	       (:file "tests")))
