;; Licensed under the MIT License found in the file named LICENSE.

(defpackage #:bitio
  (:use #:cl)
  (:export #:bitio
           #:make-bitio
           #:read-bits
           #:read-one-byte
           #:read-bytes
           #:read-integer
           #:octet-read-boundary-p))

(in-package #:bitio)
