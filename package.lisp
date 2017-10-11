;; Licensed under the MIT License found in the file named LICENSE.

(defpackage #:bitio
  (:use #:cl)
  (:export #:bitio
           #:make-bitio
           #:bit-read-bits
           #:bit-read-one-byte
           #:bit-read-bytes
           #:bit-read-integer
           #:bit-octet-read-boundary-p))

(in-package #:bitio)
