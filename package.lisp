;; Licensed under the MIT License found in the file named LICENSE.

(in-package :cl-user)

(defpackage #:bitio
  (:use #:cl)
  (:export #:bitio
           #:make-bitio
           #:read-bits
           #:read-one-byte
           #:read-bytes
           #:read-integer
           #:octet-read-boundary-p))

(uiop:define-package #:bitio.gray
    (:use #:cl #:bitio #:fast-io)
  (:mix #:uiop
        #:trivial-gray-streams
        #:cl-package-locks
        #:checkl)
  (:export :make-bitio-stream
           :input-stream-p
           :peek-byte
           :stream-bit-endian
           :stream-byte-endian))

(in-package #:bitio)
