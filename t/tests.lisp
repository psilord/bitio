(in-package :bitio-tests)

(defparameter *octet-vector*
  (make-array 10
              :element-type 'fast-io:octet
              :initial-contents
              '(#x5c #xf6 #xee #x79 #x9a #xde #xff #xf2 #x88 #x02)))

(plan 1)

(diag "read-bits, fst path, big-endian")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 8) #x5c)))

(finalize)
