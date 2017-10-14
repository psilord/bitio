(in-package :bitio-tests)

(defvar *bitio*)

(defparameter *octet-vector*
  (make-array 10
              :element-type 'fast-io:octet
              :initial-contents
              '(#x5c #xf6 #xee #x79 #x9a #xde #xff #xf2 #x88 #x02)))

(plan 68)

(diag "read-bits: fast path, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 8 :bit-endian :be) #x5C
        "8 bits, big-endian: #x5C")
    (is (read-bits bitio 16 :bit-endian :be) #xF6EE
        "16 bits, big-endian: #xF6EE")
    (is (read-bits bitio 24 :bit-endian :be) #x799ADE
        "24 bits, big-endian: #x799ADE")
    (is (read-bits bitio 32 :bit-endian :be) #xFFF28802
        "32 bits, big-endian: #xFFF28802")))

(diag "read-bits: fast path, bit-endianness: LE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 8 :bit-endian :le) #x3A
        "8 bits, little-endian: #x3A")
    (is (read-bits bitio 16 :bit-endian :le) #x6F77
        "16 bits, little-endian: #x6F77")
    (is (read-bits bitio 24 :bit-endian :le) #x9E597B
        "24 bits, little-endian: #x9E597B")
    (is (read-bits bitio 32 :bit-endian :le) #xFF4F1140
        "32 bits, little-endian: #xFF4F1140")))

(diag "read-bits: slow path, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")))

(diag "read-bits: slow path, bit-endianness: LE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")))

(diag "read-bits: slow path, bit-endianness: LE/BE mixed")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :le) #b0
        "1 bit, little-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :le) #b1
        "1 bit, little-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 1 :bit-endian :be) #b1
        "1 bit, big-endian: #b1")))

(diag "read-bits: slow path without fast, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 4 :bit-endian :be) #b0101
        "4 bits, big-endian: #b0101")
    (is (read-bits bitio 4 :bit-endian :be) #b1100
        "4 bits, big-endian: #b1100")
    (is (read-bits bitio 3 :bit-endian :be) #b111
        "3 bits, big-endian: #b111")
    (is (read-bits bitio 5 :bit-endian :be) #b10110
        "5 bits, big-endian: #b10110")
    (is (read-bits bitio 2 :bit-endian :be) #b11
        "2 bits, big-endian: #b11")
    (is (read-bits bitio 6 :bit-endian :be) #b101110
        "6 bits, big-endian: #b101110")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")
    (is (read-bits bitio 7 :bit-endian :be) #b1111001
        "7 bits, big-endian: #b1111001")
    (is (read-bits bitio 2 :bit-endian :be) #b10
        "2 bits, big-endian: #b10")
    (is (read-bits bitio 2 :bit-endian :be) #b01
        "2 bits, big-endian: #b01")
    (is (read-bits bitio 2 :bit-endian :be) #b10
        "2 bits, big-endian: #b10")
    (is (read-bits bitio 2 :bit-endian :be) #b10
        "2 bits, big-endian: #b10")
    (is (read-bits bitio 12 :bit-endian :be) #xDEF
        "12 bits, big-endian: #xDEF")
    (is (read-bits bitio 12 :bit-endian :be) #xFF2
        "12 bits, big-endian: #xFF2")
    (is (read-bits bitio 15 :bit-endian :be) #b100010000000001
        "15 bits, big-endian: #b100010000000001")
    (is (read-bits bitio 1 :bit-endian :be) #b0
        "1 bit, big-endian: #b0")))

(diag "read-bits: slow path with fast, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 4 :bit-endian :be) #x5
        "4 bits, big-endian: #x5")
    (is (read-bits bitio 72 :bit-endian :be) #xCF6EE799ADEFFF2880
        "72 bits, big-endian: #xcf6ee799adefff2880")
    (is (read-bits bitio 4 :bit-endian :be) #x2
        "4 bits, big-endian: #x5")))

(diag "read-bits: fast path with EOF, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 88 :bit-endian :be :eof-error-p nil :eof-value :eof) #x5CF6EE799ADEFFF28802
        "88 bits, big-endian: #x5cf6ee799adefff28802")
    (is (read-bits bitio 80 :bit-endian :be :eof-error-p nil :eof-value :eof) :eof
        "80 bits, big-endian: NIL")))

(diag "read-bits: slow path with EOF, bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 4 :bit-endian :be) #x5
        "4 bits, big-endian: #x5")
    (is (read-bits bitio 84 :bit-endian :be :eof-error-p nil :eof-value :eof) #xCF6EE799ADEFFF28802
        "84 bits, big-endian: #xcf6ee799adefff28802")
    (is (read-bits bitio 4 :bit-endian :be :eof-error-p nil :eof-value :eof) :eof
        "4 bits, big-endian: NIL")
    (is (read-bits bitio 6 :bit-endian :be :eof-error-p nil :eof-value :eof) :eof
        "8 bits, big-endian: NIL")
    (is (read-bits bitio 12 :bit-endian :be :eof-error-p nil :eof-value :eof) :eof
        "12 bits, big-endian: NIL")))

(diag "read-bits: slow path with EOF (again), bit-endianness: BE")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-bits bitio 76 :bit-endian :be) #x5CF6EE799ADEFFF2880
        "76 bits, big-endian: #x5cf6ee799adefff2880")
    (is (read-bits bitio 8 :bit-endian :be :eof-error-p nil :eof-value :eof) #x2
        "8 bits, big-endian: #x2")
    (is (read-bits bitio 4 :bit-endian :be :eof-error-p nil :eof-value :eof) :eof
        "4 bits, big-endian: NIL")))

(diag "read-one-byte: bit-endianness: BE, bit-width: 8")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-one-byte bitio :byte-width 8 :bit-endian :be) #x5c
        "1 8-bit byte, big-endian: #x5c")))

(diag "read-one-byte: bit-endianness: BE, bit-width: 12")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-one-byte bitio :byte-width 12 :bit-endian :be) #x5CF
        "1 12-bit byte, big-endian: #x5cf")
    (is (read-one-byte bitio :byte-width 12 :bit-endian :be) #x6EE
        "1 12-bit byte, big-endian: #x6ee")))

(diag "read-one-byte: bit-endianness: LE, bit-width: 12")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte)))
    (is (read-one-byte bitio :byte-width 12 :bit-endian :le) #x3A6
        "1 12-bit byte, little-endian: #x3a6")
    (is (read-one-byte bitio :byte-width 12 :bit-endian :le) #xF77
        "1 12-bit byte, little-endian: #xF77")))

(diag "read-bytes: bit-endianness: BE, bit-width: 4")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte))
        (sequence (make-array (ceiling (/ (length *octet-vector*) 0.5))
                              :element-type '(unsigned-byte 4)
                              :initial-element 0)))
    (fail "#(#x5 #xc #xf #x6 #xe #xe #x7 #x9 #x9 #xa #xd #xe #xf #xf #xf #x2 #x8 #x8 #x0 #x2)")))

(diag "read-bytes: bit-endianness: LE, bit-width: 4")
(fast-io:with-fast-input (in *octet-vector*)
  (let ((bitio (make-bitio in #'fast-io:fast-read-byte))
        (sequence (make-array (ceiling (/ (length *octet-vector*) 0.5))
                              :element-type '(unsigned-byte 4)
                              :initial-element 0)))
    (fail (format nil "~a" (map 'vector
                           (lambda (x) (bitio::integer-reverse x 4))
                           #(#xc #x5 #x6 #xf #xe #xe #x9 #x7
                             #xa #x9 #xe #xd #xf #xf #x2 #xf
                             #x8 #x8 #x2 #x0))))))

(finalize)
