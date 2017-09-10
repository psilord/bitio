(in-package :bitio)

(defun dbgval (val fmt &rest args)
  ;; TODO: Fix to accept the number of bits that SHOULD be printed
  ;; out, and zero pad with additional format args as appropriate.
  ;; TODO, and fix the wonky FMT usage and ordering.
  (apply #'format t fmt args)
  (format t ": #x~X (#b~B)~%" val val)
  (finish-output))


(defun make-octet-vector ()
  (make-array 10 :element-type '(unsigned-byte 8)
                 :initial-contents '(#x5c
                                     #xf6 #xee
                                     #x79 #x9a #xde
                                     #xff #xf2 #x88 #x02)))

(defun test-bit-read-bits (bitio num-bits-to-read bit-endian expected-value
                           &optional
                             (expected-bits-to-have-been-read num-bits-to-read)
                             (eof-error-p T)
                             (eof-value NIL))
  (multiple-value-bind (value bit-read-count)
      (bit-read-bits bitio num-bits-to-read bit-endian eof-error-p eof-value)
    (dbgval value (format nil "~D bits ~(~S~) should be #x~X"
                          num-bits-to-read bit-endian expected-value))

    (if (and (not eof-error-p) (equal value eof-value))
        ;; In this case, expected value is known to be the eof-value!
        (assert (eql expected-bits-to-have-been-read bit-read-count))
        ;; In this case, we either check a full value or a truncated one.
        (assert (and (equalp expected-value value)
                     (eql expected-bits-to-have-been-read bit-read-count))))))

(defun test-bit-read-byte (bitio byte-width bit-endian expected-value
                           &optional
                             (expected-bits-to-have-been-read byte-width)
                             (eof-error-p T)
                             (eof-value NIL))
  (multiple-value-bind (value bit-read-count)
      (bit-read-byte bitio byte-width bit-endian eof-error-p eof-value)
    (dbgval value (format nil "~D bits ~(~S~) should be #x~X"
                          byte-width bit-endian expected-value))

    (if (and (not eof-error-p) (equal value eof-value))
        ;; In this case, expected value is known to be the eof-value!
        (assert (eql expected-bits-to-have-been-read bit-read-count))
        ;; In this case, we either check a full value or a truncated one.
        (assert (and (equalp expected-value value)
                     (eql expected-bits-to-have-been-read bit-read-count))))))


(defun test-bit-read-integer (bitio num-bytes byte-width
                              bit-endian byte-endian unsignedp expected-value)
  (let ((value (bit-read-integer bitio
                                 :bit-endian bit-endian
                                 :byte-endian byte-endian
                                 :num-bytes num-bytes
                                 :byte-width byte-width
                                 :unsignedp unsignedp)))
    (dbgval value (format nil "bit-read-integer (num-bytes: ~A, byte-width: ~A, bit-endian: ~A, byte-endian: ~A, unsignedp: ~A): [#x~X] should be #x~X"
                          num-bytes byte-width bit-endian byte-endian unsignedp
                          value expected-value))

    ;; Currently I ignore eof-error-p and eof-value, I need to think about
    ;; how to add that in.

    (assert (equalp expected-value value))))


;; Note "byte" doesn't necessarily mean 8 bit octets!
(defun test-bit-read-bytes (bitio seq bit-endian byte-width
                            expected-seq &key (start 0) end)

  (let ((num-parts-read (bit-read-bytes
                         bitio seq bit-endian byte-width
                         :start start :end end)))
    (let ((*print-right-margin* 9999))
      (format t "seq(byte-width: ~A, bit-endian: ~A, seq ~X [start: ~A, end: ~A]) should be ~X~%"
              byte-width bit-endian seq start end expected-seq))

    (assert (eql (length expected-seq) num-parts-read))
    ;; Check the sequence range we're supposed to have read is ok.
    (loop :for i :from start :below (if (null end) (length seq) end)
          :do (assert (eql (aref seq i) (aref expected-seq i))))))



(defun doit ()
  (let ((octet-vector (make-octet-vector)))
    (format t "Test Octet vector: ~X~%" octet-vector)
    (fast-io:with-fast-input (fiobuf octet-vector)
      (format t "Case: bit-read-bits, fast read path, bit-endian: :be~%")
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-bits bitio 8 :be #x5C)
        (test-bit-read-bits bitio 16 :be #xF6EE)
        (test-bit-read-bits bitio 24 :be #x799ADE)
        (test-bit-read-bits bitio 32 :be #xFFF28802)
        ))

    (format t "Case: bit-read-bits, fast read path, bit-endian: :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-bits bitio 8 :le #x3A)
        (test-bit-read-bits bitio 16 :le #x6F77)
        (test-bit-read-bits bitio 24 :le #x9E597B)
        (test-bit-read-bits bitio 32 :le #xFF4F1140)
        ))

    (format t "Case: bit-read-bits, slow read path 1 bit, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; COnsume 1 octet
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 1 :be #b1)
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 1 :be #b1)
        (test-bit-read-bits bitio 1 :be #b1)
        (test-bit-read-bits bitio 1 :be #b1)
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 1 :be #b0)
        ))

    (format t "Case: bit-read-bits, slow read path 1 bit, bit-endian: :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1 octet
        (test-bit-read-bits bitio 1 :le #b0)
        (test-bit-read-bits bitio 1 :le #b0)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :le #b0)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :le #b0)
        ))

    (format t "Case: bit-read-bits, slow read path 1 bit, bit-endian: both~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1 octet :le bits first, then :be of the remaining bits.
        (test-bit-read-bits bitio 1 :le #b0)
        (test-bit-read-bits bitio 1 :le #b0)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :le #b1)
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 1 :be #b1)
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 1 :be #b1)
        ))


    (format t "Case: bit-read-bits, slow read path no fast, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1 octet
        (test-bit-read-bits bitio 4 :be #b0101)
        (test-bit-read-bits bitio 4 :be #b1100)

        ;; Consume 1 octet
        (test-bit-read-bits bitio 3 :be #b111)
        (test-bit-read-bits bitio 5 :be #b10110)

        ;; Consume 1 octet
        (test-bit-read-bits bitio 2 :be #b11)
        (test-bit-read-bits bitio 6 :be #b101110)

        ;; Consume 1 octet
        (test-bit-read-bits bitio 1 :be #b0)
        (test-bit-read-bits bitio 7 :be #b1111001)

        ;; Consume 1 octet
        (test-bit-read-bits bitio 2 :be #b10)
        (test-bit-read-bits bitio 2 :be #b01)
        (test-bit-read-bits bitio 2 :be #b10)
        (test-bit-read-bits bitio 2 :be #b10)

        ;; Consume 3 octets in 2 reads
        (test-bit-read-bits bitio 12 :be #xdef)
        (test-bit-read-bits bitio 12 :be #xff2)

        ;; Consume 2 octets in non-symmetric read
        (test-bit-read-bits bitio 15 :be #b100010000000001)
        (test-bit-read-bits bitio 1 :be #b0)
        ))

    (format t "Case: bit-read-bits, slow read path with fast, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume .5 octet
        (test-bit-read-bits bitio 4 :be #x5)

        ;; Consume .5 octet, 8 octets, .5 octet
        (test-bit-read-bits bitio 72 :be #xcf6ee799adefff2880)

        ;; Consume .5 octet
        (test-bit-read-bits bitio 4 :be #x2)
        ))

    (format t "Case: bit-read-bits, fast read path with eof, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 10 legal octets, and then one more (80 bits plus 8
        ;; bits) resulting in EOF.  Expected value here is the
        ;; expected truncated return.
        (test-bit-read-bits bitio 88 :be #x5cf6ee799adefff28802 80 NIL :eof)
        ;; Here we blatently read an :eof again, so we expect to see an :eof
        (test-bit-read-bits bitio 80 :be :eof 0 NIL :eof)
        ))

    (format t "Case: bit-read-bits, slow read path with eof, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume .5 octet
        (test-bit-read-bits bitio 4 :be #x5)

        ;; Consume .5 octet, 9 legal octets, then 1 more octet which is EOF
        ;; The value is the expected truncated read.
        (test-bit-read-bits bitio 84 :be #xcf6ee799adefff28802 76 NIL :eof)

        ;; Consume .5 octets, but get :eof
        (test-bit-read-bits bitio 4 :be :eof 0 NIL :eof)

        ;; Consume 1 octet, but get :eof
        (test-bit-read-bits bitio 8 :be :eof 0 NIL :eof)

        ;; Consume 1.5 octet, but get :eof
        (test-bit-read-bits bitio 12 :be :eof 0 NIL :eof)

        ))

    (format t "Case: bit-read-bits, slow read path with eof2, bit-endian: :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume .5 octet, 9 legal octets. This leaves 1 for the stable.
        (test-bit-read-bits bitio 76 :be #x5cf6ee799adefff2880)

        ;; Consume 1 octet, but get an expected short read of 4 bits
        (test-bit-read-bits bitio 8 :be #x2 4 NIL :eof)

        ;; Consume .5 octet, but get eof
        (test-bit-read-bits bitio 4 :be :eof 0 NIL :eof)

        ))

    (format t "Case: bit-read-byte, bit-endian: :be, 8 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1 octet as 1 byte.
        (test-bit-read-byte bitio 8 :be #x5c)

        ))

    (format t "Case: bit-read-byte, bit-endian: :be, 12 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1.5 octets, as 1 byte
        (test-bit-read-byte bitio 12 :be #x5cf)
        ;; Consume 1.5 octets, as 1 byte
        (test-bit-read-byte bitio 12 :be #x6ee)

        ))

    ;; This one may look non-intuitive...
    (format t "Case: bit-read-byte, bit-endian: :le, 12 bits wide~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        ;; Consume 1.5 octets, as 1 byte
        (test-bit-read-byte bitio 12 :le #x3a6)
        ;; Consume 1.5 octets, as 1 byte
        (test-bit-read-byte bitio 12 :le #xf77)

        ))

    (format t "Case: bit-read-bytes, bit-width 4, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :be 4
                               #(#x5 #xc #xf #x6 #xe #xe #x7 #x9 #x9 #xa
                                 #xd #xe #xf #xf #xf #x2 #x8 #x8 #x0 #x2)))
        ))

    (format t "Case: bit-read-bytes, bit-width 4, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :le 4
                               (map 'vector
                                    (lambda (x) (integer-reverse x 4))
                                    #(#xc #x5 #x6 #xf #xe #xe #x9 #x7
                                      #xa #x9 #xe #xd #xf #xf #x2 #xf
                                      #x8 #x8 #x2 #x0))))
        ))

    (format t "Case: bit-read-bytes, bit-width 8, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :be 8
                               #(#x5c #xf6 #xee #x79 #x9a
                                 #xde #xff #xf2 #x88 #x02)))
        ))

    (format t "Case: bit-read-bytes, bit-width 8, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :le 8
                               (map 'vector
                                    (lambda (x) (integer-reverse x 8))
                                    #(#x5c #xf6 #xee #x79 #x9a
                                      #xde #xff #xf2 #x88 #x02))))
        ))

    (format t "Case: bit-read-bytes, bit-width 12, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1.5))
                               :element-type '(unsigned-byte 12)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :be 12
                               #(#x5cf #x6ee #x799 #xade #xfff #x288 #x02)))
        ))

    (format t "Case: bit-read-bytes, bit-width 16, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 2))
                               :element-type '(unsigned-byte 16)
                               :initial-element 0)))

          (test-bit-read-bytes bitio seq :be 16
                               #(#x5cf6 #xee79 #x9ade #xfff2 #x8802)))
        ))

    (format t "Case: bit-read-integer, case 1~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 1 8 :be :le T #x5c)
        (test-bit-read-integer bitio 2 8 :be :le T #xeef6)
        (test-bit-read-integer bitio 3 8 :be :le T #xde9a79)
        (test-bit-read-integer bitio 4 8 :be :le T #x0288f2ff)
        ))

    (format t "Case: bit-read-integer, case 2~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 1 8 :be :be T #x5c)
        (test-bit-read-integer bitio 2 8 :be :be T #xf6ee)
        (test-bit-read-integer bitio 3 8 :be :be T #x799ade)
        (test-bit-read-integer bitio 4 8 :be :be T #xfff28802)
        ))

    (format t "Case: bit-read-integer, case 3~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 1 4 :be :be T #x5)
        (test-bit-read-integer bitio 1 4 :le :be T #x3)
        (test-bit-read-integer bitio 1 8 :le :be T #x6f)
        (test-bit-read-integer bitio 1 8 :be :be T #xee)
        (test-bit-read-integer bitio 1 8 :le :be NIL
                               (sign-extend (integer-reverse #x79 8) 8))
        (test-bit-read-integer bitio 1 8 :be :be NIL
                               (sign-extend #x9a 8))

        (test-bit-read-integer bitio 1 4 :be :be T #xd)
        ;; intentional misaligned octet read...
        (test-bit-read-integer bitio 4 8 :be :be T #xefff2880)
        (test-bit-read-integer bitio 1 4 :be :be T #x2)

        ))

    (format t "Case: bit-read-integer, case 4~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 4 8 :be :be T #x5cf6ee79)
        (test-bit-read-integer bitio 4 8 :be :le T #xf2ffde9a)
        ))

    (format t "Case: bit-read-integer, case 5~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 4 12 :be :be T #x5cf6ee799ade)
        ))

    (format t "Case: bit-read-integer, case 6~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 4 12 :le :be T #x3a6f779e597b)
        (test-bit-read-integer bitio 4 8 :be :be T #xfff28802)
        ))

    (format t "Case: bit-read-integer, case 7~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 4 16 :be :be T #x5cf6ee799adefff2)
        ))

    (format t "Case: bit-read-integer, case 8~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (test-bit-read-integer bitio 4 16 :be :le T #xfff29adeee795cf6)
        ))

    ;; Test a non fast-io stream
    (with-open-file (fin (asdf:system-relative-pathname :bitio "binfile")
                         :direction :input
                         :element-type '(unsigned-byte 8)
                         :if-does-not-exist :error)
      ;; wrap fin stream with a bitio stream.
      (let ((bitio (make-bitio fin #'read-byte)))
        (test-bit-read-bits bitio 88 :be #x000102030405060708090a)
        ))

    (format t "All done.~%")
    ))
