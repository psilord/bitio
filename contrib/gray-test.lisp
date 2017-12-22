;;;; bitio/contrib/gray-test.lisp

(in-package :bitio.gray)

(check (:name :bitio.gray)
  (nest
   (let ((element-type '(unsigned-byte 8))))
   (with-temporary-file (:stream octet-stream :direction :io :element-type element-type)
     (write-sequence (octets-from '(1 2 3 4)) octet-stream)
     (file-position octet-stream 0))
   (let* ((bs (make-bitio-stream octet-stream #'read-byte #'read-sequence
                                 :byte-endian :be :bit-endian :be :bits-per-byte 8)))
     (results
      (read-byte bs)
      (read-byte bs)
      (read-byte bs)
      (read-byte bs)
      (file-position bs)
      (stream-element-type bs)
      (file-position bs 0)
      (file-position bs)
      (setf (stream-element-type bs) '(unsigned-byte 16))
      (stream-element-type bs)
      (read-byte bs)
      (read-byte bs)
      (file-position bs)
      (file-position bs 0)
      (let ((seq (make-octet-vector 4)))
        (setf (stream-element-type bs) '(unsigned-byte 8))
        (values seq (read-sequence seq bs)))))))
