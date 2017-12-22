;;;; bitio/gray.lisp

(in-package :bitio.gray)

(defclass bitio-stream (bitio fundamental-binary-stream)
  ())

(defclass bitio-input-stream (bitio-stream fundamental-input-stream) ())

;; (defmethod ms:class-persistent-slots ((self bitio-input-stream))
;;   '(bitio::%default-bits-per-byte bitio::%read-bit-stable
;;     bitio::%bitio/read-octet bitio::%bitio/read-sequence bitio::%default-bit-endian
;;     bitio::%default-byte-endian bitio::%default-bits-per-byte))

(defun make-bitio-stream (octet-stream bitio/read-octet bitio/read-sequence
                          &key (bit-endian :be) (byte-endian :le) (bits-per-byte 8)
                            (octet-read-buffer-size 4096))
  (let ((stream (make-bitio octet-stream bitio/read-octet bitio/read-sequence
                            :bit-endian bit-endian :byte-endian byte-endian :bits-per-byte bits-per-byte
                            :octet-read-buffer-size octet-read-buffer-size)))
    (change-class stream 'bitio-input-stream)
    stream))

(defmethod stream-byte-endian (stream)
  (slot-value stream 'bitio::%default-byte-endian))
(defmethod stream-bit-endian (stream)
  (slot-value stream 'bitio::%default-bit-endian))

(defmethod (setf stream-byte-endian) (new-val (stream bitio-stream))
  (setf (slot-value stream 'bitio::%default-byte-endian) new-val))
(defmethod (setf stream-bit-endian) (new-val (stream bitio-stream))
  (setf (slot-value stream 'bitio::%default-bit-endian) new-val))

(defmethod input-stream-p ((stream bitio-stream))
  (typep stream 'bitio-input-stream))

(defmethod stream-file-position ((stream bitio-stream))
  "stream must implement:
   `stream-file-position, stream-element-type (also setf!)'"
  (nest
   (with-accessors ((octet-stream bitio::octet-stream)
                    (num-bits-in-stable bitio::num-bits-in-stable)
                    (byte-size bitio::default-bits-per-byte)) stream)
   (multiple-value-bind (normalized-fp rest)
       (floor (+ num-bits-in-stable (* (file-position octet-stream)
                                       (second (stream-element-type octet-stream))))
              byte-size)
     (values normalized-fp rest))))

(defmethod (setf stream-file-position) (position-spec (stream bitio-input-stream))
  (nest
   (with-accessors ((bit-endian bitio::default-bit-endian)
                    (byte-endian bitio::default-byte-endian)
                    (num-bits-in-stable bitio::num-bits-in-stable)
                    (read-bit-stable bitio::read-bit-stable)
                    (bits-per-byte bitio::default-bits-per-byte)
                    (octet-stream bitio::octet-stream)) stream)
   (let ((bit-position (* position-spec bits-per-byte))))
   (multiple-value-bind (octet-position rest) (floor bit-position 8)
     (file-position octet-stream octet-position)
     (setf read-bit-stable 0)
     (setf num-bits-in-stable 0)
     (read-bits stream rest)
     position-spec)))

(defmethod stream-read-byte ((stream bitio-input-stream))
  (read-one-byte stream))

(defmethod stream-read-sequence ((stream bitio-stream) sequence start end
                                 &key bit-endian bits-per-byte &allow-other-keys)
  (with-accessors ((default-bit-endian bitio::default-bit-endian)
                   (default-bits-per-byte bitio::default-bits-per-byte))
      stream
    (read-bytes stream sequence
                :start start
                :end end
                :bit-endian (or bit-endian default-bit-endian)
                :bits-per-byte (or bits-per-byte default-bits-per-byte))))

(defmethod stream-element-type ((stream bitio-stream))
  (values `(unsigned-byte ,(bitio::default-bits-per-byte stream))
          `(:byte-endian ,(bitio::default-byte-endian stream))
          `(:bit-endian ,(bitio::default-bit-endian stream))))

(with-packages-unlocked (cl)
  (defmethod (setf stream-element-type) (new-val (stream bitio-stream))
    (with-accessors ((byte-size bitio::default-bits-per-byte)) stream
      (assert (eq 'unsigned-byte (car new-val)))
      (setf byte-size (second new-val)))))

(defmethod peek-byte ((stream bitio-input-stream) &optional peek-type eof-error-p eof-value)
  (error "Not implemented for this class: ~s" (class-name (class-of stream))))
