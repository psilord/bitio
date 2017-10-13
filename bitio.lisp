(in-package :bitio)

(defclass bitio ()
  ((%octet-stream :initarg :octet-stream
                  :initform NIL
                  :reader octet-stream)
   ;; read-bit-stable is an unsigned integer that holds unprocessed
   ;; bits always in canonical ordering. However, depending on what
   ;; we're doing we might take bits from the MSB side or the LSB
   ;; side. The read-bit-stable is an octet worth of bits or less. Though
   ;; this may change in the future.
   (%read-bit-stable :initarg :read-bit-stable
                     :initform 0
                     :accessor read-bit-stable)
   ;; How many bits are currently in the stable.
   (%num-bits-in-stable :initarg :num-bits-in-stable
                        :initform 0
                        :accessor num-bits-in-stable)
   ;; The API to the octet-stream
   (%bitio/read-octet :initarg :bitio/read-octet
                      :initform NIL
                      :reader %bitio/read-octet)
   (%default-bit-endian :initarg :default-bit-endian
                        :initform :be
                        :reader default-bit-endian)
   (%default-byte-endian :initarg :default-byte-endian
                         :initform :le
                         :reader default-byte-endian)
   (%default-byte-width :initarg :default-byte-width
                        :initform 8
                        :reader default-byte-width)))

(defgeneric bitio/read-octet (bitio &optional eof-error-p eof-value)
  (:documentation "Read an octet from the funciton supplied with the stream
instance the bitio is wrapping."))

(defmethod bitio/read-octet (bitio &optional
                                     (eof-error-p T)
                                     (eof-value NIL))
  (funcall (%bitio/read-octet bitio)
           (octet-stream bitio)
           eof-error-p eof-value))


;; EXPORT
(defun make-bitio (octet-stream bitio/read-octet
                   &key (bit-endian :be) (byte-endian :le) (byte-width 8))
  "OCTET-STREAM must be a stream that is ready to read/write binary octets
of (unsigned-byte 8) type. BITIO/READ-OCTET is a function associated with
the OCTET-STREAM that reads a single octet from that stream. Returns an
instance of a BITIO class."
  (make-instance 'bitio
                 :octet-stream octet-stream
                 :bitio/read-octet bitio/read-octet
                 :default-bit-endian bit-endian
                 :default-byte-endian byte-endian
                 :default-byte-width byte-width))
