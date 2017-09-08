(in-package :bitio)

;; Make a system and name it 'bitio'

(defparameter *octet-bit-reverse-table*
  (make-array 256
              :element-type
              '(unsigned-byte 8)
              :initial-contents
              '(#x00 #x80 #x40 #xC0 #x20 #xA0 #x60 #xE0 #x10 #x90
                #x50 #xD0 #x30 #xB0 #x70 #xF0 #x08 #x88 #x48 #xC8
                #x28 #xA8 #x68 #xE8 #x18 #x98 #x58 #xD8 #x38 #xB8
                #x78 #xF8 #x04 #x84 #x44 #xC4 #x24 #xA4 #x64 #xE4
                #x14 #x94 #x54 #xD4 #x34 #xB4 #x74 #xF4 #x0C #x8C
                #x4C #xCC #x2C #xAC #x6C #xEC #x1C #x9C #x5C #xDC
                #x3C #xBC #x7C #xFC #x02 #x82 #x42 #xC2 #x22 #xA2
                #x62 #xE2 #x12 #x92 #x52 #xD2 #x32 #xB2 #x72 #xF2
                #x0A #x8A #x4A #xCA #x2A #xAA #x6A #xEA #x1A #x9A
                #x5A #xDA #x3A #xBA #x7A #xFA #x06 #x86 #x46 #xC6
                #x26 #xA6 #x66 #xE6 #x16 #x96 #x56 #xD6 #x36 #xB6
                #x76 #xF6 #x0E #x8E #x4E #xCE #x2E #xAE #x6E #xEE
                #x1E #x9E #x5E #xDE #x3E #xBE #x7E #xFE #x01 #x81
                #x41 #xC1 #x21 #xA1 #x61 #xE1 #x11 #x91 #x51 #xD1
                #x31 #xB1 #x71 #xF1 #x09 #x89 #x49 #xC9 #x29 #xA9
                #x69 #xE9 #x19 #x99 #x59 #xD9 #x39 #xB9 #x79 #xF9
                #x05 #x85 #x45 #xC5 #x25 #xA5 #x65 #xE5 #x15 #x95
                #x55 #xD5 #x35 #xB5 #x75 #xF5 #x0D #x8D #x4D #xCD
                #x2D #xAD #x6D #xED #x1D #x9D #x5D #xDD #x3D #xBD
                #x7D #xFD #x03 #x83 #x43 #xC3 #x23 #xA3 #x63 #xE3
                #x13 #x93 #x53 #xD3 #x33 #xB3 #x73 #xF3 #x0B #x8B
                #x4B #xCB #x2B #xAB #x6B #xEB #x1B #x9B #x5B #xDB
                #x3B #xBB #x7B #xFB #x07 #x87 #x47 #xC7 #x27 #xA7
                #x67 #xE7 #x17 #x97 #x57 #xD7 #x37 #xB7 #x77 #xF7
                #x0F #x8F #x4F #xCF #x2F #xAF #x6F #xEF #x1F #x9F
                #x5F #xDF #x3F #xBF #x7F #xFF)))

;; A fast means to reverse the bits of an octet
(defun octet-reverse (octet)
  (aref *octet-bit-reverse-table* octet))


;; Reverse all bits in a canonical integer up to the bit-max
;; This is kinda slow, but generic.
(defun integer-reverse (num num-bits)
  (let ((value 0))
    (loop :for i :below num-bits :do
      (setf (ldb (byte 1 (- (1- num-bits) i)) value)
            (ldb (byte 1 i) num)))
    value))


(defclass bitio ()
  ((%octet-stream :initarg :octet-stream
                  :initform NIL
                  :reader octet-stream)
   ;; bit-stable is an unsigned integer that holds unprocessed
   ;; bits always in canonical ordering. However, depending on what
   ;; we're doing we might take bits from the MSB side or the LSB
   ;; side. The bit-stable is an octet worth of bits or less. Though
   ;; this may change in the future.
   (%bit-stable :initarg :bit-stable
                :initform 0
                :accessor bit-stable)
   ;; How many bits are currently in the stable.
   (%num-bits-in-stable :initarg :num-bits-in-stable
                        :initform 0
                        :accessor num-bits-in-stable)

   ;; The API to the octet-stream
   (%bitio/read-octet :initarg :bitio/read-octet
                      :initform NIL
                      :reader %bitio/read-octet)))

(defgeneric bitio/read-octet (bitio &optional eof-error-p eof-value)
  (:documentation "Read an octet from the funciton supplied with the stream
instance the bitio is wrapping."))

(defmethod bitio/read-octet (bitio &optional
                                     (eof-error-p T)
                                     (eof-value NIL))
  (funcall (%bitio/read-octet bitio)
           (octet-stream bitio)
           eof-error-p eof-value))

;; Axion's old code.
(defun read-integer (byte-buffer &key (bytes 1) (bit-width 8) (unsignedp t) (endian :be))
  (let ((value 0))
    (ecase endian
      (:be (loop :for i :from (* (1- bytes) bit-width) :downto 0 :by bit-width
                 :for byte = (fast-io:fast-read-byte byte-buffer)
                 :do (setf (ldb (byte bit-width i) value) byte)))
      (:le (loop :for i :below (* bit-width bytes) :by bit-width
                 :for byte = (fast-io:fast-read-byte byte-buffer)
                 :do (setf (ldb (byte bit-width i) value) byte))))
    (if unsignedp
        value
        (logior (* (ldb (byte 1 (1- bytes)) value)
                   (- (expt 2 (* bytes bit-width))))
                value))))

;; Assumptions:
;; 1) The individual bits of an octet have canonical positions of:
;; [2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0]
;; within the octet. We label the left side of the octect as the MSBit and the
;; right side as the LSBit. The bit position of the right most bit is 0.
;;
;; 2) We assume that when reading octets from fast-io, all octets are in the
;; canonical form.
;;
;; 3) A canonical integer has its bits in the form of:
;; [2^N ... 2^3 2^2 2^1 2^0]
;; The bit position of the right most bit is 0.

(defun make-bitio (octet-stream bitio/read-octet &rest init-args)
  (apply #'make-instance 'bitio
         :octet-stream octet-stream
         :bitio/read-octet bitio/read-octet
         init-args))


(defun fill-stable (bitio &optional (eof-error-p T) (eof-value NIL))
  "This function asserts that the stable is empty, then fills it with a
single octet from the stream. Returns T if stable was filled or NIL if it
wasn't (due to an EOF)."
  (assert (zerop (num-bits-in-stable bitio)))

  (let ((result (bitio/read-octet bitio eof-error-p eof-value)))
    (cond
      ;; Oops, had an EOF
      ((equal result eof-value)
       (setf (num-bits-in-stable bitio) 0
             (bit-stable bitio) 0)
       NIL)
      ;; all good.
      (t
       (setf (num-bits-in-stable bitio) 8
             (bit-stable bitio) result)
       T))))

;; This is not entirely a fast-path since it doesn't read a sequence.
(defun fast-path/octet-aligned-bit-read (bitio bit-read-count bit-endian
                                         &optional (eof-error-p T)
                                           (eof-value NIL))
  "This function assumes the bit-stable is empty in the BITIO stream
and BIT-READ-COUNT is a multiple of 8.  It reads BIT-READ-COUNT bits
from the BITIO stream (reading the actual bits in each octet in
BIT-ENDIAN manner) and then places them into an integer that it returns.
The integer is in canonical form with the first bit read in the MSBit
position."
  (when (zerop bit-read-count)
    (return-from fast-path/octet-aligned-bit-read (values 0 0)))

  (let ((required-octets (/ bit-read-count 8))
        (value 0)
        (num-bits-read 0)
        (endian-func (if (eq bit-endian :be) #'identity #'octet-reverse)))

    (loop :for i :below required-octets
          :for octet = (bitio/read-octet bitio eof-error-p eof-value)
          :do
             ;; If we hit an eof, we may have collected some data
             ;; already into a value. In this case, we'll return a
             ;; short read. The user should check the bit-read-count
             ;; to ensure they got everything they were expecting.
             (when (and (not eof-error-p) (equal octet eof-value))
               (return-from fast-path/octet-aligned-bit-read
                 (values (if (> num-bits-read 0)
                             ;; Shift into canonical form to remove
                             ;; unreadable bits.
                             (ash value (- (- bit-read-count num-bits-read)))
                             ;; But if they insist upon the read again, they
                             ;; get the eof-value they supplied.
                             eof-value)
                         num-bits-read)))

             ;; Otherwise we know we can collect the bits.
             (incf num-bits-read 8)
             (setf (ldb (byte 8 (- (* (1- required-octets) 8) (* i 8))) value)
                   (funcall endian-func octet)))
    (values value num-bits-read)))


;; Return two values:
;; the value holding the bits in canonical form
;; the number of bits read
(defun consume-bit-stable (bitio bit-read-count bit-endian)
  (when (zerop (num-bits-in-stable bitio))
    (return-from consume-bit-stable (values 0 0)))

  ;; Figure out how many bits to read from the bit-stable
  (let* ((num-selected-bits (min (num-bits-in-stable bitio)
                                 bit-read-count))
         (bit-start-position (if (eq bit-endian :le)
                                 0
                                 (- (num-bits-in-stable bitio)
                                    num-selected-bits)))
         (byte-specification (byte num-selected-bits bit-start-position)))

    #++(format t "bs: ~8,'0B, n-s-b: ~A, b-s-p: ~A, b-s: ~A~%"
               (bit-stable bitio)
               num-selected-bits bit-start-position byte-specification)

    (let ((value 0))
      ;; First, we grab the bits we need out of the stable and put them in
      ;; the :le end of the value
      (setf value (ldb byte-specification (bit-stable bitio)))
      ;; Then, depending on bit endian, we clean up the value and bit stable.
      (case bit-endian
        (:le
         ;; Shift the bit stable to the right by the number of bits I collected.
         ;; This gets rid of those bits in the stable.
         (setf (bit-stable bitio)
               (ash (bit-stable bitio) (- num-selected-bits)))
         ;; Then reverse the bits, cause they are to come out in :le order
         (let ((result (integer-reverse value num-selected-bits)))
           ;; remove the accounting of the bits from the stable.
           (decf (num-bits-in-stable bitio) num-selected-bits)
           (values result num-selected-bits)))

        (:be
         ;; Zero out the :be stable bits we just took out.
         (setf (ldb byte-specification (bit-stable bitio))
               0)
         ;; remove the accounting of the bits from the stable.
         (decf (num-bits-in-stable bitio) num-selected-bits)
         (values value num-selected-bits))))))

(defun slow-path/octet-unaligned-bit-read (bitio bit-read-count bit-endian
                                           &optional (eof-error-p T)
                                             (eof-value NIL))

  ;; This work occurs in three phases.

  (let ((bits-remaining-to-read bit-read-count))
    ;; Phase I, we consume anything we need from the stable.
    ;; Note: we can only consume up to the amount in the stable, so
    ;; if there is an EOF we cannot detect it here yet.
    (multiple-value-bind (prefix-stable-bits num-prefix-stable-bits-read)
        (consume-bit-stable bitio bits-remaining-to-read bit-endian)

      (decf bits-remaining-to-read num-prefix-stable-bits-read)

      ;; Determine how many complete octets we can read and
      ;; how many left over bits we have with respect to an octet width.
      (multiple-value-bind (octets-to-read bits-left-over)
          (floor bits-remaining-to-read 8)

        ;; Phase II, Fast read that many octets
        ;; Note: Here is the first place we can discover an EOF.
        (multiple-value-bind (octet-bits num-octet-bits-read)
            (fast-path/octet-aligned-bit-read bitio
                                              (* octets-to-read 8)
                                              bit-endian
                                              eof-error-p
                                              eof-value)
          (decf bits-remaining-to-read num-octet-bits-read)

          ;; Short read / EOF check.
          ;; Detect if we reached a short read or EOF during the fast
          ;; read attempt.
          (cond
            ;; oops! EOF Found on octet read
            ((equal octet-bits eof-value)
             ;; Check if we had read any bits from the stable...
             (if (> num-prefix-stable-bits-read 0)
                 ;; We did! So return them as a short read.
                 (return-from slow-path/octet-unaligned-bit-read
                   (values prefix-stable-bits num-prefix-stable-bits-read))
                 ;; We didn't! So return the true eof.
                 (return-from slow-path/octet-unaligned-bit-read
                   (values octet-bits num-octet-bits-read))))

            ;; Octet-read ok, but short read just before EOF.
            ((/= num-octet-bits-read (* octets-to-read 8))
             ;; Here we're assuming we may or may not have read
             ;; something from the stable.
             (return-from slow-path/octet-unaligned-bit-read
               (values (logior octet-bits
                               (ash prefix-stable-bits num-octet-bits-read))
                       (+ num-octet-bits-read num-prefix-stable-bits-read)))))


          ;; Phase III, if there are bits-left-over, they will always fit
          ;; into a octet, so fill the stable...
          (when (> bits-left-over 0)
            (let ((eof-p (fill-stable bitio eof-error-p eof-value)))
              ;; However, we check for EOF.
              (unless eof-p
                (return-from slow-path/octet-unaligned-bit-read
                  ;; We discovered that we can't fill the remaining
                  ;; bits because we hit an EOF while getting the
                  ;; octet.
                  ;;
                  ;; So, determine if we have any bits ready to go, and
                  ;; if not, then bail with the true eof value, otherwise,
                  ;; return the necessary bits. And the EOF will get processed
                  ;; in the next call.
                  (if (> (+ num-octet-bits-read num-prefix-stable-bits-read) 0)
                      ;; return whatever bits we copuld have read up to now.
                      (values
                       (logior octet-bits
                               (ash prefix-stable-bits num-octet-bits-read))
                       (+ num-octet-bits-read num-prefix-stable-bits-read))
                      ;; we're done, eof found, full stop.
                      (values eof-value 0))))))


          (assert (eql bits-remaining-to-read bits-left-over))

          ;; And consume any bits-left-over. We can't get an EOF consuming
          ;; from the stable once it has been filled.
          (multiple-value-bind (suffix-stable-bits
                                num-suffix-stable-bits-read)
              (consume-bit-stable bitio bits-remaining-to-read bit-endian)

            ;; Now, we have three groups of bits, assemble them into the
            ;; answer we seek.
            (let ((result
                    (logior suffix-stable-bits
                            (ash octet-bits
                                 num-suffix-stable-bits-read)
                            (ash prefix-stable-bits
                                 (+ num-suffix-stable-bits-read
                                    num-octet-bits-read)))))

              (values result (+ num-prefix-stable-bits-read
                                num-octet-bits-read
                                num-suffix-stable-bits-read)))))))))

;; EXPORT
;; This is the thing that gets the bits from the octet stream. It returns them
;; as a values of the integer and how many bits read. Return a values of
;; the bits in canonical order in an integer, and how many bits are in that
;; canonical integer.
(defun bit-read-bits (bitio bit-read-count bit-endian
                      &optional (eof-error-p T) (eof-value NIL))
  (when (zerop bit-read-count)
    (return-from bit-read-bits (values 0 0)))

  (cond
    ;; This is a fast path read of the bits.
    ((and
      ;; nothing in the stable.
      (zerop (num-bits-in-stable bitio))
      ;; asking to read a divisible by 8 number of bits (so I can get them
      ;; as complete octets from the octet-stream)
      (zerop (mod bit-read-count 8)))

     (fast-path/octet-aligned-bit-read bitio
                                       bit-read-count bit-endian
                                       eof-error-p eof-value))

    ;; This is the slow path, where I may need to handle partial octets
    ;; and bits stored in the stable.
    (t
     (slow-path/octet-unaligned-bit-read bitio
                                         bit-read-count bit-endian
                                         eof-error-p eof-value))))

;; EXPORT
(defun bit-read-byte (bitio byte-width bit-endian
                      &optional (eof-error-p T) (eof-value NIL))
  "Read a single unsigned 'byte' from the bitio stream. You must specify
the BIT-ENDIAN mode (:be or :le) and how big the byte is in bits, the
BYTE-WIDTH. You can supply the optional keywords EOF-ERROR-P and
EOF-VALUE as in READ-BYTE). The returned value is always unsigned."
  (bit-read-bits bitio byte-width bit-endian eof-error-p eof-value))

;; EXPORT
;; TODO: Check what happens when short read ends in a partially available byte,
;; what is the right action to do in that case?
(defun bit-read-sequence (bitio seq bit-endian byte-width
                          &key (start 0) end)
  "This reads UNSIGNED 'bytes' into SEQ given :START and :END keywords.
THe default span is the entire sequence. BIT-ENDIAN is how the
individual bits are read from the octet stream, and byte-width is how
many bits wide a 'byte' is in the stream. Return how many elements
have been read. The sequence is destructively modified. At EOF
conditions, a short read will happen for the last element read (and
there is no notification of this) or the function will return 0."
  (let ((end (if (null end) (length seq) end)))
    (loop :for num-read :from 0
          :for i :from start :below end
          :for the-byte = (bit-read-bits bitio byte-width bit-endian NIL :eof)
          :do (when (equal :the-byte :eof)
                (return-from bit-read-sequence num-read))
              (incf num-read)
              (setf (aref seq i) the-byte)))
  (length seq))


(defun sign-extend (potential-signed-value bit-width)
  (logior (* (ldb (byte 1 (1- bit-width)) potential-signed-value)
             (- (expt 2 bit-width)))
          potential-signed-value))

;; EXPORT
;; This thing interprets bits read by bit-read-bits as a signed or unsigned
;; integer as appropriate.
(defun bit-read-integer (bitio
                         &key
                           ;; next one is passed to bit-read.
                           (bit-endian :be)
                           ;; This is related to endianess of the integer.
                           (byte-endian :le)
                           ;; Default number of bytes to read
                           (num-bytes 4)
                           ;; Bytes have this many bits in them.
                           (byte-width 8)
                           ;; T for unsigned, NIL for signed.
                           (unsignedp T))

  (let ((value 0))
    (ecase byte-endian
      (:be (loop
             :for i :from (* (1- num-bytes) byte-width) :downto 0 :by byte-width
             :for byte = (bit-read-bits bitio byte-width bit-endian)
             :do (setf (ldb (byte byte-width i) value) byte)))
      (:le (loop
             :for i :below (* byte-width num-bytes) :by byte-width
             :for byte = (bit-read-bits bitio byte-width bit-endian)
             :do (setf (ldb (byte byte-width i) value) byte))))
    (if unsignedp
        value
        (sign-extend value (* num-bytes byte-width)))))


(defun bit-octet-read-boundary-p (bitio)
  "Return T if the reading of the bit stream is at an octet boundary.
NIL otherwise."
  (zerop (num-bits-in-stable bitio)))


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
(defun test-bit-read-sequence (bitio seq bit-endian byte-width
                               expected-seq &key (start 0) end)

  (let ((num-parts-read (bit-read-sequence
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

    (format t "Case: bit-read-sequence, bit-width 4, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :be 4
                                  #(#x5 #xc #xf #x6 #xe #xe #x7 #x9 #x9 #xa
                                    #xd #xe #xf #xf #xf #x2 #x8 #x8 #x0 #x2)))
        ))

    (format t "Case: bit-read-sequence, bit-width 4, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) .5))
                               :element-type '(unsigned-byte 4)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :le 4
                                  (map 'vector
                                       (lambda (x) (integer-reverse x 4))
                                       #(#xc #x5 #x6 #xf #xe #xe #x9 #x7
                                         #xa #x9 #xe #xd #xf #xf #x2 #xf
                                         #x8 #x8 #x2 #x0))))
        ))

    (format t "Case: bit-read-sequence, bit-width 8, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :be 8
                                  #(#x5c #xf6 #xee #x79 #x9a
                                    #xde #xff #xf2 #x88 #x02)))
        ))

    (format t "Case: bit-read-sequence, bit-width 8, bit-endian :le~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1))
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :le 8
                                  (map 'vector
                                       (lambda (x) (integer-reverse x 8))
                                       #(#x5c #xf6 #xee #x79 #x9a
                                         #xde #xff #xf2 #x88 #x02))))
        ))

    (format t "Case: bit-read-sequence, bit-width 12, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 1.5))
                               :element-type '(unsigned-byte 12)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :be 12
                                  #(#x5cf #x6ee #x799 #xade #xfff #x288 #x02)))
        ))

    (format t "Case: bit-read-sequence, bit-width 16, bit-endian :be~%")
    (fast-io:with-fast-input (fiobuf octet-vector)
      (let ((bitio (make-bitio fiobuf #'fast-io:fast-read-byte)))
        (let ((seq (make-array (ceiling (/ (length octet-vector) 2))
                               :element-type '(unsigned-byte 16)
                               :initial-element 0)))

          (test-bit-read-sequence bitio seq :be 16
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


    (format t "All done.~%")
    ))
