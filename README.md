BITIO: A Common Lisp library for processing octet streams as bit streams.

This library can currently only provide READING stream of bits. Later
I'll add WRITING a stream of bits.

Reading bits as a bit stream is a funny business and requires a bit of
explanation (no pun intended) of the edge cases and structural format
of how the bits are layed out on disk, in memory, and in multi-part integers.

However, before getting into the nitty gritty, I'll just provide some
examples which might be enough for you.

```lisp
;; Suppose we want to read a binary octet stream where we must read in order:
;; 32 bits as a signed integer in little endian format, then
;; 16 bits as a signed integer in big endian format, then
;; 3 bits as an unsigned integer, then
;; 8 bits as an unsigned integer, then
;; 5 bits as an unsigned integer, then
;; read a number of octets denoted by the 8-bit number,
;; for a total of 8 + N bytes read.

;; Usually, to implement the 3/8/5 bit reads, you'd read 2 bytes, then
;; do a pile of bit masking/shifting/anding, etc in order to retrive
;; the values you desire.  Here is a possible (of several, actually)
;; rendering of the above in bitio:

(with-open-file (fin (asdf:system-relative-pathname :bitio "binfile")
                     :direction :input
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :error)
  ;; wrap fin stream with a bitio stream.
  (let ((bitio (make-bitio fin #'read-byte)))
    (let ((32-bits (bitio:bit-read-integer bitio :unsignedp NIL))
          (16-bits (bitio:bit-read-integer bitio :num-bytes 2 :byte-endian :be
	                                   :unsignedp NIL))
          (3-bits (bitio:bit-read-integer bitio :num-bytes 1 :byte-width 3))
	  (8-bits (bitio:bit-read-integer bitio :num-bytes 1))
	  (5-bits (bitio:bit-read-integer bitio :num-bytes 1 :byte-width 5))
	  (seq (make-array 8-bit :element-type '(unsigned-byte 8)))
	  (num-read (bitio:bit-read-bytes bitio seq :be 8)))

      <do someting with variables, and seq>

    ))
```

TODO: Continue.
