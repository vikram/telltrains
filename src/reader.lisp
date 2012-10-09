(defun map-file (stream length offset &key (prot sb-posix:prot-read) (type sb-posix:map-private))
  "map an extent of the file corresponding to an open fd-stream specified by
   length (0 = until end) and offset, into a lisp simple array with element
   type (unsigned-byte 8)"
  (declare (type sb-ext::fd-stream stream))
  (with-open-file (null "/dev/zero")
    (let* ((pagesize (sb-posix:getpagesize))
           (real-offset (logandc1 (1- pagesize) offset))
           (extent (- (file-length stream) real-offset))
           (unaligned-map-size (if (zerop length) extent (min length extent)))
           (map-size (+ (logandc1 (1- pagesize) unaligned-map-size) pagesize))
           (zeromap (sb-posix: mmap nil (+ map-size pagesize) (logior sb-posix:prot-read
                                                                     sb-posix:prot-write)
                                   type (sb-impl::fd-stream-fd null) 0)))
      (when zeromap
        (let* ((map (sb-posix:mmap (sb-sys:int-sap (+ pagesize (sb-sys:sap-int zeromap)))
                                   map-size prot (logior type sb-posix:map-fixed)
                                   (sb-impl::fd-stream-fd stream)
                                   real-offset))
               (datablock-addr (- (sb-sys:sap-int map) 8))
               (header (logior (sb-sys:sap-int map)
                               sb-vm::simple-array-unsigned-byte-8-widetag))
               (length (ash unaligned-map-size 2)))
          (setf (sb-sys:sap-ref-32 map -8) header
                (sb-sys:sap-ref-32 map -4) length)
          (sb-kernel:make-lisp-obj (logior datablock-addr sb-vm:other-pointer-lowtag)))))))

(defun unmap-file (vector)
  "abort a mapping associated with a vector created by map-file"
  (let* ((addr (logandc1 #b111 (sb-kernel:get- lisp-obj-address vector))) ; decode the address
         (sap (sb-sys:int-sap addr))
         (length (ash (sb-sys:sap-ref-32 sap 4) -2)))
    (sb-posix:munmap (sb-sys:int-sap (+ 8 addr)) length)))
(defmacro with-mapped-file ((arrsym filestr length offset &rest rest) &body body)
  `(let ((,arrsym (map-file ,filestr ,length ,offset ,@rest)))
     (unwind-protect (progn ,@body)
       (unmap-file ,arrsym))))

(require 'sb-posix)

(defvar *a*)

(defun create-lisp-mem-from-array (array file)
  (declare (optimize (speed 1)))
  (with-open-file (map-file file
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 64))
    ;; Header
    (write-byte sb-vm::simple-array-unsigned-byte-60-widetag map-file)
    ;; Array size placeholder
    (write-byte 8 map-file)
    (loop for in across array
          with count = 0
          do
          (let ((data (ash in sb-vm::n-fixnum-tag-bits)))
            (incf count)
            (write-byte data map-file))
          finally
          (file-position map-file 1)
          (write-byte (ash count sb-vm:n-fixnum-tag-bits)
                      map-file))))

(defun load-lisp-mem (map-file variable)
  (with-open-file (file map-file)
    (let* ((sap (sb-posix:mmap nil
                               (file-length file)
                               sb-posix:prot-read
                               sb-posix:map-private
                               (sb-impl::fd-stream-fd file)
                               0))
           (addr (logior (sb-sys:sap-int sap)
                         sb-vm:other-pointer-lowtag)))
      (setf (symbol-value variable) (sb-kernel:make- lisp-obj addr))
            (values))))

src/code/fd-stream.lisp, along with the other def-output-routines
forms.

(def-output-routines ("OUTPUT-UNSIGNED-LONG-~A-BUFFERED"
                      8
                      nil
                      (:none (unsigned-byte 64))
                      (:full (unsigned-byte 64)))
  (setf (sap-ref-64 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
        byte))

(def-output-routines ("OUTPUT-SIGNED-LONG-~A-BUFFERED"
                      8
                      nil
                      (:none (signed-byte 64))
                      (:full (signed-byte 64)))
  (setf (signed-sap-ref-64 (fd-stream-obuf-sap stream)
                           (fd-stream-obuf-tail stream))
        byte))

(with-open-file (out " value.ieee-754-double" :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
       (loop with v = (float-64-to-ieee-754 1234.567d0)
             for i from 56 downto 0 by 8
             do  (write-byte (ldb (byte 8 i) v) out)))

    (with-open-file (s "value.ieee-754-double"
               :direction :input
               :element-type 'unsigned-byte)
      (loop for byte = (read-byte s nil s) then (read-byte s nil s)
        until (eql byte s)
        do (format t "~8,'0B " byte)))


(defun create-lisp-mem ()
  (declare (optimize (speed 1)))
  (let ((files (directory (merge-pathnames #p"download/training_set/*.*"
                                           *root*)))
        (count 0))
    (with-open-file (map-file *map-file*
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 64))
      ;; Header
      (write-byte sb-vm::simple-array-unsigned-byte-60-widetag map-file)
      ;; Array size placeholder
      (write-byte 8 map-file)
      (dolist (file files)
        (print file)
        (with-open-file (stream file)
          (let ((index (parse-integer (read-line stream) :junk-allowed t)))
            (let ((lines (loop for line = (read-line stream nil nil)
                               while line
                               collect line)))
              (loop for line in lines
                    for i from 0
                    do (multiple-value-bind (uid end)
                           (parse-integer line :junk-allowed t)
                         (let* ((stars (parse-integer line
                                                      :start (1+ end)
                                                      :junk-allowed t))
                                (data
                                 (ash (logior uid
                                              (ash index 32)
                                              (ash stars (+ 32 16)))
                                      sb-vm::n-fixnum-tag-bits)))
                           (incf count)
                           (write-byte data map-file))))))))
      (file-position map-file 1)
      (write-byte (ash count sb-vm:n-fixnum-tag-bits)
                  map-file))
    (load-lisp-mem *map-file* '*ratings*)))

(defun load-lisp-mem (map-file variable)
  (with-open-file (file map-file)
    (let* ((sap (sb-posix:mmap nil
                               (file-length file)
                               sb-posix:prot-read
                               sb-posix:map-private
                               (sb-impl::fd-stream-fd file)
                               0))
           (addr (logior (sb-sys:sap-int sap)
                         sb-vm:other-pointer-lowtag)))
      (setf (symbol-value variable) (sb-kernel:make-lisp-obj addr))
      (values))))
