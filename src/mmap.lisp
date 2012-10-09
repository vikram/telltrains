(in-package :com.sondesh.database)

(require 'sb-posix)

(defun create-lisp-mem ()
  (declare (optimize (speed 1)))
  (with-open-file (out (sondesh-file-path "additional/G01.trains.mem")
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 32))
    (write-byte sb-vm:simple-array-unsigned-byte-32-widetag out)
    (write-byte 8 out)
    (let ((ht (make-hash-table :test 'equal))
	  (from-crs-id (gethash "G01" *crs->id*))
	  (count 0))
      (load-permitted-trains "G01" ht)
      (maphash #'(lambda (key values) 
		   (let ((to-crs-id (gethash (subseq key 3 6) *crs->id*)))
		     (if to-crs-id
			 (progn
			   (incf count)
			   (write-byte (ash from-crs-id sb-vm::n-fixnum-tag-bits) out)
			   (write-byte (ash to-crs-id sb-vm::n-fixnum-tag-bits) out))
			 (print key))))
	       ht)
      (file-position out 1)
      (write-byte (ash count sb-vm::n-fixnum-tag-bits) out))))

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
      
